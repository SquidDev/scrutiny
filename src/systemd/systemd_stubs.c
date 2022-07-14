#define SD_JOURNAL_SUPPRESS_LOCATION

#include <errno.h>
#include <malloc.h>
#include <stdbool.h>

#include <systemd/sd-bus.h>
#include <systemd/sd-journal.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/io.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

#define _cleanup_(f) __attribute__((cleanup(f)))

/*******************************************************************************
 * sd-journal
 ******************************************************************************/

#define Journal_val(v) (*(struct sd_journal **)Data_custom_val(v))

#define CHECK_OPEN(name, v)                                                                                            \
  if (v == NULL) caml_failwith(#name ": attempt to use closed journal.")
#define CHECK_OK(name, r)                                                                                              \
  if (r < 0) unix_error(-r, #name, Nothing)

static void finalize_sd_journal(value v) {
  sd_journal *journal = Journal_val(v);
  if (journal == NULL) return;

  sd_journal_close(journal);
  Journal_val(v) = NULL;
}

static struct custom_operations sd_journal_ops = {
    .identifier = "scrutiny.sd_journal",
    .finalize = finalize_sd_journal,
    .compare = custom_compare_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .compare_ext = custom_compare_ext_default,
    .fixed_length = custom_fixed_length_default,
};

CAMLprim value scrutiny_sd_journal_open(value flags) {
  CAMLparam1(flags);
  CAMLlocal1(v);

  sd_journal *journal;

  int r = sd_journal_open(&journal, Int_val(flags));
  CHECK_OK(sd_journal_open, r);

  v = caml_alloc_custom(&sd_journal_ops, sizeof(struct sd_journal *), 0, 1);
  Journal_val(v) = journal;
  CAMLreturn(v);
}

CAMLprim value scrutiny_sd_journal_close(value v) {
  CAMLparam1(v);

  sd_journal *journal = Journal_val(v);
  CHECK_OPEN(scrutiny_sd_journal_close, journal);

  sd_journal_close(journal);
  Journal_val(v) = NULL;

  CAMLreturn(Val_unit);
}

CAMLprim value scrutiny_sd_journal_get_fd(value v) {
  CAMLparam1(v);

  sd_journal *journal = Journal_val(v);
  CHECK_OPEN(scrutiny_sd_journal_get_fd, journal);

  int r = sd_journal_get_fd(journal);
  CHECK_OK(sd_journal_get_fd, r);

  CAMLreturn(Val_long(r));
}

CAMLprim value scrutiny_sd_journal_get_events(value v) {
  CAMLparam1(v);

  sd_journal *journal = Journal_val(v);
  CHECK_OPEN(scrutiny_sd_journal_get_events, journal);

  int r = sd_journal_get_events(journal);
  CHECK_OK(sd_journal_get_events, r);

  CAMLreturn(Val_long(r));
}

CAMLprim value scrutiny_sd_journal_process(value v) {
  CAMLparam1(v);

  sd_journal *journal = Journal_val(v);
  CHECK_OPEN(scrutiny_sd_journal_process, journal);

  int r = sd_journal_process(journal);
  CHECK_OK(sd_journal_process, r);

  CAMLreturn(Val_int(r));
}

CAMLprim value scrutiny_sd_journal_next(value v) {
  CAMLparam1(v);

  sd_journal *journal = Journal_val(v);
  CHECK_OPEN(scrutiny_sd_journal_next, journal);

  int r = sd_journal_next(journal);
  CHECK_OK(sd_journal_next, r);

  CAMLreturn(Val_bool(r > 0));
}

CAMLprim value scrutiny_sd_journal_seek_tail(value v) {
  CAMLparam1(v);

  sd_journal *journal = Journal_val(v);
  CHECK_OPEN(scrutiny_sd_journal_seek_tail, journal);

  int r = sd_journal_seek_tail(journal);
  CHECK_OK(sd_journal_seek_tail, r);

  // Ensure we're pointing at the last record, not beyond the last. Otherwise we seem to get a huge, random selection of
  // records.
  r = sd_journal_previous(journal);
  ;
  CHECK_OK(sd_journal_previous, r);

  CAMLreturn(Val_unit);
}

CAMLprim value scrutiny_sd_journal_get_data(value v1, value v2) {
  CAMLparam2(v1, v2);
  CAMLlocal1(result);

  sd_journal *journal = Journal_val(v1);
  CHECK_OPEN(scrutiny_sd_journal_get_data, journal);

  const char *field = String_val(v2);
  mlsize_t field_length = caml_string_length(v2) + 1;

  const void *data;
  size_t size;
  int r = sd_journal_get_data(journal, field, &data, &size);
  if (r == -ENOENT) CAMLreturn(Val_none);
  CHECK_OK(scrutiny_sd_journal_get_data, r);

  result = caml_alloc_initialized_string(size - field_length, (char *)data + field_length);
  CAMLreturn(caml_alloc_some(result));
}

#define USEC_PER_SEC ((uint64_t)1000000ULL)

CAMLprim value scrutiny_sd_journal_get_realtime_usec(value v1) {
  CAMLparam1(v1);

  sd_journal *journal = Journal_val(v1);
  CHECK_OPEN(scrutiny_sd_journal_get_realtime_usec, journal);

  uint64_t time;
  int r = sd_journal_get_realtime_usec(journal, &time);
  CHECK_OK(sd_journal_get_realtime_usec, r);

  time_t seconds = (time_t)(time / USEC_PER_SEC);
  CAMLreturn(caml_copy_double((double)(seconds)));
}

CAMLprim value scrutiny_sd_journal_sendv_array(value fields) {
  CAMLparam1(fields);
  CAMLlocal1(item);

  size_t count = caml_array_length(fields);
  struct iovec *field_vecs = caml_stat_alloc(sizeof(struct iovec) * count);
  for (size_t i = 0; i < count; i++) {
    item = Field(fields, i);
    field_vecs[i].iov_len = caml_string_length(item);
    field_vecs[i].iov_base = String_val(item);
  }
  sd_journal_sendv(field_vecs, count);
  caml_stat_free(field_vecs);

  CAMLreturn(Val_unit);
}

CAMLprim value scrutiny_sd_journal_sendv_list(value fields) {
  CAMLparam1(fields);
  CAMLlocal2(head, item);

  /* Count the size of the list. */
  size_t count = 0;
  head = fields;
  while (head != Val_emptylist) {
    count++;
    head = Field(head, 1);
  }

  struct iovec *field_vecs = caml_stat_alloc(sizeof(struct iovec) * count);
  head = fields;
  for (size_t i = 0; i < count; i++) {
    item = Field(head, 0);
    field_vecs[i].iov_len = caml_string_length(item);
    field_vecs[i].iov_base = String_val(item);
    head = Field(head, 1);
  }
  sd_journal_sendv(field_vecs, count);
  caml_stat_free(field_vecs);

  CAMLreturn(Val_unit);
}

#undef CHECK_OPEN

/*******************************************************************************
 * sd-bus
 ******************************************************************************/

#define Bus_val(v) (*(struct sd_bus **)Data_custom_val(v))
#define CHECK_OPEN(name, v)                                                                                            \
  if (v == NULL) caml_failwith(#name ": attempt to use closed bus.")

static void finalize_sd_bus(value v) {
  sd_bus *bus = Bus_val(v);
  if (bus == NULL) return;
  Bus_val(v) = sd_bus_flush_close_unref(bus);
}

static struct custom_operations sd_bus_ops = {
    .identifier = "scrutiny.sd_bus",
    .finalize = finalize_sd_bus,
    .compare = custom_compare_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .compare_ext = custom_compare_ext_default,
    .fixed_length = custom_fixed_length_default,
};

CAMLprim value scrutiny_sd_bus_open_user() {
  CAMLparam0();
  CAMLlocal1(v);

  sd_bus *bus;
  int r = sd_bus_open_user(&bus);
  CHECK_OK(sd_bus_open_user, r);

  v = caml_alloc_custom(&sd_bus_ops, sizeof(struct bus *), 0, 1);
  Bus_val(v) = bus;
  CAMLreturn(v);
}

CAMLprim value scrutiny_sd_bus_open_system() {
  CAMLparam0();
  CAMLlocal1(v);

  sd_bus *bus;
  int r = sd_bus_open_system(&bus);
  CHECK_OK(sd_bus_open_system, r);

  v = caml_alloc_custom(&sd_bus_ops, sizeof(struct bus *), 0, 1);
  Bus_val(v) = bus;
  CAMLreturn(v);
}

CAMLprim value scrutiny_sd_bus_open_machine(value username) {
  CAMLparam1(username);
  CAMLlocal1(v);

  sd_bus *bus;
  int r = sd_bus_open_user_machine(&bus, String_val(username));
  CHECK_OK(sd_bus_open_user_machine, r);

  v = caml_alloc_custom(&sd_bus_ops, sizeof(struct bus *), 0, 1);
  Bus_val(v) = bus;
  CAMLreturn(v);
}

CAMLprim value scrutiny_sd_bus_open_address(value path) {
  CAMLparam1(path);
  CAMLlocal1(v);

  sd_bus *bus;
  int r = sd_bus_new(&bus);
  CHECK_OK(sd_bus_new, r);

  r = sd_bus_set_address(bus, String_val(path));
  CHECK_OK(sd_bus_set_address, r);

  r = sd_bus_start(bus);
  CHECK_OK(sd_bus_start, r);

  v = caml_alloc_custom(&sd_bus_ops, sizeof(struct bus *), 0, 1);
  Bus_val(v) = bus;
  CAMLreturn(v);
}

CAMLprim value scrutiny_sd_bus_close(value v) {
  CAMLparam1(v);

  sd_bus *bus = Bus_val(v);
  CHECK_OPEN(scrutiny_sd_bus_close, bus);

  Bus_val(v) = sd_bus_flush_close_unref(bus);

  CAMLreturn(Val_unit);
}

CAMLprim value scrutiny_sd_bus_get_fd(value v) {
  CAMLparam1(v);

  sd_bus *bus = Bus_val(v);
  CHECK_OPEN(scrutiny_sd_bus_get_fd, bus);

  int r = sd_bus_get_fd(bus);
  CHECK_OK(sd_bus_get_fd, r);
  CAMLreturn(Val_long(r));
}

CAMLprim value scrutiny_sd_bus_process(value v_bus) {
  CAMLparam1(v_bus);
  CAMLlocal1(res);

  sd_bus *bus = Bus_val(v_bus);
  CHECK_OPEN(scrutiny_sd_bus_process, bus);

  while (1) {
    int r = sd_bus_process(bus, NULL);
    CHECK_OK(sd_bus_process, r);
    if (r == 0) break;
  }

  CAMLreturn(Val_unit);
}

#define DESTINATION "org.freedesktop.systemd1"
#define PATH "/org/freedesktop/systemd1"
#define IFACE_MANAGER "org.freedesktop.systemd1.Manager"

/** Callbacks get put into a box. */
value *scrutiny_sd_register_root(value v) {
  value *box = caml_stat_alloc(sizeof(value));
  if (box == NULL) unix_error(ENOMEM, "malloc", Nothing);
  *box = v;
  caml_register_generational_global_root(box);
  return box;
}

/** And then released when the object is destroyed. */
void scrutiny_sd_bus_destroy(void *userdata) {
  value *box = (void *)userdata;
  caml_remove_generational_global_root(box);
  caml_stat_free(userdata);
}

#define CHECK_MESSAGE_OK(m)                                                                                            \
  if (sd_bus_message_is_method_error(m, NULL)) {                                                                       \
    const sd_bus_error *error = sd_bus_message_get_error(m);                                                           \
    res = caml_alloc_small(2, 1);                                                                                      \
    Store_field(res, 0, caml_copy_string(error->name));                                                                \
    Store_field(res, 1, caml_copy_string(error->message));                                                             \
    CAMLreturn(res);                                                                                                   \
  }

#define QUOTE_WORKER(x) #x
#define QUOTE(x) QUOTE_WORKER(x)

#define CHECK_MESSAGE_PARSE(r)                                                                                         \
  if (r < 0) {                                                                                                         \
    res = caml_alloc_small(1, 2);                                                                                      \
    Store_field(res, 0, unix_error_of_code(-r));                                                                       \
    Store_field(res, 1, caml_copy_string("Parsing message at " __FILE__ ":" QUOTE(__LINE__)));                         \
    CAMLreturn(res);                                                                                                   \
  }

#define DEFINE_BUS_CALLBACK(meth_name)                                                                                 \
  int meth_name(sd_bus_message *m, void *userdata, sd_bus_error *ret_error) {                                          \
    value *box = (value *)userdata;                                                                                    \
    caml_callback(*box, meth_name##_impl(m));                                                                          \
    return 0;                                                                                                          \
  }

value scrutiny_sd_bus_object_path_callback_impl(sd_bus_message *m) {
  CAMLparam0();
  CAMLlocal1(res);

  CHECK_MESSAGE_OK(m);

  const char *s;
  int r = sd_bus_message_read_basic(m, SD_BUS_TYPE_OBJECT_PATH, &s);
  CHECK_MESSAGE_PARSE(r);

  res = caml_alloc_small(1, 0);
  Store_field(res, 0, caml_copy_string(s));
  CAMLreturn(res);
}

DEFINE_BUS_CALLBACK(scrutiny_sd_bus_object_path_callback)

value scrutiny_sd_bus_empty_callback_impl(sd_bus_message *m) {
  CAMLparam0();
  CAMLlocal1(res);

  CHECK_MESSAGE_OK(m);

  res = caml_alloc_small(1, 0);
  Store_field(res, 0, Val_unit);
  CAMLreturn(res);
}

DEFINE_BUS_CALLBACK(scrutiny_sd_bus_empty_callback)

/*******************************************************************************
 * sd-bus => org.freedesktop.DBus.Properties.Get(ss)
 ******************************************************************************/

value scrutiny_sd_bus_get_prop_callback_impl(sd_bus_message *m) {
  CAMLparam0();
  CAMLlocal1(res);

  CHECK_MESSAGE_OK(m);

  int r = sd_bus_message_enter_container(m, SD_BUS_TYPE_VARIANT, "s");
  CHECK_MESSAGE_PARSE(r);

  const char *s;
  r = sd_bus_message_read_basic(m, SD_BUS_TYPE_STRING, &s);
  CHECK_MESSAGE_PARSE(r);

  res = caml_alloc_small(1, 0);
  Store_field(res, 0, caml_copy_string(s));
  CAMLreturn(res);
}

DEFINE_BUS_CALLBACK(scrutiny_sd_bus_get_prop_callback)

CAMLprim value scrutiny_sd_bus_get_prop(value v_bus, value path, value interface, value prop, value v_callback) {
  CAMLparam5(v_bus, path, interface, prop, v_callback);

  sd_bus *bus = Bus_val(v_bus);
  CHECK_OPEN(scrutiny_sd_bus_get_prop, bus);
  value *async_cb = scrutiny_sd_register_root(v_callback);

  _cleanup_(sd_bus_slot_unrefp) sd_bus_slot *slot;
  int r = sd_bus_call_method_async(
      bus, &slot, DESTINATION, String_val(path), "org.freedesktop.DBus.Properties", "Get",
      scrutiny_sd_bus_get_prop_callback, async_cb, "ss", String_val(interface), String_val(prop));
  CHECK_OK(sd_bus_call_method_async, r);

  sd_bus_slot_set_destroy_callback(slot, scrutiny_sd_bus_destroy);
  sd_bus_slot_set_floating(slot, 1);
  CAMLreturn(Val_unit);
}

/*******************************************************************************
 * sd-bus => org.freedesktop.systemd1.Manager.ListUnits()
 ******************************************************************************/

value scrutiny_sd_bus_list_units_callback_impl(sd_bus_message *m) {
  CAMLparam0();
  CAMLlocal4(res, list, nextList, item);

  CHECK_MESSAGE_OK(m);

  int r = sd_bus_message_enter_container(m, SD_BUS_TYPE_ARRAY, "(ssssssouso)");
  CHECK_MESSAGE_PARSE(r);

  list = Val_emptylist;
  while (1) {
    char *machine, *id, *description, *load_state, *active_state, *sub_state, *following, *unit_path, *job_type,
        *job_path;
    uint32_t job_id;
    int r = sd_bus_message_read(
        m, "(ssssssouso)", &id, &description, &load_state, &active_state, &sub_state, &following, &unit_path, &job_id,
        &job_type, &job_path);
    CHECK_MESSAGE_PARSE(r);
    if (r == 0) break;

    item = caml_alloc_small(7, 0);
    Store_field(item, 0, caml_copy_string(id));
    Store_field(item, 1, caml_copy_string(description));
    Store_field(item, 2, caml_copy_string(load_state));
    Store_field(item, 3, caml_copy_string(active_state));
    Store_field(item, 4, caml_copy_string(sub_state));
    Store_field(item, 5, caml_copy_string(following));
    Store_field(item, 6, caml_copy_string(unit_path));

    nextList = caml_alloc_small(2, 0);
    Store_field(nextList, 0, item);
    Store_field(nextList, 1, list);
    list = nextList;
  }

  res = caml_alloc_small(1, 0);
  Store_field(res, 0, list);
  CAMLreturn(res);
}

DEFINE_BUS_CALLBACK(scrutiny_sd_bus_list_units_callback)

CAMLprim value scrutiny_sd_bus_list_units(value v_bus, value v_callback) {
  CAMLparam2(v_bus, v_callback);

  sd_bus *bus = Bus_val(v_bus);
  CHECK_OPEN(scrutiny_sd_bus_list_units, bus);
  value *async_cb = scrutiny_sd_register_root(v_callback);

  _cleanup_(sd_bus_slot_unrefp) sd_bus_slot *slot;
  int r = sd_bus_call_method_async(
      bus, &slot, DESTINATION, PATH, IFACE_MANAGER, "ListUnits", scrutiny_sd_bus_list_units_callback, async_cb, NULL);
  CHECK_OK(sd_bus_call_method_async, r);

  sd_bus_slot_set_destroy_callback(slot, scrutiny_sd_bus_destroy);
  sd_bus_slot_set_floating(slot, 1);
  CAMLreturn(Val_unit);
}

/*******************************************************************************
 * sd-bus => org.freedesktop.systemd1.Manager.LoadUnit(s) -> o
 ******************************************************************************/

CAMLprim value scrutiny_sd_bus_load_unit(value v_bus, value unit_name, value v_callback) {
  CAMLparam3(v_bus, unit_name, v_callback);

  sd_bus *bus = Bus_val(v_bus);
  CHECK_OPEN(scrutiny_sd_bus_load_unit, bus);
  value *async_cb = scrutiny_sd_register_root(v_callback);

  _cleanup_(sd_bus_slot_unrefp) sd_bus_slot *slot;
  int r = sd_bus_call_method_async(
      bus, &slot, DESTINATION, PATH, IFACE_MANAGER, "LoadUnit", scrutiny_sd_bus_object_path_callback, async_cb, "s",
      String_val(unit_name));
  CHECK_OK(sd_bus_call_method_async, r);

  sd_bus_slot_set_destroy_callback(slot, scrutiny_sd_bus_destroy);
  sd_bus_slot_set_floating(slot, 1);
  CAMLreturn(Val_unit);
}

/*******************************************************************************
 * sd-bus => org.freedesktop.systemd1.Manager.Reload()
 *        => org.freedesktop.systemd1.Manager.Subscribe()
 ******************************************************************************/

CAMLprim value scrutiny_sd_bus_call_noarg(value v_bus, value method_name, value v_callback) {
  CAMLparam3(v_bus, method_name, v_callback);

  sd_bus *bus = Bus_val(v_bus);
  CHECK_OPEN(scrutiny_sd_bus_call_noarg, bus);
  value *async_cb = scrutiny_sd_register_root(v_callback);

  _cleanup_(sd_bus_slot_unrefp) sd_bus_slot *slot;
  int r = sd_bus_call_method_async(
      bus, &slot, DESTINATION, PATH, IFACE_MANAGER, String_val(method_name), scrutiny_sd_bus_empty_callback, async_cb,
      NULL);
  CHECK_OK(sd_bus_call_method_async, r);

  sd_bus_slot_set_destroy_callback(slot, scrutiny_sd_bus_destroy);
  sd_bus_slot_set_floating(slot, 1);
  CAMLreturn(Val_unit);
}

/*******************************************************************************
 * sd-bus => org.freedesktop.systemd1.Manager.Start(unit_name, "replace")
 *        => org.freedesktop.systemd1.Manager.Stop(unit_name, "replace")
 *        => org.freedesktop.systemd1.Manager.Reload(unit_name, "replace")
 *        => org.freedesktop.systemd1.Manager.Restart(unit_name, "replace")
 ******************************************************************************/

CAMLprim value scrutiny_sd_bus_unit_action(value v_bus, value unit_name, value method_name, value v_callback) {
  CAMLparam4(v_bus, unit_name, method_name, v_callback);

  sd_bus *bus = Bus_val(v_bus);
  CHECK_OPEN(scrutiny_sd_bus_unit_action, bus);
  value *async_cb = scrutiny_sd_register_root(v_callback);

  _cleanup_(sd_bus_slot_unrefp) sd_bus_slot *slot;
  int r = sd_bus_call_method_async(
      bus, &slot, DESTINATION, PATH, IFACE_MANAGER, String_val(method_name), scrutiny_sd_bus_object_path_callback,
      async_cb, "ss", String_val(unit_name), "replace");
  CHECK_OK(sd_bus_call_method_async, r);

  sd_bus_slot_set_destroy_callback(slot, scrutiny_sd_bus_destroy);
  sd_bus_slot_set_floating(slot, 1);
  CAMLreturn(Val_unit);
}

/*******************************************************************************
 * sd-bus => org.freedesktop.systemd1.Manager.EnableUnitFiles(unit_names, false, false)
 *        => org.freedesktop.systemd1.Manager.DisableUnitFiles(unit_names, false)
 ******************************************************************************/

CAMLprim value scrutiny_sd_bus_enable_unit_file(value v_bus, value unit_name, value v_callback) {
  CAMLparam3(v_bus, unit_name, v_callback);

  sd_bus *bus = Bus_val(v_bus);
  CHECK_OPEN(scrutiny_sd_bus_enable_unit_file, bus);
  value *async_cb = scrutiny_sd_register_root(v_callback);

  _cleanup_(sd_bus_slot_unrefp) sd_bus_slot *slot;
  int r = sd_bus_call_method_async(
      bus, &slot, DESTINATION, PATH, IFACE_MANAGER, "EnableUnitFiles", scrutiny_sd_bus_empty_callback, async_cb, "asbb",
      1, String_val(unit_name), false, false);
  CHECK_OK(sd_bus_call_method_async, r);

  sd_bus_slot_set_destroy_callback(slot, scrutiny_sd_bus_destroy);
  sd_bus_slot_set_floating(slot, 1);
  CAMLreturn(Val_unit);
}

CAMLprim value scrutiny_sd_bus_disable_unit_file(value v_bus, value unit_name, value v_callback) {
  CAMLparam3(v_bus, unit_name, v_callback);

  sd_bus *bus = Bus_val(v_bus);
  CHECK_OPEN(scrutiny_sd_bus_disable_unit_file, bus);
  value *async_cb = scrutiny_sd_register_root(v_callback);

  _cleanup_(sd_bus_slot_unrefp) sd_bus_slot *slot;
  int r = sd_bus_call_method_async(
      bus, &slot, DESTINATION, PATH, IFACE_MANAGER, "DisableUnitFiles", scrutiny_sd_bus_empty_callback, async_cb, "asb",
      1, String_val(unit_name), false);
  CHECK_OK(sd_bus_call_method_async, r);

  sd_bus_slot_set_destroy_callback(slot, scrutiny_sd_bus_destroy);
  sd_bus_slot_set_floating(slot, 1);
  CAMLreturn(Val_unit);
}

/*******************************************************************************
 * sd-bus => org.freedesktop.systemd1.Manager.JobRemoved
 ******************************************************************************/

value scrutiny_sd_bus_job_removed_callback_impl(sd_bus_message *m) {
  CAMLparam0();
  CAMLlocal1(res);

  CHECK_MESSAGE_OK(m);

  char *job_path, *result;
  int r = sd_bus_message_read(m, "uoss", NULL, &job_path, NULL, &result);
  CHECK_MESSAGE_PARSE(r);

  res = caml_alloc_small(2, 0);
  Store_field(res, 0, caml_copy_string(job_path));
  Store_field(res, 1, caml_copy_string(result));
  CAMLreturn(res);
}

DEFINE_BUS_CALLBACK(scrutiny_sd_bus_job_removed_callback)

CAMLprim value scrutiny_sd_bus_subscribe_job_removed(value v_bus, value v_callback) {
  CAMLparam2(v_bus, v_callback);

  sd_bus *bus = Bus_val(v_bus);
  CHECK_OPEN(scrutiny_sd_bus_unit_action, bus);
  value *async_cb = scrutiny_sd_register_root(v_callback);

  _cleanup_(sd_bus_slot_unrefp) sd_bus_slot *slot;
  // FIXME: This isn't async right now. Not sure how to do that with two sets of userdata!
  int r = sd_bus_match_signal(
      bus, &slot, DESTINATION, PATH, IFACE_MANAGER, "JobRemoved", scrutiny_sd_bus_job_removed_callback, async_cb);
  CHECK_OK(sd_bus_call_method_async, r);

  sd_bus_slot_set_destroy_callback(slot, scrutiny_sd_bus_destroy);
  sd_bus_slot_set_floating(slot, 1);
  CAMLreturn(Val_unit);
}
