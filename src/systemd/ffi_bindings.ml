open Ctypes

module Readv = struct
  type iovec
end

module Constants (F : Cstubs.Types.TYPE) = struct
  open F

  module Journal = struct
    let sd_journal_local_only = constant "SD_JOURNAL_LOCAL_ONLY" int

    let sd_journal_nop = constant "SD_JOURNAL_NOP" int

    let sd_journal_append = constant "SD_JOURNAL_APPEND" int

    let sd_journal_invalidate = constant "SD_JOURNAL_INVALIDATE" int
  end

  module Syslog = struct
    let log_alert = constant "LOG_ALERT" int

    let log_crit = constant "LOG_CRIT" int

    let log_err = constant "LOG_ERR" int

    let log_warning = constant "LOG_WARNING" int

    let log_notice = constant "LOG_NOTICE" int

    let log_info = constant "LOG_INFO" int

    let log_debug = constant "LOG_DEBUG" int
  end

  module Readv = struct
    include Readv

    let iovec : iovec structure typ = structure "iovec"

    let iov_base = field iovec "iov_base" (ptr void)

    let iov_len = field iovec "iov_len" size_t

    let () = seal iovec
  end
end

module Values (F : Cstubs.FOREIGN) = struct
  open Ctypes
  open F

  open struct
    let iovec : Readv.iovec structure typ = structure "iovec"
  end

  module Journal = struct
    type t

    type sd_journal = t structure ptr

    let sd_journal : sd_journal typ = ptr (structure "sd_journal")

    let strerror = foreign "strerror" (int @-> returning string)

    let sd_journal_open = foreign "sd_journal_open" (ptr sd_journal @-> int @-> returning int)

    let sd_journal_close = foreign "sd_journal_close" (sd_journal @-> returning void)

    let sd_journal_next = foreign "sd_journal_next" (sd_journal @-> returning int)

    let sd_journal_seek_tail = foreign "sd_journal_seek_tail" (sd_journal @-> returning int)

    let sd_journal_wait = foreign "sd_journal_wait" (sd_journal @-> uint64_t @-> returning int)

    let sd_journal_get_fd = foreign "sd_journal_get_fd" (sd_journal @-> returning int)

    let sd_journal_process = foreign "sd_journal_process" (sd_journal @-> returning int)

    let sd_journal_sendv = foreign "sd_journal_sendv" (ptr iovec @-> int @-> returning int)

    let sd_journal_get_data =
      foreign "sd_journal_get_data"
        (sd_journal @-> string @-> ptr (ptr void) @-> ptr size_t @-> returning int)
  end
end
