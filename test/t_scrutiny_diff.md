## Scrutiny_diff

The `scrutiny.diff` library provides utilities for computing and displaying diffs.

```ocaml
# open Scrutiny_diff ;;
# let pp_full out x = pp ~full:true out x ;;
val pp_full : Format.formatter -> t -> unit = <fun>
# #install_printer pp_full ;;
```

In its simplest form, you can compute the diff of two single-line strings:

```ocaml
# of_line ~old:"equal value" ~new_:"equal value" ;;
- : t =   equal value
# of_line ~old:"old value" ~new_:"new value" ;;
- : t = - old value
        + new value
```

While diffs are opaque, we can check if they contain any changes or not.

```ocaml
# is_empty empty;;
- : bool = false
# of_line ~old:"equal value" ~new_:"equal value" |> is_empty;;
- : bool = true
# of_line ~old:"old value" ~new_:"new value" |> is_empty;;
- : bool = false
```

We can also compute diffs of more complex strings.

```ocaml
# of_diff ~old:"this\nis\na\nlong\nstring" ~new_:"this\nis\nstill\na\nbig\nstring";;
- : t =   this
          is
        + still
          a
        - long
        + big
          string
```

When comparing long strings with many changes, we (can) display the patch as hunks instead.

```ocaml
# let parts = CCString.replace ~sub:" " ~by:"\n" ;;
val parts : ?which:[ `All | `Left | `Right ] -> string -> string = <fun>
# let diff =
    let old  = parts "This is a long string which we will diff correctly and all will be amazed!"
    and new_ = parts "This is a long string which we will diff correctly and some will be amazed!"
    in of_diff ~old ~new_ ;;
val diff : t =
    This
    is
    a
    long
    string
    which
    we
    will
    diff
    correctly
    and
  - all
  + some
    will
    be
    amazed!
# Format.printf "%a@?" (pp ~full:false) diff ;;
@ -11,15 +11,15 @
  diff
  correctly
  and
- all
+ some
  will
  be
  amazed!
- : unit = ()
```

It's also possible to build diffs of more complex structures.

```ocaml
# structure [
    ("field 1", of_line ~old:"field value" ~new_:"field value");
    ("field 2", of_line ~old:"old value" ~new_:"new value");
  ] ;;
- : t = field 1:
            field value
        field 2:
          - old value
          + new value

```

Again, these can be checked for being empty:

```ocaml
# structure [ ("field", of_line ~old:"field value" ~new_:"field value") ] |> is_empty ;;
- : bool = true
# structure [ ("field", of_line ~old:"old value" ~new_:"new value") ] |> is_empty ;;
- : bool = false
```

We also provide a utility for generating diffs from more complex structures.

```ocaml
# let fields = [
    Structure.field ~name:"1" ~pp:Fun.id fst;
    Structure.field ~name:"2" ~pp:string_of_int snd;
  ] ;;
val fields : (string * int) Structure.field list =
  [{Scrutiny_diff.Structure.name = "1"; diff = <fun>; basic = <fun>};
   {Scrutiny_diff.Structure.name = "2"; diff = <fun>; basic = <fun>}]
# Structure.diff fields (Some ("old", 123)) (Some ("new", 123)) ;;
- : t = 1:
          - old
          + new
        2:
            123

```

It's also possible to diff values where one side is missing:

```ocaml
# Structure.diff fields (Some ("old", 123)) None ;;
- : t = 1:
          - old
        2:
          - 123

```
<!-- We need two separate code blocks here or otherwise we keep adding an extra new line! -->
```ocaml
# Structure.diff fields None (Some ("new", 123)) ;;
- : t = 1:
          + new
        2:
          + 123

```
