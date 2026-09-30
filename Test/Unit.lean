import Datastar

/-!
Unit tests, run with `lake test`. `#guard` is checked at compile time, so the build fails if any
of these do.
-/

open Datastar

/-! `lines` splits on every SSE line ending. -/

#guard lines "" == #[]
#guard lines "a\n" == #["a"]
#guard lines "a\n\nb" == #["a", "", "b"]
#guard lines "a\r\nb\rc" == #["a", "b", "c"]

/-! A carriage return in the payload starts a new data line instead of ending the SSE line early. -/

#guard render (patchElements "<div id=\"a\">\r</div>") ==
  "event: datastar-patch-elements\ndata: elements <div id=\"a\">\ndata: elements </div>\n\n"

/-! Line breaks in single-line fields cannot inject SSE fields. -/

#guard render (patchSignals "{}" (eventId := some "1\ndata: injected")) ==
  "event: datastar-patch-signals\nid: 1 data: injected\ndata: signals {}\n\n"

#guard render (removeElements "#a,\n#b") ==
  "event: datastar-patch-elements\ndata: selector #a, #b\ndata: mode remove\n\n"

#guard render ({ eventType := .patchSignals, dataLines := #["signals {}\r\nretry: 0"] } : DatastarEvent) ==
  "event: datastar-patch-signals\ndata: signals {}  retry: 0\n\n"
