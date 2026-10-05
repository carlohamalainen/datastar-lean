module

public import Datastar.Types

public section

/-!
Patch signals: update the browser's reactive state.

The payload is a JSON object with JSON Merge Patch semantics: a key updates the signal, `null`
removes it, an absent key leaves it alone.

```lean
sse.send <| patchSignals "{\"count\": 42}"
sse.send <| patchSignals "{\"name\": \"default\"}" (onlyIfMissing := true)
```
-/

namespace Datastar

/--
A `datastar-patch-signals` event. Build one with `patchSignals`.
-/
structure PatchSignals where
  /-- JSON object of the signals to patch. -/
  signals : String
  /-- Set only the signals that the browser does not have yet. -/
  onlyIfMissing : Bool := defaultOnlyIfMissing
  /-- SSE event ID. -/
  eventId : Option String := none
  /-- SSE retry interval in milliseconds. -/
  retryDuration : Nat := defaultRetryDuration
deriving DecidableEq, Repr

/--
Build a `PatchSignals` event with sensible defaults.
-/
def patchSignals
    (signals : String)
    (onlyIfMissing : Bool := defaultOnlyIfMissing)
    (eventId : Option String := none)
    (retryDuration : Nat := defaultRetryDuration) : PatchSignals :=
  { signals, onlyIfMissing, eventId, retryDuration }

instance : ToEvent PatchSignals where
  toEvent ps :=
    { eventType := .patchSignals
      eventId := ps.eventId
      retry := ps.retryDuration
      dataLines :=
        (if ps.onlyIfMissing then #["onlyIfMissing true"] else #[])
        ++ (lines ps.signals).map ("signals " ++ ·) }

end Datastar
