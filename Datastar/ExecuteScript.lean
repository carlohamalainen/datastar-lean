import Datastar.Types

/-!
Execute script: run JavaScript in the browser.

Datastar appends a `<script>` tag to `<body>`.

```lean
sse.send <| executeScript "window.location = \"/dashboard\""
sse.send <| executeScript "import(\"/chart.js\").then(m => m.render())" (attributes := #["type=\"module\""])
```
-/

namespace Datastar

/--
A script to run in the browser. Build one with `executeScript`.
-/
structure ExecuteScript where
  /-- The JavaScript source. -/
  script : String
  /-- Remove the `<script>` tag from the DOM once it has run. -/
  autoRemove : Bool := defaultAutoRemove
  /-- Extra attributes for the `<script>` tag, e.g. `type="module"` or a CSP `nonce`. -/
  attributes : Array String := #[]
  /-- SSE event ID. -/
  eventId : Option String := none
  /-- SSE retry interval in milliseconds. -/
  retryDuration : Nat := defaultRetryDuration
deriving DecidableEq, Repr

/--
Build an `ExecuteScript` event with sensible defaults.
-/
def executeScript
    (script : String)
    (autoRemove : Bool := defaultAutoRemove)
    (attributes : Array String := #[])
    (eventId : Option String := none)
    (retryDuration : Nat := defaultRetryDuration) : ExecuteScript :=
  { script, autoRemove, attributes, eventId, retryDuration }

namespace ExecuteScript

private def openTag (es : ExecuteScript) : String :=
  "<script"
    ++ (if es.autoRemove then " data-effect=\"el.remove()\"" else "")
    ++ es.attributes.foldl (fun acc attr => acc ++ " " ++ attr) ""
    ++ ">"

private def closeTag : String := "</script>"

/--
The `<script>` tag as `elements` data lines.
-/
def scriptLines (es : ExecuteScript) : Array String :=
  match lines es.script with
  | #[] => #["elements " ++ es.openTag ++ closeTag]
  | #[single] => #["elements " ++ es.openTag ++ single ++ closeTag]
  | multiple =>
    #["elements " ++ es.openTag]
      ++ multiple.map ("elements " ++ ·)
      ++ #["elements " ++ closeTag]

end ExecuteScript

instance : ToEvent ExecuteScript where
  toEvent es :=
    { -- Correct, there is no dedicated execute-script event type, see the ADR:
      -- https://github.com/starfederation/datastar/blob/develop/sdk/ADR.md
      eventType := .patchElements
      eventId := es.eventId
      retry := es.retryDuration
      dataLines := #["selector body", "mode append"] ++ es.scriptLines }

end Datastar
