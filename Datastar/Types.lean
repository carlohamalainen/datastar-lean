module

public section

/-!
Types and defaults of the Datastar SSE protocol, as specified by the
[ADR](https://github.com/starfederation/datastar/blob/develop/sdk/ADR.md).
-/

namespace Datastar

/--
The two SSE event types of the Datastar protocol.
-/
inductive EventType where
  /-- `datastar-patch-elements`; also carries `ExecuteScript`. -/
  | patchElements
  /-- `datastar-patch-signals`. -/
  | patchSignals
deriving DecidableEq, Repr

/-- The event name on the wire. -/
def EventType.toString : EventType → String
  | .patchElements => "datastar-patch-elements"
  | .patchSignals => "datastar-patch-signals"

instance : ToString EventType := ⟨EventType.toString⟩

/--
How the patched HTML is applied to the DOM.
-/
inductive ElementPatchMode where
  /-- Morph the target element and its contents (the default). -/
  | outer
  /-- Morph the target element's children only. -/
  | inner
  /-- Remove the target element. -/
  | remove
  /-- Replace the target element without morphing. -/
  | replace
  /-- Insert as the first child of the target element. -/
  | prepend
  /-- Insert as the last child of the target element. -/
  | append
  /-- Insert before the target element. -/
  | before
  /-- Insert after the target element. -/
  | after
deriving DecidableEq, Repr

/-- The value of the `mode` data line. -/
def ElementPatchMode.toString : ElementPatchMode → String
  | .outer => "outer"
  | .inner => "inner"
  | .remove => "remove"
  | .replace => "replace"
  | .prepend => "prepend"
  | .append => "append"
  | .before => "before"
  | .after => "after"

instance : ToString ElementPatchMode := ⟨ElementPatchMode.toString⟩

/--
The namespace in which the patched elements are created.
-/
inductive ElementNamespace where
  /-- HTML (the default). -/
  | html
  /-- For `<svg>` content. -/
  | svg
  /-- For `<math>` content. -/
  | mathml
deriving DecidableEq, Repr

/-- The value of the `namespace` data line. -/
def ElementNamespace.toString : ElementNamespace → String
  | .html => "html"
  | .svg => "svg"
  | .mathml => "mathml"

instance : ToString ElementNamespace := ⟨ElementNamespace.toString⟩

/--
Milliseconds the browser waits before reconnecting.
-/
def defaultRetryDuration : Nat := 1000

/--
Morph the target element and its contents.
-/
def defaultPatchMode : ElementPatchMode := .outer

/--
No View Transition.
-/
def defaultUseViewTransition : Bool := false

/--
Overwrite signals that the browser already has.
-/
def defaultOnlyIfMissing : Bool := false

/--
Remove the `<script>` tag once it has run.
-/
def defaultAutoRemove : Bool := true

/--
Create elements in the HTML namespace.
-/
def defaultNamespace : ElementNamespace := .html

/--
The low-level form of an event: the fields of one SSE message.

Prefer `patchElements`, `patchSignals` or `executeScript`. Line breaks in `eventId` and
`dataLines` are rendered as spaces, so a value cannot start a new SSE field.
-/
structure DatastarEvent where
  /-- The SSE `event` field. -/
  eventType : EventType
  /-- The SSE `id` field. -/
  eventId : Option String := none
  /-- The SSE `retry` field, in milliseconds. -/
  retry : Nat := defaultRetryDuration
  /-- The SSE `data` fields, one per entry, without the `data: ` prefix. -/
  dataLines : Array String := #[]
deriving DecidableEq, Repr

/--
Anything that `ServerSentEventGenerator.send` can send.
-/
class ToEvent (α : Type) where
  /-- Convert to the wire representation. -/
  toEvent : α → DatastarEvent

export ToEvent (toEvent)

instance : ToEvent DatastarEvent := ⟨id⟩

/--
Split lines like Haskell's `Data.Text.lines`; a trailing newline does
not produce an empty line. As in SSE, `\r\n`, `\r` and `\n` all end a line.

* `lines "" = #[]`
* `lines "a\n" = #["a"]`
* `lines "a\n\nb" = #["a", "", "b"]`
* `lines "a\r\nb\rc" = #["a", "b", "c"]`
-/
def lines (s : String) : Array String :=
  let parts := ((s.replace "\r\n" "\n").replace "\r" "\n").splitOn "\n" |>.toArray
  if parts.back? == some "" then parts.pop else parts

/--
Replace line breaks with spaces, for values that must stay on one SSE line.
-/
def oneLine (s : String) : String :=
  s.map fun c => if c == '\n' || c == '\r' then ' ' else c

end Datastar
