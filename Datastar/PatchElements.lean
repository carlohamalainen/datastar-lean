import Datastar.Types

/-!
Patch elements: the server sends complete HTML elements and the browser morphs them into the DOM.

Without a `selector`, each top-level element needs an `id`.

```lean
gen.send <| patchElements "<div id=\"count\">42</div>"
gen.send <| patchElements "<li>new item</li>" (selector := "#todo-list") (mode := .append)
gen.send <| removeElements "#flash-message"
```

Animate the update with a View Transition, optionally scoped to one element:

```lean
gen.send <| patchElements "<div id=\"feed\">...</div>"
  (useViewTransition := true) (viewTransitionSelector := "#main")
```
-/

namespace Datastar

/--
A `datastar-patch-elements` event. Build one with `patchElements` or `removeElements`.
-/
structure PatchElements where
  /-- One or more complete HTML elements; `none` when removing elements. -/
  elements : Option String := none
  /-- CSS selector of the target; `none` matches on the `id` of the elements. -/
  selector : Option String := none
  /-- How the elements are applied to the target. -/
  mode : ElementPatchMode := defaultPatchMode
  /-- Wrap the DOM update in a View Transition. -/
  useViewTransition : Bool := defaultUseViewTransition
  /--
  CSS selector of the element to scope the View Transition to; `none` transitions the whole
  document. Only sent when `useViewTransition` is set. Needs Datastar 1.0.2 in the browser.
  -/
  viewTransitionSelector : Option String := none
  /-- Namespace of the new elements; `ns` because `namespace` is a Lean keyword. -/
  ns : ElementNamespace := defaultNamespace
  /-- SSE event ID; the browser sends it back as `Last-Event-ID` when it reconnects. -/
  eventId : Option String := none
  /-- SSE retry interval in milliseconds. -/
  retryDuration : Nat := defaultRetryDuration
deriving DecidableEq, Repr

/--
Build a `PatchElements` event with sensible defaults.
-/
def patchElements
    (html : String)
    (selector : Option String := none)
    (mode : ElementPatchMode := defaultPatchMode)
    (useViewTransition : Bool := defaultUseViewTransition)
    (viewTransitionSelector : Option String := none)
    (ns : ElementNamespace := defaultNamespace)
    (eventId : Option String := none)
    (retryDuration : Nat := defaultRetryDuration) : PatchElements :=
  { elements := if html.isEmpty then none else some html
    selector
    mode
    useViewTransition
    viewTransitionSelector
    ns
    eventId
    retryDuration
  }

/--
Remove the elements matching a CSS selector.
-/
def removeElements
    (selector : String)
    (useViewTransition : Bool := defaultUseViewTransition)
    (viewTransitionSelector : Option String := none)
    (eventId : Option String := none)
    (retryDuration : Nat := defaultRetryDuration) : PatchElements :=
  { selector := some selector
    mode := .remove
    useViewTransition
    viewTransitionSelector
    eventId
    retryDuration
  }

instance : ToEvent PatchElements where
  toEvent pe :=
    { eventType := .patchElements
      eventId := pe.eventId
      retry := pe.retryDuration
      dataLines :=
        (match pe.selector with
          | some s => #["selector " ++ s]
          | none => #[])
        ++ (if pe.mode != defaultPatchMode then #["mode " ++ pe.mode.toString] else #[])
        ++ (if pe.useViewTransition then #["useViewTransition true"] else #[])
        ++ (match pe.viewTransitionSelector with
          | some s =>
            if pe.useViewTransition && !s.isEmpty then #["viewTransitionSelector " ++ s] else #[]
          | none => #[])
        ++ (if pe.ns != defaultNamespace then #["namespace " ++ pe.ns.toString] else #[])
        ++ (match pe.elements with
          | some html => (lines html).map ("elements " ++ ·)
          | none => #[]) }

end Datastar
