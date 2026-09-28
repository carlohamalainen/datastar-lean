import Datastar.Types

/-!
Rendering of events in the SSE wire format.
-/

namespace Datastar

/--
Render an event as SSE text. Fields that have their default value are left out.
-/
def renderEvent (event : DatastarEvent) : String :=
  "event: " ++ event.eventType.toString ++ "\n"
    ++ (match event.eventId with
      | some eid => "id: " ++ eid ++ "\n"
      | none => "")
    ++ (if event.retry != defaultRetryDuration then "retry: " ++ toString event.retry ++ "\n" else "")
    ++ event.dataLines.foldl (fun acc line => acc ++ "data: " ++ line ++ "\n") ""
    ++ "\n"

/--
`renderEvent` for anything that converts to an event.
-/
def render [ToEvent α] (x : α) : String :=
  renderEvent (toEvent x)

end Datastar
