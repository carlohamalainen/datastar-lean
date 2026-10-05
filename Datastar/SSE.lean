module

public import Datastar.Types

public section

/-!
Rendering of events in the SSE wire format.
-/

namespace Datastar

/--
Render an event as SSE text. Fields that have their default value are left out, and line
breaks in the id and data lines become spaces.
-/
def renderEvent (event : DatastarEvent) : String :=
  "event: " ++ event.eventType.toString ++ "\n"
    ++ (match event.eventId with
      | some eid => "id: " ++ oneLine eid ++ "\n"
      | none => "")
    ++ (if event.retry != defaultRetryDuration then "retry: " ++ toString event.retry ++ "\n" else "")
    ++ event.dataLines.foldl (fun acc line => acc ++ "data: " ++ oneLine line ++ "\n") ""
    ++ "\n"

/--
`renderEvent` for anything that converts to an event.
-/
def render [ToEvent α] (x : α) : String :=
  renderEvent (toEvent x)

end Datastar
