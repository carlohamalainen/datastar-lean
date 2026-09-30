import Datastar

import Std.Async
import Std.Http
import Std.Net.Addr
import Std.Sync.Notify

open Std Async Http Server
open Datastar

structure Signals where
  delay : Nat
deriving Lean.FromJson

structure Settings where
  delay : Nat := 400
  version : Nat := 0

/--
State shared by every connection.

`changed` wakes the animations that are sleeping; `Notify` does not buffer, so an animation that
was busy sending finds out from `version` instead.
-/
structure SharedState where
  settings : IO.Ref Settings
  changed : Notify

def message : String := "Hello, world!"

def indexHtml : String := include_str "hello-world-channel.html"

/--
Animate character by character; returns early if the version changes (i.e. Start was clicked),
letting the caller restart from the beginning.
-/
def animate (sse : ServerSentEventGenerator) (state : SharedState) (settings : Settings) : ContextAsync Unit := do
  for i in [0:message.length + 1] do
    sse.send <| patchElements s!"<div id='message'>{message.take i}</div>"
    -- Race the delay against a version change
    let delay ← Selector.sleep (.ofNat settings.delay)
    let interrupted ← Selectable.one #[.case delay fun _ => pure false, .case state.changed.selector fun _ => pure true]
    if interrupted || (← state.settings.get).version != settings.version then
      return

def setDelay (state : SharedState) (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  match ← readSignals (α := Signals) req with
  | .error err =>
    Response.badRequest |>.text s!"Bad signals: {err}"
  | .ok signals =>
    state.settings.modify fun s => { delay := signals.delay, version := s.version + 1 }
    state.changed.notify
    sseResponse fun _ => pure ()

def helloWorld (state : SharedState) : ContextAsync (Response Body.Any) :=
  sseResponse fun sse => do
    repeat
      animate sse state (← state.settings.get)

def app (state : SharedState) (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  match req.line.method, toString req.line.uri.path with
  | .get, "/" => Response.ok |>.html indexHtml
  | .get, "/set-delay" => setDelay state req
  | .get, "/hello-world" => helloWorld state
  | _, _ => Response.notFound |>.text "Not found"

def main (args : List String) : IO Unit := Async.block do
  let port := (args.head? >>= String.toNat?).getD 3000
  let addr : Std.Net.SocketAddressV4 := ⟨.ofParts 127 0 0 1, port.toUInt16⟩

  let state : SharedState := { settings := ← IO.mkRef {}, changed := ← Notify.new }
  let server ← Server.serve addr <| Handler.ofFn (app state)

  IO.println s!"Listening on http://127.0.0.1:{port}"
  (← IO.getStdout).flush
  server.waitShutdown
