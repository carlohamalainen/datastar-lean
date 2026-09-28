import Datastar

import Std.Async
import Std.Http
import Std.Net.Addr
import Std.Time

open Std Async Http Server
open Datastar

structure Signals where
  interval : Nat
  events : Nat
  generating : Bool
  total : Nat
  done : Nat
  warn : Nat
  fail : Nat
  info : Nat
deriving Lean.FromJson

inductive EventStatus where
  | done
  | warn
  | fail
  | info

namespace EventStatus

def ofString? : String → Option EventStatus
  | "done" => some .done
  | "warn" => some .warn
  | "fail" => some .fail
  | "info" => some .info
  | _ => none

def color : EventStatus → String
  | .done => "green"
  | .warn => "yellow"
  | .fail => "red"
  | .info => "blue"

def indicator : EventStatus → String
  | .done => "Done"
  | .warn => "Warn"
  | .fail => "Fail"
  | .info => "Info"

end EventStatus

def indexHtml : String := include_str "activity-feed.html"

def eventEntry (status : EventStatus) (index : Nat) (source : String) : IO String := do
  let now := Time.DateTime.ofTimestampWithZone (← Time.Timestamp.now) .UTC
  let timestamp := now.format "uuuu-MM-dd HH:mm:ss.SSS"
  return s!"<div id='event-{index}' class='text-{status.color}-500'>{timestamp} [ {status.indicator} ] {source} event {index}</div>"

def generate (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  match ← readSignals (α := Signals) req with
  | .error err =>
    Response.badRequest |>.text s!"Bad signals: {err}"
  | .ok signals =>
    sseResponse fun gen => do
      gen.send <| patchSignals "{\"generating\": true}"

      for i in [1:signals.events + 1] do
        let total := signals.total + i
        let done := signals.done + i
        let html ← eventEntry .done total "Auto"
        gen.send <| patchElements html (selector := "#feed") (mode := .after)
        gen.send <| patchSignals s!"\{\"total\": {total}, \"done\": {done}}"
        sleep (.ofNat signals.interval)

      gen.send <| patchSignals "{\"generating\": false}"

def event (status : EventStatus) (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  match ← readSignals (α := Signals) req with
  | .error err =>
    Response.badRequest |>.text s!"Bad signals: {err}"
  | .ok signals =>
    sseResponse fun gen => do
      let total := signals.total + 1
      let counter :=
        match status with
        | .done => s!"\"done\": {signals.done + 1}"
        | .warn => s!"\"warn\": {signals.warn + 1}"
        | .fail => s!"\"fail\": {signals.fail + 1}"
        | .info => s!"\"info\": {signals.info + 1}"
      gen.send <| patchSignals s!"\{\"total\": {total}, {counter}}"

      let html ← eventEntry status total "Manual"
      gen.send <| patchElements html (selector := "#feed") (mode := .after)

def app (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  match req.line.method, (toString req.line.uri.path).splitOn "/" with
  | .get, ["", ""] => Response.ok |>.html indexHtml
  | .post, ["", "event", "generate"] => generate req
  | .post, ["", "event", name] =>
    match EventStatus.ofString? name with
    | some status => event status req
    | none => Response.notFound |>.text "Not found"
  | _, _ => Response.notFound |>.text "Not found"

def main (args : List String) : IO Unit := Async.block do
  let port := (args.head? >>= String.toNat?).getD 3000
  let addr : Std.Net.SocketAddressV4 := ⟨.ofParts 127 0 0 1, port.toUInt16⟩

  let server ← Server.serve addr <| Handler.ofFn app

  IO.println s!"Listening on http://127.0.0.1:{port}"
  (← IO.getStdout).flush
  server.waitShutdown
