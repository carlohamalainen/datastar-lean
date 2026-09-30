import Datastar

import Std.Async
import Std.Http
import Std.Net.Addr

/-!
Server for the official Datastar SDK test suite
(<https://github.com/starfederation/datastar/tree/main/sdk/tests>).

`/test` reads an `events` array with `readSignals` and sends each event with the SDK.
`scripts/test-sdk.sh` starts this server and runs the suite against it.
-/

open Std Async Http Server
open Datastar
open Lean (Json)

def str? (j : Json) (key : String) : Option String :=
  (j.getObjValAs? String key).toOption

def flag (j : Json) (key : String) (default : Bool) : Bool :=
  (j.getObjValAs? Bool key).toOption.getD default

def parseMode : String → Except String ElementPatchMode
  | "outer" => pure .outer
  | "inner" => pure .inner
  | "remove" => pure .remove
  | "replace" => pure .replace
  | "prepend" => pure .prepend
  | "append" => pure .append
  | "before" => pure .before
  | "after" => pure .after
  | other => throw s!"invalid mode {other.quote}"

def parseNamespace : String → Except String ElementNamespace
  | "html" => pure .html
  | "svg" => pure .svg
  | "mathml" => pure .mathml
  | other => throw s!"invalid namespace {other.quote}"

/-- The suite gives script attributes as an object; render each entry as `key="value"`. -/
def scriptAttributes (j : Json) : Array String :=
  match j.getObjVal? "attributes" with
  | .ok (.obj kvs) => kvs.toArray.map fun ⟨key, value⟩ =>
    let value := match value with
      | .str s => s
      | other => other.compress
    s!"{key}=\"{value}\""
  | _ => #[]

def parseEvent (j : Json) : Except String DatastarEvent := do
  let eventId := str? j "eventId"
  let retryDuration := (j.getObjValAs? Nat "retryDuration").toOption.getD defaultRetryDuration
  match str? j "type" with
  | some "patchElements" =>
    let mode ← (str? j "mode").elim (pure defaultPatchMode) parseMode
    let ns ← (str? j "namespace").elim (pure defaultNamespace) parseNamespace
    return toEvent <| patchElements ((str? j "elements").getD "")
      (selector := str? j "selector")
      (mode := mode)
      (useViewTransition := flag j "useViewTransition" defaultUseViewTransition)
      (viewTransitionSelector := str? j "viewTransitionSelector")
      (ns := ns)
      (eventId := eventId)
      (retryDuration := retryDuration)
  | some "patchSignals" =>
    -- `signals-raw` carries pre-formatted, possibly multi-line, JSON.
    let signals ← match str? j "signals-raw" with
      | some raw => pure raw
      | none => Json.compress <$> j.getObjVal? "signals"
    return toEvent <| patchSignals signals
      (onlyIfMissing := flag j "onlyIfMissing" defaultOnlyIfMissing)
      (eventId := eventId)
      (retryDuration := retryDuration)
  | some "executeScript" =>
    return toEvent <| executeScript ((str? j "script").getD "")
      (autoRemove := flag j "autoRemove" defaultAutoRemove)
      (attributes := scriptAttributes j)
      (eventId := eventId)
      (retryDuration := retryDuration)
  | other => throw s!"unknown event type {other}"

def app (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  if toString req.line.uri.path != "/test" then
    return ← Response.notFound |>.text "Not found"
  let input : Except String Json ← readSignals req
  match input >>= (·.getObjValAs? (Array Json) "events") >>= (·.mapM parseEvent) with
  | .error err => Response.badRequest |>.text err
  | .ok events => sseResponse fun gen => events.forM fun event => gen.send event

def main (args : List String) : IO Unit := Async.block do
  let port := (args.head? >>= String.toNat?).getD 7331
  let addr : Std.Net.SocketAddressV4 := ⟨.ofParts 127 0 0 1, port.toUInt16⟩

  let server ← Server.serve addr <| Handler.ofFn app

  IO.println s!"Listening on http://127.0.0.1:{port}/test"
  (← IO.getStdout).flush
  server.waitShutdown
