import Datastar

import Std.Async
import Std.Http
import Std.Net.Addr

open Std Async Http Server
open Datastar

structure Greeting where
  greeting : String
deriving Lean.FromJson

def testPage : String := include_str "e2e-server.html"

def app (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  match req.line.method, toString req.line.uri.path with
  | .get, "/" => Response.ok |>.html testPage
  | .get, "/sse/patch-elements" =>
    sseResponse fun sse =>
      sse.send <| patchElements "<div id=\"pe-result\">Patched Content</div>"
  | .get, "/sse/patch-signals" =>
    sseResponse fun sse =>
      sse.send <| patchSignals "{\"message\":\"Signal Updated\"}"
  | .get, "/sse/execute-script" =>
    sseResponse fun sse =>
      sse.send <| executeScript "document.getElementById('es-result').textContent = 'Script Executed'"
  | .get, "/sse/read-signals" =>
    sseResponse fun sse => do
      match ← readSignals (α := Greeting) req with
      | .ok signals =>
        sse.send <| patchElements s!"<div id=\"rs-result\">{signals.greeting}</div>"
      | .error err =>
        sse.send <| patchElements s!"<div id=\"rs-result\">Error: {err}</div>"
  | .get, "/sse/multiple-events" =>
    sseResponse fun sse => do
      sse.send <| patchElements "<div id=\"me-result\">Event 1</div>"
      sse.send <| patchElements "<div id=\"me-result\">Event 2</div>"
      sse.send <| patchElements "<div id=\"me-result\">Event 3</div>"
  | _, _ => Response.notFound |>.text "Not found"

def main : IO Unit := Async.block do
  let addr : Std.Net.SocketAddressV4 := ⟨.ofParts 127 0 0 1, 3113⟩

  let server ← Server.serve addr <| Handler.ofFn app

  IO.println "e2e-server running on http://127.0.0.1:3113"
  (← IO.getStdout).flush
  server.waitShutdown
