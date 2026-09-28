import Datastar

import Std.Async
import Std.Http
import Std.Net.Addr

open Std Async Http Server
open Datastar

structure HelloSignals where
  delay : Nat
deriving Lean.FromJson

def message : String := "Hello, world!"

def indexHtml : String := include_str "hello-world.html"

def helloWorld (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  match ← readSignals (α := HelloSignals) req with
  | .error err =>
    Response.badRequest |>.text s!"Bad signals: {err}"
  | .ok signals =>
    sseResponse fun gen => do
      for i in [1:message.length + 1] do
        gen.send <| patchElements s!"<div id='message'>{message.take i}</div>"
        sleep (.ofNat signals.delay)

def app (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  match req.line.method, toString req.line.uri.path with
  | .get, "/" => Response.ok |>.html indexHtml
  | .get, "/hello-world" => helloWorld req
  | _, _ => Response.notFound |>.text "Not found"

def main (args : List String) : IO Unit := Async.block do
  let port := (args.head? >>= String.toNat?).getD 3000
  let addr : Std.Net.SocketAddressV4 := ⟨.ofParts 127 0 0 1, port.toUInt16⟩

  let server ← Server.serve addr <| Handler.ofFn app

  IO.println s!"Listening on http://127.0.0.1:{port}"
  (← IO.getStdout).flush
  server.waitShutdown
