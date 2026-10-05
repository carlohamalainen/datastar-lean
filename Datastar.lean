module

public import Datastar.Core
public import Datastar.LeanJson

/-!
Lean SDK for [Datastar](https://data-star.dev/): the server holds an SSE stream open and pushes
HTML elements, signal updates or scripts to the browser.

```lean
import Datastar

open Std Async Http Server
open Datastar

def app (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  match req.line.method, toString req.line.uri.path with
  | .get, "/hello" =>
    sseResponse fun sse =>
      sse.send <| patchElements "<div id=\"message\">Hello!</div>"
  | _, _ => Response.notFound |>.text "Not found"

def main : IO Unit := Async.block do
  let addr : Std.Net.SocketAddressV4 := ⟨.ofParts 127 0 0 1, 3000⟩
  let server ← Server.serve addr <| Handler.ofFn app
  server.waitShutdown
```

* `Datastar.PatchElements` — send HTML to morph into the DOM
* `Datastar.PatchSignals` — update the browser's reactive signals
* `Datastar.ExecuteScript` — run JavaScript in the browser
* `Datastar.StdHttp` — SSE streaming, and the signals a request carries as JSON text
* `Datastar.LeanJson` — signals decoded with `Lean.Data.Json`; leave it out, by importing
  `Datastar.Core`, to avoid linking the Lean frontend
* `Datastar.Compression` — the codec interface and `Content-Encoding` negotiation
* `Datastar.SSE` — the wire format
* `Datastar.Types` — protocol types and defaults
-/
