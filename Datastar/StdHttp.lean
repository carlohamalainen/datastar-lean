import Std.Http
import Lean.Data.Json
import Datastar.Types
import Datastar.SSE
import Datastar.Compression

/-!
SSE streaming and signal decoding on `Std.Http`.

The browser sends its signals as JSON: in the `datastar` query parameter for GET and DELETE, in
the request body otherwise.

```lean
structure Signals where
  count : Nat
deriving Lean.FromJson

def handler (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  match ← readSignals (α := Signals) req with
  | .error err => Response.badRequest |>.text err
  | .ok signals =>
    sseResponse fun sse =>
      sse.send <| patchElements s!"<div id=\"count\">{signals.count}</div>"
```
-/

open Std Async Http
open Lean (FromJson)

namespace Datastar

/--
An opaque handle for sending SSE events to the browser.

Obtain one from the callback passed to `sseResponse`.

The handle is safe to share between tasks.
-/
structure ServerSentEventGenerator where
  private mk ::
  private stream : Body.Stream
  private lock : Std.Semaphore
  private encoder : Option Encoder
  private finished : IO.Ref Bool

namespace ServerSentEventGenerator

private def withLock (sse : ServerSentEventGenerator) (action : Async α) : Async α := do
  let p ← sse.lock.acquire
  let res : Option Unit ← await p.result?

  match res with
    | none => throw (IO.userError "SSE generator lock was dropped")
    | some _ => try action finally sse.lock.release

private def encode (sse : ServerSentEventGenerator) (bytes : ByteArray) : IO ByteArray := do
  let some encoder := sse.encoder | return bytes
  if ← sse.finished.get then
    throw (IO.userError "SSE stream has ended")
  encoder.compress bytes

private def finish (sse : ServerSentEventGenerator) : Async Unit := do
  let some encoder := sse.encoder | return
  sse.withLock do
    sse.finished.set true
    sse.stream.send { data := ← encoder.finish }

/--
Send a `PatchElements`, `PatchSignals` or `ExecuteScript` event.

Throws once the client has gone away.
-/
def send [ToEvent α] (sse : ServerSentEventGenerator) (x : α) : Async Unit := do
  let event := toEvent x
  let text := renderEvent event
  sse.withLock do
    sse.stream.send { data := ← sse.encode text.toUTF8 }

end ServerSentEventGenerator

private def cacheControl : Header.Name := .mk "cache-control"

private def acceptEncoding : Header.Name := .mk "accept-encoding"

private def clientEncodings (req : Request β) : List String :=
  match req.line.headers.get? acceptEncoding with
  | some header => parseEncodings header.value
  | none => []

private def sseResponseCore
    (chosen : Option Compressor)
    (callback : ServerSentEventGenerator → ContextAsync Unit) :
    ContextAsync (Response Body.Any) := do
  let ctx ← ContextAsync.getContext
  let lock ← Semaphore.new 1
  let finished ← IO.mkRef false
  let builder := Response.ok
        |>.header cacheControl (.mk "no-cache")
        |>.header Header.Name.contentType (.mk "text/event-stream")

  let (builder, encoder) ← match chosen with
    | none => pure (builder, none)
    | some compressor =>
      let some encoding := Header.Value.ofString? compressor.encoding
        | throw (IO.userError s!"invalid Content-Encoding: {compressor.encoding.quote}")
      let encoder ← compressor.start
      pure (builder.header Header.Name.contentEncoding encoding, some encoder)

  builder.stream fun stream => do
    let sse : ServerSentEventGenerator := {stream, lock, encoder, finished}
    try
      ContextAsync.runIn ctx (callback sse)
    finally
      try sse.finish catch _ => pure ()

/--
A response that streams SSE events. The connection stays open until `callback` returns.
-/
def sseResponse (callback : ServerSentEventGenerator → ContextAsync Unit) : ContextAsync (Response Body.Any) :=
  sseResponseCore none callback

/--
`sseResponse`, compressed with the first of `compressors` that the client accepts; uncompressed
if there is none.
-/
def sseResponseWith
    (compressors : List Compressor)
    (req : Request β)
    (callback : ServerSentEventGenerator → ContextAsync Unit)
    (strategy : CompressionStrategy := .serverPriority) :
    ContextAsync (Response Body.Any) :=
  sseResponseCore (negotiate compressors (clientEncodings req) strategy) callback

private def decodeJson [FromJson α] (raw : String) : Except String α := do
  let j ← Lean.Json.parse raw
  Lean.fromJson? j

/--
Decode signals from the `datastar` query parameter.
-/
def signalsFromQuery [FromJson α] (req : Request β) : Except String α := do
  let some (some encoded) := req.line.uri.query.find? "datastar"
    | throw "missing 'datastar' query parameter"
  let some raw := encoded.decode
    | throw "'datastar' query parameter is not valid percent-encoded UTF-8"
  decodeJson raw

/--
Decode signals from the request body.
-/
def signalsFromBody [FromJson α] (req : Request Body.Stream) : ContextAsync (Except String α) := do
  try
    let raw : String ← req.body.readAll
    return decodeJson raw
  catch e =>
    return .error s!"could not read request body: {e}"

/--
Decode the signals sent by the browser: from the query for GET and DELETE, from the body otherwise.
-/
def readSignals [FromJson α] (req : Request Body.Stream) : ContextAsync (Except String α) :=
  if req.line.method == .get || req.line.method == .delete then
    pure (signalsFromQuery req)
  else
    signalsFromBody req

end Datastar
