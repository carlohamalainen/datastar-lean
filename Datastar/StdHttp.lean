module

public import Std.Http
public import Datastar.Types
public import Datastar.SSE
public import Datastar.Compression

public section

/-!
SSE streaming on `Std.Http`, and the signals a request carries.

The browser sends its signals as JSON: in the `datastar` query parameter for GET and DELETE, in
the request body otherwise. `signalsText` returns that JSON as text, for any JSON library to
read; `Datastar.LeanJson` decodes it with `Lean.Data.Json`.
-/

open Std Async Http

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

/--
The signals in the `datastar` query parameter, as the JSON text the browser sent.
-/
def signalsTextFromQuery (req : Request β) : Except String String := do
  let some (some encoded) := req.line.uri.query.find? "datastar"
    | throw "missing 'datastar' query parameter"
  let some raw := encoded.decode
    | throw "'datastar' query parameter is not valid percent-encoded UTF-8"
  return raw

/--
The signals in the request body, as the JSON text the browser sent.
-/
def signalsTextFromBody (req : Request Body.Stream) : ContextAsync (Except String String) := do
  try
    let raw : String ← req.body.readAll
    return .ok raw
  catch e =>
    return .error s!"could not read request body: {e}"

/--
The signals the browser sent, as JSON text: from the query for GET and DELETE, from the body
otherwise.
-/
def signalsText (req : Request Body.Stream) : ContextAsync (Except String String) :=
  if req.line.method == .get || req.line.method == .delete then
    pure (signalsTextFromQuery req)
  else
    signalsTextFromBody req

end Datastar
