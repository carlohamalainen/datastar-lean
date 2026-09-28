/-!
Compression of SSE streams: the codec interface and `Content-Encoding` negotiation.

No codecs ship with this package. A codec is a `Compressor`:

```lean
def identity : Compressor where
  encoding := "identity"
  start := pure { compress := pure, finish := pure .empty }
```

Pass compressors to `sseResponseWith` in preference order.
-/

namespace Datastar

/--
A compression stream for one connection. Calls are serialised, and `compress` is never called
after `finish`.
-/
structure Encoder where
  /-- Compress one event and flush, so that the result decodes to the whole event. -/
  compress : ByteArray → IO ByteArray
  /-- End the stream, returning any trailing bytes. -/
  finish : IO ByteArray

/--
A compression algorithm for SSE streams.
-/
structure Compressor where
  /-- The `Content-Encoding` token, e.g. `"br"`. -/
  encoding : String
  /-- Start a stream; called once per connection. -/
  start : IO Encoder

/--
How a compressor is chosen from the request's `Accept-Encoding`.
-/
inductive CompressionStrategy where
  /-- The first compressor that the client accepts (the default). -/
  | serverPriority
  /-- The first encoding in the client's list that has a compressor. -/
  | clientPriority
  /-- The first compressor, whatever the client accepts. -/
  | forced
deriving DecidableEq, Repr

/--
The encoding tokens of an `Accept-Encoding` value, in order. Parameters such as `;q=0.5` are
dropped, not interpreted.
-/
def parseEncodings (header : String) : List String :=
  (header.splitOn ",").filterMap fun part =>
    let trimmed := String.ofList <| part.toList.dropWhile isOWS |>.reverse.dropWhile isOWS |>.reverse
    let token := String.ofList <| trimmed.toList.takeWhile (· != ';')
    if token.isEmpty then none else some token
where
  isOWS (c : Char) : Bool := c == ' ' || c == '\t'

/--
Choose a compressor for the encodings that the client accepts. Tokens are compared exactly.
-/
def negotiate
    (compressors : List Compressor)
    (accepted : List String)
    (strategy : CompressionStrategy := .serverPriority) : Option Compressor :=
  match strategy with
  | .serverPriority => compressors.find? fun c => accepted.contains c.encoding
  | .clientPriority => accepted.findSome? fun enc => compressors.find? (·.encoding == enc)
  | .forced => compressors.head?

end Datastar
