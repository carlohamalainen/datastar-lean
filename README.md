<p align="center"><img width="150" height="150" src="https://data-star.dev/static/images/rocket-512x512.png"></p>

# Datastar Lean SDK

[Lean 4](https://lean-lang.org) implementation of the [Datastar](https://data-star.dev/) SDK for building real-time hypermedia applications with server-sent events (SSE). Inspired by the [Haskell SDK](https://github.com/starfederation/datastar-haskell).

## License

This package is licensed for free under the [MIT License](LICENSE).

## Design

- **No dependencies** -- only Lean's standard library.
- **Std.Http streaming** -- `sseResponse` gives you a `ServerSentEventGenerator`
callback, and `sse.send` takes any event.

## API Overview

```lean
import Datastar

-- Create an SSE response
sseResponse : (ServerSentEventGenerator → ContextAsync Unit) → ContextAsync (Response Body.Any)

-- Create a compressed SSE response; optionally (strategy := .clientPriority) or .forced
sseResponseWith : List Compressor → Request β → (ServerSentEventGenerator → ContextAsync Unit)
  → ContextAsync (Response Body.Any)

-- Build events; options are named arguments, e.g. (selector := "#feed") (mode := .append)
patchElements  : String → PatchElements
removeElements : String → PatchElements
patchSignals   : String → PatchSignals
executeScript  : String → ExecuteScript

-- Send an event
ServerSentEventGenerator.send : [ToEvent α] → ServerSentEventGenerator → α → Async Unit

-- Read signals from a request
readSignals : [FromJson α] → Request Body.Stream → ContextAsync (Except String α)
```

## Quick Start

Add the package to `lakefile.toml`:

```toml
[[require]]
name = "datastar"
git = "https://github.com/carlohamalainen/datastar-lean"
rev = "v0.1.0"
```

Tested with Lean 4.34.1, see [lean-toolchain](lean-toolchain).

then:

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

## Compression

`sseResponseWith` negotiates a `Content-Encoding` against the request's `Accept-Encoding`. Pass
the compressors in preference order:

```lean
sseResponseWith [brotli, gzip] req fun sse =>
  sse.send <| patchElements "<div id=\"message\">Hello!</div>"
```

If the client accepts none of them, the stream is sent uncompressed.

**No codecs ship with this package yet.**

A codec is a `Compressor`, see [Datastar/Compression.lean](Datastar/Compression.lean).

## Examples

```
lake build
.lake/build/bin/hello-world
```

Then open <http://127.0.0.1:3000>. Each example takes the port as an optional argument.

| Executable | What it shows |
| --- | --- |
| `hello-world` | Reads a signal, streams `patchElements` one character at a time. |
| `hello-world-channel` | State shared between connections; Start restarts the animation on every open page. |
| `activity-feed` | `patchSignals` with `patchElements`; `@post` requests with signals in the body. |

## SDK tests

The official [Datastar SDK test suite](https://github.com/starfederation/datastar/tree/main/sdk/tests)
runs against `sdk-test-server` (`Test/SdkTestServer.lean`, port 7331). It needs Go:

```
scripts/test-sdk.sh
```

## End-to-end tests

Playwright drives a browser against `e2e-server` (`Test/E2EServer.lean`, port 3113), which it
starts itself.

```
cd e2e
npm ci
npx playwright install chromium
npx playwright test
```
