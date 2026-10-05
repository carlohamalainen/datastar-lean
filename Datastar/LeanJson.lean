module

public import Lean.Data.Json
public import Datastar.StdHttp

public section

/-!
Signals decoded with `Lean.Data.Json`.

`Lean.Data.Json` is part of the Lean frontend, so a program that imports this module links it.
A program that reads JSON some other way imports `Datastar.Core` and decodes `signalsText`.

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

private def decodeJson [FromJson α] (raw : String) : Except String α := do
  let j ← Lean.Json.parse raw
  Lean.fromJson? j

/--
Decode signals from the `datastar` query parameter.
-/
def signalsFromQuery [FromJson α] (req : Request β) : Except String α :=
  signalsTextFromQuery req >>= decodeJson

/--
Decode signals from the request body.
-/
def signalsFromBody [FromJson α] (req : Request Body.Stream) : ContextAsync (Except String α) :=
  return (← signalsTextFromBody req) >>= decodeJson

/--
Decode the signals sent by the browser: from the query for GET and DELETE, from the body otherwise.
-/
def readSignals [FromJson α] (req : Request Body.Stream) : ContextAsync (Except String α) :=
  return (← signalsText req) >>= decodeJson

end Datastar
