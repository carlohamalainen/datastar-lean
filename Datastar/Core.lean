module

public import Datastar.Types
public import Datastar.PatchElements
public import Datastar.PatchSignals
public import Datastar.ExecuteScript
public import Datastar.SSE
public import Datastar.Compression
public import Datastar.StdHttp

/-!
Everything in `Datastar` except `Datastar.LeanJson`, for a program that reads the browser's signals
with its own JSON library and should not link the Lean frontend.
-/
