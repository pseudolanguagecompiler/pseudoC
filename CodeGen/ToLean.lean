import Ast.UniversalIR

namespace Codegen.ToLean

-- Expression → Lean 4 expression
def exprToLean : UniversalIR.Expr → String
  | .num n => toString n
  | .var x => x
  | .binOp UniversalIR.BinOp.add l r => s!"({exprToLean l} + {exprToLean r})"
  | .binOp UniversalIR.BinOp.sub l r => s!"({exprToLean l} - {exprToLean r})"
  | .binOp UniversalIR.BinOp.gt l r => s!"({exprToLean l} > {exprToLean r})"

-- Statement → Lean 4 do-block line
def stmtToLean : UniversalIR.Stmt → String
  | .set x e => s!"let {x} := {exprToLean e}"
  | .print e => s!"IO.println \"{exprToLean e}\""
  | .while cond body => 
      s!"while {exprToLean cond} do\n" ++ 
      (body.toList.map stmtToLean).joinSep "  " ++ "\n"

-- Full program → compilable Lean 4
def fromUniversal (stmts : List UniversalIR.Stmt) : String :=
  s!"def main : IO Unit := do\n" ++ 
  "  -- Initial state: all variables undefined\n" ++
  (stmts.map stmtToLean).joinSep "\n  " ++ "\n"

end Codegen.ToLean
