import Ast

def evalExpr : Expr → State → Nat
  | .var x, s => s x |>.getD 0
  | .num n, _ => n
  | .binOp .add l r, s => evalExpr l s + evalExpr r s
  | .binOp .sub l r, s => evalExpr l s - evalExpr r s
  | .binOp .gt l r, s => if evalExpr l s > evalExpr r s then 1 else 0

def evalStmt : Stmt → State → State
  | .assign x e, s => fun y => if y == x then some (evalExpr e s) else s y
  | .print e, s => fun y => do IO.println (evalExpr e s); s y
  | .if_ cond then_ else_, s => 
    if evalExpr cond s ≠ 0 then 
      else_.foldl (init := s) (· ∘ evalStmt ·)
    else 
      then_.foldl (init := s) (· ∘ evalStmt ·)
  | .while cond body, s => 
    if evalExpr cond s ≠ 0 then
      evalStmt (.while cond body) (body.foldl (init := s) (· ∘ evalStmt ·))
    else s

def evalProgram (stmts : List Stmt) (init : State) : IO Nat := do
  let final := stmts.foldl (init := init) (· ∘ evalStmt ·)
  pure (final "")
