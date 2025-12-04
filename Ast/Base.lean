namespace Ast
class IR (ι : Type) where
  Expr : Type
  Stmt : Type
  State : Type
  denoteExpr : Expr → State → Nat
  denoteStmt : Stmt → (State → State)
  seq : (State → State) → (State → State) → (State → State)
end Ast
