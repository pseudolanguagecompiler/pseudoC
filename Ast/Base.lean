namespace Ast

-- Universal IR interface for all dialects
class IR (ι : Type) where
  Expr : Type
  Stmt : Type  
  State : Type := String → Option Nat
  Value : Type := Nat  -- Bool encoded as 0/1
  
  denoteExpr : Expr → State → Value
  denoteStmt : Stmt → State → State
  seq : (State → State) → (State → State) → State → State
    := fun f g s => f (g s)

-- Default notation for all IR instances
notation:50 "⟦" e:51 "⟧" s:52 => IR.denoteExpr _ e s
notation:50 "⟦" S:51 "⟧" s:52 => IR.denoteStmt _ S s

end Ast
