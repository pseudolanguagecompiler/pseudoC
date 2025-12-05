import Ast.UniversalIR

namespace UniversalIR

-- 1. EXACT Value type from bootstrap
inductive Value where
  | nat (n : Nat)
  | bool (b : Bool)
deriving Repr, BEq

-- 2. EXACT State = Map<String, Value> (function style)
abbrev State := String → Option Value

-- 3. EXACT stateGet/stateSet from bootstrap
def stateGet (σ : State) (x : String) : Value := σ x |>.getD (.nat 0)
def stateSet (σ : State) (x : String) (v : Value) : State := 
  fun y => if y == x then some v else σ y

-- 4. EXACT asNat/asBool coercion from bootstrap
def asNat (v : Value) : Nat := match v with
  | .nat n => n
  | .bool b => if b then 1 else 0
def asBool (v : Value) : Bool := match v with
  | .nat n => n ≠ 0
  | .bool b => b

-- 5. EXACT applyBin from bootstrap
def applyBin (op : String) (v1 : Value) (v2 : Value) : Value :=
  let n1 := asNat v1; let n2 := asNat v2
  if op = "+" then .nat (n1 + n2)
  else if op = "-" then .nat (n1 - n2)
  else if op = ">" then .bool (n1 > n2)
  else .nat 0

-- 6. EXACT evalExprU → denoteExpr
def denoteExpr : Expr → State → Value
  | .var x, s => stateGet s x
  | .num n, _ => .nat n
  | .binOp .add l r, s => applyBin "+" (denoteExpr l s) (denoteExpr r s)
  | .binOp .sub l r, s => applyBin "-" (denoteExpr l s) (denoteExpr r s)
  | .binOp .gt l r, s => applyBin ">" (denoteExpr l s) (denoteExpr r s)

notation:50 "⟦" e:51 "⟧" s:52 => denoteExpr e s

-- 7. EXACT evalStmtU → denoteStmt  
def denoteStmt : Stmt → State → State
  | .set x e, s => stateSet s x (⟦e⟧ s)
  | .print e, s => unsafe {IO.println s!"{asNat (⟦e⟧ s)}"; pure s}
  | .while cond body, s => 
    let bodyFun := body.foldl (init := id) fun acc stmt => acc ∘ (denoteStmt stmt)
    let rec loop (σ : State) : State := 
      if asBool (⟦cond⟧ σ) then loop (bodyFun σ) else σ
    loop s

-- 8. EXACT evalProgramU → execProgram
def execProgram (stmts : List Stmt) (init : State) : State :=
  stmts.foldl (init := id) fun acc stmt => acc ∘ (denoteStmt stmt) | init

end UniversalIR
