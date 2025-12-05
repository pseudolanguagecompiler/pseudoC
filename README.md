
# Practical Pseudocode Compiler in Lean 4

**Status**: ✅ Compiles & runs  
**Command**: `lake build && lake exe pseudo_c test.pseudo`

---

`Pseudo-idea for the pseudocode for the pseudo_c compiler`

## 🌐 Overview

This project implements a **semantics-verified pseudocode compiler** written in **Lean 4**, designed to decouple *surface parsing* from *semantic interpretation*. Its long-term goal is to provide a **Universal Intermediate Representation (UniversalIR)** capable of capturing the structure and meaning of pseudocode written in multiple grammar styles.

Lean 4 is used not for convenience, but for rigor. The project uses Lean’s dependent type theory to prove **semantic correctness** of parsing and evaluation — bridging human-readable pseudocode and verifiable execution.

---

## 🧠 Philosophy

Traditional compilers blend parsing, semantics, and execution, making verification difficult.  
This compiler instead emphasizes **separation of concerns**:

1. **Parsing**: Surface grammar → abstract syntax tree (AST)  
2. **Normalization**: AST → UniversalIR (canonical form independent of grammar)  
3. **Denotation**: AST → mathematical meaning (`⟦Stmt⟧ : State → State`)  
4. **Verification**: Lean-proof theorems guarantee composability and preservation  
5. **Execution**: Lean-generated code → VM → native machine code

This approach enables **multi-grammar pseudocode compilation** and **semantic-level proofs** of correctness.

---

## ⚙️ Pipeline

Pseudocode
↓
UniversalParser.lean
↓
UniversalIR (AST + Semantics)
↓
Lean Semantic Evaluation / Proofs
↓
ToLean Codegen (Lean functions)
↓
Lean VM / LLVM Compilation → Native Execution

---

## 📑 Example

### Input: `test.pseudo`

set x := 5;
while x > 0 do
print x;
set x := x - 1;
end


### Intermediate Representation (AST)

def progAST :=
[ Stmt.While (Expr.BinOp ">" (Expr.Var "x") (Expr.Num 0))
[ Stmt.Print (Expr.Var "x")
, Stmt.Set "x" (Expr.BinOp "-" (Expr.Var "x") (Expr.Num 1)) ] ]


### Denotational Semantics in Lean

abbrev State := String → Option Nat

def evalExpr : Expr → State → Nat
| Expr.Num n, _ => n
| Expr.Var v, s => s v |>.getD 0
| Expr.BinOp "+" e1 e2, s => evalExpr e1 s + evalExpr e2 s
| Expr.BinOp "-" e1 e2, s => evalExpr e1 s - evalExpr e2 s
| Expr.BinOp ">" e1 e2, s => if evalExpr e1 s > evalExpr e2 s then 1 else 0
| _, _ => 0

def evalStmt : Stmt → State → State
| .Set x e, s => fun v => if v = x then some (evalExpr e s) else s v
| .Print e, s => (IO.println (evalExpr e s); s)
| .While cond body, s =>
if evalExpr cond s == 1 then
evalStmt (.While cond body) (evalProgram body s)
else s


### Example theorem (proof-friendly semantics)

theorem seq_compositional :
⟦S₁ ; S₂⟧ = ⟦S₁⟧ ∘ ⟦S₂⟧


This expresses **compositional semantics** — complex statements are mathematically reducible to smaller sub-statements enabling evaluation to an UniversalIR that dispatches to an intermediate IR where the denotational semantics with provable properties.

---

## 🧩 Architecture

Ast/
├── Base.lean # Core IR interface
└── UniversalIR.lean # AST definitions + semantics

Parser/
└── UniversalParser.lean # Grammar-agnostic pseudocode parser

Codegen/
└── ToLean.lean # Lean function code generation

Proofs/
└── Semantics.lean # Denotational theorems and proofs

Main.lean
lakefile.lean


---

## 🧬 Denotational Semantics and Verification

We define evaluation as a **valuation function** over typed syntactic structures:

\[
⟦Stmt⟧ : State → State
\quad\text{and}\quad
⟦Expr⟧ : State → Value
\]

By defining semantics directly over UniversalIR, the system supports:
- Multi-grammar input equivalence proofs  
- Referential transparency via recursion over ASTs  
- Composability and algebraic reasoning over programs

**Goal:** Formalize proofs of termination, correctness, and compositionality.

---

## 🧮 Why This Is Hard (and Worth It)

| Challenge | Traditional Compiler | Lean/UniversalIR Approach |
|------------|----------------------|----------------------------|
| Syntax Variability | Brittle parsing | Grammar-agnostic IR |
| Semantic Proofs | Ad hoc testing | Formal theorems in Lean |
| Flexibility vs. Rigor | Trade-off | Decoupled via UniversalIR |
| Execution Model | Hidden runtime | Explicit verified VM semantics |

A true pseudocode compiler must generalize across human syntaxes while preserving formal meaning.  
UniversalIR solves this by providing a *shared semantic domain* for all grammars — a necessary foundation for verified interpretation, translation, and optimization.

---

## 🧠 Research Goals

1. Prove **preservation theorems**: parsing → evaluation → codegen equivalence.
2. Demonstrate verified execution equivalence across grammars.
3. Extend UniversalIR with **typed expression layers**.
4. Define a micro **bytecode VM** and prove evaluation equivalence between denotation and execution.

---

## ⏱ Development Roadmap

| Week | Milestone |
|------|------------|
| 1 | Parser + UniversalIR + Main (✅ done) |
| 2 | Full `if`/`while` parsing |
| 3 | Type checker + VerifiedIR |
| 4 | Error recovery + symbol tables |
| 5 | C/JS backend codegen |
| 6 | Formal proofs + test suite |
| 7 | Boostrap pseudo code compiler in pseudocode |
---

## 🧩 Example Theorems (in progress)

theorem assign_correct :
⟦assign x e⟧ s x = some (⟦e⟧ s)

theorem seq_compositional :
⟦S1; S2⟧ = ⟦S1⟧ ∘ ⟦S2⟧

These form the basis for semantic soundness and program equivalence proofs.

---

## 🔧 Build & Run

lake build
lake exe pseudoC test.pseudo


Expected: Prints countdown 5 → 1 and outputs generated Lean 4 code equivalent to the pseudocode.

---

## 🧩 Future Work

- **Verified bytecode VM** with denotational equivalence proof  
- Multi-language frontends (C-style, Pythonic pseudocode)  
- Functional transformations verified by Lean tactics (`simp`, `rw`)  
- Type refinement and optimization proofs

---

## 🧾 References

- Winskel, *The Formal Semantics of Programming Languages*  
- Harper, *Practical Foundations for Programming Languages*  
- Chomsky, *Aspects of the Theory of Syntax*  
- Lean Prover Documentation (https://leanprover.github.io/)

---

## 🧑‍💻 Contributors

- Core developer — *LydiaQ*  
- Proofs & semantics — *coming soon, done in jupyter notebook to start* 
- Design philosophy inspired by **Chomsky's Universal Grammar + Verified Compilation**

---

## 🧮 License

MIT License.  
© 2025, [Your Name].  
Distributed under the MIT License; see `LICENSE` for details.
