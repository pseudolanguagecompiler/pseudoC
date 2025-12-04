# Practical Pseudocode Compiler in Lean 4

**Status: Compiles & runs** (`lake build && lake exe pseudoC test.pseudo`)

**Design philosophy**: Our philosophy is to decouple surface parsing from semantics enabling parsing pseoducode into consitutent trees independent of grammar specifcs. We do this by providing a universal IR interface.

Lean works well verification/proofs, not parsing. Focus on **one clean grammar** with solid semantics first, **modular design** for multiple IRs/grammars later. This multi-grammar abstraction supports shared hierarchical intermediate languages that abstract surface variations across languages, enabling a single parser (defined in AST folder) to handle diverse syntactic rules via parameter settings or transformations.

Universal grammars allow head direction while maintaining core hierarchies, mirroring traditional AST-based IR noralization of inputs from multiple grammars into one tree structure for pseudocode parsing. 

**Pipeline**: Pseudocode → `UniversalParser` → `UniversalIR` (AST+Semantics) → `ToLean` codegen

## Formal Properties

-- Prove in Proofs/Semantics.lean or Jupyter:
theorem assign_correct : ⟦assign x e⟧ s x = some (⟦e⟧ s)
theorem seq_compositional : ⟦S1; S2⟧ = ⟦S1⟧ ∘ ⟦S2⟧


**Real denotational semantics** ⟦S⟧ : Stmt → (State → State)  
**Proof-ready**: Direct equations for `simp`, `rw` tactics


## Current Features
- Parses `set`, `print`, basic `Expr` (`x > 0`, `x - 1`)
- Denotational semantics: `State = Name → Option Nat`
- Lean 4 code generation
- Modular IR architecture (`Ast/Base.lean` interface) for support multi-grammars with a single UniversalGrammar interface (inspired by Chomnsky universal grammar)

## 📁 File Structure

Ast/
├── Base.lean # IR interface
└── UniversalIR.lean # AST + semantics
Parser/
└── UniversalParser.lean
Codegen/
└── ToLean.lean
Main.lean
lakefile.lean

text

## To build and run:

lake build
lake exe pseudocode_compiler test.pseudo

text

## test.pseudo

set x := 5;
while x > 0 do
print x;
set x := x - 1;
end

text

**Expected**: Prints execution result + generated Lean 4 code

## Grammar Specification (Phase 1)

Program ::= Statement*
Statement ::= "set" ID ":=" Expr ";" | "print" Expr ";"
Expr ::= ID | Number | "(" Expr ")" | Expr ("+"|"-"|">") Expr
ID ::= [a-zA-Z_][a-zA-Z0-9_]*

text

**Future**: `if/while` full recursive parsing (Week 2)

## Architecture

Pseudocode → UniversalParser → UniversalIR.AST → UniversalIR.Semantics → Codegen.ToLean
↓
Ast.Base.IR interface (extensible)

text

## Denotational Semantics (in `UniversalIR.lean`)

abbrev State := String → Option Nat
def evalExpr : Expr → State → Nat
def evalStmt : Stmt → State → State
def evalProgram : List Stmt → State → IO Nat

text

## 6-Week Plan (Updated)
- **Week 1**: Parser + UniversalIR + Main (done)
- **Week 2**: Full `if/while` parsing + roundtrip tests
- **Week 3**: Type checker + VerifiedIR
- **Week 4**: Error recovery + symbol tables
- **Week 5**: C/JS backends
- **Week 6**: Proofs + test suite

## Academic Contributions
1. **Verified Semantics**: Lean 4 proves preservation
2. **Modular IRs**: `Ast/Base.IR` interface
3. **Pedagogical Grammar**: LL(1), textbook-friendly
4. **Extensible**: Dialect registry ready

**Next**: Full `while` loop execution (fix recursive `evalStmt`)
