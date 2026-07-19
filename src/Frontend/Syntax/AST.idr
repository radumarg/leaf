module Frontend.Syntax.AST

import Data.List1
import Frontend.Token
import Frontend.ASTPhases
import Frontend.Syntax.Attribute
import Frontend.Syntax.Common
import Frontend.Syntax.Contract
import Frontend.Syntax.Doc
import Frontend.Syntax.Literal
import Frontend.Syntax.Name
import Frontend.Syntax.Operator
import Frontend.Syntax.Pattern
import Frontend.Syntax.Type

%default total

--------------------------------------------------------------------------------
-- The central mutually recursive surface AST
--------------------------------------------------------------------------------
-- Everything mutually recursive lives in this one module: programs, items,
-- declarations, blocks, statements, and expressions. The leaf modules
-- (Name, Doc, Literal, Operator, Common, Attribute, Pattern) are acyclic;
-- Type and Contract are parameterized over the expression type, and THIS
-- module ties both knots:
--
--   SurfaceTy             = LocatedTy             SurfaceExpr
--   SurfaceContractClause = LocatedContractClause SurfaceExpr
--
-- Design rules carried through from the earlier modules:
--
--   * every important node is located (wrapped in SurfaceAstNode)
--   * operators and keyword-like leaves that diagnostics point AT are
--     individually located (assignment/unary/binary/range operators, the
--     := marker, storage qualifiers, effects, support kinds)
--   * raw literal spellings are preserved; nothing is normalized
--   * if / qif / sif are three node families, not one; likewise
--     match / qmatch / smatch
--   * `()` is LiteralUnit; tuples are List1 (>= 1 element, `(e,)` legal);
--     ExprParenthesized keeps `(e)` distinct from `(e,)`
--   * semantic rules stay representable (see per-node comments)
--
-- Spec-tracking omissions (parser rejections pending a language ruling):
--   * nested items inside blocks (the spec's top-level-items rule is read
--     as: items appear at top level and inside mod/impl only)
--   * `use path as alias;` and glob imports
--   * `pub` on struct fields
--   * labeled loops / labeled break
--------------------------------------------------------------------------------

mutual

  ------------------------------------------------------------------------------
  -- Located aliases (defined first; mutual-block visibility makes the
  -- forward references to the node families below legal)
  ------------------------------------------------------------------------------

  public export
  SurfaceExpr : Type
  SurfaceExpr = SurfaceAstNode ExpressionNode

  public export
  SurfaceBlock : Type
  SurfaceBlock = SurfaceAstNode BlockNode

  public export
  SurfaceStatement : Type
  SurfaceStatement = SurfaceAstNode StatementNode

  public export
  SurfaceItem : Type
  SurfaceItem = SurfaceAstNode ItemNode

  -- The knots: written types and contract clauses with full surface
  -- expressions in their expression positions.
  public export
  SurfaceTy : Type
  SurfaceTy = LocatedTy SurfaceExpr

  public export
  SurfaceContractClause : Type
  SurfaceContractClause = LocatedContractClause SurfaceExpr

  ------------------------------------------------------------------------------
  -- Source file
  ------------------------------------------------------------------------------

  -- One parsed SOURCE FILE -- deliberately not called "program". A Leaf
  -- program is a tree of files stitched together by `mod name;` external
  -- module declarations; the parser only ever produces one file's worth of
  -- items. The assembled whole-program structure (crate/compilation unit,
  -- one SurfaceSourceFile per file) belongs to the module loader outside
  -- the Syntax namespace, which is also where whole-program checks such as
  -- "exactly one main at the root" live -- a lone file with zero mains is a
  -- perfectly good library module and must parse.
  --
  -- Inner docs (//! and /*! ... */ at the top of the file) document the
  -- file/module itself.
  public export
  record SourceFileNode where
    constructor MkSourceFileNode
    sourceFileInnerDocs : List SurfaceDocComment
    sourceFileItems     : List SurfaceItem

  ------------------------------------------------------------------------------
  -- Items
  ------------------------------------------------------------------------------
  -- Top-level code consists of items; `let i = 1;` at top level is a parse
  -- error, which this family encodes by simply having no statement arm.

  public export
  data ItemNode : Type where
    ItemModule   : (moduleDeclaration   : ModuleDeclarationNode)   -> ItemNode -- module declaration
    ItemUse      : (useDeclaration      : UseDeclarationNode)      -> ItemNode -- use statement
    ItemConst    : (constDeclaration    : ConstDeclarationNode)    -> ItemNode -- const declaration
    ItemEnum     : (enumDeclaration     : EnumDeclarationNode)     -> ItemNode -- enum declaration
    ItemQEnum    : (qenumDeclaration    : QEnumDeclarationNode)    -> ItemNode -- qenum declaration
    ItemStruct   : (structDeclaration   : StructDeclarationNode)   -> ItemNode -- struct declaration
    ItemImpl     : (implDeclaration     : ImplDeclarationNode)     -> ItemNode -- impl block declaration
    ItemFunction : (functionDeclaration : FunctionDeclarationNode) -> ItemNode -- function declaration

  ------------------------------------------------------------------------------
  -- Function declarations
  ------------------------------------------------------------------------------

  -- A function declaration preserves everything the user wrote around the
  -- signature. The surface AST records that the user WROTE `unitary`; it
  -- does not know or care whether the body is unitary.
  public export
  record FunctionDeclarationNode where
    constructor MkFunctionDeclarationNode
    functionDocs       : List SurfaceDocComment
    functionAttributes : List SurfaceAttribute
    functionVisibility : Maybe (SurfaceAstNode VisbilityQualifier)
    -- `const fn` -- may be evaluated at compile time.
    isConstFunction    : Bool
    -- Nothing: no effect written (treated as general later).
    -- Just (located EffectGeneral): the user explicitly wrote `general`.
    functionEffect     : Maybe (SurfaceAstNode FunctionEffect)
    functionName       : SurfaceName
    functionParameters : List (SurfaceAstNode FunctionParameterNode)
    -- Nothing: no `->` written (distinct from an explicit `-> ()`).
    returnType         : Maybe SurfaceTy
    -- Empty: no `supports` clause written. A written clause always names
    -- at least one kind, so emptiness is unambiguous.
    supportClause      : List (SurfaceAstNode SupportKind)
    -- requires/ensures in SOURCE ORDER; the requires-before-ensures rule
    -- is a validation check against this order, not an AST shape.
    contractClauses    : List SurfaceContractClause
    functionBody       : SurfaceBlock

  -- Declaration-side parameters. Two shapes:
  --
  --   fn f(mut x: i32, person: &Person)   -- typed name binders
  --   fn is_adult(&self) -> bool          -- method receiver
  --
  -- Normal parameters require BOTH a name and a type (the spec never shows
  -- untyped or pattern-bound parameters; if `fn f((a, b): (i32, i32))` is
  -- ever ruled legal, the name field becomes a SurfacePattern). Parameters
  -- carry their own outer docs, per the doc-comment attachment rules.
  public export
  data FunctionParameterNode : Type where

    NormalParameter :
         (parameterDocs       : List SurfaceDocComment)
      -> (parameterMutability : Maybe (SurfaceAstNode Mutability))
      -> (parameterName       : SurfaceName)
      -> (parameterType       : SurfaceTy)
      -> FunctionParameterNode

    -- `self`, `&self`, `&mut self`. Nothing = plain `self` (by value);
    -- Just borrow = `&self` / `&mut self`. Only the spec's `&self` is
    -- currently exercised; the others are representable and validated later.
    ReceiverParameter :
         (receiverDocs   : List SurfaceDocComment)
      -> (receiverBorrow : Maybe (SurfaceAstNode BorrowKind))
      -> FunctionParameterNode

  ------------------------------------------------------------------------------
  -- Struct / enum / qenum declarations
  ------------------------------------------------------------------------------

  public export
  record StructDeclarationNode where
    constructor MkStructDeclarationNode
    structDocs       : List SurfaceDocComment
    structAttributes : List SurfaceAttribute
    structVisibility : VisbilityQualifier
    structName       : SurfaceName
    structFields     : List (SurfaceAstNode StructFieldNode)

  -- One named field: `x: f64`. (No per-field visibility: the spec never
  -- writes `pub` on fields; parser rejection pending a ruling.)
  public export
  record StructFieldNode where
    constructor MkStructFieldNode
    fieldDocs : List SurfaceDocComment
    fieldName : SurfaceName
    fieldType : SurfaceTy

  public export
  record EnumDeclarationNode where
    constructor MkEnumDeclarationNode
    enumDocs       : List SurfaceDocComment
    enumAttributes : List SurfaceAttribute
    enumVisibility : VisbilityQualifier
    enumName       : SurfaceName
    enumVariants   : List (SurfaceAstNode EnumVariantNode)

  -- Classical enum variants mix freely within one enum:
  --   Zero,                      -- unit-like
  --   Left(i32),                 -- tuple-like (>= 1 payload type)
  --   Move { x: i32, y: i32 },   -- struct-like
  public export
  record EnumVariantNode where
    constructor MkEnumVariantNode
    variantDocs : List SurfaceDocComment
    variantName : SurfaceName
    variantBody : EnumVariantBody

  public export
  data EnumVariantBody : Type where
    VariantUnit   : EnumVariantBody
    VariantTuple  : (payloadTypes  : List1 SurfaceTy) -> EnumVariantBody
    VariantStruct : (payloadFields : List (SurfaceAstNode StructFieldNode))
                 -> EnumVariantBody

  -- qenum: ONLY tuple-like variants exist, which the AST enforces by
  -- construction -- there is no unit or struct arm to build.
  public export
  record QEnumDeclarationNode where
    constructor MkQEnumDeclarationNode
    qenumDocs       : List SurfaceDocComment
    qenumAttributes : List SurfaceAttribute
    qenumVisibility : VisbilityQualifier
    qenumName       : SurfaceName
    qenumVariants   : List (SurfaceAstNode QEnumVariantNode)

  public export
  record QEnumVariantNode where
    constructor MkQEnumVariantNode
    qenumVariantDocs         : List SurfaceDocComment
    qenumVariantName         : SurfaceName
    qenumVariantPayloadTypes : List1 SurfaceTy

  ------------------------------------------------------------------------------
  -- impl / const / use / mod declarations
  ------------------------------------------------------------------------------

  -- `impl Person { fn new(...) -> Person { ... } ... }`. Only function
  -- declarations occur inside impl blocks per the spec.
  public export
  record ImplDeclarationNode where
    constructor MkImplDeclarationNode
    implDocs      : List SurfaceDocComment
    implTarget    : SurfacePath
    implFunctions : List (SurfaceAstNode FunctionDeclarationNode)

  -- `const FIVE: i32 = 5;`. The type annotation is MANDATORY on consts
  -- (as in Rust; the spec always writes it).
  public export
  record ConstDeclarationNode where
    constructor MkConstDeclarationNode
    constDocs       : List SurfaceDocComment
    constVisibility : VisbilityQualifier
    constName       : SurfaceName
    constType       : SurfaceTy
    constValue      : SurfaceExpr

  -- `use my_library::helper;`
  public export
  record UseDeclarationNode where
    constructor MkUseDeclarationNode
    useDocs       : List SurfaceDocComment
    useVisibility : VisbilityQualifier
    usePath       : SurfacePath

  -- Two source forms:
  --   mod my_module { ...items... }   -- inline body
  --   mod my_library;                 -- external file
  public export
  record ModuleDeclarationNode where
    constructor MkModuleDeclarationNode
    moduleDocs       : List SurfaceDocComment
    moduleVisibility : VisbilityQualifier
    moduleName       : SurfaceName
    moduleBody       : ModuleBody

  public export
  data ModuleBody : Type where
    ModuleInline :
         (moduleInnerDocs : List SurfaceDocComment)
      -> (moduleItems     : List SurfaceItem)
      -> ModuleBody
    ModuleExternal :
         ModuleBody

  ------------------------------------------------------------------------------
  -- Blocks
  ------------------------------------------------------------------------------

  -- Rust-style semicolon rules, preserved structurally: statements end in
  -- `;` (or are block-like), and the optional trailing expression WITHOUT
  -- `;` is the block's value.
  public export
  record BlockNode where
    constructor MkBlockNode
    blockInnerDocs  : List SurfaceDocComment
    blockStatements : List SurfaceStatement
    finalExpression : Maybe SurfaceExpr

  ------------------------------------------------------------------------------
  -- Statements
  ------------------------------------------------------------------------------

  public export
  data StatementNode : Type where

    StatementLet :
         (letBinding : LetBindingNode)
      -> StatementNode

    StatementAssignment :
         (assignment : AssignmentNode)
      -> StatementNode

    -- An expression statement WITH a written `;`:  f(&q);  x + 1;
    StatementSemiExpression :
         (statementExpression : SurfaceExpr)
      -> StatementNode

    -- A block-like expression in statement position WITHOUT `;`, in
    -- non-final position:  if c { ... } else { ... }  loop { ... }
    -- Distinct from StatementSemiExpression so "expected `;`" diagnostics
    -- and pretty-printing see what was written.
    StatementExpression :
         (statementExpression : SurfaceExpr)
      -> StatementNode

  -- let [qualifiers] pattern [: Ty] [= | := expr] ;
  --
  -- The initializer bundles the marker WITH the expression, so
  -- "marker without initializer" is unrepresentable; `let a: [i32; 4];`
  -- (no initializer at all) is Nothing.
  public export
  record LetBindingNode where
    constructor MkLetBindingNode
    -- Source order; empty = none written. `let scratch linear q = ...`.
    letQualifiers    : List (SurfaceAstNode QuantumStorageQualifier)
    letPattern       : SurfacePattern
    letTypeAnnotation : Maybe SurfaceTy
    letInitializer   : Maybe LetInitializerNode

  public export
  record LetInitializerNode where
    constructor MkLetInitializerNode
    -- Located: the `:=` span is what "auto-uncompute is not allowed on
    -- classical bindings"-style diagnostics point at.
    initializerMarker : SurfaceAstNode InitializerMarker
    initializerValue  : SurfaceExpr

  -- target op value;  where op is =, +=, <<=, ... (never :=).
  public export
  record AssignmentNode where
    constructor MkAssignmentNode
    assignmentTarget   : SurfaceAstNode AssignmentTargetNode
    assignmentOperator : SurfaceAstNode AssignmentOperator
    assignmentValue    : SurfaceExpr

  -- The assignable-place grammar, kept apart from general expressions:
  --   x = 5;   x[0] = 10;   p.x = v;   t.0 = 3;
  -- Base positions are expressions (qs[i].0 = ... nests), but the OUTER
  -- shape is one of exactly these four; `f() = 5;` is unrepresentable.
  public export
  data AssignmentTargetNode : Type where

    AssignTargetName :
         (targetName : SurfaceName)
      -> AssignmentTargetNode

    AssignTargetIndex :
         (targetObject    : SurfaceExpr)
      -> (indexExpression : SurfaceExpr)
      -> AssignmentTargetNode

    AssignTargetField :
         (targetObject : SurfaceExpr)
      -> (fieldName    : SurfaceName)
      -> AssignmentTargetNode

    -- t.0 = 3;  index spelling preserved raw, like every numeric literal.
    AssignTargetTupleIndex :
         (targetObject      : SurfaceExpr)
      -> (tupleIndexRawText : String)
      -> AssignmentTargetNode

  ------------------------------------------------------------------------------
  -- Expressions
  ------------------------------------------------------------------------------

  public export
  data ExpressionNode : Type where

    ExprLiteral :
         (literal : SurfaceLiteral)
      -> ExpressionNode

    -- A lone identifier. PARSER RULE (mirroring PatternName/PatternPath):
    -- one segment => ExprName, multiple segments => ExprPath.
    ExprName :
         (valueName : SurfaceName)
      -> ExpressionNode

    -- Data::Left, Person::new, my_library::helper -- what the path denotes
    -- (variant, associated function, imported item) is resolution's job.
    ExprPath :
         (valuePath : SurfacePath)
      -> ExpressionNode

    -- A non-shadowable builtin in expression (usually callee) position:
    -- qalloc, measr, reset, discard, uncompute, weaken, barrier, ...
    -- These are ordinary calls -- ExprCall (ExprBuiltin BuiltinMeasr) [q] --
    -- with NO dedicated per-builtin nodes; only ctrl/adjoint have real
    -- grammar of their own (below).
    ExprBuiltin :
         (builtinFunction : Builtin)
      -> ExpressionNode

    -- `self` in method bodies.
    ExprSelf :
         ExpressionNode

    -- (e) -- kept distinct from (e,) [one-element ExprTuple]; discarded at
    -- canonicalization. Same story as TyParenthesized/PatternParenthesized.
    ExprParenthesized :
         (innerExpression : SurfaceExpr)
      -> ExpressionNode

    -- (a, b), (e,). At least one element; `()` is ExprLiteral LiteralUnit.
    ExprTuple :
         (tupleElements : List1 SurfaceExpr)
      -> ExpressionNode

    -- [1, 2, 3]; [] is legal syntax (typed elsewhere).
    ExprArray :
         (arrayElements : List SurfaceExpr)
      -> ExpressionNode

    -- [0; 3] -- element repeated count times; count is const-checked later.
    ExprRepeatedArray :
         (repeatedElement : SurfaceExpr)
      -> (repeatCount     : SurfaceExpr)
      -> ExpressionNode

    -- Point { x: 1.0, y: 2.0 } / Pair { q0, q1 } -- shorthand vs explicit
    -- preserved per field.
    ExprStructLiteral :
         (structPath        : SurfacePath)
      -> (fieldInitializers : List (SurfaceAstNode FieldInitializerNode))
      -> ExpressionNode

    -- f(a, b) -- callee is a full expression: names, paths, builtins,
    -- adjoint(f), ctrl(...).apply(f), parenthesized expressions.
    ExprCall :
         (callee        : SurfaceExpr)
      -> (callArguments : List SurfaceExpr)
      -> ExpressionNode

    -- a.len(), sq1.tensor(sq2), (0..n).rev(). The method name is stored
    -- textually even when it arrives as a builtin token (`tensor`): its
    -- category is recoverable via builtinFromString, and method-position
    -- resolution is a later concern.
    ExprMethodCall :
         (receiver        : SurfaceExpr)
      -> (methodName      : SurfaceName)
      -> (methodArguments : List SurfaceExpr)
      -> ExpressionNode

    -- p.x
    ExprField :
         (fieldObject : SurfaceExpr)
      -> (fieldName   : SurfaceName)
      -> ExpressionNode

    -- t.0 -- raw index spelling preserved.
    ExprTupleIndex :
         (indexedTuple      : SurfaceExpr)
      -> (tupleIndexRawText : String)
      -> ExpressionNode

    -- a[i], and also slicing: a[1..4] is ExprIndex with a range index --
    -- slicing is not a separate node, exactly as in Rust's AST. `&a[1..4]`
    -- is ExprUnary borrow around this.
    ExprIndex :
         (indexedObject   : SurfaceExpr)
      -> (indexExpression : SurfaceExpr)
      -> ExpressionNode

    -- Operators are located so "cannot apply `+` here" points at the `+`.
    ExprUnary :
         (unaryOperator : SurfaceAstNode UnaryOperator)
      -> (operand       : SurfaceExpr)
      -> ExpressionNode

    ExprBinary :
         (binaryOperator : SurfaceAstNode BinaryOperator)
      -> (leftOperand    : SurfaceExpr)
      -> (rightOperand   : SurfaceExpr)
      -> ExpressionNode

    -- a..b, a.., ..=5, .. -- both endpoints optional; `a..=` (inclusive
    -- with no end) is a parser rejection, not an AST impossibility.
    ExprRange :
         (rangeStart    : Maybe SurfaceExpr)
      -> (rangeOperator : SurfaceAstNode RangeOperator)
      -> (rangeEnd      : Maybe SurfaceExpr)
      -> ExpressionNode

    -- e as T
    ExprCast :
         (castOperand : SurfaceExpr)
      -> (castTarget  : SurfaceTy)
      -> ExpressionNode

    ExprBlock :
         (blockExpression : SurfaceBlock)
      -> ExpressionNode

    ExprIf :
         (ifExpression : ClassicalIfNode)
      -> ExpressionNode

    ExprQIf :
         (qifExpression : QuantumIfNode)
      -> ExpressionNode

    ExprSIf :
         (sifExpression : StateIfNode)
      -> ExpressionNode

    ExprMatch :
         (matchExpression : ClassicalMatchNode)
      -> ExpressionNode

    ExprQMatch :
         (qmatchExpression : QuantumMatchNode)
      -> ExpressionNode

    ExprSMatch :
         (smatchExpression : StateMatchNode)
      -> ExpressionNode

    ExprLoop :
         (loopBody : SurfaceBlock)
      -> ExpressionNode

    ExprWhile :
         (whileCondition : SurfaceExpr)
      -> (whileBody      : SurfaceBlock)
      -> ExpressionNode

    -- for i in 1..6 { ... } -- the binder is a full pattern
    -- (for (a, b) in ... is representable; a ruling can restrict it).
    ExprFor :
         (forPattern       : SurfacePattern)
      -> (forIterExpression : SurfaceExpr)
      -> (forBody          : SurfaceBlock)
      -> ExpressionNode

    ExprBreak :
         (breakValue : Maybe SurfaceExpr)
      -> ExpressionNode

    ExprContinue :
         ExpressionNode

    ExprReturn :
         (returnValue : Maybe SurfaceExpr)
      -> ExpressionNode

    ExprCtrl :
         (controlExpression : ControlExpressionNode)
      -> ExpressionNode

    ExprAdjoint :
         (adjointExpression : AdjointExpressionNode)
      -> ExpressionNode

  -- One field in a struct literal: Pair { q0, q1 } vs Point { x: 1.0 }.
  public export
  data FieldInitializerNode : Type where

    FieldInitShorthand :
         (fieldAndValueName : SurfaceName)
      -> FieldInitializerNode

    FieldInitExplicit :
         (fieldName  : SurfaceName)
      -> (fieldValue : SurfaceExpr)
      -> FieldInitializerNode

  ------------------------------------------------------------------------------
  -- if / qif / sif -- three families, deliberately not merged
  ------------------------------------------------------------------------------

  -- if cond { ... } [else { ... } | else if ...]
  -- Branches are blocks; else-if chains nest through ElseChainedIf, so
  -- "else associates with the nearest unmatched if" is structural.
  public export
  record ClassicalIfNode where
    constructor MkClassicalIfNode
    ifCondition  : SurfaceExpr
    ifThenBlock  : SurfaceBlock
    ifElseBranch : Maybe ClassicalElseNode

  public export
  data ClassicalElseNode : Type where
    ElseBlock     : (elseBlock : SurfaceBlock) -> ClassicalElseNode
    ElseChainedIf : (chainedIf : SurfaceAstNode ClassicalIfNode)
                 -> ClassicalElseNode

  -- qif cond { ... } [qelse { ... }]      -- block branches
  -- qif c e1 qelse e2                     -- bare expression branches
  -- qelse optional, no `then` keyword; each branch independently records
  -- whether it was a block or a bare expression.
  public export
  record QuantumIfNode where
    constructor MkQuantumIfNode
    qifCondition  : SurfaceExpr
    qifThenBranch : QuantumBranchNode
    qifElseBranch : Maybe QuantumBranchNode

  public export
  data QuantumBranchNode : Type where
    QuantumBranchBlock      : (branchBlock : SurfaceBlock) -> QuantumBranchNode
    QuantumBranchExpression : (branchExpression : SurfaceExpr)
                           -> QuantumBranchNode

  -- sif cond then e1 selse e2 -- expression-only, selse MANDATORY, `then`
  -- keyword required: all structural here, none of it deferred.
  public export
  record StateIfNode where
    constructor MkStateIfNode
    sifCondition      : SurfaceExpr
    sifThenExpression : SurfaceExpr
    sifElseExpression : SurfaceExpr

  ------------------------------------------------------------------------------
  -- match / qmatch / smatch -- three families
  ------------------------------------------------------------------------------

  public export
  record ClassicalMatchNode where
    constructor MkClassicalMatchNode
    matchScrutinee : SurfaceExpr
    matchArms      : List (SurfaceAstNode ClassicalMatchArmNode)

  -- pattern [if guard] => body   -- the guard lives on the ARM (it
  -- conditions the arm, it is not part of the pattern), which is also what
  -- keeps Pattern.idr independent of expressions.
  public export
  record ClassicalMatchArmNode where
    constructor MkClassicalMatchArmNode
    armPattern : SurfacePattern
    armGuard   : Maybe SurfaceExpr
    armBody    : SurfaceExpr

  public export
  record QuantumMatchNode where
    constructor MkQuantumMatchNode
    qmatchScrutinee : SurfaceExpr
    qmatchArms      : List (SurfaceAstNode QuantumMatchArmNode)

  public export
  record StateMatchNode where
    constructor MkStateMatchNode
    smatchScrutinee : SurfaceExpr
    smatchArms      : List (SurfaceAstNode QuantumMatchArmNode)

  -- Shared by qmatch and smatch: the pattern grammar is the shared
  -- QuantumMatchPatternNode (Pattern.idr), with smatch's restrictions
  -- (no wildcard, no qenum variants) enforced at parse time. No guards in
  -- quantum matches -- the spec has none, so the field does not exist.
  public export
  record QuantumMatchArmNode where
    constructor MkQuantumMatchArmNode
    quantumArmPattern : SurfaceQuantumMatchPattern
    quantumArmBody    : SurfaceExpr

  ------------------------------------------------------------------------------
  -- ctrl and adjoint -- the two builtins with genuine grammar
  ------------------------------------------------------------------------------

  -- Both are modeled as CALLABLE-PRODUCING expressions plus a block form.
  -- Application is ordinary ExprCall around them:
  --
  --   adjoint(f)(q1, q2)      = ExprCall (ExprAdjoint (AdjointOfCallable f)) [q1, q2]
  --   let g = adjoint(f);     = ExprAdjoint (AdjointOfCallable f)   -- no call
  --   ctrl(c).apply(H)(&q2)   = ExprCall (ExprCtrl (ControlledCallable ...)) [&q2]
  --
  -- This shape exists because `adjoint(f)` IS a first-class value in the
  -- spec ("adjoint as higher order function"); bundling target arguments
  -- into the node, as a plainer design would, cannot represent the uncalled
  -- form without an empty-argument hack.

  public export
  data ControlExpressionNode : Type where

    -- ctrl(c0, c1)[.on(bs"10")].apply(f) -- at least one control; the
    -- optional basis string is preserved raw (bs"10" as written).
    ControlledCallable :
         (controlQubits  : List1 SurfaceExpr)
      -> (onBasisRaw     : Maybe (SurfaceAstNode String))
      -> (controlledCallable : SurfaceExpr)
      -> ControlExpressionNode

    -- ctrl(c0, c1)[.on(bs"10")] { ...body... }
    ControlledBlock :
         (controlQubits   : List1 SurfaceExpr)
      -> (onBasisRaw      : Maybe (SurfaceAstNode String))
      -> (controlledBlock : SurfaceBlock)
      -> ControlExpressionNode

  public export
  data AdjointExpressionNode : Type where

    -- adjoint(f) -- a first-class callable value.
    AdjointOfCallable :
         (adjointedCallable : SurfaceExpr)
      -> AdjointExpressionNode

    -- adjoint { ...body... }
    AdjointBlock :
         (adjointedBlock : SurfaceBlock)
      -> AdjointExpressionNode

--------------------------------------------------------------------------------
-- Remaining located aliases
--------------------------------------------------------------------------------

public export
SurfaceSourceFile : Type
SurfaceSourceFile = SurfaceAstNode SourceFileNode

public export
SurfaceFunctionDeclaration : Type
SurfaceFunctionDeclaration = SurfaceAstNode FunctionDeclarationNode

public export
SurfaceFunctionParameter : Type
SurfaceFunctionParameter = SurfaceAstNode FunctionParameterNode

public export
SurfaceStructDeclaration : Type
SurfaceStructDeclaration = SurfaceAstNode StructDeclarationNode

public export
SurfaceEnumDeclaration : Type
SurfaceEnumDeclaration = SurfaceAstNode EnumDeclarationNode

public export
SurfaceQEnumDeclaration : Type
SurfaceQEnumDeclaration = SurfaceAstNode QEnumDeclarationNode

public export
SurfaceImplDeclaration : Type
SurfaceImplDeclaration = SurfaceAstNode ImplDeclarationNode

public export
SurfaceConstDeclaration : Type
SurfaceConstDeclaration = SurfaceAstNode ConstDeclarationNode

public export
SurfaceUseDeclaration : Type
SurfaceUseDeclaration = SurfaceAstNode UseDeclarationNode

public export
SurfaceModuleDeclaration : Type
SurfaceModuleDeclaration = SurfaceAstNode ModuleDeclarationNode

public export
SurfaceLetBinding : Type
SurfaceLetBinding = SurfaceAstNode LetBindingNode

public export
SurfaceAssignmentTarget : Type
SurfaceAssignmentTarget = SurfaceAstNode AssignmentTargetNode

public export
SurfaceClassicalMatchArm : Type
SurfaceClassicalMatchArm = SurfaceAstNode ClassicalMatchArmNode

public export
SurfaceQuantumMatchArm : Type
SurfaceQuantumMatchArm = SurfaceAstNode QuantumMatchArmNode

public export
SurfaceContractPredicate : Type
SurfaceContractPredicate = LocatedContractPredicate SurfaceExpr
