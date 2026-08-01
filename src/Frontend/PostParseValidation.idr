module Frontend.PostParseValidation

import Data.List
import Data.List1
import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Source
import Frontend.Token
import Frontend.Syntax.AST
import Frontend.Syntax.Attribute
import Frontend.Syntax.Common
import Frontend.Syntax.Contract
import Frontend.Syntax.Name
import Frontend.Syntax.Operator
import Frontend.Syntax.Pattern
import Frontend.Syntax.Type

%default total

--------------------------------------------------------------------------------
-- Post-parse syntactic validation
--------------------------------------------------------------------------------
-- The pass that owns every check which is (a) NOT enforced by the AST's
-- shape, (b) NOT a parser rejection, and (c) decidable on the SURFACE TREE
-- ALONE -- no name resolution, no types. It runs immediately after parsing
-- and reports against the spans the AST preserved for exactly this purpose.
--
-- CHECKS OWNED HERE:
--   * storage qualifier conflicts and duplicates
--       `linear affine q`, `scratch scratch q` -- on let binders and on
--       quantum-qualified types
--   * requires-before-ensures ordering of contract clauses
--   * known-attribute argument shapes
--       qasm_gate / qasm_def take nothing or exactly one string; `#[...]()`
--       with empty parens is rejected; unknown attributes are rejected
--       (currently an ERROR; soften to a warning when warning infrastructure
--       exists)
--   * duplicate kinds in a `supports` clause (`supports adjoint, adjoint`)
--   * mixing integer and basis-string patterns in one qmatch/smatch
--   * `&mut` on a SYNTACTICALLY-qubit type (&mut qubit, &mut [qubit],
--       &mut [qubit; 2], through parens/qualifiers) -- the general case
--       (&mut SomeStructContainingQubits) needs types and is deferred
--   * break/continue outside a loop (including Rust's rule that a while
--       condition / for iterator does NOT count as "inside" its own loop)
--   * return outside a function body (e.g. inside a const initializer)
--
-- DEFERRED, with their owners -- this is the consolidated inventory of every
-- "later pass" comment in the Syntax modules:
--   RESOLUTION: duplicate binders in one pattern; duplicate fields in struct
--     literals/patterns/declarations; unknown names/fields/variants; variant
--     arity vs. declaration; path-pattern variant-vs-const disambiguation
--   TYPING: const-ness of array sizes and repeat counts; Pauli string length
--     vs. qubit count; contract arguments must be qubit designators;
--     := only on qubit-producing bindings; qif/sif condition and branch
--     typing; &mut on qubit types reached through paths; qmatch scrutinee
--     arity vs. pattern width; effect checking (unitary is actually unitary)
--   PARSER (already rejected before this pass runs): wildcard/variant
--     patterns in smatch; `a..=` with no end; inner docs not at block start;
--     nested items in blocks; non-item top-level statements
--   MODULE LOADER: exactly one `main` at the crate root; `mod name;`
--     resolves to a file
--
-- Shape of the pass: a plain structural walk accumulating a List of errors
-- (empty list = valid). No early exit -- diagnostics improve when the user
-- sees every independent problem at once.
--
-- TOTALITY DISCIPLINE (learned the hard way; applies to every future walk):
-- Idris 2's size-change checker credits CONSTRUCTOR PATTERNS ONLY. A record
-- dot-projection feeding a recursive argument (`validateBlock ctx
-- fd.functionBody`) is size-unknown, and one unknown edge poisons the whole
-- SCC -- and this walk is ONE SCC, because expressions and types are mutual
-- (casts contain types; array sizes contain expressions). So: destructure
-- every record in the pattern head or in a constructor pattern; never
-- project in a recursive position. Projections remain fine on non-recursive
-- data (ValidationContext, SourceSpan).
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Validation errors
--------------------------------------------------------------------------------

public export
data ValidationError : Type where

  ConflictingStorageQualifiers :
       (errorSpan : SourceSpan)
    -> (firstQualifier  : QuantumStorageQualifier)
    -> (secondQualifier : QuantumStorageQualifier)
    -> ValidationError

  DuplicateStorageQualifier :
       (errorSpan : SourceSpan)
    -> (qualifier : QuantumStorageQualifier)
    -> ValidationError

  RequiresAfterEnsures :
       (errorSpan : SourceSpan)
    -> ValidationError

  UnknownAttribute :
       (errorSpan : SourceSpan)
    -> (attributeNameText : String)
    -> ValidationError

  MalformedKnownAttributeArguments :
       (errorSpan : SourceSpan)
    -> (attributeKind : KnownAttributeKind)
    -> ValidationError

  EmptyAttributeArgumentList :
       (errorSpan : SourceSpan)
    -> ValidationError

  MixedQuantumMatchPatternKinds :
       (errorSpan : SourceSpan)
    -> ValidationError

  DuplicateSupportKind :
       (errorSpan : SourceSpan)
    -> (supportKind : SupportKind)
    -> ValidationError

  MutableBorrowOfQubit :
       (errorSpan : SourceSpan)
    -> ValidationError

  BreakOutsideLoop :
       (errorSpan : SourceSpan)
    -> ValidationError

  ContinueOutsideLoop :
       (errorSpan : SourceSpan)
    -> ValidationError

  ReturnOutsideFunction :
       (errorSpan : SourceSpan)
    -> ValidationError

-- The span a renderer should point at.
public export
validationErrorSpan : ValidationError -> SourceSpan
validationErrorSpan err =
  case err of
    ConflictingStorageQualifiers s _ _   => s
    DuplicateStorageQualifier s _        => s
    RequiresAfterEnsures s               => s
    UnknownAttribute s _                 => s
    MalformedKnownAttributeArguments s _ => s
    EmptyAttributeArgumentList s         => s
    MixedQuantumMatchPatternKinds s      => s
    DuplicateSupportKind s _             => s
    MutableBorrowOfQubit s               => s
    BreakOutsideLoop s                   => s
    ContinueOutsideLoop s                => s
    ReturnOutsideFunction s              => s

-- "file:line:col" prefix, matching the lexer-error rendering style.
renderSpanPrefix : SourceSpan -> String
renderSpanPrefix s =
  s.file ++ ":" ++ show s.start.line ++ ":" ++ show s.start.column

public export
Interpolation ValidationError where
  interpolate err =
    renderSpanPrefix (validationErrorSpan err) ++ ": " ++
      case err of
        ConflictingStorageQualifiers _ q1 q2 =>
          "storage qualifiers `" ++ show q1 ++ "` and `" ++ show q2 ++
          "` are mutually exclusive"
        DuplicateStorageQualifier _ q =>
          "duplicate storage qualifier `" ++ show q ++ "`"
        RequiresAfterEnsures _ =>
          "`requires` clauses must precede all `ensures` clauses"
        UnknownAttribute _ nm =>
          "unknown attribute `" ++ nm ++
          "` (supported: qasm_gate, qasm_def)"
        MalformedKnownAttributeArguments _ kind =>
          "`" ++ show kind ++
          "` takes no arguments or exactly one string argument"
        EmptyAttributeArgumentList _ =>
          "empty attribute argument list; write the attribute without parentheses"
        MixedQuantumMatchPatternKinds _ =>
          "integer and basis-string patterns cannot be mixed in one quantum match"
        DuplicateSupportKind _ k =>
          "duplicate `" ++ show k ++ "` in supports clause"
        MutableBorrowOfQubit _ =>
          "`mut` is never written on a qubit reference; qubit references are mutable by default"
        BreakOutsideLoop _ =>
          "`break` outside of a loop"
        ContinueOutsideLoop _ =>
          "`continue` outside of a loop"
        ReturnOutsideFunction _ =>
          "`return` outside of a function body"

--------------------------------------------------------------------------------
-- Validation context
--------------------------------------------------------------------------------
-- The little positional state the walk threads. Note the Rust rule encoded
-- at the while/for sites below: the CONDITION of a while and the ITERATOR
-- expression of a for are validated with insideLoop = False -- a break
-- there does not target the loop it syntactically sits in.
--------------------------------------------------------------------------------

record ValidationContext where
  constructor MkValidationContext
  insideLoop         : Bool
  insideFunctionBody : Bool

-- Top level / signatures / const initializers.
topLevelContext : ValidationContext
topLevelContext = MkValidationContext False False

-- Entering a function body.
functionBodyContext : ValidationContext
functionBodyContext = MkValidationContext False True

--------------------------------------------------------------------------------
-- Leaf checks (no traversal needed)
--------------------------------------------------------------------------------

-- Conflicts and duplicates in a source-ordered qualifier list. Reports at
-- the span of the SECOND offender, which is what the user should delete.
validateQualifierList :
     List (SurfaceAstNode QuantumStorageQualifier)
  -> List ValidationError
validateQualifierList = go []
  where
    conflictsWith : QuantumStorageQualifier -> QuantumStorageQualifier -> Bool
    conflictsWith QualifierLinear QualifierAffine = True
    conflictsWith QualifierAffine QualifierLinear = True
    conflictsWith _ _ = False

    go : List QuantumStorageQualifier
      -> List (SurfaceAstNode QuantumStorageQualifier)
      -> List ValidationError
    go seen [] = []
    go seen (MkAstNode info _ q :: rest) =
      let dupErrors =
            if elem q seen
              then [DuplicateStorageQualifier info.span q]
              else []
          conflictErrors =
            case find (\prev => conflictsWith prev q) seen of
              Just prev => [ConflictingStorageQualifiers info.span prev q]
              Nothing   => []
      in dupErrors ++ conflictErrors ++ go (q :: seen) rest

-- Once an ensures clause appears, no requires clause may follow.
validateContractOrdering :
     List SurfaceContractClause
  -> List ValidationError
validateContractOrdering = go False
  where
    go : (seenEnsures : Bool)
      -> List SurfaceContractClause
      -> List ValidationError
    go seenEnsures [] = []
    go seenEnsures (MkAstNode info _ clause :: rest) =
      case clause of
        RequiresClause _ =>
          (if seenEnsures then [RequiresAfterEnsures info.span] else [])
            ++ go seenEnsures rest
        EnsuresClause _ =>
          go True rest

-- Known attributes: no argument list, or exactly one string literal.
-- `#[name()]` is rejected outright. Unknown attributes are errors for now.
validateAttribute : SurfaceAttribute -> List ValidationError
validateAttribute (MkAstNode attrInfo _ (MkAttributeNode nameNode maybeArgs)) =
  let MkAstNode _ _ (MkNameNode nameText) = nameNode
  in case recognizeKnownAttribute nameText of
       Nothing => [UnknownAttribute attrInfo.span nameText]
       Just kind =>
         case maybeArgs of
           Nothing => []
           Just [] => [EmptyAttributeArgumentList attrInfo.span]
           Just [MkAstNode _ _ (AttributeArgumentStringLit _)] => []
           Just _  => [MalformedKnownAttributeArguments attrInfo.span kind]

validateAttributeList : List SurfaceAttribute -> List ValidationError
validateAttributeList [] = []
validateAttributeList (a :: rest) =
  validateAttribute a ++ validateAttributeList rest

-- `supports adjoint, adjoint` -- duplicates are meaningless.
validateSupportClause :
     List (SurfaceAstNode SupportKind)
  -> List ValidationError
validateSupportClause = go []
  where
    go : List SupportKind
      -> List (SurfaceAstNode SupportKind)
      -> List ValidationError
    go seen [] = []
    go seen (MkAstNode info _ k :: rest) =
      (if elem k seen then [DuplicateSupportKind info.span k] else [])
        ++ go (k :: seen) rest

-- Homogeneity of qmatch/smatch pattern kinds: all-integer or all-basis
-- (wildcards and qenum variants are neutral). Reports at the first pattern
-- whose kind disagrees with the first committed kind.
validateQuantumArmHomogeneity :
     List (SurfaceAstNode QuantumMatchArmNode)
  -> List ValidationError
validateQuantumArmHomogeneity = go Nothing
  where
    -- True = basis string, False = integer
    patternKind : QuantumMatchPatternNode -> Maybe Bool
    patternKind (QuantumPatternBasisStringRaw _)  = Just True
    patternKind (QuantumPatternIntegerRaw _)      = Just False
    patternKind QuantumPatternWildcard            = Nothing
    patternKind (QuantumPatternQenumVariant _ _)  = Nothing

    go : Maybe Bool
      -> List (SurfaceAstNode QuantumMatchArmNode)
      -> List ValidationError
    go committed [] = []
    go committed
       (MkAstNode _ _
          (MkQuantumMatchArmNode (MkAstNode patInfo _ pat) _) :: rest) =
      case (committed, patternKind pat) of
        (Nothing, k)      => go k rest
        (Just _, Nothing) => go committed rest
        (Just c, Just k)  =>
          if c == k
            then go committed rest
            else MixedQuantumMatchPatternKinds patInfo.span
                   :: go committed rest

-- Is this type SYNTACTICALLY a qubit-carrying type? Only the cases visible
-- without resolution: qubit itself, arrays/slices of it, through parens and
-- qualifiers. Path types that CONTAIN qubits are typing's problem.
isSyntacticallyQubitTy : TyNode SurfaceExpr -> Bool
isSyntacticallyQubitTy ty =
  case ty of
    TyPrimitive TypPrimQubit                        => True
    TyParenthesized (MkAstNode _ _ inner)      => isSyntacticallyQubitTy inner
    TyQualified _ (MkAstNode _ _ inner)        => isSyntacticallyQubitTy inner
    TySlice (MkAstNode _ _ element)            => isSyntacticallyQubitTy element
    TyArray (MkAstNode _ _ element) _          => isSyntacticallyQubitTy element
    _                                               => False

--------------------------------------------------------------------------------
-- The traversal
--------------------------------------------------------------------------------
-- Every function here destructures records with constructor patterns, per
-- the totality discipline in the header. Unused fields are wildcards; the
-- field ORDER in each pattern must track the record declarations in AST.idr.
--------------------------------------------------------------------------------

mutual

  -- Entry point: validate one parsed source file.
  public export
  validateSourceFile : SurfaceSourceFile -> List ValidationError
  validateSourceFile (MkAstNode _ _ (MkSourceFileNode _ items)) =
    validateItemList items

  ------------------------------------------------------------------
  -- Items and declarations
  ------------------------------------------------------------------

  validateItemList : List SurfaceItem -> List ValidationError
  validateItemList [] = []
  validateItemList (i :: rest) = validateItem i ++ validateItemList rest

  validateItem : SurfaceItem -> List ValidationError
  validateItem (MkAstNode _ _ item) =
    case item of
      ItemFunction fd => validateFunctionDecl fd
      ItemStruct sd   => validateStructDecl sd
      ItemEnum ed     => validateEnumDecl ed
      ItemQEnum qd    => validateQEnumDecl qd
      ItemImpl impl   => validateImplDecl impl
      ItemConst cd    => validateConstDecl cd
      ItemUse _       => []
      ItemModule md   => validateModuleDecl md

  -- Fields (in declaration order): docs, attributes, visibility, constness,
  -- effect, name, parameters, returnType, supports, contracts, body.
  validateFunctionDecl : FunctionDeclarationNode -> List ValidationError
  validateFunctionDecl
    (MkFunctionDeclarationNode _ attrs _ _ _ _ params retTy supports contracts body) =
       validateAttributeList attrs
    ++ validateParameterList params
    ++ validateMaybeTy topLevelContext retTy
    ++ validateSupportClause supports
    ++ validateContractOrdering contracts
    ++ validateContractClauseList contracts
    ++ validateBlock functionBodyContext body

  validateParameterList :
       List (SurfaceAstNode FunctionParameterNode)
    -> List ValidationError
  validateParameterList [] = []
  validateParameterList (MkAstNode _ _ p :: rest) =
    (case p of
       NormalParameter _ _ _ ty  => validateTy topLevelContext ty
       ReceiverParameter _ _     => [])
      ++ validateParameterList rest

  validateStructDecl : StructDeclarationNode -> List ValidationError
  validateStructDecl (MkStructDeclarationNode _ attrs _ _ fields) =
       validateAttributeList attrs
    ++ validateStructFieldList fields

  validateStructFieldList :
       List (SurfaceAstNode StructFieldNode)
    -> List ValidationError
  validateStructFieldList [] = []
  validateStructFieldList
    (MkAstNode _ _ (MkStructFieldNode _ _ fieldTy) :: rest) =
    validateTy topLevelContext fieldTy ++ validateStructFieldList rest

  validateEnumDecl : EnumDeclarationNode -> List ValidationError
  validateEnumDecl (MkEnumDeclarationNode _ attrs _ _ variants) =
       validateAttributeList attrs
    ++ validateEnumVariantList variants

  validateEnumVariantList :
       List (SurfaceAstNode EnumVariantNode)
    -> List ValidationError
  validateEnumVariantList [] = []
  validateEnumVariantList
    (MkAstNode _ _ (MkEnumVariantNode _ _ body) :: rest) =
    (case body of
       VariantUnit            => []
       VariantTuple tys       => validateTyList1 topLevelContext tys
       VariantStruct fields   => validateStructFieldList fields)
      ++ validateEnumVariantList rest

  validateQEnumDecl : QEnumDeclarationNode -> List ValidationError
  validateQEnumDecl (MkQEnumDeclarationNode _ attrs _ _ variants) =
       validateAttributeList attrs
    ++ validateQEnumVariantList variants

  validateQEnumVariantList :
       List (SurfaceAstNode QEnumVariantNode)
    -> List ValidationError
  validateQEnumVariantList [] = []
  validateQEnumVariantList
    (MkAstNode _ _ (MkQEnumVariantNode _ _ payloadTys) :: rest) =
    validateTyList1 topLevelContext payloadTys
      ++ validateQEnumVariantList rest

  validateImplDecl : ImplDeclarationNode -> List ValidationError
  validateImplDecl (MkImplDeclarationNode _ _ fns) =
    validateImplFunctionList fns

  validateImplFunctionList :
       List (SurfaceAstNode FunctionDeclarationNode)
    -> List ValidationError
  validateImplFunctionList [] = []
  validateImplFunctionList (MkAstNode _ _ fd :: rest) =
    validateFunctionDecl fd ++ validateImplFunctionList rest

  -- Const initializers are NOT function bodies: `return` inside one is an
  -- error, which topLevelContext encodes.
  validateConstDecl : ConstDeclarationNode -> List ValidationError
  validateConstDecl (MkConstDeclarationNode _ _ _ constTy constVal) =
       validateTy topLevelContext constTy
    ++ validateExpr topLevelContext constVal

  validateModuleDecl : ModuleDeclarationNode -> List ValidationError
  validateModuleDecl (MkModuleDeclarationNode _ _ _ body) =
    case body of
      ModuleInline _ items => validateItemList items
      ModuleExternal       => []

  ------------------------------------------------------------------
  -- Contracts: walk the argument expressions inside predicates.
  -- Signature position: neither in a loop nor in a function body.
  ------------------------------------------------------------------

  validateContractClauseList :
       List SurfaceContractClause
    -> List ValidationError
  validateContractClauseList [] = []
  validateContractClauseList (MkAstNode _ _ clause :: rest) =
    (case clause of
       RequiresClause p => validateContractPredicate p
       EnsuresClause p  => validateContractPredicate p)
      ++ validateContractClauseList rest

  validateContractPredicate :
       SurfaceAstNode (ContractPredicateNode SurfaceExpr)
    -> List ValidationError
  validateContractPredicate (MkAstNode _ _ predicate) =
    case predicate of
      ContractClean e          => validateExpr topLevelContext e
      ContractBasis e _        => validateExpr topLevelContext e
      ContractSeparable e      => validateExpr topLevelContext e
      ContractIsolated e       => validateExpr topLevelContext e
      ContractProduct e (x ::: xs) =>
           validateExpr topLevelContext e
        ++ validateExpr topLevelContext x
        ++ validateExprList topLevelContext xs
      ContractStabilized e _   => validateExpr topLevelContext e

  ------------------------------------------------------------------
  -- Blocks and statements
  ------------------------------------------------------------------

  validateBlock : ValidationContext -> SurfaceBlock -> List ValidationError
  validateBlock ctx (MkAstNode _ _ (MkBlockNode _ stmts finalE)) =
       validateStatementList ctx stmts
    ++ validateMaybeExpr ctx finalE

  validateStatementList :
       ValidationContext
    -> List SurfaceStatement
    -> List ValidationError
  validateStatementList ctx [] = []
  validateStatementList ctx (s :: rest) =
    validateStatement ctx s ++ validateStatementList ctx rest

  validateStatement :
       ValidationContext
    -> SurfaceStatement
    -> List ValidationError
  validateStatement ctx (MkAstNode _ _ stmt) =
    case stmt of
      StatementLet (MkLetBindingNode quals _ tyAnn maybeInit) =>
           validateQualifierList quals
        ++ validateMaybeTy ctx tyAnn
        ++ (case maybeInit of
              Nothing => []
              Just (MkLetInitializerNode _ initValue) =>
                validateExpr ctx initValue)
      StatementAssignment (MkAssignmentNode target _ assignValue) =>
           validateAssignmentTarget ctx target
        ++ validateExpr ctx assignValue
      StatementSemiExpression e => validateExpr ctx e
      StatementExpression e     => validateExpr ctx e

  validateAssignmentTarget :
       ValidationContext
    -> SurfaceAstNode AssignmentTargetNode
    -> List ValidationError
  validateAssignmentTarget ctx (MkAstNode _ _ target) =
    case target of
      AssignTargetName _             => []
      AssignTargetIndex obj idx      =>
        validateExpr ctx obj ++ validateExpr ctx idx
      AssignTargetField obj _        => validateExpr ctx obj
      AssignTargetTupleIndex obj _   => validateExpr ctx obj

  ------------------------------------------------------------------
  -- Expressions
  ------------------------------------------------------------------

  validateExprList :
       ValidationContext -> List SurfaceExpr -> List ValidationError
  validateExprList ctx [] = []
  validateExprList ctx (e :: rest) =
    validateExpr ctx e ++ validateExprList ctx rest

  validateExprList1 :
       ValidationContext -> List1 SurfaceExpr -> List ValidationError
  validateExprList1 ctx (e ::: rest) =
    validateExpr ctx e ++ validateExprList ctx rest

  validateMaybeExpr :
       ValidationContext -> Maybe SurfaceExpr -> List ValidationError
  validateMaybeExpr ctx Nothing  = []
  validateMaybeExpr ctx (Just e) = validateExpr ctx e

  validateExpr : ValidationContext -> SurfaceExpr -> List ValidationError
  validateExpr ctx (MkAstNode info _ expr) =
    case expr of
      ExprLiteral _  => []
      ExprName _     => []
      ExprPath _     => []
      ExprBuiltin _  => []
      ExprSelf       => []

      ExprParenthesized e   => validateExpr ctx e
      ExprTuple es          => validateExprList1 ctx es
      ExprArray es          => validateExprList ctx es
      ExprRepeatedArray e c => validateExpr ctx e ++ validateExpr ctx c

      ExprStructLiteral _ fields => validateFieldInitList ctx fields

      ExprCall callee args =>
        validateExpr ctx callee ++ validateExprList ctx args
      ExprMethodCall recv _ args =>
        validateExpr ctx recv ++ validateExprList ctx args
      ExprField obj _        => validateExpr ctx obj
      ExprTupleIndex obj _   => validateExpr ctx obj
      ExprIndex obj idx      => validateExpr ctx obj ++ validateExpr ctx idx

      ExprUnary _ operand    => validateExpr ctx operand
      ExprBinary _ lhs rhs   => validateExpr ctx lhs ++ validateExpr ctx rhs
      ExprRange s _ e        => validateMaybeExpr ctx s ++ validateMaybeExpr ctx e
      ExprCast e ty          => validateExpr ctx e ++ validateTy ctx ty

      ExprBlock b            => validateBlock ctx b

      ExprIf ifNode          => validateClassicalIf ctx ifNode
      ExprQIf (MkQuantumIfNode qifCond thenBranch elseBranch) =>
           validateExpr ctx qifCond
        ++ validateQuantumBranch ctx thenBranch
        ++ (case elseBranch of
              Nothing => []
              Just b  => validateQuantumBranch ctx b)
      ExprSIf (MkStateIfNode sifCond thenE elseE) =>
           validateExpr ctx sifCond
        ++ validateExpr ctx thenE
        ++ validateExpr ctx elseE

      ExprMatch (MkClassicalMatchNode scrut arms) =>
           validateExpr ctx scrut
        ++ validateClassicalArmList ctx arms
      ExprQMatch (MkQuantumMatchNode scrut arms) =>
           validateExpr ctx scrut
        ++ validateQuantumArmHomogeneity arms
        ++ validateQuantumArmBodies ctx arms
      ExprSMatch (MkStateMatchNode scrut arms) =>
           validateExpr ctx scrut
        ++ validateQuantumArmHomogeneity arms
        ++ validateQuantumArmBodies ctx arms

      -- Loop bodies set insideLoop; while conditions and for iterator
      -- expressions do NOT count as inside the loop they head.
      ExprLoop body =>
        validateBlock (MkValidationContext True ctx.insideFunctionBody) body
      ExprWhile cond body =>
           validateExpr (MkValidationContext False ctx.insideFunctionBody) cond
        ++ validateBlock (MkValidationContext True ctx.insideFunctionBody) body
      ExprFor _ iterExpr body =>
           validateExpr (MkValidationContext False ctx.insideFunctionBody) iterExpr
        ++ validateBlock (MkValidationContext True ctx.insideFunctionBody) body

      ExprBreak breakValue =>
        (if ctx.insideLoop then [] else [BreakOutsideLoop info.span])
          ++ validateMaybeExpr ctx breakValue
      ExprContinue =>
        if ctx.insideLoop then [] else [ContinueOutsideLoop info.span]
      ExprReturn returnValue =>
        (if ctx.insideFunctionBody then [] else [ReturnOutsideFunction info.span])
          ++ validateMaybeExpr ctx returnValue

      ExprCtrl c    => validateControlExpr ctx c
      ExprAdjoint a => validateAdjointExpr ctx a

  validateFieldInitList :
       ValidationContext
    -> List (SurfaceAstNode FieldInitializerNode)
    -> List ValidationError
  validateFieldInitList ctx [] = []
  validateFieldInitList ctx (MkAstNode _ _ f :: rest) =
    (case f of
       FieldInitShorthand _   => []
       FieldInitExplicit _ e  => validateExpr ctx e)
      ++ validateFieldInitList ctx rest

  validateClassicalIf :
       ValidationContext -> ClassicalIfNode -> List ValidationError
  validateClassicalIf ctx (MkClassicalIfNode ifCond thenBlock elseBranch) =
       validateExpr ctx ifCond
    ++ validateBlock ctx thenBlock
    ++ (case elseBranch of
          Nothing                => []
          Just (ElseBlock b)     => validateBlock ctx b
          Just (ElseChainedIf (MkAstNode _ _ chained)) =>
            validateClassicalIf ctx chained)

  validateQuantumBranch :
       ValidationContext -> QuantumBranchNode -> List ValidationError
  validateQuantumBranch ctx branch =
    case branch of
      QuantumBranchBlock b      => validateBlock ctx b
      QuantumBranchExpression e => validateExpr ctx e

  validateClassicalArmList :
       ValidationContext
    -> List (SurfaceAstNode ClassicalMatchArmNode)
    -> List ValidationError
  validateClassicalArmList ctx [] = []
  validateClassicalArmList ctx
    (MkAstNode _ _ (MkClassicalMatchArmNode _ guard armBody) :: rest) =
       validateMaybeExpr ctx guard
    ++ validateExpr ctx armBody
    ++ validateClassicalArmList ctx rest

  validateQuantumArmBodies :
       ValidationContext
    -> List (SurfaceAstNode QuantumMatchArmNode)
    -> List ValidationError
  validateQuantumArmBodies ctx [] = []
  validateQuantumArmBodies ctx
    (MkAstNode _ _ (MkQuantumMatchArmNode _ armBody) :: rest) =
    validateExpr ctx armBody ++ validateQuantumArmBodies ctx rest

  validateControlExpr :
       ValidationContext -> ControlExpressionNode -> List ValidationError
  validateControlExpr ctx c =
    case c of
      ControlledCallable controls _ callable =>
        validateExprList1 ctx controls ++ validateExpr ctx callable
      ControlledBlock controls _ body =>
        validateExprList1 ctx controls ++ validateBlock ctx body

  validateAdjointExpr :
       ValidationContext -> AdjointExpressionNode -> List ValidationError
  validateAdjointExpr ctx a =
    case a of
      AdjointOfCallable callable => validateExpr ctx callable
      AdjointBlock body          => validateBlock ctx body

  ------------------------------------------------------------------
  -- Types: qualifier lists, the &mut-qubit check, and recursion into
  -- array-size expressions (which are full surface expressions).
  ------------------------------------------------------------------

  validateTyList :
       ValidationContext -> List SurfaceTy -> List ValidationError
  validateTyList ctx [] = []
  validateTyList ctx (t :: rest) =
    validateTy ctx t ++ validateTyList ctx rest

  validateTyList1 :
       ValidationContext -> List1 SurfaceTy -> List ValidationError
  validateTyList1 ctx (t ::: rest) =
    validateTy ctx t ++ validateTyList ctx rest

  validateMaybeTy :
       ValidationContext -> Maybe SurfaceTy -> List ValidationError
  validateMaybeTy ctx Nothing  = []
  validateMaybeTy ctx (Just t) = validateTy ctx t

  validateTy : ValidationContext -> SurfaceTy -> List ValidationError
  validateTy ctx (MkAstNode _ _ ty) =
    case ty of
      TyPrimitive _        => []
      TyPath _             => []
      TyUnit               => []
      TyParenthesized t    => validateTy ctx t
      TyTuple ts           => validateTyList1 ctx ts
      TyArray element size =>
        validateTy ctx element ++ validateExpr ctx size
      TySlice element      => validateTy ctx element
      TyReference (MkAstNode borrowInfo _ borrow)
                  innerNode@(MkAstNode _ _ innerTy) =>
        (case borrow of
           MutableBorrow =>
             if isSyntacticallyQubitTy innerTy
               then [MutableBorrowOfQubit borrowInfo.span]
               else []
           SharedBorrow  => [])
          ++ validateTy ctx innerNode
      TyQualified qualifiers inner =>
           validateQualifierList (forget qualifiers)
        ++ validateTy ctx inner
      TyFunction _ params returnTy =>
        validateFunctionTypeParams ctx params
          ++ validateMaybeTy ctx returnTy

  validateFunctionTypeParams :
       ValidationContext
    -> List (SurfaceAstNode (FunctionTypeParameterNode SurfaceExpr))
    -> List ValidationError
  validateFunctionTypeParams ctx [] = []
  validateFunctionTypeParams ctx
    (MkAstNode _ _ (MkFunctionTypeParameterNode _ paramTy) :: rest) =
    validateTy ctx paramTy ++ validateFunctionTypeParams ctx rest
