module Parser.Semantic where

import Scanner
import Text.Parsec (getPosition, getState, modifyState)
import Text.Parsec.Pos (sourceColumn, sourceLine)
import Parser.Types
import Parser.SymbolTable

builtInFuncSigs :: [(String, (Int, Maybe Int, CorbieType))]
builtInFuncSigs =
  [ ("input",     (0, Just 1, TDynamic))
  , ("len",       (1, Just 1, TInt))
  , ("split",     (2, Just 2, TList))
  , ("zeros",     (1, Nothing, TList))
  , ("ones",      (1, Nothing, TList))
  , ("reshape",   (2, Just 2, TList))
  , ("dot",       (2, Just 2, TDynamic))
  , ("to_int",    (1, Just 1, TInt))
  , ("to_float",  (1, Just 1, TFloat))
  , ("to_string", (1, Just 1, TString))
  , ("to_bool",   (1, Just 1, TBool))
  , ("int",       (1, Just 1, TInt))
  , ("float",     (1, Just 1, TFloat))
  , ("str",       (1, Just 1, TString))
  , ("bool",      (1, Just 1, TBool))
  ]

initialSemanticState :: ParserState
initialSemanticState = initialParserState { funcSigs = builtInFuncSigs }

semanticError :: String -> Parser a
semanticError msg = fail ("Semantic error: " ++ msg)

tokPos :: Token -> AlexPosn
tokPos (Token p _) = p

liftEitherState :: Either String ParserState -> Parser ()
liftEitherState (Left err)   = semanticError err
liftEitherState (Right newS) = modifyState (const newS)

declareSymbol :: Token -> Token -> CorbieType -> Bool -> Parser ()
declareSymbol nameTok valTok typ isConst = do
  st <- getState
  liftEitherState (insertScopedSymbol nameTok valTok typ isConst st)

declareParam :: Token -> Token -> Parser ()
declareParam paramTok defTok = declareSymbol paramTok defTok TDynamic False

declareModel :: Token -> Token -> Parser ()
declareModel nameTok modelTok =
  declareSymbol nameTok modelTok (TModel (rawTokName nameTok)) False

declareEnum :: Token -> Token -> [Token] -> Parser ()
declareEnum nameTok enumTok members = do
  let enumT = TEnum (rawTokName nameTok)
  declareSymbol nameTok enumTok enumT True
  mapM_ (\m -> declareSymbol m enumTok enumT True) members

declareFunc :: Token -> Token -> [Token] -> Parser ()
declareFunc nameTok defTok params = do
  let fnName = rawTokName nameTok
      arity  = length params
  st <- getState
  let scope = currentScopeName st
      scopedFn = scopedName scope fnName
  case insertScopedSymbol nameTok defTok TDynamic False st of
    Left err -> semanticError err
    Right st' -> do
      let newSigs = upsertAssoc scopedFn (arity, Just arity, TDynamic) (funcSigs st')
      modifyState (const (st' { funcSigs = newSigs }))

declareConst :: Token -> Expr -> Maybe Token -> Parser ()
declareConst nameTok valExpr mConstIdTok = do
  typ <- checkConstExpr valExpr mConstIdTok
  let valTok = case valExpr of
        LitExpr (IntLit n)    -> Token (tokPos nameTok) (IntLiteral n)
        LitExpr (FloatLit n)  -> Token (tokPos nameTok) (FloatLiteral n)
        LitExpr (StringLit s) -> Token (tokPos nameTok) (StringLiteral s)
        LitExpr (BoolLit b)   -> Token (tokPos nameTok) (BoolLiteral b)
        _ -> case mConstIdTok of
               Just t  -> t
               Nothing -> Token (tokPos nameTok) Const
  declareSymbol nameTok valTok typ True

checkConstExpr :: Expr -> Maybe Token -> Parser CorbieType
checkConstExpr (LitExpr (IntLit _)) _    = return TInt
checkConstExpr (LitExpr (FloatLit _)) _  = return TFloat
checkConstExpr (LitExpr (StringLit _)) _ = return TString
checkConstExpr (LitExpr (BoolLit _)) _   = return TBool
checkConstExpr (VarExpr (SimpleVar refName)) _ = do
  st <- getState
  case symtable_lookup_scoped refName st of
    Nothing -> semanticError ("undeclared constant '" ++ refName ++ "'")
    Just (key, _) ->
      if key `elem` constSet st
        then return (lookupTypeOrDyn key st)
        else semanticError ("constant initializer '" ++ refName ++ "' is not a constant")
checkConstExpr _ _ = semanticError "constant must be initialized with a literal or another constant"

lookupTypeOrDyn :: String -> ParserState -> CorbieType
lookupTypeOrDyn key st = case lookup key (varTypes st) of
  Just t  -> t
  Nothing -> TDynamic

lookupFuncSig :: String -> ParserState -> Maybe (Int, Maybe Int, CorbieType)
lookupFuncSig fnName st = go (scopeStack st)
  where
    go [] = lookup fnName (funcSigs st)
    go (scope : outer) =
      let key = scopedName scope fnName
      in case lookup key (funcSigs st) of
           Just sig -> Just sig
           Nothing  -> go outer

checkFuncCall :: String -> [Expr] -> Parser CorbieType
checkFuncCall fnName args = do
  st <- getState
  sig <- case lookupFuncSig fnName st of
    Nothing -> semanticError ("undeclared function '" ++ fnName ++ "'")
    Just s  -> return s
  argTypes <- mapM evalExpr args
  let (minArgs, mMaxArgs, retType) = sig
      argCount = length argTypes
  if argCount < minArgs
    then semanticError ("function '" ++ fnName ++ "' expects at least " ++ show minArgs ++ " argument(s), got " ++ show argCount)
    else case mMaxArgs of
      Just maxArgs | argCount > maxArgs ->
        semanticError ("function '" ++ fnName ++ "' expects at most " ++ show maxArgs ++ " argument(s), got " ++ show argCount)
      _ -> return retType

checkVarDeclared :: String -> Parser (String, CorbieType)
checkVarDeclared name = do
  st <- getState
  case symtable_lookup_scoped name st of
    Nothing       -> semanticError ("undeclared variable '" ++ name ++ "'")
    Just (key, _) -> return (key, lookupTypeOrDyn key st)

checkVarChain :: Var -> Parser (String, CorbieType)
checkVarChain (SimpleVar name) = checkVarDeclared name
checkVarChain (IndexVar inner slices) = do
  (key, innerType) <- checkVarChain inner
  if not (isIndexable innerType)
    then semanticError ("type " ++ show innerType ++ " is not indexable")
    else do
      mapM_ (checkSlice innerType) slices
      let resType = case innerType of
            TString -> TString
            _       -> TDynamic
      return (key, resType)

isIndexable :: CorbieType -> Bool
isIndexable TList    = True
isIndexable TString  = True
isIndexable TDict    = True
isIndexable TDynamic = True
isIndexable _        = False

checkSlice :: CorbieType -> Slice -> Parser ()
checkSlice containerType (SliceSingle e) = do
  t <- evalExpr e
  case containerType of
    TDict -> if t `elem` [TString, TInt, TDynamic]
               then return ()
               else semanticError ("invalid dictionary key type " ++ show t)
    _     -> if isIntCompatible t
               then return ()
               else semanticError ("index must be int, got " ++ show t)
checkSlice containerType (SliceFromTo mStart mEnd) = do
  case containerType of
    TDict -> semanticError "slicing with ':' is not supported on dict"
    _     -> do
      maybe (return ()) checkIntBound mStart
      maybe (return ()) checkIntBound mEnd
  where
    checkIntBound e = do
      t <- evalExpr e
      if isIntCompatible t
        then return ()
        else semanticError ("slice bound must be int, got " ++ show t)

isIntCompatible :: CorbieType -> Bool
isIntCompatible TInt     = True
isIntCompatible TDynamic = True
isIntCompatible _        = False

isNumeric :: CorbieType -> Bool
isNumeric TInt     = True
isNumeric TFloat   = True
isNumeric TDynamic = True
isNumeric _        = False

isIterable :: CorbieType -> Bool
isIterable TList    = True
isIterable TString  = True
isIterable TDict    = True
isIterable TDynamic = True
isIterable _        = False

checkBoolCondition :: String -> CorbieType -> Parser ()
checkBoolCondition _ TBool    = return ()
checkBoolCondition _ TDynamic = return ()
checkBoolCondition ctx t      = semanticError ("condition in '" ++ ctx ++ "' must be boolean, got " ++ show t)

checkReturnStmt :: Maybe Expr -> Parser ()
checkReturnStmt mExpr = do
  st <- getState
  if not (inFunction st)
    then semanticError "'return' statement not allowed outside of a function"
    else case mExpr of
      Nothing -> return ()
      Just e  -> evalExpr e >> return ()

checkAssign :: Token -> Var -> AssignOp -> Expr -> Parser ()
checkAssign baseTok var op rhsExpr = do
  rhsType <- evalExpr rhsExpr
  let baseName = rawTokName baseTok
      valTok = case rhsExpr of
        LitExpr (IntLit n)    -> Token (tokPos baseTok) (IntLiteral n)
        LitExpr (FloatLit n)  -> Token (tokPos baseTok) (FloatLiteral n)
        LitExpr (StringLit s) -> Token (tokPos baseTok) (StringLiteral s)
        LitExpr (BoolLit b)   -> Token (tokPos baseTok) (BoolLiteral b)
        _                     -> Token (tokPos baseTok) Assign
  st <- getState
  case var of
    IndexVar _ _ -> do
      (key, _) <- checkVarChain var
      if key `elem` constSet st
        then semanticError ("cannot mutate constant '" ++ baseName ++ "'")
        else case op of
          OpAssign -> return ()
          _        -> checkCompoundCompat op TDynamic rhsType
    SimpleVar _ ->
      case symtable_lookup_scoped baseName st of
        Just (key, _) ->
          if key `elem` constSet st
            then semanticError ("cannot assign to constant '" ++ baseName ++ "'")
            else do
              let curType = lookupTypeOrDyn key st
                  newType = case op of
                    OpAssign -> rhsType
                    _        -> combineCompoundType op curType rhsType
              case op of
                OpAssign -> return ()
                _        -> checkCompoundCompat op curType rhsType
              stNow <- getState
              liftEitherState (updateScopedSymbol key baseTok valTok newType stNow)
        Nothing ->
          case op of
            OpAssign -> do
              stNow <- getState
              liftEitherState (insertScopedSymbol baseTok valTok rhsType False stNow)
            _ -> semanticError ("undeclared variable '" ++ baseName ++ "' in compound assignment")

checkCompoundCompat :: AssignOp -> CorbieType -> CorbieType -> Parser ()
checkCompoundCompat OpPlusAssign lt rt  = evalBinaryType OpAdd lt rt >> return ()
checkCompoundCompat OpMinusAssign lt rt = evalBinaryType OpSub lt rt >> return ()
checkCompoundCompat OpMultAssign lt rt  = evalBinaryType OpMul lt rt >> return ()
checkCompoundCompat OpDivAssign lt rt   = evalBinaryType OpDiv lt rt >> return ()
checkCompoundCompat OpAssign _ _        = return ()

combineCompoundType :: AssignOp -> CorbieType -> CorbieType -> CorbieType
combineCompoundType OpPlusAssign TInt TInt       = TInt
combineCompoundType OpPlusAssign TFloat _        = TFloat
combineCompoundType OpPlusAssign _ TFloat        = TFloat
combineCompoundType OpPlusAssign TString TString = TString
combineCompoundType OpPlusAssign TList TList     = TList
combineCompoundType OpDivAssign TInt TInt        = TInt
combineCompoundType _ TFloat _                   = TFloat
combineCompoundType _ _ TFloat                   = TFloat
combineCompoundType _ TInt TInt                  = TInt
combineCompoundType _ _ _                        = TDynamic

evalExpr :: Expr -> Parser CorbieType
evalExpr (LitExpr (IntLit _))    = return TInt
evalExpr (LitExpr (FloatLit _))  = return TFloat
evalExpr (LitExpr (StringLit _)) = return TString
evalExpr (LitExpr (BoolLit _))   = return TBool
evalExpr (VarExpr var)           = fmap snd (checkVarChain var)
evalExpr (FuncCallExpr (CallFunc name args)) = checkFuncCall name args
evalExpr (ListExpr elems)        = mapM_ evalExpr elems >> return TList
evalExpr (DictExpr pairs)        = mapM_ (evalExpr . snd) pairs >> return TDict
evalExpr (ListCompExpr valExpr varName srcExpr mCond) = do
  srcType <- evalExpr srcExpr
  if not (isIterable srcType)
    then semanticError ("list comprehension source must be iterable, got " ++ show srcType)
    else do
      modifyState (enterAnonScope "lc")
      pos <- getPosition
      let alexP  = AlexPn 0 (sourceLine pos) (sourceColumn pos)
          varTok = Token alexP (Identifier varName)
          inTok  = Token alexP In
      declareSymbol varTok inTok TDynamic False
      _ <- evalExpr valExpr
      case mCond of
        Nothing   -> return ()
        Just cond -> evalExpr cond >>= checkBoolCondition "list comprehension"
      modifyState exitScope
      return TList
evalExpr (UnaryExpr OpNot e) = do
  t <- evalExpr e
  if t == TBool || t == TDynamic
    then return TBool
    else semanticError ("'not' requires boolean operand, got " ++ show t)
evalExpr (UnaryExpr _ e) = do
  t <- evalExpr e
  if isNumeric t
    then return t
    else semanticError ("unary +/- requires numeric operand, got " ++ show t)
evalExpr (BinaryExpr op left right) = do
  checkZeroDivisor op right
  lt <- evalExpr left
  rt <- evalExpr right
  evalBinaryType op lt rt

checkZeroDivisor :: BinOp -> Expr -> Parser ()
checkZeroDivisor OpDiv (LitExpr (IntLit 0))     = semanticError "division by zero"
checkZeroDivisor OpDiv (LitExpr (FloatLit 0.0)) = semanticError "division by zero"
checkZeroDivisor OpMod (LitExpr (IntLit 0))     = semanticError "modulo by zero"
checkZeroDivisor OpMod (LitExpr (FloatLit 0.0)) = semanticError "modulo by zero"
checkZeroDivisor _ _                            = return ()

evalBinaryType :: BinOp -> CorbieType -> CorbieType -> Parser CorbieType
evalBinaryType OpOr lt rt = checkLogical "or" lt rt
evalBinaryType OpAnd lt rt = checkLogical "and" lt rt
evalBinaryType OpIn _ rt
  | isIterable rt = return TBool
  | otherwise     = semanticError ("right operand of 'in' must be iterable, got " ++ show rt)
evalBinaryType op lt rt
  | op `elem` [OpEqual, OpNotEqual] = checkEqCompat lt rt
  | op `elem` [OpLess, OpLessEqual, OpGreater, OpGreaterEqual] = checkOrdCompat lt rt
evalBinaryType OpAdd lt rt
  | lt == TDynamic || rt == TDynamic =
      if lt == TList || rt == TList then return TList else return TDynamic
  | lt == TInt && rt == TInt         = return TInt
  | isNumeric lt && isNumeric rt     = return TFloat
  | lt == TString && rt == TString   = return TString
  | lt == TList && rt == TList       = return TList
  | otherwise = semanticError ("incompatible types for '+': " ++ show lt ++ " and " ++ show rt)
evalBinaryType OpCat lt rt
  | lt == TDynamic || rt == TDynamic = return TDynamic
  | lt == TString && rt == TString   = return TString
  | lt == TList && rt == TList       = return TList
  | otherwise = semanticError ("incompatible types for 'cat': " ++ show lt ++ " and " ++ show rt)
evalBinaryType OpMatMul lt rt
  | (lt == TList || lt == TDynamic) && (rt == TList || rt == TDynamic) = return TList
  | otherwise = semanticError ("operator '@' requires matrix/list operands, got " ++ show lt ++ " and " ++ show rt)
evalBinaryType op lt rt
  | op `elem` [OpSub, OpMul, OpDiv, OpMod, OpPow] =
      if isNumeric lt && isNumeric rt
        then if lt == TDynamic || rt == TDynamic
               then return TDynamic
               else if lt == TFloat || rt == TFloat
                      then return TFloat
                      else return TInt
        else semanticError ("arithmetic operator requires numeric operands, got " ++ show lt ++ " and " ++ show rt)
evalBinaryType _ _ _ = return TDynamic

checkLogical :: String -> CorbieType -> CorbieType -> Parser CorbieType
checkLogical opName lt rt
  | (lt == TBool || lt == TDynamic) && (rt == TBool || rt == TDynamic) = return TBool
  | otherwise = semanticError ("logical '" ++ opName ++ "' requires boolean operands, got " ++ show lt ++ " and " ++ show rt)

checkEqCompat :: CorbieType -> CorbieType -> Parser CorbieType
checkEqCompat lt rt
  | lt == TDynamic || rt == TDynamic = return TBool
  | isNumeric lt && isNumeric rt     = return TBool
  | lt == rt                         = return TBool
  | otherwise = semanticError ("cannot compare incompatible types " ++ show lt ++ " and " ++ show rt)

checkOrdCompat :: CorbieType -> CorbieType -> Parser CorbieType
checkOrdCompat lt rt
  | lt == TDynamic || rt == TDynamic = return TBool
  | isNumeric lt && isNumeric rt     = return TBool
  | lt == TString && rt == TString   = return TBool
  | lt == TList && rt == TList       = return TBool
  | otherwise = semanticError ("cannot order incompatible types " ++ show lt ++ " and " ++ show rt)
