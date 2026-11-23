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
  , ("split",     (2, Just 2, TList ShapeUnknown))
  , ("zeros",     (1, Nothing, TList ShapeUnknown))
  , ("ones",      (1, Nothing, TList ShapeUnknown))
  , ("reshape",   (2, Just 2, TList ShapeUnknown))
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
  let scope    = currentScopeName st
      scopedFn = scopedName scope fnName
  case insertScopedSymbol nameTok defTok TDynamic False st of
    Left err -> semanticError err
    Right st' -> do
      let newSigs = upsertAssoc scopedFn (arity, Just arity, TDynamic) (funcSigs st')
      modifyState (const (st' { funcSigs = newSigs }))

declareConst :: Token -> Expr -> Maybe Token -> Parser ()
declareConst nameTok valExpr mConstIdTok = do
  (typ, isZero) <- checkConstExpr valExpr
  let valTok = case valExpr of
        LitExpr (IntLit n)    -> Token (tokPos nameTok) (IntLiteral n)
        LitExpr (FloatLit n)  -> Token (tokPos nameTok) (FloatLiteral n)
        LitExpr (StringLit s) -> Token (tokPos nameTok) (StringLiteral s)
        LitExpr (BoolLit b)   -> Token (tokPos nameTok) (BoolLiteral b)
        _ -> case mConstIdTok of
               Just t  -> t
               Nothing -> Token (tokPos nameTok) Const
  declareSymbol nameTok valTok typ True
  if isZero
    then do
      st <- getState
      let key = scopedName (currentScopeName st) (rawTokName nameTok)
      modifyState (\s -> s { constZeros = key : constZeros s })
    else return ()

checkConstExpr :: Expr -> Parser (CorbieType, Bool)
checkConstExpr (LitExpr (IntLit n))    = return (TInt, n == 0)
checkConstExpr (LitExpr (FloatLit n))  = return (TFloat, n == 0.0)
checkConstExpr (LitExpr (StringLit _)) = return (TString, False)
checkConstExpr (LitExpr (BoolLit _))   = return (TBool, False)
checkConstExpr (VarExpr (SimpleVar refName)) = do
  st <- getState
  case symtable_lookup_scoped refName st of
    Nothing -> semanticError ("undeclared constant '" ++ refName ++ "'")
    Just (key, _) ->
      if key `elem` constSet st
        then return (lookupTypeOrDyn key st, key `elem` constZeros st)
        else semanticError ("constant initializer '" ++ refName ++ "' is not a constant")
checkConstExpr _ = semanticError "constant must be initialized with a literal or another constant"

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
  let (minArgs, mMaxArgs, retType) = sig
      argCount = length args
  if argCount < minArgs
    then semanticError ("function '" ++ fnName ++ "' expects at least " ++ show minArgs ++ " argument(s), got " ++ show argCount)
    else case mMaxArgs of
      Just maxArgs | argCount > maxArgs ->
        semanticError ("function '" ++ fnName ++ "' expects at most " ++ show maxArgs ++ " argument(s), got " ++ show argCount)
      _ -> do
        argTypes <- mapM evalExpr args
        checkBuiltInArgs fnName args argTypes retType

checkBuiltInArgs :: String -> [Expr] -> [CorbieType] -> CorbieType -> Parser CorbieType
checkBuiltInArgs "len" _ [t] retT
  | isIterable t = return retT
  | otherwise    = semanticError ("'len' requires iterable argument, got " ++ show t)
checkBuiltInArgs "split" _ [sT, sepT] retT
  | (sT == TString || sT == TDynamic) && (sepT == TString || sepT == TDynamic) = return retT
  | otherwise = semanticError ("'split' requires (string, string), got (" ++ show sT ++ ", " ++ show sepT ++ ")")
checkBuiltInArgs "input" _ [pT] retT
  | pT == TString || pT == TDynamic = return retT
  | otherwise = semanticError ("'input' prompt must be string, got " ++ show pT)
checkBuiltInArgs fn args argTypes _
  | fn `elem` ["zeros", "ones"] = do
      mapM_ checkDimType argTypes
      mDims <- mapM extractPosLitInt args
      case sequence mDims of
        Just dims -> return (TList (ShapeKnown dims))
        Nothing   -> return (TList ShapeUnknown)
  | fn == "reshape" = checkReshape args argTypes
  | fn == "dot"     = checkDot args argTypes
  | fn `elem` ["to_int", "int", "to_float", "float"] =
      case argTypes of
        [t] | isCastableToNum t -> return (if fn `elem` ["to_int", "int"] then TInt else TFloat)
        [t] -> semanticError ("cannot convert " ++ show t ++ " to number in '" ++ fn ++ "'")
        _   -> return TDynamic
checkBuiltInArgs _ _ _ retT = return retT

checkDimType :: CorbieType -> Parser ()
checkDimType t
  | isIntCompatible t = return ()
  | otherwise         = semanticError ("matrix dimension must be int, got " ++ show t)

extractPosLitInt :: Expr -> Parser (Maybe Int)
extractPosLitInt (LitExpr (IntLit n))
  | n > 0     = return (Just n)
  | otherwise = semanticError ("matrix dimension must be positive, got " ++ show n)
extractPosLitInt (UnaryExpr OpNeg (LitExpr (IntLit n)))
  | n >= 0    = semanticError ("matrix dimension must be positive, got " ++ show (-n))
  | otherwise = return (Just (-n))
extractPosLitInt _ = return Nothing

checkReshape :: [Expr] -> [CorbieType] -> Parser CorbieType
checkReshape [_, shapeExpr] [mType, shapeType] = do
  case mType of
    TList ShapeIrregular -> semanticError "cannot reshape irregular matrix"
    TList _              -> return ()
    TDynamic             -> return ()
    _                    -> semanticError ("'reshape' first argument must be list/matrix, got " ++ show mType)
  case shapeType of
    TList ShapeIrregular -> semanticError "'reshape' shape argument must be a 1D int list"
    TList _              -> return ()
    TDynamic             -> return ()
    _                    -> semanticError ("'reshape' shape argument must be list, got " ++ show shapeType)
  case shapeExpr of
    ListExpr elems -> do
      elemTypes <- mapM evalExpr elems
      mapM_ checkDimType elemTypes
      mNewDims <- mapM extractPosLitInt elems
      case sequence mNewDims of
        Nothing -> return (TList ShapeUnknown)
        Just newDims ->
          case mType of
            TList (ShapeKnown oldDims) ->
              if product oldDims == product newDims
                then return (TList (ShapeKnown newDims))
                else semanticError ("cannot reshape matrix of size " ++ show (product oldDims) ++ " into shape " ++ show newDims)
            TList (ShapeNonNumeric oldDims) ->
              if product oldDims == product newDims
                then return (TList (ShapeNonNumeric newDims))
                else semanticError ("cannot reshape matrix of size " ++ show (product oldDims) ++ " into shape " ++ show newDims)
            _ -> return (TList (ShapeKnown newDims))
    _ -> return (TList ShapeUnknown)
checkReshape _ _ = return (TList ShapeUnknown)

checkDot :: [Expr] -> [CorbieType] -> Parser CorbieType
checkDot [aExpr, bExpr] [lt, rt] = do
  checkNumericMatrixExpr aExpr
  checkNumericMatrixExpr bExpr
  evalMatMul "dot" lt rt
checkDot _ _ = return TDynamic

isCastableToNum :: CorbieType -> Bool
isCastableToNum TInt     = True
isCastableToNum TFloat   = True
isCastableToNum TBool    = True
isCastableToNum TString  = True
isCastableToNum TDynamic = True
isCastableToNum _        = False

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
      case innerType of
        TString ->
          if length slices > 1
            then semanticError "too many indices for string"
            else return ()
        TDict ->
          if length slices > 1
            then semanticError "too many indices for dict"
            else return ()
        TList (ShapeKnown dims) ->
          if length slices > length dims
            then semanticError ("too many indices for list/matrix: expected at most " ++ show (length dims) ++ ", got " ++ show (length slices))
            else return ()
        TList (ShapeNonNumeric dims) ->
          if length slices > length dims
            then semanticError ("too many indices for list/matrix: expected at most " ++ show (length dims) ++ ", got " ++ show (length slices))
            else return ()
        _ -> return ()
      mapM_ (checkSlice innerType) slices
      let resType = indexResultType innerType slices
      return (key, resType)

indexResultType :: CorbieType -> [Slice] -> CorbieType
indexResultType TString _ = TString
indexResultType (TList (ShapeKnown dims)) slices
  | any isRangeSlice slices = TList ShapeUnknown
  | otherwise =
      let remDims = drop (length slices) dims
      in if null remDims then TDynamic else TList (ShapeKnown remDims)
indexResultType (TList (ShapeNonNumeric dims)) slices
  | any isRangeSlice slices = TList ShapeUnknown
  | otherwise =
      let remDims = drop (length slices) dims
      in if null remDims then TDynamic else TList (ShapeNonNumeric remDims)
indexResultType _ _ = TDynamic

isRangeSlice :: Slice -> Bool
isRangeSlice (SliceFromTo _ _) = True
isRangeSlice (SliceSingle _)   = False

isIndexable :: CorbieType -> Bool
isIndexable (TList _) = True
isIndexable TString   = True
isIndexable TDict     = True
isIndexable TDynamic  = True
isIndexable _         = False

checkSlice :: CorbieType -> Slice -> Parser ()
checkSlice containerType (SliceSingle e) = do
  t <- evalExpr e
  case containerType of
    TDict -> if t == TString || t == TDynamic
               then return ()
               else semanticError ("dictionary key must be string, got " ++ show t)
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

isListType :: CorbieType -> Bool
isListType (TList _) = True
isListType _         = False

isIterable :: CorbieType -> Bool
isIterable (TList _) = True
isIterable TString   = True
isIterable TDict     = True
isIterable TDynamic  = True
isIterable _         = False

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
      let baseT = lookupTypeOrDyn key st
      if key `elem` constSet st
        then semanticError ("cannot mutate constant '" ++ baseName ++ "'")
        else if baseT == TString
          then semanticError ("strings are immutable and cannot be assigned by index ('" ++ baseName ++ "')")
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
combineCompoundType OpPlusAssign TInt TInt           = TInt
combineCompoundType OpPlusAssign TFloat _            = TFloat
combineCompoundType OpPlusAssign _ TFloat            = TFloat
combineCompoundType OpPlusAssign TString TString     = TString
combineCompoundType OpPlusAssign (TList _) (TList _) = TList ShapeUnknown
combineCompoundType OpDivAssign TInt TInt            = TInt
combineCompoundType _ TFloat _                       = TFloat
combineCompoundType _ _ TFloat                       = TFloat
combineCompoundType _ TInt TInt                      = TInt
combineCompoundType _ _ _                            = TDynamic

evalExpr :: Expr -> Parser CorbieType
evalExpr (LitExpr (IntLit _))    = return TInt
evalExpr (LitExpr (FloatLit _))  = return TFloat
evalExpr (LitExpr (StringLit _)) = return TString
evalExpr (LitExpr (BoolLit _))   = return TBool
evalExpr (VarExpr var)           = fmap snd (checkVarChain var)
evalExpr (FuncCallExpr (CallFunc name args)) = checkFuncCall name args
evalExpr (ListExpr elems)        = inferListType elems
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
      return (TList ShapeUnknown)
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
evalExpr (BinaryExpr OpMatMul left right) = do
  checkNumericMatrixExpr left
  checkNumericMatrixExpr right
  lt <- evalExpr left
  rt <- evalExpr right
  evalMatMul "@" lt rt
evalExpr (BinaryExpr op left right) = do
  checkZeroDivisor op right
  lt <- evalExpr left
  rt <- evalExpr right
  evalBinaryType op lt rt

inferListType :: [Expr] -> Parser CorbieType
inferListType [] = return (TList (ShapeKnown [0]))
inferListType elems = do
  elemTypes <- mapM evalExpr elems
  if any (== TList ShapeIrregular) elemTypes
    then return (TList ShapeIrregular)
    else case mapM extractRectShape elemTypes of
      Just ((firstNonNum, firstD) : rest) ->
        if all ((== firstD) . snd) rest
          then let nonNum = firstNonNum || any fst rest
                   ctor = if nonNum then ShapeNonNumeric else ShapeKnown
               in return (TList (ctor (length elems : firstD)))
          else return (TList ShapeIrregular)
      Just [] -> return (TList (ShapeKnown [0]))
      Nothing ->
        if any isListType elemTypes
          then if all isListOrDyn elemTypes
                 then return (TList ShapeUnknown)
                 else return (TList ShapeIrregular)
          else if any isScalarNonNumeric elemTypes
            then return (TList (ShapeNonNumeric [length elems]))
            else if any (== TDynamic) elemTypes
              then return (TList ShapeUnknown)
              else return (TList (ShapeKnown [length elems]))
  where
    extractRectShape (TList (ShapeKnown d))      = Just (False, d)
    extractRectShape (TList (ShapeNonNumeric d)) = Just (True, d)
    extractRectShape _                           = Nothing
    isScalarNonNumeric TInt     = False
    isScalarNonNumeric TFloat   = False
    isScalarNonNumeric TDynamic = False
    isScalarNonNumeric _        = True
    isListOrDyn (TList _) = True
    isListOrDyn TDynamic  = True
    isListOrDyn _         = False

checkNumericMatrixExpr :: Expr -> Parser ()
checkNumericMatrixExpr (ListExpr elems) = mapM_ checkElem elems
  where
    checkElem (ListExpr inner) = mapM_ checkElem inner
    checkElem e = do
      t <- evalExpr e
      if isNumeric t || isListType t
        then return ()
        else semanticError ("matrix elements must be numeric, got " ++ show t)
checkNumericMatrixExpr _ = return ()

evalMatMul :: String -> CorbieType -> CorbieType -> Parser CorbieType
evalMatMul opName (TList ShapeIrregular) _ =
  semanticError ("operator '" ++ opName ++ "' cannot be applied to irregular matrix")
evalMatMul opName _ (TList ShapeIrregular) =
  semanticError ("operator '" ++ opName ++ "' cannot be applied to irregular matrix")
evalMatMul opName (TList (ShapeNonNumeric _)) _ =
  semanticError ("matrix elements must be numeric for '" ++ opName ++ "'")
evalMatMul opName _ (TList (ShapeNonNumeric _)) =
  semanticError ("matrix elements must be numeric for '" ++ opName ++ "'")
evalMatMul opName (TList (ShapeKnown s1)) (TList (ShapeKnown s2))
  | null s1 || null s2 || any (== 0) s1 || any (== 0) s2 =
      semanticError ("cannot multiply empty matrix in '" ++ opName ++ "'")
  | otherwise = case (s1, s2) of
      ([m, n], [p, q]) ->
        if n == p then return (TList (ShapeKnown [m, q]))
        else semanticError ("matrix dimension mismatch in '" ++ opName ++ "': cannot multiply (" ++ show m ++ "x" ++ show n ++ ") by (" ++ show p ++ "x" ++ show q ++ ")")
      ([m, n], [p]) ->
        if n == p then return (TList (ShapeKnown [m]))
        else semanticError ("matrix dimension mismatch in '" ++ opName ++ "': cannot multiply (" ++ show m ++ "x" ++ show n ++ ") by vector of length " ++ show p)
      ([n], [p, q]) ->
        if n == p then return (TList (ShapeKnown [q]))
        else semanticError ("matrix dimension mismatch in '" ++ opName ++ "': cannot multiply vector of length " ++ show n ++ " by (" ++ show p ++ "x" ++ show q ++ ")")
      ([n], [p]) ->
        if n == p then return TDynamic
        else semanticError ("vector dimension mismatch in '" ++ opName ++ "': lengths " ++ show n ++ " and " ++ show p ++ " do not match")
      _ ->
        let innerLeft  = last s1
            innerRight = if length s2 >= 2 then s2 !! (length s2 - 2) else head s2
        in if innerLeft == innerRight
             then return (TList ShapeUnknown)
             else semanticError ("tensor dimension mismatch in '" ++ opName ++ "': " ++ show s1 ++ " vs " ++ show s2)
evalMatMul _ (TList _) (TList _) = return (TList ShapeUnknown)
evalMatMul _ (TList _) TDynamic  = return (TList ShapeUnknown)
evalMatMul _ TDynamic (TList _)  = return (TList ShapeUnknown)
evalMatMul _ TDynamic TDynamic   = return (TList ShapeUnknown)
evalMatMul opName lt rt =
  semanticError ("operator '" ++ opName ++ "' requires matrix/list operands, got " ++ show lt ++ " and " ++ show rt)

checkZeroDivisor :: BinOp -> Expr -> Parser ()
checkZeroDivisor op exprNode
  | op `elem` [OpDiv, OpMod] = do
      isZ <- isZeroExpr exprNode
      if isZ
        then semanticError (if op == OpDiv then "division by zero" else "modulo by zero")
        else return ()
  | otherwise = return ()

isZeroExpr :: Expr -> Parser Bool
isZeroExpr (LitExpr (IntLit 0))     = return True
isZeroExpr (LitExpr (FloatLit 0.0)) = return True
isZeroExpr (UnaryExpr OpNeg e)      = isZeroExpr e
isZeroExpr (UnaryExpr OpPos e)      = isZeroExpr e
isZeroExpr (VarExpr (SimpleVar name)) = do
  st <- getState
  case symtable_lookup_scoped name st of
    Just (key, _) -> return (key `elem` constZeros st)
    Nothing       -> return False
isZeroExpr _ = return False

evalBinaryType :: BinOp -> CorbieType -> CorbieType -> Parser CorbieType
evalBinaryType OpOr lt rt  = checkLogical "or" lt rt
evalBinaryType OpAnd lt rt = checkLogical "and" lt rt
evalBinaryType OpIn lt rt
  | rt == TString =
      if lt == TString || lt == TDynamic then return TBool
      else semanticError ("left operand of 'in' on string must be string, got " ++ show lt)
  | rt == TDict =
      if lt == TString || lt == TDynamic then return TBool
      else semanticError ("left operand of 'in' on dict must be string key, got " ++ show lt)
  | isIterable rt = return TBool
  | otherwise     = semanticError ("right operand of 'in' must be iterable, got " ++ show rt)
evalBinaryType op lt rt
  | op `elem` [OpEqual, OpNotEqual] = checkEqCompat lt rt
  | op `elem` [OpLess, OpLessEqual, OpGreater, OpGreaterEqual] = checkOrdCompat lt rt
evalBinaryType OpAdd lt rt
  | lt == TDynamic || rt == TDynamic =
      if isListType lt || isListType rt then return (TList ShapeUnknown) else return TDynamic
  | lt == TInt && rt == TInt         = return TInt
  | isNumeric lt && isNumeric rt     = return TFloat
  | lt == TString && rt == TString   = return TString
  | isListType lt && isListType rt   = return (TList ShapeUnknown)
  | otherwise = semanticError ("incompatible types for '+': " ++ show lt ++ " and " ++ show rt)
evalBinaryType OpCat lt rt
  | lt == TDynamic || rt == TDynamic = return TDynamic
  | lt == TString && rt == TString   = return TString
  | isListType lt && isListType rt   = return (TList ShapeUnknown)
  | otherwise = semanticError ("incompatible types for 'cat': " ++ show lt ++ " and " ++ show rt)
evalBinaryType OpMatMul lt rt = evalMatMul "@" lt rt
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
  | isListType lt && isListType rt   = return TBool
  | lt == rt                         = return TBool
  | otherwise = semanticError ("cannot compare incompatible types " ++ show lt ++ " and " ++ show rt)

checkOrdCompat :: CorbieType -> CorbieType -> Parser CorbieType
checkOrdCompat lt rt
  | lt == TDynamic || rt == TDynamic = return TBool
  | isNumeric lt && isNumeric rt     = return TBool
  | lt == TString && rt == TString   = return TBool
  | isListType lt && isListType rt   = return TBool
  | otherwise = semanticError ("cannot order incompatible types " ++ show lt ++ " and " ++ show rt)
