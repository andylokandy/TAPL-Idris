import Data.Maybe
import Data.List
import Text.Parser
import Text.Quantity
import Text.Token
import Text.Lexer
import Data.Strings
import Data.String.Extra

data RawTokenKind = Ident | Keyword String | Punct String | White

Eq RawTokenKind where
  (==) Ident Ident = True
  (==) (Keyword a) (Keyword b) = a == b
  (==) (Punct a) (Punct b) = a == b
  (==) White White = True
  (==) _ _ = False

RawToken : Type
RawToken = Token RawTokenKind

TokenKind RawTokenKind where
  TokType Ident = String
  TokType (Keyword _) = ()
  TokType (Punct _) = ()
  TokType White = ()

  tokValue Ident x = x
  tokValue (Keyword _) _ = ()
  tokValue (Punct _) _ = ()
  tokValue White _ = ()

Show RawToken where
  show (Tok Ident x) = "Ident \"" ++ (Token.tokValue Ident x) ++ "\""
  show (Tok (Keyword x) _) = "Keyword \"" ++  x ++ "\""
  show (Tok (Punct x) _) = "Punct \"" ++  x ++ "\""
  show (Tok White _) = "White"

rawTokenMap : TokenMap RawToken
rawTokenMap = 
  toTokenMap $ [ 
      (is '\\', Punct "\\"),
      (is ':', Punct ":"),
      (is '(', Punct "("),
      (is ')', Punct ")"),
      (is '.', Punct "."),
      (exact "->", Punct "->"),
      (exact "if", Keyword "if"),
      (exact "then", Keyword "then"),
      (exact "else", Keyword "else"),
      (oneOf " \t\n\r", White),
      (some $ non $ oneOf "->:\\(). \t\n\r", Ident)
    ]

lexRaw : String -> List RawToken
lexRaw str =
  let
    (tokens, _, _, _) = lex rawTokenMap str
  in
    List.filter (\tok => tok.kind /= White) $ map TokenData.tok tokens

data Ty = TyBool | TyArrow Ty Ty

Eq Ty where
  (==) TyBool TyBool = True
  (==) (TyArrow a1 a2) (TyArrow b1 b2) = a1 == b1 && a2 == b2
  (==) _ _ = False

||| Raw AST representation generated directly from the parser
data RawTerm =
    RawTrue
  | RawFalse
  | RawIf RawTerm RawTerm RawTerm
  | RawVar String
  | RawAbs String Ty RawTerm
  | RawApp RawTerm RawTerm

Show Ty where
  show TyBool = "bool"
  show (TyArrow a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

Show RawTerm where
  show RawTrue = "RawTrue"
  show RawFalse = "RawFalse"
  show (RawIf ifCond ifTrue ifFalse) = "RawIf (" ++ show ifCond ++ ") then (" ++ show ifTrue ++ ") else (" ++ show ifFalse ++ ")"
  show (RawVar name) = name
  show (RawAbs argName argTy body) = "(λ" ++ argName ++ ": " ++ show argTy ++ ". " ++ show body ++ ")"
  show (RawApp t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

mutual
  blockTerm : Grammar RawToken True RawTerm
  blockTerm = between (match $ Punct "(") (match $ Punct ")") rawTerm

  constantTerm : Grammar RawToken True RawTerm
  constantTerm =
    do
      ty <- match Ident
      the (Grammar RawToken False RawTerm) $
        case ty of
          "true" => pure $ RawTrue
          "false" => pure $ RawFalse
          x => fail $ "unrecognised constant " ++ x

  ifTerm : Grammar RawToken True RawTerm
  ifTerm =
    do
      match $ Keyword "if"
      ifCond <- rawTerm
      match $ Keyword "then"
      ifTrue <- rawTerm
      match $ Keyword "else"
      ifFalse <- rawTerm
      pure $ RawIf ifCond ifTrue ifFalse

  absTerm : Grammar RawToken True RawTerm
  absTerm =
    do
      match $ Punct "\\"
      argName <- match Ident
      match $ Punct ":"
      argTy <- tyTerm
      match $ Punct "."
      body <- rawTerm
      pure $ RawAbs argName argTy body 
      
  varTerm : Grammar RawToken True RawTerm
  varTerm =
    do
      name <- match Ident
      pure $ RawVar name

  appTerm : Grammar RawToken True RawTerm
  appTerm =
    do
      termsWithPrf <- some' $ ifTerm <|> constantTerm <|> absTerm <|> varTerm <|> blockTerm
      pure $ List.foldl1 RawApp (fst termsWithPrf) {ok=snd termsWithPrf}

  constantTyTerm : Grammar RawToken True Ty
  constantTyTerm =
    do
      ty <- match Ident
      the (Grammar RawToken False Ty) $
        case ty of
          "bool" => pure $ TyBool
          x => fail $ "unrecognised type " ++ x
  
  arrowTyTerm : Grammar RawToken True Ty
  arrowTyTerm =
    do
      tysWithPrf <- sepBy1' (match $ Punct "->") (constantTyTerm <|> blockTyTerm)
      pure $ List.foldl1 TyArrow (fst tysWithPrf) {ok=snd tysWithPrf} 
  
  blockTyTerm : Grammar RawToken True Ty
  blockTyTerm = between (match $ Punct "(") (match $ Punct ")") tyTerm

  tyTerm : Grammar RawToken True Ty
  tyTerm = arrowTyTerm <|> constantTyTerm <|> blockTyTerm

  rawTerm : Grammar RawToken True RawTerm
  rawTerm = ifTerm <|> constantTerm <|> absTerm <|> appTerm <|> varTerm <|> blockTerm

||| Terms that are type-checked and erased from RawTerm
data Term =
    TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmVar Nat Nat
  | TmAbs String Term
  | TmApp Term Term

Show Term where
  show TmTrue = "TmTrue"
  show TmFalse = "TmFalse"
  show (TmIf ifCond ifTrue ifFalse) = "TmIf (" ++ show ifCond ++ ") then (" ++ show ifTrue ++ ") else (" ++ show ifFalse ++ ")"
  show (TmVar index ctxlen) = "TmVar[" ++ show index ++ "]"
  show (TmAbs name t) = "(λ" ++ name ++ ". " ++ show t ++ ")"
  show (TmApp t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

Context : Type
Context = List (String, Ty)

findVar : String -> Context -> Either String (Nat, Ty)
findVar = findVar' Z
where
  findVar' : Nat -> String -> Context -> Either String (Nat, Ty)
  findVar' index targetName [] = Left $ "variable " ++ targetName ++ " not found in context"
  findVar' index targetName ((varName, ty)::xs) =
    if targetName == varName
      then pure $ (index, ty)
      else findVar' (S index) targetName xs

||| Type-check and erase type annotations.
||| Return the type-ok term and it's type.
compile : Context -> RawTerm -> Either String (Term, Ty)
compile ctx RawTrue = pure $ (TmTrue, TyBool)
compile ctx RawFalse = pure $ (TmFalse, TyBool)
compile ctx (RawIf ifCond ifTrue ifFalse) =
  do
    (ifCond', ifCondTy) <- compile ctx ifCond
    (ifTrue', ifTrueTy) <- compile ctx ifTrue
    (ifFalse', ifFalseTy) <- compile ctx ifFalse
    if ifCondTy == TyBool
      then if ifTrueTy == ifFalseTy
        then pure $ (TmIf ifCond' ifTrue' ifFalse', ifTrueTy)
        else Left "arms of conditional have different types"
      else Left "guard of conditional is not a boolean"
compile ctx (RawVar name) =
  do
    (index, ty) <- findVar name ctx
    pure $ (TmVar index (length ctx), ty)
compile ctx (RawAbs argName argTy body) =
  do
    let ctx' = (argName, argTy)::ctx
    (body', bodyTy) <- compile ctx' body
    pure $ (TmAbs argName body', TyArrow argTy bodyTy)
compile ctx (RawApp t1 t2) =
  do
    (t1', t1Ty) <- compile ctx t1
    (t2', t2Ty) <- compile ctx t2
    let TyArrow argTy retTy = t1Ty
      | _ => Left "arrow type expected"
    if argTy == t2Ty
      then pure $ (TmApp t1' t2', retTy)
      else Left "argument type mismatch"
  
shiftTerm : (cut : Nat) -> (shift : Nat -> Nat) -> Term -> Term
shiftTerm cut shift TmTrue = TmTrue
shiftTerm cut shift TmFalse = TmFalse
shiftTerm cut shift (TmIf ifCond ifTrue ifFalse) =
  TmIf (shiftTerm cut shift ifCond) (shiftTerm cut shift ifTrue) (shiftTerm cut shift ifFalse)
shiftTerm cut shift (TmVar index ctxlen) =
  if index >= cut
    then TmVar (shift index) (shift ctxlen)
    else TmVar index (shift ctxlen)
shiftTerm cut shift (TmAbs name body) = TmAbs name (shiftTerm (S cut) shift body)
shiftTerm cut shift (TmApp t1 t2) = TmApp (shiftTerm cut shift t1) (shiftTerm cut shift t2)

substTerm : (j : Nat) -> (t : Term) -> Term -> Term
substTerm j t TmTrue = TmTrue
substTerm j t TmFalse = TmFalse
substTerm j t (TmIf ifCond ifTrue ifFalse) =
  TmIf (substTerm j t ifCond) (substTerm j t ifTrue) (substTerm j t ifFalse)
substTerm j t (TmVar index ctxlen) =
  if index == j
    then t
    else TmVar index ctxlen
substTerm j t (TmAbs name body) = TmAbs name (substTerm (S j) (shiftTerm 0 (plus 1) t) body)
substTerm j t (TmApp t1 t2) = TmApp (substTerm j t t1) (substTerm j t t2)

isVal : Term -> Bool
isVal (TmAbs _ _) = True
isVal TmTrue = True 
isVal TmFalse = True
isVal _ = False

eval1 : Term -> Maybe Term
eval1 (TmIf TmTrue ifTrue ifFalse) = pure ifTrue
eval1 (TmIf TmFalse ifTrue ifFalse) = pure ifFalse
eval1 (TmIf ifCond ifTrue ifFalse) = pure $ TmIf !(eval1 ifCond) ifTrue ifFalse
eval1 (TmApp t1@(TmAbs _ body) t2) =
  if isVal t2
    then pure $ shiftTerm 0 (minus 1) (substTerm 0 (shiftTerm 0 (plus 1) t2) body)
    else pure $ TmApp t1 !(eval1 t2)
eval1 (TmApp t1 t2) = pure $ TmApp !(eval1 t1) t2
eval1 _ = Nothing

main : IO ()
main =
  do
    -- let input = "(\\x: bool. x) true"
    let input = "(\\x: (bool -> bool) -> bool. x (\\y: bool. if y then false else true)) (\\z: bool -> bool. z false)"
    -- let input = "(\\x: bool. if x then (\\x: bool -> bool. x true) else (\\x: bool -> bool. x false)) false (\\x: bool. x)"
    putStrLn $ "input: " ++ input

    let tokens = lexRaw input
    putStrLn $ "tokens: " ++ show tokens

    Right (rawTerms, []) <- pure $ parse rawTerm tokens
      | _ => printLn "parse failure"
    putStrLn $ "rawTerms: " ++ show rawTerms

    Right (terms, termTy) <- pure $ compile [] rawTerms
      | Left err => printLn err
    putStrLn $ "terms: " ++ show terms
    putStrLn $ "type of terms: " ++ show termTy
    
    let evalSteps = List.iterate eval1 terms
    putStrLn $ "evalSteps: " ++ show evalSteps

    let result = fromMaybe terms $ List.last' evalSteps
    if isVal result
      then putStrLn $ "result: " ++ show result
      else putStrLn $ "unreachable"
