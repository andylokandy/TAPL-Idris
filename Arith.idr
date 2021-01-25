import Data.Maybe
import Data.List
import Text.Parser
import Text.Quantity
import Text.Token
import Text.Lexer
import Data.Strings
import Data.String.Extra

data ArithTokenKind = Ident | Keyword String | White

Eq ArithTokenKind where
  (==) Ident Ident = True
  (==) (Keyword a) (Keyword b) = a == b
  (==) White White = True
  (==) _ _ = False

ArithToken : Type
ArithToken = Token ArithTokenKind

TokenKind ArithTokenKind where
  TokType Ident = String
  TokType (Keyword _) = ()
  TokType White = ()

  tokValue Ident x = x
  tokValue (Keyword _) _ = ()
  tokValue White _ = ()

Show ArithToken where
  show (Tok Ident x) = "Ident \"" ++ (Token.tokValue Ident x) ++ "\""
  show (Tok (Keyword x) _) = "Keyword \"" ++ x ++ "\""
  show (Tok White _) = "White"

arithTokenMap : TokenMap ArithToken
arithTokenMap = 
  toTokenMap $ [ 
      (exact "if", Keyword "if"),
      (exact "then", Keyword "then"),
      (exact "else", Keyword "else"),
      (oneOf " \t\n\r", White),
      (some $ non $ oneOf " \t\n\r", Ident)
    ]

lexArith : String -> List ArithToken
lexArith str =
  let
    (tokens, _, _, _) = lex arithTokenMap str
  in
    List.filter (\tok => tok.kind /= White) $ map TokenData.tok tokens

data ArithTerm =
    TmTrue
  | TmFalse
  | TmZero
  | TmSucc ArithTerm
  | TmPred ArithTerm
  | TmIsZero ArithTerm
  | TmIf ArithTerm ArithTerm ArithTerm

Show ArithTerm where
  show TmTrue = "TmTrue"
  show TmFalse = "TmFalse"
  show TmZero = "0"
  show (TmSucc t) = "TmSucc (" ++ show t ++ ")"
  show (TmPred t) = "TmPred (" ++ show t ++ ")"
  show (TmIsZero t) = "TmIsZero (" ++ show t ++ ")"
  show (TmIf t1 t2 t3) = "TmIf (" ++ show t1 ++ ") then (" ++ show t2 ++ ") else (" ++ show t3 ++ ")"

mutual
  constTerm : String -> Grammar ArithToken False ArithTerm
  constTerm "true" = pure TmTrue
  constTerm "false" = pure TmFalse
  constTerm "0" = pure TmZero
  constTerm _ = fail "unrecognised const"

  funcTerm : String -> Grammar ArithToken True ArithTerm
  funcTerm "succ" = pure $ TmSucc !arithTerm
  funcTerm "pred" = pure $ TmPred !arithTerm
  funcTerm "isZero" = pure $ TmIsZero !arithTerm
  funcTerm _ = fail "unrecognised func"

  exprTerm : Grammar ArithToken True ArithTerm
  exprTerm =
    do
      ident <- match Ident
      constTerm ident <|> funcTerm ident

  ifTerm : Grammar ArithToken True ArithTerm
  ifTerm = 
    do
      match $ Keyword "if"
      ifPred <- exprTerm
      match $ Keyword "then"
      ifTrue <- exprTerm
      match $ Keyword "else"
      ifFalse <- exprTerm
      pure $ TmIf ifPred ifTrue ifFalse

  arithTerm : Grammar ArithToken True ArithTerm
  arithTerm = exprTerm <|> ifTerm

parseArith : String -> Maybe ArithTerm
parseArith str =
  case parse arithTerm (lexArith str) of
    Right (j, []) => Just j
    _ => Nothing

isNumericVal : ArithTerm -> Bool
isNumericVal TmZero = True
isNumericVal (TmSucc t) = isNumericVal t
isNumericVal (TmPred t) = isNumericVal t
isNumericVal _ = False

isVal : ArithTerm -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t = isNumericVal t

eval1 : ArithTerm -> Maybe ArithTerm
eval1 (TmIf TmTrue ifTrue _) = Just ifTrue
eval1 (TmIf TmFalse _ ifFalse) = Just ifFalse
eval1 (TmIf ifPred ifTrue ifFalse) = Just $ TmIf !(eval1 ifPred) ifTrue ifFalse
eval1 (TmPred TmZero) = Just TmZero
eval1 (TmPred (TmSucc t)) = if isNumericVal t then Just t else Nothing
eval1 (TmPred t) = Just $ TmPred !(eval1 t)
eval1 (TmIsZero TmZero) = Just TmTrue
eval1 (TmIsZero (TmSucc t)) = if isNumericVal t then Just TmFalse else Nothing
eval1 (TmIsZero t) = if isNumericVal t then Just (TmIsZero !(eval1 t)) else Nothing
eval1 _ = Nothing

eval : ArithTerm -> ArithTerm
eval t =
  case eval1 t of
    Just t' => eval t'
    Nothing => t

main : IO ()
main =
  do
    let input = "if isZero pred succ pred succ pred 0 then isZero succ 0 else succ 0"
    putStrLn $ "input: " ++ show input

    let tokens = lexArith input
    putStrLn $ "tokens: " ++ show tokens

    Right (terms, []) <- pure $ parse arithTerm tokens
      | _ => printLn "parse failure"
    putStrLn $ "terms: " ++ show terms

    let evalSteps = List.iterate eval1 terms
    putStrLn $ "evalSteps: " ++ show evalSteps

    let result = fromMaybe terms $ List.last' evalSteps
    if isVal result
      then putStrLn $ "result: " ++ show result
      else putStrLn $ "no more rules applies :("
