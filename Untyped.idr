import Data.Maybe
import Data.List
import Text.Parser
import Text.Quantity
import Text.Token
import Text.Lexer
import Data.Strings
import Data.String.Extra

data RawTokenKind = Ident | Punct Char | White

Eq RawTokenKind where
  (==) Ident Ident = True
  (==) (Punct a) (Punct b) = a == b
  (==) White White = True
  (==) _ _ = False

RawToken : Type
RawToken = Token RawTokenKind

TokenKind RawTokenKind where
  TokType Ident = String
  TokType (Punct _) = ()
  TokType White = ()

  tokValue Ident x = x
  tokValue (Punct _) _ = ()
  tokValue White _ = ()

Show RawToken where
  show (Tok Ident x) = "Ident \"" ++ (Token.tokValue Ident x) ++ "\""
  show (Tok (Punct x) _) = "Punct \"" ++  (Strings.singleton x) ++ "\""
  show (Tok White _) = "White"

rawTokenMap : TokenMap RawToken
rawTokenMap = 
  toTokenMap $ [ 
      (is '\\', Punct '\\'),
      (is '(', Punct '('),
      (is ')', Punct ')'),
      (oneOf " \t\n\r", White),
      (some $ non $ oneOf "\\() \t\n\r", Ident)
    ]

lexRaw : String -> List RawToken
lexRaw str =
  let
    (tokens, _, _, _) = lex rawTokenMap str
  in
    List.filter (\tok => tok.kind /= White) $ map TokenData.tok tokens

data RawTerm =
    RawVar String
  | RawAbs String RawTerm
  | RawApp RawTerm RawTerm

Show RawTerm where
  show (RawVar name) = name
  show (RawAbs name t) = "(λ" ++ name ++ ". " ++ show t ++ ")"
  show (RawApp t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

mutual
  absTerm : Grammar RawToken True RawTerm
  absTerm =
    do
      match $ Punct '('
      match $ Punct '\\'
      name <- match Ident
      body <- rawTerm
      match $ Punct ')'
      pure $ RawAbs name body 
      
  varTerm : Grammar RawToken True RawTerm
  varTerm =
    do
      name <- match Ident
      pure $ RawVar name

  appTerm : Grammar RawToken True RawTerm
  appTerm =
    do
      match $ Punct '('
      t1 <- rawTerm
      t2 <- rawTerm
      match $ Punct ')'
      pure $ RawApp t1 t2

  rawTerm : Grammar RawToken True RawTerm
  rawTerm = absTerm <|> varTerm <|> appTerm

data Term =
    Var Nat Nat
  | Abs String Term
  | App Term Term

Show Term where
  show (Var index ctxlen) = show index
  show (Abs name t) = "(λ" ++ name ++ ". " ++ show t ++ ")"
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

Context : Type
Context = List String

position : String -> Context -> Maybe Nat
position = position' Z
where
  position' : Nat -> String -> Context -> Maybe Nat
  position' index name [] = Nothing
  position' index name (x::xs) =
    if x == name 
      then Just index 
      else position' (S index) name xs

compile : Context -> RawTerm -> Maybe Term
compile ctx (RawVar name) = pure $ Var !(position name ctx) (length ctx)
compile ctx (RawAbs name body) = pure $ Abs name !(compile (name::ctx) body)
compile ctx (RawApp t1 t2) = pure $ App !(compile ctx t1) !(compile ctx t2)

shiftTerm : (cut : Nat) -> (shift : Nat -> Nat) -> Term -> Term
shiftTerm cut shift (Var index ctxlen) =
  if index >= cut
    then Var (shift index) (shift ctxlen)
    else Var index (shift ctxlen)
shiftTerm cut shift (Abs name body) = Abs name (shiftTerm (S cut) shift body)
shiftTerm cut shift (App t1 t2) = App (shiftTerm cut shift t1) (shiftTerm cut shift t2)

substTerm : (j : Nat) -> (t : Term) -> Term -> Term
substTerm j t (Var index ctxlen) =
  if index == j
    then t
    else Var index ctxlen
substTerm j t (Abs name body) = Abs name (substTerm (S j) (shiftTerm Z (plus 1) t) body)
substTerm j t (App t1 t2) = App (substTerm j t t1) (substTerm j t t2)

isVal : Term -> Bool
isVal (Abs _ _) = True
isVal _ = False

eval1 : Term -> Maybe Term
eval1 (App (Abs name body) v) =
  if isVal v 
    then pure $ shiftTerm 0 (minus 1) (substTerm 0 (shiftTerm 0 (plus 1) v) body)
    else pure $ App (Abs name body) !(eval1 v)
eval1 (App t1 t2) =
  if isVal t1 
    then pure $ App t1 !(eval1 t2)
    else pure $ App !(eval1 t1) t2
eval1 _ = Nothing

main : IO ()
main =
  do
    let input = "(((((\\xx ((\\x (\\y (\\z (x (y z))))) (\\yy (xx yy)))) (\\x x)) (\\x x)) (\\x x)) (\\x x))"
    -- let input = "(\\x (x (\\y (x y))))"
    let tokens = lexRaw input
    Right (rawTerms, []) <- pure $ parse rawTerm tokens
      | _ => printLn "parse failure"
    Just terms <- pure $ compile [] rawTerms
      | _ => printLn "variable not found in context"
    let evalSteps = List.iterate eval1 terms
    putStrLn $ "input: " ++ input
    putStrLn $ "tokens: " ++ show tokens
    putStrLn $ "rawTerms: " ++ show rawTerms
    putStrLn $ "terms: " ++ show terms
    putStrLn $ "evalSteps: " ++ show evalSteps
