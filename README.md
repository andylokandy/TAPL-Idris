# [Types and Programing Language](https://www.cis.upenn.edu/~bcpierce/tapl/) implemented in Idris

## Arith

```bash
> idris2 -p contrib Arith.idr --exec main

input: "if isZero pred succ pred succ pred 0 then isZero succ 0 else succ 0"
tokens: [Keyword "if", Ident "isZero", Ident "pred", Ident "succ", Ident "pred", Ident "succ", Ident "pred", Ident "0", Keyword "then", Ident "isZero", Ident "succ", Ident "0", Keyword "else", Ident "succ", Ident "0"]
terms: TmIf (TmIsZero (TmPred (TmSucc (TmPred (TmSucc (TmPred (0))))))) then (TmIsZero (TmSucc (0))) else (TmSucc (0))
evalSteps: [TmIf (TmIsZero (TmPred (TmSucc (TmPred (TmSucc (TmPred (0))))))) then (TmIsZero (TmSucc (0))) else (TmSucc (0)), TmIf (TmIsZero (TmPred (TmSucc (TmPred (0))))) then (TmIsZero (TmSucc (0))) else (TmSucc (0)), TmIf (TmIsZero (TmPred (0))) then (TmIsZero (TmSucc (0))) else (TmSucc (0)), TmIf (TmIsZero (0)) then (TmIsZero (TmSucc (0))) else (TmSucc (0)), TmIf (TmTrue) then (TmIsZero (TmSucc (0))) else (TmSucc (0)), TmIsZero (TmSucc (0)), TmFalse]
result: TmFalse
```

## Untyped lambda calculus

```bash
> idris2 -p contrib Untyped.idr --exec main

input: (\b. \c. b c (\t. \f. f)) (\t. \f. t) (\t. \f. f)
tokens: [Punct "(", Punct "\", Ident "b", Punct ".", Punct "\", Ident "c", Punct ".", Ident "b", Ident "c", Punct "(", Punct "\", Ident "t", Punct ".", Punct "\", Ident "f", Punct ".", Ident "f", Punct ")", Punct ")", Punct "(", Punct "\", Ident "t", Punct ".", Punct "\", Ident "f", Punct ".", Ident "t", Punct ")", Punct "(", Punct "\", Ident "t", Punct ".", Punct "\", Ident "f", Punct ".", Ident "f", Punct ")"]
rawTerms: (((λb. (λc. ((b c) (λt. (λf. f))))) (λt. (λf. t))) (λt. (λf. f)))
terms: (((λb. (λc. ((1 0) (λt. (λf. 0))))) (λt. (λf. 1))) (λt. (λf. 0)))
evalSteps: [(((λb. (λc. ((1 0) (λt. (λf. 0))))) (λt. (λf. 1))) (λt. (λf. 0))), ((λc. (((λt. (λf. 1)) 0) (λt. (λf. 0)))) (λt. (λf. 0))), (((λt. (λf. 1)) (λt. (λf. 0))) (λt. (λf. 0))), ((λf. (λt. (λf. 0))) (λt. (λf. 0))), (λt. (λf. 0))]
result: (λt. (λf. 0))
```

## Simply typed lambda calculus

```bash
> idris2 -p contrib Typed.idr --exec main

input: (\x: bool. if x then (\x: bool -> bool. x true) else (\x: bool -> bool. x false)) false (\x: bool. x)
tokens: [Punct "(", Punct "\", Ident "x", Punct ":", Ident "bool", Punct ".", Keyword "if", Ident "x", Keyword "then", Punct "(", Punct "\", Ident "x", Punct ":", Ident "bool", Punct "->", Ident "bool", Punct ".", Ident "x", Ident "true", Punct ")", Keyword "else", Punct "(", Punct "\", Ident "x", Punct ":", Ident "bool", Punct "->", Ident "bool", Punct ".", Ident "x", Ident "false", Punct ")", Punct ")", Ident "false", Punct "(", Punct "\", Ident "x", Punct ":", Ident "bool", Punct ".", Ident "x", Punct ")"]
rawTerms: (((λx: bool. RawIf (x) then ((λx: (bool -> bool). (x RawTrue))) else ((λx: (bool -> bool). (x RawFalse)))) RawFalse) (λx: bool. x))
terms: (((λx. TmIf (0) then ((λx. (0 TmTrue))) else ((λx. (0 TmFalse)))) TmFalse) (λx. 0))
type of terms: bool
evalSteps: [(((λx. TmIf (0) then ((λx. (0 TmTrue))) else ((λx. (0 TmFalse)))) TmFalse) (λx. 0)), (TmIf (TmFalse) then ((λx. (0 TmTrue))) else ((λx. (0 TmFalse))) (λx. 0)), ((λx. (0 TmFalse)) (λx. 0)), ((λx. 0) TmFalse), TmFalse]
result: TmFalse
```
