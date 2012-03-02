{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Core.ShellParser(parseCommand, parseTactic) where

import Core.TT
import Core.Elaborate
import Core.CoreParser

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as PTok

import Debug.Trace

type TokenParser a = PTok.TokenParser a

lexer :: TokenParser ()
lexer  = PTok.makeTokenParser haskellDef

whiteSpace= PTok.whiteSpace lexer
lexeme    = PTok.lexeme lexer
symbol    = PTok.symbol lexer
natural   = PTok.natural lexer
parens    = PTok.parens lexer
semi      = PTok.semi lexer
comma     = PTok.comma lexer
identifier= PTok.identifier lexer
reserved  = PTok.reserved lexer
operator  = PTok.operator lexer
reservedOp= PTok.reservedOp lexer
lchar = lexeme.char

parseCommand = parse pCommand "(input)"
parseTactic  = parse (pTactic >>= return . Tac) "(input)"

pCommand :: Parser Command
pCommand = do reserved "theorem"; n <- iName []; lchar ':'; ty <- pTerm
              return (Theorem n ty)
       <|> do reserved "eval"; tm <- pTerm
              return (Eval tm)
       <|> do reserved "print"; n <- iName []; return (Print n)
       <|> do reserved "quit";
              return Quit

pTactic :: Parser Tactic
pTactic = do reserved "attack";  return Attack
      <|> do reserved "claim";   n <- iName []; lchar ':'; ty <- pTerm
             return (Claim n ty)
      <|> do reserved "regret";  return Regret
      <|> do reserved "exact";   tm <- pTerm; return (Exact tm)
      <|> do reserved "fill";    tm <- pTerm; return (Fill tm)
      <|> do reserved "apply";   tm <- pTerm; args <- many pArgType; 
             return (Apply tm args)
      <|> do reserved "solve";   return Solve
      <|> do reserved "compute"; return Compute
      <|> do reserved "intro";   n <- iName []; return (Intro (Just n))
      <|> do reserved "forall";  n <- iName []; lchar ':'; ty <- pTerm
             return (Forall n ty)
      <|> do reserved "arg";     n <- iName []; t <- iName []; return (Arg n t)
      <|> do reserved "patvar";  n <- iName []; return (Patvar n)
--       <|> do reserved "patarg";  n <- iName []; t <- iName []; return (patarg n t)
      <|> do reserved "eval";    t <- pTerm; return (EvalIn t)
      <|> do reserved "check";   t <- pTerm; return (CheckIn t)
      <|> do reserved "focus";   n <- iName []; return (Focus n)
      <|> do reserved "state";   return ProofState
      <|> do reserved "undo";    return Undo
      <|> do reserved "qed";     return QED

pArgType :: Parser Bool
pArgType = do lchar '_'; return True   -- implicit (machine fills in)
       <|> do lchar '?'; return False  -- user fills in

