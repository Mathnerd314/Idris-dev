{-# LANGUAGE PatternGuards, DeriveDataTypeable #-}

module Idris.ElabTerm where

import Idris.AbsSyntax
import Idris.DSL
import Idris.Delaborate

import Core.Elaborate hiding (Tactic(..))
import Core.TT
import Core.Evaluate

import Control.Monad
import Control.Monad.State
import Data.List
import Data.Data(Data, Typeable)
import Debug.Trace

-- Data to pass to recursively called elaborators; e.g. for where blocks,
-- paramaterised declarations, etc.

data ElabInfo = EInfo { params :: [(Name, PTerm)],
                        inblock :: Ctxt [Name], -- names in the block, and their params
                        namespace :: Maybe [String] }
            deriving (Data, Typeable)

toplevel = EInfo [] emptyContext Nothing

type ElabD a = Elab' [PDecl] a

-- Using the elaborator, convert a term in raw syntax to a fully
-- elaborated, typechecked term.
--
-- If building a pattern match, we convert undeclared variables from
-- holes to pattern bindings.

-- Also find deferred names in the term and their types

build :: IState -> ElabInfo -> Bool -> Name -> PTerm -> 
         ElabD (Term, [(Name, Type)], [PDecl])
build ist info pattern fn tm 
    = do elab ist info pattern False fn tm
         is <- getAux
         tt <- get_term
         let (tm, ds) = runState (collectDeferred tt) []
         return (tm, ds, is)

-- Build a term autogenerated as a typeclass method definition
-- (Separate, so we don't go overboard resolving things that we don't
-- know about yet on the LHS of a pattern def)

buildTC :: IState -> ElabInfo -> Bool -> Bool -> Name -> PTerm -> 
         ElabD (Term, [(Name, Type)], [PDecl])
buildTC ist info pattern tcgen fn tm 
    = do elab ist info pattern tcgen fn tm
         is <- getAux
         tt <- get_term
         let (tm, ds) = runState (collectDeferred tt) []
         return (tm, ds, is)

-- Returns the set of declarations we need to add to complete the definition
-- (most likely case blocks to elaborate)

elab :: IState -> ElabInfo -> Bool -> Bool -> Name -> PTerm -> 
        ElabD ()
elab ist info pattern tcgen fn tm 
    = do elabE (False, False) tm -- (in argument, guarded)
         when pattern -- convert remaining holes to pattern vars
              mkPat
         inj <- get_inj
         mapM_ checkInjective inj
  where
    isph arg = case getTm arg of
        Placeholder -> (True, priority arg)
        _ -> (False, priority arg)

    toElab ina arg = case getTm arg of
        Placeholder -> Nothing
        v -> Just (priority arg, elabE ina v)

    toElab' ina arg = case getTm arg of
        Placeholder -> Nothing
        v -> Just (elabE ina v)

    mkPat = do hs <- get_holes
               case hs of
                  (h: hs) -> do patvar h; mkPat
                  [] -> return ()

    elabE ina t = {- do g <- goal
                 tm <- get_term
                 trace ("Elaborating " ++ show t ++ " : " ++ show g ++ "\n\tin " ++ show tm) 
                    $ -} elab' ina t

    local f = do e <- get_env
                 return (f `elem` map fst e)

    elab' ina PSet           = do apply RSet []; solve
    elab' ina (PConstant c)  = do apply (RConstant c) []; solve
    elab' ina (PQuote r)     = do fill r; solve
    elab' ina (PTrue fc)     = try (elab' ina (PRef fc unitCon))
                                   (elab' ina (PRef fc unitTy))
    elab' ina (PFalse fc)    = elab' ina (PRef fc falseTy)
    elab' ina (PResolveTC (FC "HACK" _)) -- for chasing parent classes
       = resolveTC 5 fn ist
    elab' ina (PResolveTC fc) 
        | pattern = do c <- unique_hole (MN 0 "c")
                       instanceArg c
        | otherwise = try (resolveTC 2 fn ist)
                          (do c <- unique_hole (MN 0 "c")
                              instanceArg c)
    elab' ina (PRefl fc)     = elab' ina (PApp fc (PRef fc eqCon) [pimp (MN 0 "a") Placeholder,
                                                           pimp (MN 0 "x") Placeholder])
    elab' ina (PEq fc l r)   = elab' ina (PApp fc (PRef fc eqTy) [pimp (MN 0 "a") Placeholder,
                                                          pimp (MN 0 "b") Placeholder,
                                                          pexp l, pexp r])
    elab' ina@(_, a) (PPair fc l r) 
                             = try (elabE (True, a) (PApp fc (PRef fc pairTy)
                                            [pexp l,pexp r]))
                                   (elabE (True, a) (PApp fc (PRef fc pairCon)
                                            [pimp (MN 0 "A") Placeholder,
                                             pimp (MN 0 "B") Placeholder,
                                             pexp l, pexp r]))
    elab' ina (PDPair fc l@(PRef _ n) t r)
            = case t of 
                Placeholder -> try asType asValue
                _ -> asType
         where asType = elab' ina (PApp fc (PRef fc sigmaTy)
                                        [pexp t,
                                         pexp (PLam n Placeholder r)])
               asValue = elab' ina (PApp fc (PRef fc existsCon)
                                         [pimp (MN 0 "a") t,
                                          pimp (MN 0 "P") Placeholder,
                                          pexp l, pexp r])
    elab' ina (PDPair fc l t r) = elab' ina (PApp fc (PRef fc existsCon)
                                            [pimp (MN 0 "a") t,
                                             pimp (MN 0 "P") Placeholder,
                                             pexp l, pexp r])
    elab' ina (PAlternative as) 
        = let as' = pruneAlt as in
              try (tryAll (zip (map (elab' ina) as') (map showHd as')))
                  (tryAll (zip (map (elab' ina) as) (map showHd as)))
        where showHd (PApp _ h _) = show h
              showHd x = show x
    elab' (ina, guarded) (PRef fc n) | pattern && not (inparamBlock n)
                         = do ctxt <- get_context
                              let iscon = isConName Nothing n ctxt
                              let defined = case lookupTy Nothing n ctxt of
                                                [] -> False
                                                _ -> True
                            -- this is to stop us resolve type classes recursively
                              -- trace (show (n, guarded)) $
                              if (tcname n && ina) then erun fc $ patvar n
                                else if (defined && not guarded)
                                        then do apply (Var n) []; solve
                                        else try (do apply (Var n) []; solve)
                                                 (patvar n)
      where inparamBlock n = case lookupCtxtName Nothing n (inblock info) of
                                [] -> False
                                _ -> True
    elab' ina (PRef fc n) = erun fc $ do apply (Var n) []; solve
    elab' ina@(_, a) (PLam n Placeholder sc)
          = do attack; intro (Just n); elabE (True, a) sc; solve
    elab' ina@(_, a) (PLam n ty sc)
          = do tyn <- unique_hole (MN 0 "lamty")
               claim tyn RSet
               attack
               introTy (Var tyn) (Just n)
               -- end_unify
               focus tyn
               elabE (True, a) ty
               elabE (True, a) sc
               solve
    elab' ina@(_,a) (PPi _ n Placeholder sc)
          = do attack; arg n (MN 0 "ty"); elabE (True, a) sc; solve
    elab' ina@(_,a) (PPi _ n ty sc) 
          = do attack; tyn <- unique_hole (MN 0 "ty")
               claim tyn RSet
               n' <- case n of 
                        MN _ _ -> unique_hole n
                        _ -> return n
               forall n' (Var tyn)
               focus tyn
               elabE (True, a) ty
               elabE (True, a) sc
               solve
    elab' ina@(_,a) (PLet n ty val sc)
          = do attack;
               tyn <- unique_hole (MN 0 "letty")
               claim tyn RSet
               valn <- unique_hole (MN 0 "letval")
               claim valn (Var tyn)
               letbind n (Var tyn) (Var valn)
               case ty of
                   Placeholder -> return ()
                   _ -> do focus tyn
                           elabE (True, a) ty
               focus valn
               elabE (True, a) val
               elabE (True, a) sc
               solve
--     elab' ina (PTyped val ty)
--           = do tyn <- unique_hole (MN 0 "castty")
--                claim tyn RSet
--                valn <- unique_hole (MN 0 "castval")
--                claim valn (Var tyn)
--                focus tyn
--                elabE True ty
--                focus valn
--                elabE True val
--     elab' ina (PApp fc (PRef _ dsl) [arg])
--        | [d] <- lookupCtxt Nothing dsl (idris_dsls ist)
--                 = let dsl' = expandDo d (getTm arg) in
--                       trace (show dsl') $ elab' ina dsl'
    elab' (ina, g) tm@(PApp fc (PRef _ f) args') 
       = do let args = {- case lookupCtxt f (inblock info) of
                          Just ps -> (map (pexp . (PRef fc)) ps ++ args')
                          _ ->-} args'
--             newtm <- mkSpecialised ist fc f (map getTm args') tm
            ivs <- get_instances
            -- HACK: we shouldn't resolve type classes if we're defining an instance
            -- function or default definition.
            let isinf = f == inferCon || tcname f
            ctxt <- get_context
            let guarded = isConName Nothing f ctxt
            try (do ns <- apply (Var f) (map isph args)
                    let (ns', eargs) 
                         = unzip $
                             sortBy (\(_,x) (_,y) -> compare (priority x) (priority y))
                                    (zip ns args)
                    try (elabArgs (ina || not isinf, guarded)
                             [] False ns' (map (\x -> (lazyarg x, getTm x)) eargs))
                        (elabArgs (ina || not isinf, guarded)
                             [] False (reverse ns') 
                                      (map (\x -> (lazyarg x, getTm x)) (reverse eargs)))
                    mkSpecialised ist fc f (map getTm args') tm
                    solve)
                (do apply_elab f (map (toElab (ina || not isinf, guarded)) args)
                    mkSpecialised ist fc f (map getTm args') tm
                    solve)
            ivs' <- get_instances
            when (not pattern || (ina && not tcgen)) $
                mapM_ (\n -> do focus n
                                -- let insts = filter tcname $ map fst (ctxtAlist (tt_ctxt ist))
                                resolveTC 7 fn ist) (ivs' \\ ivs) 
      where tcArg (n, PConstraint _ _ Placeholder) = True
            tcArg _ = False

    elab' ina@(_, a) (PApp fc f [arg])
          = erun fc $ 
             do simple_app (elabE ina f) (elabE (True, a) (getTm arg))
                solve
    elab' ina Placeholder = do (h : hs) <- get_holes
                               movelast h
    elab' ina (PMetavar n) = let n' = mkN n in
                                 do attack; defer n'; solve
        where mkN n@(NS _ _) = n
              mkN n = case namespace info of
                        Just xs@(_:_) -> NS n xs
                        _ -> n
    elab' ina (PProof ts) = do mapM_ (runTac True ist) ts
    elab' ina (PTactics ts) 
        | not pattern = do mapM_ (runTac False ist) ts
        | otherwise = elab' ina Placeholder
    elab' ina (PElabError e) = fail (pshow ist e)
    elab' ina@(_, a) c@(PCase fc scr opts)
        = do attack
             tyn <- unique_hole (MN 0 "scty")
             claim tyn RSet
             valn <- unique_hole (MN 0 "scval")
             scvn <- unique_hole (MN 0 "scvar")
             claim valn (Var tyn)
             letbind scvn (Var tyn) (Var valn)
             focus valn
             elabE (True, a) scr
             args <- get_env
             cname <- unique_hole (mkCaseName fn)
             elab' ina (PMetavar cname)
             let cname' = mkN cname
             let newdef = PClauses fc [] cname' (caseBlock fc cname' (reverse args) opts)
             -- fail $ "Not implemented " ++ show c ++ "\n" ++ show args
             -- elaborate case
             updateAux (newdef : )
             solve
        where mkCaseName (NS n ns) = NS (mkCaseName n) ns
              mkCaseName (UN x) = UN (x ++ "_case")
              mkCaseName (MN i x) = MN i (x ++ "_case")
              mkN n@(NS _ _) = n
              mkN n = case namespace info of
                        Just xs@(_:_) -> NS n xs
                        _ -> n
    elab' ina x = fail $ "Something's gone wrong. Did you miss a semi-colon somewhere?"

    caseBlock :: FC -> Name -> [(Name, Binder Term)] -> [(PTerm, PTerm)] -> [PClause]
    caseBlock fc n env opts 
        = let args = map mkarg (map fst (init env)) in
              map (mkClause args) opts
       where -- mkarg (MN _ _) = Placeholder
             mkarg n = PRef fc n
             mkClause args (l, r) 
                = PClause fc n (PApp fc (PRef fc n)
                                     (map pexp args ++ [pexp l])) [] r []

    elabArgs ina failed retry [] _
        | retry = let (ns, ts) = unzip (reverse failed) in
                      elabArgs ina [] False ns ts
        | otherwise = return ()
    elabArgs ina failed r (n:ns) ((_, Placeholder) : args) 
        = elabArgs ina failed r ns args
    elabArgs ina failed r (n:ns) ((lazy, t) : args)
        | lazy && not pattern 
          = do elabArg n (PApp bi (PRef bi (UN "lazy"))
                               [pimp (UN "a") Placeholder,
                                pexp t]); 
        | otherwise = elabArg n t
      where elabArg n t 
                = do hs <- get_holes
                     tm <- get_term
                     failed' <- -- trace (show (n, t, hs, tm)) $ 
                                case n `elem` hs of
                                   True ->
                                      if r
                                         then try (do focus n; elabE ina t; return failed)
                                                  (return ((n,(lazy, t)):failed))
                                         else do focus n; elabE ina t; return failed
                                   False -> return failed
                     elabArgs ina failed r ns args

-- For every alternative, look at the function at the head. Automatically resolve
-- any nested alternatives where that function is also at the head

pruneAlt :: [PTerm] -> [PTerm]
pruneAlt xs = map prune xs
  where
    prune (PApp fc1 (PRef fc2 f) as) 
        = PApp fc1 (PRef fc2 f) (fmap (fmap (choose f)) as)
    prune t = t

    choose f (PAlternative as)
        = let as' = fmap (choose f) as
              fs = filter (headIs f) as' in
              case fs of
                 [a] -> a
                 _ -> PAlternative as'
    choose f (PApp fc f' as) = PApp fc (choose f f') (fmap (fmap (choose f)) as)
    choose f t = t

    headIs f (PApp _ (PRef _ f') _) = f == f'
    headIs f (PApp _ f' _) = headIs f f'
    headIs f _ = True -- keep if it's not an application

trivial :: IState -> ElabD ()
trivial ist = try (do elab ist toplevel False False (MN 0 "tac") (PRefl (FC "prf" 0))
                      return ())
                  (do env <- get_env
                      tryAll (map fst env)
                      return ())
      where
        tryAll []     = fail "No trivial solution"
        tryAll (x:xs) = try (elab ist toplevel False False
                                    (MN 0 "tac") (PRef (FC "prf" 0) x))
                            (tryAll xs)

findInstances :: IState -> Term -> [Name]
findInstances ist t 
    | (P _ n _, _) <- unApply t 
        = case lookupCtxt Nothing n (idris_classes ist) of
            [CI _ _ _ _ ins] -> ins
            _ -> []
    | otherwise = []

resolveTC :: Int -> Name -> IState -> ElabD ()
resolveTC 0 fn ist = fail $ "Can't resolve type class"
resolveTC 1 fn ist = try (trivial ist) (resolveTC 0 fn ist)
resolveTC depth fn ist 
         = try (trivial ist)
               (do t <- goal
                   let insts = findInstances ist t
                   let (tc, ttypes) = unApply t
                   scopeOnly <- needsDefault t tc ttypes
                   tm <- get_term
--                    traceWhen (depth > 6) ("GOAL: " ++ show t ++ "\nTERM: " ++ show tm) $
--                        (tryAll (map elabTC (map fst (ctxtAlist (tt_ctxt ist)))))
                   let depth' = if scopeOnly then 2 else depth
                   blunderbuss t depth' insts)
  where
    elabTC n | n /= fn && tcname n = (resolve n depth, show n)
             | otherwise = (fail "Can't resolve", show n)

--     needsDefault t num@(P _ (NS (UN "Num") ["builtins"]) _) [P Bound a _]
--         = do focus a
--              fill (RConstant IType) -- default Int
--              solve
--              return False
    needsDefault t f as
          | all boundVar as = return True -- fail $ "Can't resolve " ++ show t
    needsDefault t f a = return False -- trace (show t) $ return ()

    boundVar (P Bound _ _) = True
    boundVar _ = False

    blunderbuss t d [] = lift $ tfail $ CantResolve t
    blunderbuss t d (n:ns) 
        | n /= fn && tcname n = try (resolve n d)
                                    (blunderbuss t d ns)
        | otherwise = blunderbuss t d ns

    resolve n depth
       | depth == 0 = fail $ "Can't resolve type class"
       | otherwise 
           = do t <- goal
                let (tc, ttypes) = unApply t
--                 if (all boundVar ttypes) then resolveTC (depth - 1) fn insts ist 
--                   else do
                   -- if there's a hole in the goal, don't even try
                let imps = case lookupCtxtName Nothing n (idris_implicits ist) of
                                [] -> []
                                [args] -> map isImp (snd args) -- won't be overloaded!
                args <- apply (Var n) imps
--                 traceWhen (all boundVar ttypes) ("Progress: " ++ show t ++ " with " ++ show n) $
                mapM_ (\ (_,n) -> do focus n
                                     t' <- goal
                                     let (tc', ttype) = unApply t'
                                     let depth' = if t == t' then depth - 1 else depth
                                     resolveTC depth' fn ist) 
                      (filter (\ (x, y) -> not x) (zip (map fst imps) args))
                -- if there's any arguments left, we've failed to resolve
                solve
       where isImp (PImp p _ _ _) = (True, p)
             isImp arg = (False, priority arg)

collectDeferred :: Term -> State [(Name, Type)] Term
collectDeferred (Bind n (GHole t) app) =
    do ds <- get
       put ((n, t) : ds)
       return app
collectDeferred (Bind n b t) = do b' <- cdb b
                                  t' <- collectDeferred t
                                  return (Bind n b' t')
  where
    cdb (Let t v)   = liftM2 Let (collectDeferred t) (collectDeferred v)
    cdb (Guess t v) = liftM2 Guess (collectDeferred t) (collectDeferred v)
    cdb b           = do ty' <- collectDeferred (binderTy b)
                         return (b { binderTy = ty' })
collectDeferred (App f a) = liftM2 App (collectDeferred f) (collectDeferred a)
collectDeferred t = return t

-- Running tactics directly

runTac :: Bool -> IState -> PTactic -> ElabD ()
runTac autoSolve ist tac = runT (fmap (addImpl ist) tac) where
    runT (Intro []) = do g <- goal
                         attack; intro (bname g)
      where
        bname (Bind n _ _) = Just n
        bname _ = Nothing
    runT (Intro xs) = mapM_ (\x -> do attack; intro (Just x)) xs
    runT Intros = do g <- goal
                     attack; intro (bname g)
                     try (runT Intros)
                         (return ())
      where
        bname (Bind n _ _) = Just n
        bname _ = Nothing
    runT (Exact tm) = do elab ist toplevel False False (MN 0 "tac") tm
                         when autoSolve solveAll
    runT (Refine fn [])   
        = do (fn', imps) <- case lookupCtxtName Nothing fn (idris_implicits ist) of
                                    [] -> do a <- envArgs fn
                                             return (fn, a)
                                    -- FIXME: resolve ambiguities
                                    [(n, args)] -> return $ (n, map isImp args)
             ns <- apply (Var fn') (map (\x -> (x,0)) imps)
             when autoSolve solveAll
       where isImp (PImp _ _ _ _) = True
             isImp _ = False
             envArgs n = do e <- get_env
                            case lookup n e of
                               Just t -> return $ map (const False)
                                                      (getArgTys (binderTy t))
                               _ -> return []
    runT (Refine fn imps) = do ns <- apply (Var fn) (map (\x -> (x,0)) imps)
                               when autoSolve solveAll
    runT (Rewrite tm) -- to elaborate tm, let bind it, then rewrite by that
              = do attack; -- (h:_) <- get_holes
                   tyn <- unique_hole (MN 0 "rty")
                   -- start_unify h
                   claim tyn RSet
                   valn <- unique_hole (MN 0 "rval")
                   claim valn (Var tyn)
                   letn <- unique_hole (MN 0 "rewrite_rule")
                   letbind letn (Var tyn) (Var valn)  
                   focus valn
                   elab ist toplevel False False (MN 0 "tac") tm
                   rewrite (Var letn)
                   when autoSolve solveAll
    runT (LetTac n tm)
              = do attack
                   tyn <- unique_hole (MN 0 "letty")
                   claim tyn RSet
                   valn <- unique_hole (MN 0 "letval")
                   claim valn (Var tyn)
                   letn <- unique_hole n
                   letbind letn (Var tyn) (Var valn)
                   focus valn
                   elab ist toplevel False False (MN 0 "tac") tm
                   when autoSolve solveAll
    runT Compute = compute
    runT Trivial = do trivial ist; when autoSolve solveAll
    runT (Focus n) = focus n
    runT Solve = solve
    runT (Try l r) = do try (runT l) (runT r)
    runT (TSeq l r) = do runT l; runT r
    runT x = fail $ "Not implemented " ++ show x

solveAll = try (do solve; solveAll) (return ())

-- If the function application is specialisable, make a new
-- top level function by normalising the application
-- and elaborating the new expression.

mkSpecialised :: IState -> FC -> Name -> [PTerm] -> PTerm -> ElabD PTerm
mkSpecialised i fc n args def
    = do let tm' = def
         case lookupCtxt Nothing n (idris_statics i) of
           [] -> return tm'
           [as] -> if (not (or as)) then return tm' else
                       mkSpecDecl i n (zip args as) tm'

mkSpecDecl :: IState -> Name -> [(PTerm, Bool)] -> PTerm -> ElabD PTerm
mkSpecDecl i n pargs tm'
    = do t <- goal
         g <- get_guess
         let (f, args) = unApply g
         let sargs = zip args (map snd pargs)
         let staticArgs = map fst (filter (\ (_,x) -> x) sargs)
         let ns = group (sort (concatMap staticFnNames staticArgs))
         let ntimes = map (\xs -> (head xs, length xs - 1)) ns
         if (not (null ns)) then
           do env <- get_env
              let g' = g -- specialise ctxt env ntimes g
              return tm'
--               trace (show t ++ "\n" ++
--                      show ntimes ++ "\n" ++ 
--                      show (delab i g) ++ "\n" ++ show (delab i g')) $ return tm' -- TODO
           else return tm'
  where
    ctxt = tt_ctxt i
    cg = idris_callgraph i

    staticFnNames tm | (P _ f _, as) <- unApply tm
        = if not (isFnName Nothing f ctxt) then [] 
             else case lookupCtxt Nothing f cg of
                    [ns] -> f : f : [] --(ns \\ [f])
                    [] -> [f,f]
                    _ -> []
    staticFnNames _ = []

