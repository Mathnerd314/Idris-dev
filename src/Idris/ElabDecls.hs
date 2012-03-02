{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveFunctor, DeriveDataTypeable #-}

module Idris.ElabDecls where

import Idris.AbsSyntax
import Idris.Error
import Idris.Delaborate
import Idris.Imports
import Idris.ElabTerm
import Idris.Coverage
import Paths_idris

import Core.TT
import Core.Elaborate hiding (Tactic(..))
import Core.Evaluate
import Core.Typecheck
import Core.CaseTree

import Control.Monad
import Control.Monad.State
import Data.Data(Data, Typeable)
import Data.List
import Data.Maybe
import Debug.Trace


recheckC ctxt fc env t 
    = do (tm, ty, cs) <- tclift $ recheck ctxt env t
         addConstraints fc cs
         return (tm, ty)

checkDef fc ns = do ctxt <- getContext
                    mapM (\(n, t) -> do (t', _) <- recheckC ctxt fc [] t
                                        return (n, t')) ns

elabType :: ElabInfo -> SyntaxInfo -> FC -> Name -> PTerm -> Idris ()
elabType info syn fc n ty' = {- let ty' = piBind (params info) ty_in 
                                      n  = liftname info n_in in    -}
      do checkUndefined fc n
         ctxt <- getContext
         i <- get
         ty' <- implicit syn n ty'
         let ty = addImpl i ty'
         logLvl 2 $ show n ++ " type " ++ showImp True ty
         ((ty', defer, is), log) <- tclift $ elaborate ctxt n (Set (UVal 0)) []
                                             (erun fc (build i info False n ty))
         (cty, _)   <- recheckC ctxt fc [] ty'
         logLvl 2 $ "---> " ++ show cty
         let nty = normalise ctxt [] cty
         ds <- checkDef fc ((n, nty):defer)
         addIBC (IBCDef n)
         addDeferred ds
         mapM_ (elabCaseBlock info) is 

elabData :: ElabInfo -> SyntaxInfo -> FC -> PData -> Idris ()
elabData info syn fc (PDatadecl n t_in dcons)
    = do iLOG (show fc)
         checkUndefined fc n
         ctxt <- getContext
         i <- get
         t_in <- implicit syn n t_in
         let t = addImpl i t_in
         ((t', defer, is), log) <- tclift $ elaborate ctxt n (Set (UVal 0)) []
                                            (erun fc (build i info False n t))
         def' <- checkDef fc defer
         addDeferred def'
         mapM_ (elabCaseBlock info) is
         (cty, _)  <- recheckC ctxt fc [] t'
         logLvl 2 $ "---> " ++ show cty
         updateContext (addTyDecl n cty) -- temporary, to check cons
         cons <- mapM (elabCon info syn) dcons
         ttag <- getName
         addIBC (IBCDef n)
         updateContext (addDatatype (Data n ttag cty cons))

elabCon :: ElabInfo -> SyntaxInfo -> (Name, PTerm, FC) -> Idris (Name, Type)
elabCon info syn (n, t_in, fc)
    = do checkUndefined fc n
         ctxt <- getContext
         i <- get
         t_in <- implicit syn n t_in
         let t = addImpl i t_in
         logLvl 2 $ show fc ++ ":Constructor " ++ show n ++ " : " ++ showImp True t
         ((t', defer, is), log) <- tclift $ elaborate ctxt n (Set (UVal 0)) []
                                            (erun fc (build i info False n t))
         logLvl 2 $ "Rechecking " ++ show t'
         def' <- checkDef fc defer
         addDeferred def'
         mapM_ (elabCaseBlock info) is
         ctxt <- getContext
         (cty, _)  <- recheckC ctxt fc [] t'
         logLvl 2 $ "---> " ++ show n ++ " : " ++ show cty
         addIBC (IBCDef n)
         return (n, cty)

elabClauses :: ElabInfo -> FC -> FnOpts -> Name -> [PClause] -> Idris ()
elabClauses info fc opts n_in cs_in = let n = n_in in  
      do let cs = genClauses cs_in
         pats_in <- mapM (elabClause info fc (TCGen `elem` opts)) cs
         solveDeferred n
         let pats = mapMaybe id pats_in
         logLvl 3 (showSep "\n" (map (\ (l,r) -> 
                                        show l ++ " = " ++ 
                                        show r) pats))
         ist <- get
         let tcase = opt_typecase (idris_options ist)
         let pdef = map debind pats
         let tree = simpleCase tcase pdef
         tclift $ sameLength pdef
         logLvl 3 (show tree)
         ctxt <- getContext
         case lookupTy (namespace info) n ctxt of
             [ty] -> do updateContext (addCasedef n (inlinable opts)
                                                     tcase pdef ty)
                        addIBC (IBCDef n)
             [] -> return ()
  where
    debind (x, y) = (depat x, depat y)
    depat (Bind n (PVar t) sc) = depat (instantiate (P Bound n t) sc)
    depat x = x

    sameLength ((x, _) : xs) 
        = do l <- sameLength xs
             let (f, as) = unApply x
             if (null xs || l == length as) then return (length as)
                else tfail (At fc (Msg "Clauses have differing numbers of arguments "))
    sameLength [] = return 0

elabVal :: ElabInfo -> Bool -> PTerm -> Idris (Term, Type)
elabVal info aspat tm_in
   = do ctxt <- getContext
        i <- get
        let tm = addImpl i tm_in
        logLvl 10 (showImp True tm)
        ((tm', defer, is), _) <- tclift $ elaborate ctxt (MN 0 "val") infP []
                                          (build i info aspat (MN 0 "val") (infTerm tm))
        logLvl 3 ("Value: " ++ show tm')
        let vtm = getInferTerm tm'
        logLvl 2 (show vtm)
        recheckC ctxt (FC "prompt" 0) [] vtm

elabClause :: ElabInfo -> FC -> Bool -> PClause -> Idris (Maybe (Term, Term))
elabClause info fc tcgen (PClause fname lhs_in [] PImpossible [])
   = do ctxt <- getContext
        i <- get
        let lhs = addImpl i lhs_in
        -- if the LHS type checks, it is possible, so report an error
        case elaborate ctxt (MN 0 "patLHS") infP []
                            (erun fc (buildTC i info True tcgen fname (infTerm lhs))) of
            OK _ -> fail $ show fc ++ ":" ++ show lhs ++ " is a possible case"
            Error _ -> return ()
        return Nothing
elabClause info fc tcgen (PClause fname lhs_in withs rhs_in whereblock) 
   = do ctxt <- getContext
        -- Build the LHS as an "Infer", and pull out its type and
        -- pattern bindings
        i <- get
        let lhs = addImpl i lhs_in
        logLvl 5 ("LHS: " ++ showImp True lhs)
        ((lhs', dlhs, []), _) <- 
            tclift $ elaborate ctxt (MN 0 "patLHS") infP []
                     (erun fc (buildTC i info True tcgen fname (infTerm lhs)))
        let lhs_tm = orderPats (getInferTerm lhs')
        let lhs_ty = getInferType lhs'
        logLvl 3 (show lhs_tm)
        (clhs, clhsty) <- recheckC ctxt fc [] lhs_tm
        logLvl 5 ("Checked " ++ show clhs)
        -- Elaborate where block
        ist <- getIState
        windex <- getName
        let winfo = pinfo (pvars ist lhs_tm) whereblock windex
        let decls = concatMap declared whereblock
        let newargs = pvars ist lhs_tm
        let wb = map (expandParamsD ist decorate newargs decls) whereblock
        logLvl 5 $ show wb
        mapM_ (elabDecl' info) wb
        -- Now build the RHS, using the type of the LHS as the goal.
        i <- get -- new implicits from where block
        logLvl 5 (showImp True (expandParams decorate newargs decls rhs_in))
        let rhs = addImpl i (expandParams decorate newargs decls rhs_in)
                        -- TODO: but don't do names in scope
        logLvl 2 (showImp True rhs)
        ctxt <- getContext -- new context with where block added
        ((rhs', defer, is), _) <- 
           tclift $ elaborate ctxt (MN 0 "patRHS") clhsty []
                    (do pbinds lhs_tm
                        (_, _, is) <- erun fc (build i info False fname rhs)
                        psolve lhs_tm
                        tt <- get_term
                        let (tm, ds) = runState (collectDeferred tt) []
                        return (tm, ds, is))
        logLvl 2 $ "---> " ++ show rhs'
        when (not (null defer)) $ iLOG $ "DEFERRED " ++ show defer
        def' <- checkDef fc defer
        addDeferred def'
        mapM_ (elabCaseBlock info) is
        ctxt <- getContext
        logLvl 5 $ "Rechecking"
        (crhs, crhsty) <- recheckC ctxt fc [] rhs'
        return $ Just (clhs, crhs)
  where
    decorate x = UN (show fname ++ "#" ++ show x)
    pinfo ns ps i 
          = let ds = concatMap declared ps
                newps = params info ++ ns
                dsParams = map (\n -> (n, map fst newps)) ds
                newb = addAlist dsParams (inblock info) 
                -- l = liftname info
              in
                info { params = newps
                     , inblock = newb
                --   , liftname = id -- (\n -> case lookupCtxt n newb of
                                     --      Nothing -> n
                                     --      _ -> MN i (show n)) . l
                    }

elabClause info fc tcgen (PWith fname lhs_in withs wval_in withblock) 
   = do ctxt <- getContext
        -- Build the LHS as an "Infer", and pull out its type and
        -- pattern bindings
        i <- get
        let lhs = addImpl i lhs_in
        logLvl 5 ("LHS: " ++ showImp True lhs)
        ((lhs', dlhs, []), _) <- tclift $ elaborate ctxt (MN 0 "patLHS") infP []
                                      (erun fc (buildTC i info True tcgen fname (infTerm lhs)))
        let lhs_tm = orderPats (getInferTerm lhs')
        let lhs_ty = getInferType lhs'
        let ret_ty = getRetTy lhs_ty
        logLvl 3 (show lhs_tm)
        (clhs, clhsty) <- recheckC ctxt fc [] lhs_tm
        logLvl 5 ("Checked " ++ show clhs)
        let bargs = getPBtys lhs_tm
        let wval = addImpl i wval_in
        logLvl 5 ("Checking " ++ showImp True wval)
        -- Elaborate wval in this context
        ((wval', defer, is), _) <- 
            tclift $ elaborate ctxt (MN 0 "withRHS") 
                        (bindTyArgs PVTy bargs infP) []
                        (do pbinds lhs_tm
                            -- TODO: may want where here - see winfo abpve
                            (_', d, is) <- erun fc (build i info False fname (infTerm wval))
                            psolve lhs_tm
                            tt <- get_term
                            return (tt, d, is))
        def' <- checkDef fc defer
        addDeferred def'
        mapM_ (elabCaseBlock info) is
        (cwval, cwvalty) <- recheckC ctxt fc [] (getInferTerm wval')
        logLvl 3 ("With type " ++ show cwvalty ++ "\nRet type " ++ show ret_ty)
        windex <- getName
        -- build a type declaration for the new function:
        -- (ps : Xs) -> (withval : cwvalty) -> ret_ty 
        let wtype = bindTyArgs Pi (bargs ++ [(MN 0 "warg", getRetTy cwvalty)]) ret_ty
        logLvl 3 ("New function type " ++ show wtype)
        let wname = MN windex (show fname)
        let imps = getImps wtype -- add to implicits context
        put (i { idris_implicits = addDef wname imps (idris_implicits i) })
        def' <- checkDef fc [(wname, wtype)]
        addDeferred def'

        -- in the subdecls, lhs becomes:
        --         fname  pats | wpat [rest]
        --    ==>  fname' ps   wpat [rest], match pats against toplevel for ps
        wb <- mapM (mkAuxC wname lhs (map fst bargs)) withblock
        logLvl 5 ("with block " ++ show wb)
        mapM_ (elabDecl info) wb

        -- rhs becomes: fname' ps wval
        let rhs = PApp fc (PRef fc wname) (map (pexp . (PRef fc) . fst) bargs ++ 
                                                [pexp wval])
        logLvl 3 ("New RHS " ++ show rhs)
        ctxt <- getContext -- New context with block added
        i <- get
        ((rhs', defer, is), _) <-
           tclift $ elaborate ctxt (MN 0 "wpatRHS") clhsty []
                    (do pbinds lhs_tm
                        (_, d, is) <- erun fc (build i info False fname rhs)
                        psolve lhs_tm
                        tt <- get_term
                        return (tt, d, is))
        def' <- checkDef fc defer
        addDeferred def'
        mapM_ (elabCaseBlock info) is
        (crhs, crhsty) <- recheckC ctxt fc [] rhs'
        return $ Just (clhs, crhs)
  where
    getImps (Bind n (Pi _) t) = pexp Placeholder : getImps t
    getImps _ = []

    mkAuxC wname lhs ns (PClauses fc o n cs)
        | True  = do cs' <- mapM (mkAux wname lhs ns) cs
                     return $ PClauses fc o wname cs'
        | otherwise = fail $ "with clause uses wrong function name " ++ show n
    mkAuxC wname lhs ns d = return $ d

    mkAux wname toplhs ns (PClause n tm_in (w:ws) rhs wheres)
        = do i <- get
             let tm = addImpl i tm_in
             logLvl 2 ("Matching " ++ showImp True tm ++ " against " ++ 
                                      showImp True toplhs)
             case matchClause toplhs tm of
                Nothing -> fail "with clause does not match top level"
                Just mvars -> do logLvl 3 ("Match vars : " ++ show mvars)
                                 lhs <- updateLHS n wname mvars ns (fullApp tm) w
                                 return $ PClause wname lhs ws rhs wheres
    mkAux wname toplhs ns (PWith n tm_in (w:ws) wval withs)
        = do i <- get
             let tm = addImpl i tm_in
             logLvl 2 ("Matching " ++ showImp True tm ++ " against " ++ 
                                      showImp True toplhs)
             withs' <- mapM (mkAuxC wname toplhs ns) withs
             case matchClause toplhs tm of
                Nothing -> fail "with clause does not match top level"
                Just mvars -> do lhs <- updateLHS n wname mvars ns (fullApp tm) w
                                 return $ PWith wname lhs ws wval withs'
        
    updateLHS n wname mvars ns (PApp fc (PRef fc' n') args) w
        = return $ substMatches mvars $ 
                PApp fc (PRef fc' wname) (map (pexp . (PRef fc')) ns ++ [pexp w])
    updateLHS n wname mvars ns tm w = fail $ "Not implemented match " ++ show tm 

    fullApp (PApp _ (PApp fc f args) xs) = fullApp (PApp fc f (args ++ xs))
    fullApp x = x

data MArgTy = IA | EA | CA deriving (Show, Eq, Data, Typeable)

elabClass :: ElabInfo -> SyntaxInfo -> 
             FC -> [PTerm] -> 
             Name -> [(Name, PTerm)] -> [PDecl] -> Idris ()
elabClass info syn fc constraints tn ps ds 
    = do let cn = UN ("instance" ++ show tn) -- MN 0 ("instance" ++ show tn)
         let tty = pibind ps PSet
         let constraint = PApp fc (PRef fc tn)
                                  (map (pexp . PRef fc) (map fst ps))
         -- build data declaration
         ims <- mapM tdecl (filter tydecl ds)
         defs <- mapM (defdecl (map (\ (x,y,z) -> z) ims) constraint) 
                      (filter clause ds)
         let (methods, imethods) = unzip (map (\ (x,y,z) -> (x, y)) ims)
         let cty = impbind ps $ conbind constraints $ pibind methods constraint
         let cons = [(cn, cty, fc)]
         let ddecl = PData syn fc (PDatadecl tn tty cons)
         elabDecl info ddecl
         -- for each constraint, build a top level function to chase it
         let usyn = syn { using = ps ++ using syn }
         fns <- mapM (cfun cn constraint usyn (map fst imethods)) constraints
         mapM_ (elabDecl info) (concat fns)
         -- for each method, build a top level function
         fns <- mapM (tfun cn constraint usyn (map fst imethods)) imethods
         mapM_ (elabDecl info) (concat fns)
         -- add the default definitions
         mapM_ (elabDecl info) (concat (map (snd.snd) defs))
         i <- get
         let defaults = map (\ (x, (y, z)) -> (x,y)) defs
         put (i { idris_classes = addDef tn (CI cn imethods defaults (map fst ps)) 
                                            (idris_classes i) })
         addIBC (IBCClass tn)
  where
    pibind [] x = x
    pibind ((n, ty): ns) x = PPi expl n ty (pibind ns x) 
    impbind [] x = x
    impbind ((n, ty): ns) x = PPi impl n ty (impbind ns x) 
    conbind (ty : ns) x = PPi constraint (MN 0 "c") ty (conbind ns x)
    conbind [] x = x

    tdecl (PTy syn _ n t) = do t' <- implicit syn n t
                               return ( (n, (toExp (map fst ps) Exp t')),
                                        (n, (toExp (map fst ps) Imp t')),
                                        (n, (syn, t) ) )
    tdecl _ = fail "Not allowed in a class declaration"

    -- Create default definitions 
    defdecl mtys c d@(PClauses fc opts n cs) =
        case lookup n mtys of
            Just (syn, ty) -> do let ty' = insertConstraint c ty
                                 let ds = map (decorateid defaultdec)
                                              [PTy syn fc n ty', 
                                               PClauses fc (TCGen:opts) n cs]
                                 iLOG (show ds)
                                 return (n, (defaultdec n, ds))
            _ -> fail $ show n ++ " is not a method"
    defdecl _ _ _ = fail "Can't happen (defdecl)"

    defaultdec (UN n) = UN ("default#" ++ n)
    defaultdec (NS n ns) = NS (defaultdec n) ns

    tydecl (PTy _ _ _ _) = True
    tydecl _ = False
    clause (PClauses _ _ _ _) = True
    clause _ = False

    cfun cn c syn all con
        = do let cfn = UN ('@':show cn ++ "#" ++ show con)
             let mnames = take (length all) $ map (\x -> MN x "meth") [0..]
             let capp = PApp fc (PRef fc cn) (map (pexp . PRef fc) mnames)
             let lhs = PApp fc (PRef fc cfn) [pconst capp]
             let rhs = PResolveTC (FC "HACK" 0)
             let ty = PPi constraint (MN 0 "pc") c con
             iLOG (showImp True ty)
             iLOG (showImp True lhs ++ " = " ++ showImp True rhs)
             return [PTy syn fc cfn ty,
                     PClauses fc [Inlinable,TCGen] cfn [PClause cfn lhs [] rhs []]]

    tfun cn c syn all (m, ty) 
        = do let ty' = insertConstraint c ty
             let mnames = take (length all) $ map (\x -> MN x "meth") [0..]
             let capp = PApp fc (PRef fc cn) (map (pexp . PRef fc) mnames)
             let margs = getMArgs ty
             let anames = map (\x -> MN x "arg") [0..]
             let lhs = PApp fc (PRef fc m) (pconst capp : lhsArgs margs anames)
             let rhs = PApp fc (getMeth mnames all m) (rhsArgs margs anames)
             iLOG (showImp True ty)
             iLOG (show (m, ty', capp, margs))
             iLOG (showImp True lhs ++ " = " ++ showImp True rhs)
             return [PTy syn fc m ty',
                     PClauses fc [Inlinable,TCGen] m [PClause m lhs [] rhs []]]

    getMArgs (PPi (Imp _ _) n ty sc) = IA : getMArgs sc
    getMArgs (PPi (Exp _ _) n ty sc) = EA  : getMArgs sc
    getMArgs (PPi (Constraint _ _) n ty sc) = CA : getMArgs sc
    getMArgs _ = []

    getMeth (m:ms) (a:as) x | x == a = PRef fc m
                            | otherwise = getMeth ms as x

    lhsArgs (EA : xs) (n : ns) = pexp (PRef fc n) : lhsArgs xs ns 
    lhsArgs (IA : xs) ns = lhsArgs xs ns 
    lhsArgs (CA : xs) ns = lhsArgs xs ns
    lhsArgs [] _ = []

    rhsArgs (EA : xs) (n : ns) = pexp (PRef fc n) : rhsArgs xs ns 
    rhsArgs (IA : xs) ns = pexp Placeholder : rhsArgs xs ns 
    rhsArgs (CA : xs) ns = pconst (PResolveTC fc) : rhsArgs xs ns
    rhsArgs [] _ = []

    insertConstraint c (PPi p@(Imp _ _) n ty sc)
                          = PPi p n ty (insertConstraint c sc)
    insertConstraint c sc = PPi constraint (MN 0 "c") c sc

    -- make arguments explicit and don't bind class parameters
    toExp ns e (PPi (Imp l s) n ty sc)
        | n `elem` ns = toExp ns e sc
        | otherwise = PPi (e l s) n ty (toExp ns e sc)
    toExp ns e (PPi p n ty sc) = PPi p n ty (toExp ns e sc)
    toExp ns e sc = sc

elabInstance :: ElabInfo -> SyntaxInfo -> 
                FC -> [PTerm] -> Name -> 
                [PTerm] -> PTerm -> [PDecl] -> Idris ()
elabInstance info syn fc cs n ps t ds
    = do i <- get 
         (n, ci) <- case lookupCtxtName (namespace info) n (idris_classes i) of
                       [c] -> return c
                       _ -> fail $ show n ++ " is not a type class"
         let iname = UN ('@':show n ++ "$" ++ show ps)
         elabType info syn fc iname t
         let ips = zip (class_params ci) ps
         let ns = case n of
                    NS n ns' -> ns'
                    _ -> []
         let mtys = map (\ (n, t) -> let t' = substMatches ips t in
                                         (decorate ns n, coninsert cs t', t'))
                        (class_methods ci)
         logLvl 3 (show (mtys, ips))
         let ds' = insertDefaults (class_defaults ci) ns ds
         iLOG ("Defaults inserted: " ++ show ds' ++ "\n" ++ show ci)
         mapM_ (warnMissing ds' ns) (map fst (class_methods ci))
         let wb = map mkTyDecl mtys ++ map (decorateid (decorate ns)) ds'
         let lhs = PRef fc iname
         let rhs = PApp fc (PRef fc (instanceName ci))
                           (map (pexp . mkMethApp) mtys)
         let idecl = PClauses fc [Inlinable, TCGen] iname 
                                 [PClause iname lhs [] rhs wb]
         iLOG (show idecl)
         elabDecl info idecl
  where
    mkMethApp (n, _, ty) = lamBind 0 ty (papp fc (PRef fc n) (methArgs 0 ty))
    lamBind i (PPi (Constraint _ _) _ _ sc) sc' 
                                  = PLam (MN i "meth") Placeholder (lamBind (i+1) sc sc')
    lamBind i (PPi _ n ty sc) sc' = PLam (MN i "meth") Placeholder (lamBind (i+1) sc sc')
    lamBind i _ sc = sc
    methArgs i (PPi (Imp _ _) n ty sc) 
        = PImp 0 False n (PRef fc (MN i "meth")) : methArgs (i+1) sc
    methArgs i (PPi (Exp _ _) n ty sc) 
        = PExp 0 False (PRef fc (MN i "meth")) : methArgs (i+1) sc
    methArgs i (PPi (Constraint _ _) n ty sc) 
        = PConstraint 0 False (PResolveTC fc) : methArgs (i+1) sc
    methArgs i _ = []

    papp fc f [] = f
    papp fc f as = PApp fc f as

    decorate ns (UN n) = NS (UN ('!':n)) ns
    decorate ns (NS (UN n) s) = NS (UN ('!':n)) ns

    mkTyDecl (n, t, _) = PTy syn fc n t

    conbind (ty : ns) x = PPi constraint (MN 0 "c") ty (conbind ns x)
    conbind [] x = x

    coninsert cs (PPi p@(Imp _ _) n t sc) = PPi p n t (coninsert cs sc)
    coninsert cs sc = conbind cs sc

    insertDefaults :: [(Name, Name)] -> [String] -> [PDecl] -> [PDecl]
    insertDefaults [] ns ds = ds
    insertDefaults ((n,dn) : defs) ns ds 
       = insertDefaults defs ns (insertDef n dn ns ds)

    insertDef meth def ns decls
        | null $ filter (clauseFor meth ns) decls
            = decls ++ [PClauses fc [Inlinable,TCGen] meth 
                        [PClause meth (PApp fc (PRef fc meth) []) [] 
                                      (PApp fc (PRef fc def) []) []]]
        | otherwise = decls

    warnMissing decls ns meth
        | null $ filter (clauseFor meth ns) decls
            = iWarn fc $ "method " ++ show meth ++ " not defined"
        | otherwise = return ()

    clauseFor m ns (PClauses _ _ m' _) = decorate ns m == decorate ns m'
    clauseFor m ns _ = False

decorateid decorate (PTy s f n t) = PTy s f (decorate n) t
decorateid decorate (PClauses f o n cs) 
   = PClauses f o (decorate n) (map dc cs)
    where dc (PClause n t as w ds) = PClause (decorate n) (dappname t) as w ds
          dc (PWith   n t as w ds) = PWith   (decorate n) (dappname t) as w 
                                              (map (decorateid decorate) ds)
          dappname (PApp fc (PRef fc' n) as) = PApp fc (PRef fc' (decorate n)) as
          dappname t = t

pbinds (Bind n (PVar t) sc) = do attack; patbind n 
                                 pbinds sc
pbinds tm = return ()

pbty (Bind n (PVar t) sc) tm = Bind n (PVTy t) (pbty sc tm)
pbty _ tm = tm

getPBtys (Bind n (PVar t) sc) = (n, t) : getPBtys sc
getPBtys _ = []

getRetTy (Bind n (PVar _) sc) = getRetTy sc
getRetTy (Bind n (PVTy _) sc) = getRetTy sc
getRetTy (Bind n (Pi _) sc)   = getRetTy sc
getRetTy sc = sc

psolve (Bind n (PVar t) sc) = do solve; psolve sc
psolve tm = return ()

pvars ist (Bind n (PVar t) sc) = (n, delab ist t) : pvars ist sc
pvars ist _ = []

-- TODO: Also build a 'binary' version of each declaration for fast reloading

elabDecl :: ElabInfo -> PDecl -> Idris ()
elabDecl info d = idrisCatch (elabDecl' info d) 
                             (\e -> do let msg = report e
                                       setErrLine (getErrLine msg)
                                       iputStrLn msg)

elabDecl' info (PFix _ _ _)      = return () -- nothing to elaborate
elabDecl' info (PSyntax _ p) = return () -- nothing to elaborate
elabDecl' info (PTy s f n ty)    = do iLOG $ "Elaborating type decl " ++ show n
                                      elabType info s f n ty
elabDecl' info (PData s f d)     = do iLOG $ "Elaborating " ++ show (d_name d)
                                      elabData info s f d
elabDecl' info d@(PClauses f o n ps) = do iLOG $ "Elaborating clause " ++ show n
                                          elabClauses info f o n ps
elabDecl' info (PParams f ns ps) = mapM_ (elabDecl' pinfo) ps
  where
    pinfo = let ds = concatMap declared ps
                newps = params info ++ ns
                dsParams = map (\n -> (n, map fst newps)) ds
                newb = addAlist dsParams (inblock info) in 
                info { params = newps,
                       inblock = newb }
elabDecl' info (PNamespace n ps) = mapM_ (elabDecl' ninfo) ps
  where
    ninfo = case namespace info of
                Nothing -> info { namespace = Just [n] }
                Just ns -> info { namespace = Just (n:ns) } 
elabDecl' info (PClass s f cs n ps ds) = do iLOG $ "Elaborating class " ++ show n
                                            elabClass info s f cs n ps ds
elabDecl' info (PInstance s f cs n ps t ds) 
    = do iLOG $ "Elaborating instance " ++ show n
         elabInstance info s f cs n ps t ds
elabDecl' info (PDirective i) = case i of
  Lib lib -> do addLib lib
                addIBC (IBCLib lib)
  Link obj -> do datadir <- liftIO $ getDataDir
                 o <- liftIO $ findInPath [".", datadir] obj
                 addIBC (IBCObj o)
                 addObjectFile o
  Include hdr -> do addHdr hdr
                    addIBC (IBCHeader hdr)
  Hide n -> do setAccessibility n Hidden
               addIBC (IBCAccess n Hidden)
  Freeze n -> do setAccessibility n Frozen
                 addIBC (IBCAccess n Frozen)
  Access acc -> do i <- getIState
                   putIState (i { default_access = acc })
  Logging i -> setLogLevel (fromInteger i)
  

elabCaseBlock info d@(PClauses f o n ps) 
        = elabDecl' info d 

-- elabDecl' info (PImport i) = loadModule i


