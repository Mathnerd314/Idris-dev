{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveFunctor, DeriveDataTypeable #-}

module Core.TT where

import Control.Monad.State
import Debug.Trace
import qualified Data.Map as Map
import Data.Char
import Data.List
import qualified Data.Binary as B
import Data.Binary hiding (get, put)
import Data.Data(Data, Typeable)

{- The language has:
   * Full dependent types
   * A hierarchy of universes, with cumulativity: Set : Set1, Set1 : Set2, ...
   * Pattern matching letrec binding
   * (primitive types defined externally)

   Some technical stuff:
   * Typechecker is kept as simple as possible 
        - no unification, just a checker for incomplete terms.
   * We have a simple collection of tactics which we use to elaborate source
     programs with implicit syntax into fully explicit terms.
-}

data Option = SetInSet
            | CheckConv
  deriving (Eq, Data, Typeable)

data FC = FC { fc_fname :: String,
               fc_line :: Int }
    deriving (Eq, Data, Typeable)
{-! 
deriving instance Binary FC 
!-}

instance Show FC where
    show (FC f l) = f ++ ":" ++ show l

data Err = Msg String
         | CantUnify Term Term Err
         | NotInjective Term Term Term
         | IncompleteTerm Term
         | UniverseError
         | ProgramLineComment
         | At FC Err
  deriving (Eq, Data, Typeable)

instance Show Err where
    show (Msg s) = s

data TC a = OK a
          | Error Err
  deriving (Eq, Functor, Data, Typeable)

instance Show a => Show (TC a) where
    show (OK x) = show x
    show (Error str) = "Error: " ++ show str

-- at some point, this instance should also carry type checking options
-- (e.g. Set:Set)

instance Monad TC where
    return = OK 
    x >>= k = case x of 
                OK v -> k v
                Error e -> Error e
    fail e = Error (Msg e)

tfail :: Err -> TC a
tfail e = Error e

trun :: FC -> TC a -> TC a
trun fc (OK a)    = OK a
trun fc (Error e) = Error (At fc e) 

instance MonadPlus TC where
    mzero = fail "Unknown error"
    (OK x) `mplus` _ = OK x
    _ `mplus` (OK y) = OK y
    err `mplus` _    = err

discard :: Monad m => m a -> m ()
discard f = f >> return ()

showSep :: String -> [String] -> String
showSep sep [] = ""
showSep sep [x] = x
showSep sep (x:xs) = x ++ sep ++ showSep sep xs

pmap f (x, y) = (f x, f y)

traceWhen True msg a = trace msg a
traceWhen False _  a = a

-- RAW TERMS ----------------------------------------------------------------

-- Names are hierarchies of strings, describing scope (so no danger of
-- duplicate names, but need to be careful on lookup).
-- Also MN for machine chosen names

data Name = UN String
          | NS Name [String] -- root, namespaces 
          | MN Int String
  deriving (Eq, Ord, Data, Typeable)
{-! 
deriving instance Binary Name 
!-}

instance Show Name where
    show (UN n) = n
    show (NS n s) = showSep "." (reverse s) ++ "." ++ show n
    show (MN i s) = "{" ++ s ++ show i ++ "}"


-- Contexts allow us to map names to things. A root name maps to a collection
-- of things in different namespaces with that name.

type Ctxt a = Map.Map Name (Map.Map Name a)
emptyContext = Map.empty

nsroot (NS n _) = n
nsroot n = n

addDef :: Name -> a -> Ctxt a -> Ctxt a
addDef n v ctxt = case Map.lookup (nsroot n) ctxt of
                        Nothing -> Map.insert (nsroot n) 
                                        (Map.insert n v Map.empty) ctxt
                        Just xs -> Map.insert (nsroot n) 
                                        (Map.insert n v xs) ctxt

{- lookup a name in the context, given an optional namespace.
   The name (n) may itself have a (partial) namespace given.

   Rules for resolution:
    - if an explicit namespace is given, return the names which match it. If none
      match, return all names.
    - if the name has has explicit namespace given, return the names which match it
      and ignore the given namespace.
    - otherwise, return all names.

-}

lookupCtxtName :: Maybe [String] -> Name -> Ctxt a -> [(Name, a)]
lookupCtxtName nspace n ctxt = case Map.lookup (nsroot n) ctxt of
                                  Just xs -> filterNS (Map.toList xs)
                                  Nothing -> []
  where
    filterNS [] = []
    filterNS ((found, v) : xs) 
        | nsmatch n found = (found, v) : filterNS xs
        | otherwise       = filterNS xs

    nsmatch (NS n ns) (NS p ps) = ns `isPrefixOf` ps
    nsmatch (NS _ _)  _         = False
    nsmatch looking   found     = True

lookupCtxt :: Maybe [String] -> Name -> Ctxt a -> [a]
lookupCtxt ns n ctxt = map snd (lookupCtxtName ns n ctxt)

updateDef :: Name -> (a -> a) -> Ctxt a -> Ctxt a
updateDef n f ctxt 
  = let ds = lookupCtxtName Nothing n ctxt in
        foldr (\ (n, t) c -> addDef n (f t) c) ctxt ds  

toAlist :: Ctxt a -> [(Name, a)]
toAlist ctxt = let allns = map snd (Map.toList ctxt) in
                concat (map (Map.toList) allns)

addAlist :: Show a => [(Name, a)] -> Ctxt a -> Ctxt a
addAlist [] ctxt = ctxt
addAlist ((n, tm) : ds) ctxt = addDef n tm (addAlist ds ctxt)

data Const = I Int | BI Integer | Fl Double | Ch Char | Str String 
           | IType | BIType     | FlType    | ChType  | StrType    
           | PtrType
  deriving (Eq, Data, Typeable)
{-! 
deriving instance Binary Const 
!-}

data Raw = Var Name
         | RBind Name (Binder Raw) Raw
         | RApp Raw Raw
         | RSet
         | RConstant Const
  deriving (Show, Eq, Data, Typeable)
{-! 
deriving instance Binary Raw 
!-}

data Binder b = Lam   { binderTy  :: b }
              | Pi    { binderTy  :: b }
              | Let   { binderTy  :: b,
                        binderVal :: b }
              | NLet  { binderTy  :: b,
                        binderVal :: b }
              | Hole  { binderTy  :: b}
              | GHole { binderTy  :: b}
              | Guess { binderTy  :: b,
                        binderVal :: b }
              | PVar  { binderTy  :: b }
              | PVTy  { binderTy  :: b }
  deriving (Show, Eq, Functor, Data, Typeable)
{-! 
deriving instance Binary Binder 
!-}

fmapMB :: Monad m => (a -> m b) -> Binder a -> m (Binder b)
fmapMB f (Let t v)   = liftM2 Let (f t) (f v)
fmapMB f (NLet t v)  = liftM2 NLet (f t) (f v)
fmapMB f (Guess t v) = liftM2 Guess (f t) (f v)
fmapMB f (Lam t)     = liftM Lam (f t)
fmapMB f (Pi t)      = liftM Pi (f t)
fmapMB f (Hole t)    = liftM Hole (f t)
fmapMB f (GHole t)   = liftM GHole (f t)
fmapMB f (PVar t)    = liftM PVar (f t)
fmapMB f (PVTy t)    = liftM PVTy (f t)

raw_apply :: Raw -> [Raw] -> Raw
raw_apply f [] = f
raw_apply f (a : as) = raw_apply (RApp f a) as

data RawFun = RawFun { rtype :: Raw,
                       rval  :: Raw
                     }
  deriving (Show, Data, Typeable)

data RawDatatype = RDatatype Name Raw [(Name, Raw)]
  deriving (Show, Data, Typeable)

data RDef = RFunction RawFun
          | RConst Raw
          | RData RawDatatype
  deriving (Show, Data, Typeable)

type RProgram = [(Name, RDef)]

-- WELL TYPED TERMS ---------------------------------------------------------

data UExp = UVar Int -- universe variable
          | UVal Int -- explicit universe level
  deriving (Eq, Ord, Data, Typeable)

-- We assume that universe levels have been checked, so anything external
-- can just have the same universe variable and we won't get any new
-- cycles.

instance Binary UExp where
    put x = return ()
    get = return (UVar (-1))

instance Show UExp where
    show (UVar x) | x < 26 = [toEnum (x + fromEnum 'a')]
                  | otherwise = toEnum ((x `mod` 26) + fromEnum 'a') : show (x `div` 26)
    show (UVal x) = show x
--     show (UMax l r) = "max(" ++ show l ++ ", " ++ show r ++")"

data UConstraint = ULT UExp UExp
                 | ULE UExp UExp
  deriving (Eq, Data, Typeable)

instance Show UConstraint where
    show (ULT x y) = show x ++ " < " ++ show y
    show (ULE x y) = show x ++ " <= " ++ show y

type UCs = (Int, [UConstraint])

data NameType = Bound | Ref | DCon Int Int | TCon Int Int
  deriving (Show, Eq, Data, Typeable)
{-! 
deriving instance Binary NameType 
!-}

data TT n = P NameType n (TT n) -- embed type
          | V Int 
          | Bind n (Binder (TT n)) (TT n)
          | App (TT n) (TT n) -- function, function type, arg
          | Constant Const
          | Set UExp
  deriving (Functor, Data, Typeable)
{-! 
deriving instance Binary TT 
!-}

type EnvTT n = [(n, Binder (TT n))]

data Datatype n = Data { d_typename :: n,
                         d_typetag  :: Int,
                         d_type     :: (TT n),
                         d_cons     :: [(n, TT n)] }
  deriving (Show, Functor, Eq, Data, Typeable)

instance Eq n => Eq (TT n) where
    (==) (P xt x _)     (P yt y _)     = xt == yt && x == y
    (==) (V x)          (V y)          = x == y
    (==) (Bind _ xb xs) (Bind _ yb ys) = xb == yb && xs == ys
    (==) (App fx ax)    (App fy ay)    = fx == fy && ax == ay
    (==) (Set _)        (Set _)        = True -- deal with constraints later
    (==) (Constant x)   (Constant y)   = x == y
    (==) _              _              = False

convEq :: Eq n => TT n -> TT n -> StateT UCs TC Bool
convEq (P xt x _) (P yt y _) = return (xt == yt && x == y)
convEq (V x)      (V y)      = return (x == y)
convEq (Bind _ xb xs) (Bind _ yb ys) 
                             = liftM2 (&&) (convEqB xb yb) (convEq xs ys)
  where convEqB (Let v t) (Let v' t') = liftM2 (&&) (convEq v v') (convEq t t')
        convEqB (Guess v t) (Guess v' t') = liftM2 (&&) (convEq v v') (convEq t t')
        convEqB b b' = convEq (binderTy b) (binderTy b')
convEq (App fx ax) (App fy ay)   = liftM2 (&&) (convEq fx fy) (convEq ax ay)
convEq (Constant x) (Constant y) = return (x == y)
convEq (Set x) (Set y)           = do (v, cs) <- get
                                      put (v, ULE x y : cs)
                                      return True
convEq _ _ = return False

-- A few handy operations on well typed terms:

isInjective :: TT n -> Bool
isInjective (P (DCon _ _) _ _) = True
isInjective (P (TCon _ _) _ _) = True
isInjective (Constant _)       = True
isInjective (Set x)            = True
isInjective (Bind _ (Pi _) sc) = True
isInjective (App f a)          = isInjective f
isInjective _                  = False

instantiate :: TT n -> TT n -> TT n
instantiate e = subst 0 where
    subst i (V x) | i == x = e
    subst i (Bind x b sc) = Bind x (fmap (subst i) b) (subst (i+1) sc)
    subst i (App f a) = App (subst i f) (subst i a)
    subst i t = t

pToV :: Eq n => n -> TT n -> TT n
pToV n = pToV' n 0
pToV' n i (P _ x _) | n == x = V i
pToV' n i (Bind x b sc)
                | n == x    = Bind x (fmap (pToV' n i) b) sc
                | otherwise = Bind x (fmap (pToV' n i) b) (pToV' n (i+1) sc)
pToV' n i (App f a) = App (pToV' n i f) (pToV' n i a)
pToV' n i t = t

-- Convert several names. First in the list comes out as V 0
pToVs :: Eq n => [n] -> TT n -> TT n
pToVs ns tm = pToVs' ns tm 0 where
    pToVs' []     tm i = tm
    pToVs' (n:ns) tm i = pToV' n i (pToVs' ns tm (i+1))

vToP :: TT n -> TT n
vToP = vToP' [] where
    vToP' env (V i) = let (n, b) = (env !! i) in
                          P Bound n (binderTy b)
    vToP' env (Bind n b sc) = let b' = fmap (vToP' env) b in
                                  Bind n b' (vToP' ((n, b'):env) sc)
    vToP' env (App f a) = App (vToP' env f) (vToP' env a)
    vToP' env t = t

finalise :: Eq n => TT n -> TT n
finalise (Bind x b sc) = Bind x (fmap finalise b) (pToV x (finalise sc))
finalise (App f a) = App (finalise f) (finalise a)
finalise t = t

subst :: Eq n => n -> TT n -> TT n -> TT n
subst n v tm = instantiate v (pToV n tm)

substNames :: Eq n => [(n, TT n)] -> TT n -> TT n
substNames []             t = t
substNames ((n, tm) : xs) t = subst n tm (substNames xs t)

-- Returns true if V 0 and bound name n do not occur in the term

noOccurrence :: Eq n => n -> TT n -> Bool
noOccurrence n t = no' 0 t
  where
    no' i (V x) = not (i == x)
    no' i (P Bound x _) = not (n == x)
    no' i (Bind n b sc) = noB' i b && no' (i+1) sc
       where noB' i (Let t v) = no' i t && no' i v
             noB' i (Guess t v) = no' i t && no' i v
             noB' i b = no' i (binderTy b)
    no' i (App f a) = no' i f && no' i a
    no' i _ = True

-- Return the arity of a (normalised) type

arity :: TT n -> Int
arity (Bind n (Pi t) sc) = 1 + arity sc
arity _ = 0

-- deconstruct an application; returns the function and a list of arguments

unApply :: TT n -> (TT n, [TT n])
unApply t = ua [] t where
    ua args (App f a) = ua (a:args) f
    ua args t         = (t, args)

mkApp :: TT n -> [TT n] -> TT n
mkApp f [] = f
mkApp f (a:as) = mkApp (App f a) as

forget :: TT Name -> Raw
forget tm = fe [] tm
  where
    fe env (P _ n _) = Var n
    fe env (V i)     = Var (env !! i)
    fe env (Bind n b sc) = RBind n (fmap (fe env) b) 
                                   (fe (n:env) sc)
    fe env (App f a) = RApp (fe env f) (fe env a)
    fe env (Constant c) 
                     = RConstant c
    fe env (Set i)   = RSet

bindAll :: [(n, Binder (TT n))] -> TT n -> TT n 
bindAll [] t =t
bindAll ((n, b) : bs) t = Bind n b (bindAll bs t)

bindTyArgs :: (TT n -> Binder (TT n)) -> [(n, TT n)] -> TT n -> TT n
bindTyArgs b xs = bindAll (map (\ (n, ty) -> (n, b ty)) xs)

getArgTys :: TT n -> [(n, TT n)]
getArgTys (Bind n (Pi t) sc) = (n, t) : getArgTys sc
getArgTys _ = []

uniqueName :: Name -> [Name] -> Name
uniqueName n hs | n `elem` hs = uniqueName (nextName n) hs
                | otherwise   = n

nextName (NS x s)    = NS (nextName x) s
nextName (MN i n)    = MN (i+1) n
nextName (UN x) = let (num', nm') = span isDigit (reverse x)
                      nm = reverse nm'
                      num = readN (reverse num') in
                          UN (nm ++ show (num+1))
  where
    readN "" = 0
    readN x  = read x

type Term = TT Name
type Type = Term

type Env  = EnvTT Name

-- an environment with de Bruijn indices 'normalised' so that they all refer to
-- this environment

newtype WkEnvTT n = Wk (EnvTT n)
type WkEnv = WkEnvTT Name

instance (Eq n, Show n) => Show (TT n) where
    show t = showEnv [] t

instance Show Const where
    show (I i) = show i
    show (BI i) = show i ++ "L"
    show (Fl f) = show f
    show (Ch c) = show c
    show (Str s) = show s
    show IType = "Int"
    show BIType = "Integer"
    show FlType = "Float"
    show ChType = "Char"
    show StrType = "String"
    show PtrType = "Ptr"

showEnv env t = showEnv' env t False
showEnvDbg env t = showEnv' env t True

showEnv' env t dbg = se 10 env t where
    se p env (P nt n t) = show n 
                            ++ if dbg then "{" ++ show nt ++ " : " ++ se 10 env t ++ "}" else ""
    se p env (V i) | i < length env = (show $ fst $ env!!i) ++
                                      if dbg then "{" ++ show i ++ "}" else ""
                   | otherwise = "!!V " ++ show i ++ "!!"
    se p env (Bind n b@(Pi t) sc)  
        | noOccurrence n sc && not dbg = bracket p 2 $ se 1 env t ++ " -> " ++ se 10 ((n,b):env) sc
    se p env (Bind n b sc) = bracket p 2 $ sb env n b ++ se 10 ((n,b):env) sc
    se p env (App f a) = bracket p 1 $ se 1 env f ++ " " ++ se 0 env a
    se p env (Constant c) = show c
    se p env (Set i) = "Set " ++ show i

    sb env n (Lam t)  = showb env "\\ " " => " n t
    sb env n (Hole t) = showb env "? " ". " n t
    sb env n (GHole t) = showb env "?defer " ". " n t
    sb env n (Pi t)   = showb env "(" ") -> " n t
    sb env n (PVar t) = showb env "pat " ". " n t
    sb env n (PVTy t) = showb env "pty " ". " n t
    sb env n (Let t v)   = showbv env "let " " in " n t v
    sb env n (Guess t v) = showbv env "?? " " in " n t v

    showb env op sc n t    = op ++ show n ++ " : " ++ se 10 env t ++ sc
    showbv env op sc n t v = op ++ show n ++ " : " ++ se 10 env t ++ " = " ++ 
                             se 10 env v ++ sc 

    bracket outer inner str | inner > outer = "(" ++ str ++ ")"
                            | otherwise = str

-- Check whether a term has any holes in it - impure if so

pureTerm :: TT n -> Bool
pureTerm (App f a) = pureTerm f && pureTerm a
pureTerm (Bind n b sc) = pureBinder b && pureTerm sc where
    pureBinder (Hole _) = False
    pureBinder (Guess _ _) = False
    pureBinder (Let t v) = pureTerm t && pureTerm v
    pureBinder t = pureTerm (binderTy t)
pureTerm _ = True

-- weaken a term by adding i to each de Bruijn index (i.e. lift it over i bindings)

weakenTm :: Int -> TT n -> TT n
weakenTm i t = wk i 0 t
  where wk i min (V x) | x >= min = V (i + x)
        wk i m (App f a)     = App (wk i m f) (wk i m a)
        wk i m (Bind x b sc) = Bind x (wkb i m b) (wk i (m + 1) sc)
        wk i m t = t
        wkb i m t           = fmap (wk i m) t

-- weaken an environment so that all the de Bruijn indices are correct according
-- to the latest bound variable

weakenEnv :: EnvTT n -> EnvTT n
weakenEnv env = wk (length env - 1) env
  where wk i [] = []
        wk i ((n, b) : bs) = (n, weakenTmB i b) : wk (i - 1) bs
        weakenTmB i (Let   t v) = Let (weakenTm i t) (weakenTm i v)
        weakenTmB i (Guess t v) = Guess (weakenTm i t) (weakenTm i v)
        weakenTmB i t           = t { binderTy = weakenTm i (binderTy t) }

weakenTmEnv :: Int -> EnvTT n -> EnvTT n
weakenTmEnv i = map (\ (n, b) -> (n, fmap (weakenTm i) b))

orderPats :: Term -> Term
orderPats tm = op [] tm
  where
    op ps (Bind n (PVar t) sc) = op ((n, t) : ps) sc
    op ps sc = bindAll (map (\ (n, t) -> (n, PVar t)) (sortP ps)) sc 

    sortP ps = pick [] (reverse ps)

    namesIn (P _ n _) = [n]
    namesIn (Bind n b t) = nub $ nb b ++ (namesIn t \\ [n])
      where nb (Let   t v) = nub (namesIn t) ++ nub (namesIn v)
            nb (Guess t v) = nub (namesIn t) ++ nub (namesIn v)
            nb t = namesIn (binderTy t)
    namesIn (App f a) = nub (namesIn f ++ namesIn a)
    namesIn _ = []

    pick acc [] = reverse acc
    pick acc ((n, t) : ps) = pick (insert n t acc) ps

    insert n t [] = [(n, t)]
    insert n t ((n',t') : ps)
        | n `elem` (namesIn t' ++ concatMap namesIn (map snd ps))
            = (n', t') : insert n t ps
        | otherwise = (n,t):(n',t'):ps

