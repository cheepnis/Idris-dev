{-# LANGUAGE PatternGuards #-}
module Idris.Elab.Clause where

import Idris.AbsSyntax
import Idris.ASTUtils
import Idris.DSL
import Idris.Error
import Idris.Delaborate
import Idris.Imports
import Idris.ElabTerm
import Idris.Coverage
import Idris.DataOpts
import Idris.Providers
import Idris.Primitives
import Idris.Inliner
import Idris.PartialEval
import Idris.DeepSeq
import Idris.Output (iputStrLn, pshow, iWarn)
import IRTS.Lang

import Idris.Elab.Type
import Idris.Elab.Utils

import Idris.Core.TT
import Idris.Core.Elaborate hiding (Tactic(..))
import Idris.Core.Evaluate
import Idris.Core.Execute
import Idris.Core.Typecheck
import Idris.Core.CaseTree

import Idris.Docstrings

import Prelude hiding (id, (.))
import Control.Category

import Control.Applicative hiding (Const)
import Control.DeepSeq
import Control.Monad
import Control.Monad.State.Strict as State
import Data.List
import Data.Maybe
import Debug.Trace

import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Char(isLetter, toLower)
import Data.List.Split (splitOn)

import Util.Pretty(pretty, text)

-- | Elaborate a collection of left-hand and right-hand pairs - that is, a
-- top-level definition.
elabClauses :: ElabInfo -> FC -> FnOpts -> Name -> [PClause] -> Idris ()
elabClauses info fc opts n_in cs = let n = liftname info n_in in
      do ctxt <- getContext
         ist  <- getIState
         inacc <- map fst <$> fgetState (opt_inaccessible . ist_optimisation n)

         -- Check n actually exists, with no definition yet
         let tys = lookupTy n ctxt
         let reflect = Reflection `elem` opts
         checkUndefined n ctxt
         unless (length tys > 1) $ do
           fty <- case tys of
              [] -> -- TODO: turn into a CAF if there's no arguments
                    -- question: CAFs in where blocks?
                    tclift $ tfail $ At fc (NoTypeDecl n)
              [ty] -> return ty
           let atys = map snd (getArgTys fty)
           cs_elab <- mapM (elabClause info opts)
                           (zip [0..] cs)
           let (pats_in, cs_full) = unzip cs_elab

           logLvl 3 $ "Elaborated patterns:\n" ++ show pats_in

           solveDeferred n

           -- just ensure that the structure exists
           fmodifyState (ist_optimisation n) id
           addIBC (IBCOpt n)

           ist <- getIState
           let pats = map (simple_lhs (tt_ctxt ist)) $ doTransforms ist pats_in

  --          logLvl 3 (showSep "\n" (map (\ (l,r) ->
  --                                         show l ++ " = " ++
  --                                         show r) pats))
           let tcase = opt_typecase (idris_options ist)

           -- Summary of what's about to happen: Definitions go:
           --
           -- pats_in -> pats -> pdef -> pdef'

           -- addCaseDef builds case trees from <pdef> and <pdef'>

           -- pdef is the compile-time pattern definition.
           -- This will get further optimised for run-time, and, separately,
           -- further inlined to help with totality checking.
           let pdef = map debind pats

           logLvl 5 $ "Initial typechecked patterns:\n" ++ show pats
           logLvl 5 $ "Initial typechecked pattern def:\n" ++ show pdef

           -- Look for 'static' names and generate new specialised
           -- definitions for them

           mapM_ (\ e -> case e of
                           Left _ -> return ()
                           Right (l, r) -> elabPE info fc n r) pats

           -- NOTE: Need to store original definition so that proofs which
           -- rely on its structure aren't affected by any changes to the
           -- inliner. Just use the inlined version to generate pdef' and to
           -- help with later inlinings.

           ist <- getIState
           let pdef_inl = inlineDef ist pdef

           numArgs <- tclift $ sameLength pdef

           case specNames opts of
                Just _ -> logLvl 5 $ "Partially evaluated:\n" ++ show pats
                _ -> return ()

           erInfo <- getErasureInfo <$> getIState
           tree@(CaseDef scargs sc _) <- tclift $
                   simpleCase tcase False reflect CompileTime fc inacc atys pdef erInfo
           cov <- coverage
           pmissing <-
                   if cov && not (hasDefault cs)
                      then do missing <- genClauses fc n (map getLHS pdef) cs_full
                              -- missing <- genMissing n scargs sc
                              missing' <- filterM (checkPossible info fc True n) missing
                              let clhs = map getLHS pdef
                              logLvl 2 $ "Must be unreachable:\n" ++
                                          showSep "\n" (map showTmImpls missing') ++
                                         "\nAgainst: " ++
                                          showSep "\n" (map (\t -> showTmImpls (delab ist t)) (map getLHS pdef))
                              -- filter out anything in missing' which is
                              -- matched by any of clhs. This might happen since
                              -- unification may force a variable to take a
                              -- particular form, rather than force a case
                              -- to be impossible.
                              return (filter (noMatch ist clhs) missing')
                      else return []
           let pcover = null pmissing

           -- pdef' is the version that gets compiled for run-time
           pdef_in' <- applyOpts pdef
           let pdef' = map (simple_rt (tt_ctxt ist)) pdef_in'

           logLvl 5 $ "After data structure transformations:\n" ++ show pdef'

           ist <- getIState
  --          let wf = wellFounded ist n sc
           let tot = if pcover || AssertTotal `elem` opts
                      then Unchecked -- finish checking later
                      else Partial NotCovering -- already know it's not total

  --          case lookupCtxt (namespace info) n (idris_flags ist) of
  --             [fs] -> if TotalFn `elem` fs
  --                       then case tot of
  --                               Total _ -> return ()
  --                               t -> tclift $ tfail (At fc (Msg (show n ++ " is " ++ show t)))
  --                       else return ()
  --             _ -> return ()
           case tree of
               CaseDef _ _ [] -> return ()
               CaseDef _ _ xs -> mapM_ (\x ->
                   iputStrLn $ show fc ++
                                ":warning - Unreachable case: " ++
                                   show (delab ist x)) xs
           let knowncovering = (pcover && cov) || AssertTotal `elem` opts

           tree' <- tclift $ simpleCase tcase knowncovering reflect
                                        RunTime fc inacc atys pdef' erInfo
           logLvl 3 $ "Unoptimised " ++ show n ++ ": " ++ show tree
           logLvl 3 $ "Optimised: " ++ show tree'
           ctxt <- getContext
           ist <- getIState
           let opt = idris_optimisation ist
           putIState (ist { idris_patdefs = addDef n (force pdef', force pmissing)
                                                (idris_patdefs ist) })
           let caseInfo = CaseInfo (inlinable opts) (dictionary opts)
           case lookupTy n ctxt of
               [ty] -> do updateContext (addCasedef n erInfo caseInfo
                                                       tcase knowncovering
                                                       reflect
                                                       (AssertTotal `elem` opts)
                                                       atys
                                                       inacc
                                                       pats
                                                       pdef pdef pdef_inl pdef' ty)
                          addIBC (IBCDef n)
                          setTotality n tot
                          when (not reflect) $ do totcheck (fc, n)
                                                  defer_totcheck (fc, n)
                          when (tot /= Unchecked) $ addIBC (IBCTotal n tot)
                          i <- getIState
                          case lookupDef n (tt_ctxt i) of
                              (CaseOp _ _ _ _ _ cd : _) ->
                                let (scargs, sc) = cases_compiletime cd
                                    (scargs', sc') = cases_runtime cd in
                                  do let calls = findCalls sc' scargs'
                                     let used = findUsedArgs sc' scargs'
                                     -- let scg = buildSCG i sc scargs
                                     -- add SCG later, when checking totality
                                     let cg = CGInfo scargs' calls [] used []  -- TODO: remove this, not needed anymore
                                     logLvl 2 $ "Called names: " ++ show cg
                                     addToCG n cg
                                     addToCalledG n (nub (map fst calls)) -- plus names in type!
                                     addIBC (IBCCG n)
                              _ -> return ()
                          return ()
  --                         addIBC (IBCTotal n tot)
               [] -> return ()
           -- Check it's covering, if 'covering' option is used. Chase
           -- all called functions, and fail if any of them are also
           -- 'Partial NotCovering'
           when (CoveringFn `elem` opts) $ checkAllCovering fc [] n n
  where
    noMatch i cs tm = all (\x -> case matchClause i (delab' i x True True) tm of
                                      Right _ -> False
                                      Left miss -> True) cs

    checkUndefined n ctxt = case lookupDef n ctxt of
                                 [] -> return ()
                                 [TyDecl _ _] -> return ()
                                 _ -> tclift $ tfail (At fc (AlreadyDefined n))

    debind (Right (x, y)) = let (vs, x') = depat [] x
                                (_, y') = depat [] y in
                                (vs, x', y')
    debind (Left x)       = let (vs, x') = depat [] x in
                                (vs, x', Impossible)

    depat acc (Bind n (PVar t) sc) = depat (n : acc) (instantiate (P Bound n t) sc)
    depat acc x = (acc, x)

    hasDefault cs | (PClause _ _ last _ _ _ :_) <- reverse cs
                  , (PApp fn s args) <- last = all ((==Placeholder) . getTm) args
    hasDefault _ = False

    getLHS (_, l, _) = l

    simple_lhs ctxt (Right (x, y)) = Right (normalise ctxt [] x, 
                                            force (normalisePats ctxt [] y))
    simple_lhs ctxt t = t

    simple_rt ctxt (p, x, y) = (p, x, force (uniqueBinders p 
                                                (rt_simplify ctxt [] y)))

    -- this is so pattern types are in the right form for erasure
    normalisePats ctxt env (Bind n (PVar t) sc) 
       = let t' = normalise ctxt env t in
             Bind n (PVar t') (normalisePats ctxt ((n, PVar t') : env) sc)
    normalisePats ctxt env (Bind n (PVTy t) sc) 
       = let t' = normalise ctxt env t in
             Bind n (PVTy t') (normalisePats ctxt ((n, PVar t') : env) sc)
    normalisePats ctxt env t = t

    specNames [] = Nothing
    specNames (Specialise ns : _) = Just ns
    specNames (_ : xs) = specNames xs

    sameLength ((_, x, _) : xs)
        = do l <- sameLength xs
             let (f, as) = unApply x
             if (null xs || l == length as) then return (length as)
                else tfail (At fc (Msg "Clauses have differing numbers of arguments "))
    sameLength [] = return 0

    -- apply all transformations (just specialisation for now, add
    -- user defined transformation rules later)
    doTransforms ist pats =
           case specNames opts of
                Nothing -> pats
                Just ns -> partial_eval (tt_ctxt ist) ns pats

-- | Find 'static' applications in a term and partially evaluate them
elabPE :: ElabInfo -> FC -> Name -> Term -> Idris ()
elabPE info fc caller r =
  do ist <- getIState
     let sa = getSpecApps ist [] r
     mapM_ (mkSpecialised ist) sa
  where 
    -- TODO: Add a PTerm level transformation rule, which is basically the 
    -- new definition in reverse (before specialising it). 
    -- RHS => LHS where implicit arguments are left blank in the 
    -- transformation.

    -- Apply that transformation after every PClauses elaboration

    mkSpecialised ist specapp_in = do
        let (specTy, specapp) = getSpecTy ist specapp_in
        let (n, newnm, pats) = getSpecClause ist specapp
        let undef = case lookupDef newnm (tt_ctxt ist) of
                         [] -> True
                         _ -> False
        logLvl 5 $ show (newnm, map (concreteArg ist) (snd specapp))
        idrisCatch
          (when (undef && all (concreteArg ist) (snd specapp)) $ do
            cgns <- getAllNames n
            let opts = [Specialise (map (\x -> (x, Nothing)) cgns ++ 
                                     mapMaybe specName (snd specapp))]
            logLvl 3 $ "Specialising application: " ++ show specapp
            logLvl 2 $ "New name: " ++ show newnm
            iLOG $ "PE definition type : " ++ (show specTy)
                        ++ "\n" ++ show opts
            logLvl 2 $ "PE definition " ++ show newnm ++ ":\n" ++
                        showSep "\n" 
                           (map (\ (lhs, rhs) ->
                              (showTmImpls lhs ++ " = " ++ 
                               showTmImpls rhs)) pats)
            elabType info defaultSyntax emptyDocstring [] fc opts newnm specTy
            let def = map (\ (lhs, rhs) -> PClause fc newnm lhs [] rhs []) pats
            elabClauses info fc opts newnm def
            logLvl 2 $ "Specialised " ++ show newnm)
          -- if it doesn't work, just don't specialise. Could happen for lots
          -- of valid reasons (e.g. local variables in scope which can't be
          -- lifted out).
          (\e -> logLvl 4 $ "Couldn't specialise: " ++ (pshow ist e)) 

    specName (ImplicitS, tm) 
        | (P Ref n _, _) <- unApply tm = Just (n, Just 1)
    specName (ExplicitS, tm)
        | (P Ref n _, _) <- unApply tm = Just (n, Just 1)
    specName _ = Nothing

    concreteArg ist (ImplicitS, tm) = concreteTm ist tm
    concreteArg ist (ExplicitS, tm) = concreteTm ist tm
    concreteArg ist _ = True

    concreteTm ist tm | (P _ n _, _) <- unApply tm =
        case lookupTy n (tt_ctxt ist) of
             [] -> False
             _ -> True
    concreteTm ist (Constant _) = True
    concreteTm ist _ = False

    -- get the type of a specialised application
    getSpecTy ist (n, args)
       = case lookupTy n (tt_ctxt ist) of
              [ty] -> let (specty_in, args') = specType args (explicitNames ty)
                          specty = normalise (tt_ctxt ist) [] (finalise specty_in)
                          t = mkPE_TyDecl ist args' (explicitNames specty) in
                          (t, (n, args'))
--                             (normalise (tt_ctxt ist) [] (specType args ty))
              _ -> error "Can't happen (getSpecTy)"

    -- get the clause of a specialised application
    getSpecClause ist (n, args)
       = let newnm = sUN ("__"++show (nsroot n) ++ "_" ++ 
                               showSep "_" (map showArg args)) in 
                               -- UN (show n ++ show (map snd args)) in
             (n, newnm, mkPE_TermDecl ist newnm n args)
      where showArg (ExplicitS, n) = show n
            showArg (ImplicitS, n) = show n
            showArg _ = ""

-- checks if the clause is a possible left hand side. Returns the term if
-- possible, otherwise Nothing.

checkPossible :: ElabInfo -> FC -> Bool -> Name -> PTerm -> Idris Bool
checkPossible info fc tcgen fname lhs_in
   = do ctxt <- getContext
        i <- getIState
        let lhs = addImplPat i lhs_in
        -- if the LHS type checks, it is possible
        case elaborate ctxt (sMN 0 "patLHS") infP []
                            (erun fc (buildTC i info ELHS [] fname (infTerm lhs))) of
            OK ((lhs', _, _), _) ->
               do let lhs_tm = orderPats (getInferTerm lhs')
                  case recheck ctxt [] (forget lhs_tm) lhs_tm of
                       OK _ -> return True
                       err -> return False
            -- if it's a recoverable error, the case may become possible
            Error err -> if tcgen then return (recoverable ctxt err)
                                  else return (validCase ctxt err ||
                                                 recoverable ctxt err)
    where validCase ctxt (CantUnify _ topx topy e _ _)
              = let topx' = normalise ctxt [] topx
                    topy' = normalise ctxt [] topy in
                    not (sameFam topx' topy' || not (validCase ctxt e))
          validCase ctxt (CantConvert _ _ _) = False
          validCase ctxt (At _ e) = validCase ctxt e
          validCase ctxt (Elaborating _ _ e) = validCase ctxt e
          validCase ctxt (ElaboratingArg _ _ _ e) = validCase ctxt e
          validCase ctxt _ = True
           
          recoverable ctxt (CantUnify r topx topy e _ _) 
              = let topx' = normalise ctxt [] topx
                    topy' = normalise ctxt [] topy in
                    checkRec topx' topy'
          recoverable ctxt (At _ e) = recoverable ctxt e
          recoverable ctxt (Elaborating _ _ e) = recoverable ctxt e
          recoverable ctxt (ElaboratingArg _ _ _ e) = recoverable ctxt e
          recoverable _ _ = False

          sameFam topx topy 
              = case (unApply topx, unApply topy) of
                     ((P _ x _, _), (P _ y _, _)) -> x == y
                     _ -> False

          -- different notion of recoverable than in unification, since we
          -- have no metavars -- just looking to see if a constructor is failing
          -- to unify with a function that may be reduced later

          checkRec (App f a) p@(P _ _ _) = checkRec f p
          checkRec p@(P _ _ _) (App f a) = checkRec p f
          checkRec fa@(App _ _) fa'@(App _ _) 
              | (f, as) <- unApply fa,
                (f', as') <- unApply fa'
                   = if (length as /= length as') 
                        then checkRec f f' 
                        else checkRec f f' && and (zipWith checkRec as as')
          checkRec (P xt x _) (P yt y _) = x == y || ntRec xt yt
          checkRec _ _ = False

          ntRec x y | Ref <- x = True
                    | Ref <- y = True
                    | otherwise = False -- name is different, unrecoverable

propagateParams :: IState -> [Name] -> Type -> PTerm -> PTerm
propagateParams i ps t tm@(PApp _ (PRef fc n) args)
     = PApp fc (PRef fc n) (addP t args)
   where addP (Bind n _ sc) (t : ts)
              | Placeholder <- getTm t,
                n `elem` ps,
                not (n `elem` allNamesIn tm)
                    = t { getTm = PRef fc n } : addP sc ts
         addP (Bind n _ sc) (t : ts) = t : addP sc ts
         addP _ ts = ts
propagateParams i ps t (PRef fc n)
     = case lookupCtxt n (idris_implicits i) of
            [is] -> let ps' = filter (isImplicit is) ps in
                        PApp fc (PRef fc n) (map (\x -> pimp x (PRef fc x) True) ps')
            _ -> PRef fc n
    where isImplicit [] n = False
          isImplicit (PImp _ _ _ x _ : is) n | x == n = True
          isImplicit (_ : is) n = isImplicit is n
propagateParams i ps t x = x

-- Return the elaborated LHS/RHS, and the original LHS with implicits added
elabClause :: ElabInfo -> FnOpts -> (Int, PClause) ->
              Idris (Either Term (Term, Term), PTerm)
elabClause info opts (_, PClause fc fname lhs_in [] PImpossible [])
   = do let tcgen = Dictionary `elem` opts
        i <- get
        let lhs = addImpl i lhs_in
        b <- checkPossible info fc tcgen fname lhs_in
        case b of
            True -> tclift $ tfail (At fc 
                                (Msg $ show lhs_in ++ " is a valid case"))
            False -> do ptm <- mkPatTm lhs_in
                        return (Left ptm, lhs)
elabClause info opts (cnum, PClause fc fname lhs_in withs rhs_in whereblock)
   = do let tcgen = Dictionary `elem` opts
        ctxt <- getContext

        -- Build the LHS as an "Infer", and pull out its type and
        -- pattern bindings
        i <- getIState
        inf <- isTyInferred fname
        -- get the parameters first, to pass through to any where block
        let fn_ty = case lookupTy fname (tt_ctxt i) of
                         [t] -> t
                         _ -> error "Can't happen (elabClause function type)"
        let fn_is = case lookupCtxt fname (idris_implicits i) of
                         [t] -> t
                         _ -> []
        let params = getParamsInType i [] fn_is fn_ty
        let lhs = mkLHSapp $ stripUnmatchable i $
                    propagateParams i params fn_ty (addImplPat i (stripLinear i lhs_in))
        logLvl 5 ("LHS: " ++ show fc ++ " " ++ showTmImpls lhs)
        logLvl 4 ("Fixed parameters: " ++ show params ++ " from " ++ show lhs_in ++
                  "\n" ++ show (fn_ty, fn_is))

        (((lhs', dlhs, []), probs, inj), _) <-
            tclift $ elaborate ctxt (sMN 0 "patLHS") infP []
                     (do res <- errAt "left hand side of " fname
                                  (erun fc (buildTC i info ELHS opts fname (infTerm lhs)))
                         probs <- get_probs
                         inj <- get_inj
                         return (res, probs, inj))

        when inf $ addTyInfConstraints fc (map (\(x,y,_,_,_,_) -> (x,y)) probs)

        let lhs_tm = orderPats (getInferTerm lhs')
        let lhs_ty = getInferType lhs'
        logLvl 3 ("Elaborated: " ++ show lhs_tm)
        logLvl 3 ("Elaborated type: " ++ show lhs_ty)
        logLvl 5 ("Injective: " ++ show fname ++ " " ++ show inj)

        -- If we're inferring metavariables in the type, don't recheck,
        -- because we're only doing this to try to work out those metavariables
        (clhs_c, clhsty) <- if not inf
                               then recheckC fc [] lhs_tm
                               else return (lhs_tm, lhs_ty)
        let clhs = normalise ctxt [] clhs_c
        
        logLvl 3 ("Normalised LHS: " ++ showTmImpls (delabMV i clhs))

        rep <- useREPL
        when rep $ do
          addInternalApp (fc_fname fc) (fst . fc_start $ fc) (delabMV i clhs) -- TODO: Should use span instead of line and filename?
          addIBC (IBCLineApp (fc_fname fc) (fst . fc_start $ fc) (delabMV i clhs))

        logLvl 5 ("Checked " ++ show clhs ++ "\n" ++ show clhsty)
        -- Elaborate where block
        ist <- getIState
        windex <- getName
        let decls = nub (concatMap declared whereblock)
        let defs = nub (decls ++ concatMap defined whereblock)
        let newargs = pvars ist lhs_tm
        let winfo = pinfo info newargs defs windex
        let wb = map (expandParamsD False ist decorate newargs defs) whereblock

        -- Split the where block into declarations with a type, and those
        -- without
        -- Elaborate those with a type *before* RHS, those without *after*
        let (wbefore, wafter) = sepBlocks wb

        logLvl 2 $ "Where block:\n " ++ show wbefore ++ "\n" ++ show wafter
        mapM_ (rec_elabDecl info EAll winfo) wbefore
        -- Now build the RHS, using the type of the LHS as the goal.
        i <- getIState -- new implicits from where block
        logLvl 5 (showTmImpls (expandParams decorate newargs defs (defs \\ decls) rhs_in))
        let rhs = addImplBoundInf i (map fst newargs) (defs \\ decls)
                                 (expandParams decorate newargs defs (defs \\ decls) rhs_in)
        logLvl 2 $ "RHS: " ++ showTmImpls rhs
        ctxt <- getContext -- new context with where block added
        logLvl 5 "STARTING CHECK"
        ((rhs', defer, is, probs), _) <-
           tclift $ elaborate ctxt (sMN 0 "patRHS") clhsty []
                    (do pbinds ist lhs_tm
                        mapM_ setinj (nub (params ++ inj))
                        setNextName 
                        (_, _, is) <- errAt "right hand side of " fname
                                         (erun fc (build i winfo ERHS opts fname rhs))
                        errAt "right hand side of " fname
                              (erun fc $ psolve lhs_tm)
                        hs <- get_holes
                        aux <- getAux
                        mapM_ (elabCaseHole aux) hs
                        tt <- get_term
                        let (tm, ds) = runState (collectDeferred (Just fname) tt) []
                        probs <- get_probs
                        return (tm, ds, is, probs))

        when inf $ addTyInfConstraints fc (map (\(x,y,_,_,_,_) -> (x,y)) probs)

        logLvl 5 "DONE CHECK"
        logLvl 2 $ "---> " ++ show rhs'
        when (not (null defer)) $ iLOG $ "DEFERRED " ++ 
                    show (map (\ (n, (_,_,t)) -> (n, t)) defer)
        def' <- checkDef fc defer
        let def'' = map (\(n, (i, top, t)) -> (n, (i, top, t, False))) def'
        addDeferred def''
        mapM_ (\(n, _) -> addIBC (IBCDef n)) def''

        when (not (null def')) $ do
           mapM_ defer_totcheck (map (\x -> (fc, fst x)) def'')

        -- Now the remaining deferred (i.e. no type declarations) clauses
        -- from the where block

        mapM_ (rec_elabDecl info EAll winfo) wafter
        mapM_ (elabCaseBlock winfo opts) is

        ctxt <- getContext
        logLvl 5 $ "Rechecking"
        logLvl 6 $ " ==> " ++ show (forget rhs')
        (crhs, crhsty) <- if not inf 
                             then recheckC fc [] rhs'
                             else return (rhs', clhsty)
        logLvl 6 $ " ==> " ++ show crhsty ++ "   against   " ++ show clhsty
        case  converts ctxt [] clhsty crhsty of
            OK _ -> return ()
            Error e -> ierror (At fc (CantUnify False clhsty crhsty e [] 0))
        i <- getIState
        checkInferred fc (delab' i crhs True True) rhs
        -- if the function is declared '%error_reverse', or its type,
        -- then we'll try running it in reverse to improve error messages
        let (ret_fam, _) = unApply (getRetTy crhsty)
        rev <- case ret_fam of
                    P _ rfamn _ -> 
                        case lookupCtxt rfamn (idris_datatypes i) of
                             [TI _ _ dopts _ _] -> 
                                 return (DataErrRev `elem` dopts)
                             _ -> return False
                    _ -> return False

        when (rev || ErrorReverse `elem` opts) $ do
           addIBC (IBCErrRev (crhs, clhs))
           addErrRev (crhs, clhs) 
        return $ (Right (clhs, crhs), lhs)
  where
    pinfo :: ElabInfo -> [(Name, PTerm)] -> [Name] -> Int -> ElabInfo
    pinfo info ns ds i
          = let newps = params info ++ ns
                dsParams = map (\n -> (n, map fst newps)) ds
                newb = addAlist dsParams (inblock info)
                l = liftname info in
                info { params = newps,
                       inblock = newb,
                       liftname = id -- (\n -> case lookupCtxt n newb of
                                     --      Nothing -> n
                                     --      _ -> MN i (show n)) . l
                     }

    mkLHSapp t@(PRef _ _) = trace ("APP " ++ show t) $ PApp fc t []
    mkLHSapp t = t

    decorate (NS x ns)
       = NS (SN (WhereN cnum fname x)) ns -- ++ [show cnum])
--        = NS (UN ('#':show x)) (ns ++ [show cnum, show fname])
    decorate x
       = SN (WhereN cnum fname x)
--        = NS (SN (WhereN cnum fname x)) [show cnum]
--        = NS (UN ('#':show x)) [show cnum, show fname]

    sepBlocks bs = sepBlocks' [] bs where
      sepBlocks' ns (d@(PTy _ _ _ _ _ n t) : bs)
            = let (bf, af) = sepBlocks' (n : ns) bs in
                  (d : bf, af)
      sepBlocks' ns (d@(PClauses _ _ n _) : bs)
         | not (n `elem` ns) = let (bf, af) = sepBlocks' ns bs in
                                   (bf, d : af)
      sepBlocks' ns (b : bs) = let (bf, af) = sepBlocks' ns bs in
                                   (b : bf, af)
      sepBlocks' ns [] = ([], [])


    -- if a hole is just an argument/result of a case block, treat it as
    -- the unit type. Hack to help elaborate case in do blocks.
    elabCaseHole aux h = do
        focus h
        g <- goal
        case g of
             TType _ -> when (any (isArg h) aux) $ do apply (Var unitTy) []; solve
             _ -> return ()

    -- Is the name a pattern argument in the declaration
    isArg :: Name -> PDecl -> Bool
    isArg n (PClauses _ _ _ cs) = any isArg' cs
      where
        isArg' (PClause _ _ (PApp _ _ args) _ _ _) 
           = any (\x -> case x of
                          PRef _ n' -> n == n'
                          _ -> False) (map getTm args)
        isArg' _ = False
    isArg _ _ = False

elabClause info opts (_, PWith fc fname lhs_in withs wval_in withblock)
   = do let tcgen = Dictionary `elem` opts
        ctxt <- getContext
        -- Build the LHS as an "Infer", and pull out its type and
        -- pattern bindings
        i <- getIState
        -- get the parameters first, to pass through to any where block
        let fn_ty = case lookupTy fname (tt_ctxt i) of
                         [t] -> t
                         _ -> error "Can't happen (elabClause function type)"
        let fn_is = case lookupCtxt fname (idris_implicits i) of
                         [t] -> t
                         _ -> []
        let params = getParamsInType i [] fn_is fn_ty
        let lhs = propagateParams i params fn_ty (addImplPat i (stripLinear i lhs_in))
        logLvl 2 ("LHS: " ++ show lhs)
        ((lhs', dlhs, []), _) <-
            tclift $ elaborate ctxt (sMN 0 "patLHS") infP []
              (errAt "left hand side of with in " fname
                (erun fc (buildTC i info ELHS opts fname (infTerm lhs))) )
        let lhs_tm = orderPats (getInferTerm lhs')
        let lhs_ty = getInferType lhs'
        let ret_ty = getRetTy (explicitNames (normalise ctxt [] lhs_ty))
        logLvl 3 (show lhs_tm)
        (clhs, clhsty) <- recheckC fc [] lhs_tm
        logLvl 5 ("Checked " ++ show clhs)
        let bargs = getPBtys (explicitNames (normalise ctxt [] lhs_tm))
        let wval = addImplBound i (map fst bargs) wval_in
        logLvl 5 ("Checking " ++ showTmImpls wval)
        -- Elaborate wval in this context
        ((wval', defer, is), _) <-
            tclift $ elaborate ctxt (sMN 0 "withRHS")
                        (bindTyArgs PVTy bargs infP) []
                        (do pbinds i lhs_tm
                            setNextName
                            -- TODO: may want where here - see winfo abpve
                            (_', d, is) <- errAt "with value in " fname
                              (erun fc (build i info ERHS opts fname (infTerm wval)))
                            erun fc $ psolve lhs_tm
                            tt <- get_term
                            return (tt, d, is))
        def' <- checkDef fc defer
        let def'' = map (\(n, (i, top, t)) -> (n, (i, top, t, False))) def'
        addDeferred def''
        mapM_ (elabCaseBlock info opts) is
        logLvl 5 ("Checked wval " ++ show wval')
        (cwval, cwvalty) <- recheckC fc [] (getInferTerm wval')
        let cwvaltyN = explicitNames (normalise ctxt [] cwvalty)
        let cwvalN = explicitNames (normalise ctxt [] cwval)
        logLvl 5 ("With type " ++ show cwvalty ++ "\nRet type " ++ show ret_ty)
        let pvars = map fst (getPBtys cwvalty)
        -- we need the unelaborated term to get the names it depends on
        -- rather than a de Bruijn index.
        let pdeps = usedNamesIn pvars i (delab i cwvalty)
        let (bargs_pre, bargs_post) = split pdeps bargs []
        logLvl 10 ("With type " ++ show (getRetTy cwvaltyN) ++
                  " depends on " ++ show pdeps ++ " from " ++ show pvars)
        logLvl 10 ("Pre " ++ show bargs_pre ++ "\nPost " ++ show bargs_post)
        windex <- getName
        -- build a type declaration for the new function:
        -- (ps : Xs) -> (withval : cwvalty) -> (ps' : Xs') -> ret_ty
        let wargval = getRetTy cwvalN
        let wargtype = getRetTy cwvaltyN
        logLvl 5 ("Abstract over " ++ show wargval ++ " in " ++ show wargtype)
        let wtype = bindTyArgs Pi (bargs_pre ++
                     (sMN 0 "warg", wargtype) :
                     map (abstract (sMN 0 "warg") wargval wargtype) bargs_post)
                     (substTerm wargval (P Bound (sMN 0 "warg") wargtype) ret_ty)
        logLvl 5 ("New function type " ++ show wtype)
        let wname = SN (WithN windex fname)

        let imps = getImps wtype -- add to implicits context
        putIState (i { idris_implicits = addDef wname imps (idris_implicits i) })
        addIBC (IBCDef wname)
        def' <- checkDef fc [(wname, (-1, Nothing, wtype))]
        let def'' = map (\(n, (i, top, t)) -> (n, (i, top, t, False))) def'
        addDeferred def''

        -- in the subdecls, lhs becomes:
        --         fname  pats | wpat [rest]
        --    ==>  fname' ps   wpat [rest], match pats against toplevel for ps
        wb <- mapM (mkAuxC wname lhs (map fst bargs_pre) (map fst bargs_post))
                       withblock
        logLvl 3 ("with block " ++ show wb)
        -- propagate totality assertion to the new definitions
        when (AssertTotal `elem` opts) $ setFlags wname [AssertTotal]
        mapM_ (rec_elabDecl info EAll info) wb

        -- rhs becomes: fname' ps wval
        let rhs = PApp fc (PRef fc wname)
                    (map (pexp . (PRef fc) . fst) bargs_pre ++
                        pexp wval :
                    (map (pexp . (PRef fc) . fst) bargs_post))
        logLvl 5 ("New RHS " ++ showTmImpls rhs)
        ctxt <- getContext -- New context with block added
        i <- getIState
        ((rhs', defer, is), _) <-
           tclift $ elaborate ctxt (sMN 0 "wpatRHS") clhsty []
                    (do pbinds i lhs_tm
                        setNextName
                        (_, d, is) <- erun fc (build i info ERHS opts fname rhs)
                        psolve lhs_tm
                        tt <- get_term
                        return (tt, d, is))
        def' <- checkDef fc defer
        let def'' = map (\(n, (i, top, t)) -> (n, (i, top, t, False))) def'
        addDeferred def''
        mapM_ (elabCaseBlock info opts) is
        logLvl 5 ("Checked RHS " ++ show rhs')
        (crhs, crhsty) <- recheckC fc [] rhs'
        return $ (Right (clhs, crhs), lhs)
  where
    getImps (Bind n (Pi _) t) = pexp Placeholder : getImps t
    getImps _ = []

    mkAuxC wname lhs ns ns' (PClauses fc o n cs)
        | True  = do cs' <- mapM (mkAux wname lhs ns ns') cs
                     return $ PClauses fc o wname cs'
        | otherwise = ifail $ show fc ++ "with clause uses wrong function name " ++ show n
    mkAuxC wname lhs ns ns' d = return $ d

    mkAux wname toplhs ns ns' (PClause fc n tm_in (w:ws) rhs wheres)
        = do i <- getIState
             let tm = addImplPat i tm_in
             logLvl 2 ("Matching " ++ showTmImpls tm ++ " against " ++
                                      showTmImpls toplhs)
             case matchClause i toplhs tm of
                Left (a,b) -> ifail $ show fc ++ ":with clause does not match top level"
                Right mvars ->
                    do logLvl 3 ("Match vars : " ++ show mvars)
                       lhs <- updateLHS n wname mvars ns ns' (fullApp tm) w
                       return $ PClause fc wname lhs ws rhs wheres
    mkAux wname toplhs ns ns' (PWith fc n tm_in (w:ws) wval withs)
        = do i <- getIState
             let tm = addImplPat i tm_in
             logLvl 2 ("Matching " ++ showTmImpls tm ++ " against " ++
                                      showTmImpls toplhs)
             withs' <- mapM (mkAuxC wname toplhs ns ns') withs
             case matchClause i toplhs tm of
                Left (a,b) -> trace ("matchClause: " ++ show a ++ " =/= " ++ show b) (ifail $ show fc ++ "with clause does not match top level")
                Right mvars ->
                    do lhs <- updateLHS n wname mvars ns ns' (fullApp tm) w
                       return $ PWith fc wname lhs ws wval withs'
    mkAux wname toplhs ns ns' c
        = ifail $ show fc ++ ":badly formed with clause"

    addArg (PApp fc f args) w = PApp fc f (args ++ [pexp w])
    addArg (PRef fc f) w = PApp fc (PRef fc f) [pexp w]

    updateLHS n wname mvars ns_in ns_in' (PApp fc (PRef fc' n') args) w
        = let ns = map (keepMvar (map fst mvars) fc') ns_in
              ns' = map (keepMvar (map fst mvars) fc') ns_in' in
              return $ substMatches mvars $
                  PApp fc (PRef fc' wname)
                      (map pexp ns ++ pexp w : (map pexp ns'))
    updateLHS n wname mvars ns_in ns_in' tm w
        = updateLHS n wname mvars ns_in ns_in' (PApp fc tm []) w

    keepMvar mvs fc v | v `elem` mvs = PRef fc v
                      | otherwise = Placeholder

    fullApp (PApp _ (PApp fc f args) xs) = fullApp (PApp fc f (args ++ xs))
    fullApp x = x

    split [] rest pre = (reverse pre, rest)
    split deps ((n, ty) : rest) pre
          | n `elem` deps = split (deps \\ [n]) rest ((n, ty) : pre)
          | otherwise = split deps rest ((n, ty) : pre)
    split deps [] pre = (reverse pre, [])

    abstract wn wv wty (n, argty) = (n, substTerm wv (P Bound wn wty) argty)


