module Pedant.TypeCheck.LambdaCalculus (inferType) where

import qualified Pedant.TypeCheck.Types as TC
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Pedant.Types as Types
import qualified Pedant.FileResolver as Resolver

data TypeCheckedModule = TypeCheckedModule {
    -- | The environment of the checker. This contains references to all the variables and schemes of those variables currently declared.
    tcmVars :: TC.TypeEnv,
    -- | Substitutions, the current substitutions that are required for the expression to unify
    tcsSubs :: TC.Substitution,
    -- | Units, the units currently declared
    tcsUnits :: Set.Set Types.VariableName,
    -- | A list of the modules that have been checked
    tcsCheckedModules :: Set.Set T.Text,
    tcsCurrentModule :: T.Text,
    tcsSeed :: Int
  }

typeCheckFile :: Resolver.Module -> TC.TypeInferenceMonad TypeCheckedModule
typeCheckFile m =
  let setStateModuleName = tcState {tcsCurrentModule = Resolver.moduleName m}
      (result, _) = runTI (inferLoop setStateModuleName (Resolver.moduleStatements m))
   in case result of
        Right (err, state) ->
          let addedCheckedModule = state {tcsCheckedModules = Resolver.moduleName m `Set.insert` tcsCheckedModules setStateModuleName}
           in (err, addedCheckedModule)
        Left err ->
          (Just err, tcState)
  where
    inferLoop :: TypeCheckState -> [Parser.Statement] -> TI (Maybe (Parser.Positioned TypeError), TypeCheckState)
    inferLoop state [] = return (Nothing, state)
    inferLoop state (statement : rest) =
      let moduleName = tcsCurrentModule state
       in case statement of
            Parser.UnitStatement units ->
              let newUnits = tcsUnits state `Set.union` Set.fromList (map (\(Parser.Positioned _ p) -> VariableName moduleName p) units)
               in inferLoop (state {tcsUnits = newUnits}) rest
            Parser.ImportStatement importedModuleName imports -> do
              moduleState <- importModule moduleName importedModuleName imports state
              inferLoop moduleState rest
            Parser.AssignmentStatement assignment -> do
              -- First, assign polymorphic types to all arguments of the function (make the definition as loose as possible)
              mapPairs <-
                mapM
                  ( \a -> do
                      tv <- newTyVar a
                      return (VariableName moduleName a, VariableInfo (Scheme [] tv) (EVariable (VariableName moduleName a)) assignment) -- This is a fake execution expression, it's an argument, it's deliberately unknown
                  )
                  (Parser.assignmentArguments assignment)

              -- Then, add these arguments to the type environment
              let (TypeEnv env) = tcsEnv state
                  arguments = Parser.assignmentArguments assignment
                  env'' = TypeEnv (env OMap.<>| OMap.fromList mapPairs)
              TypeCheckResult s1 t1 ex <- ti (state {tcsEnv = env''}) (Parser.assignmentExpression assignment)

              let varType = foldr (\(_, VariableInfo (Scheme _ tv) _ _) acc -> apply s1 tv `FuncType` acc) (apply s1 t1) mapPairs
                  name = VariableName moduleName (Parser.assignmentName assignment)
                  t' = generalize (apply s1 env'') varType
                  -- Note that this env is not env''. This is because otherwise we will add arguments as variables
                  -- We want to not include those
                  envWithVar = addToEnv name (VariableInfo t' (wrapFunctionArgs arguments ex) assignment) (TypeEnv env)
              let newTcState =
                    state
                      { tcsEnv = envWithVar,
                        tcsSubs = s1 `composeSubst` tcsSubs state
                      }
              inferLoop newTcState rest
            `catchError` (\err -> return (Just err, state))