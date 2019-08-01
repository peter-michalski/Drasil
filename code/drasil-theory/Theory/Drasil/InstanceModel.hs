{-# LANGUAGE Rank2Types, ScopedTypeVariables, TemplateHaskell #-}
module Theory.Drasil.InstanceModel (Constraints, InstanceModel, getEqMod,
  im, imNoDeriv, imNoRefs, imNoDerivNoRefs,
  inCons, outCons, imOutput, imInputs -- FIXME, these should be done via lenses
  ) where

import Language.Drasil
import Data.Drasil.IdeaDicts (inModel)
import Theory.Drasil.ModelKinds (ModelKinds(..), elim_mk, set_mk)

import Control.Lens (Lens', (^.), lens, makeLenses, set, to, view)
import Data.Maybe (mapMaybe) 

type Inputs = [QuantityDict]
type Output = QuantityDict

-- All constraints in an InstanceModel are always 'Assumed' !
type Constraints = [Relation]

type OutputConstraints = Constraints
type InputConstraints  = Constraints

-- | An Instance Model is a RelationConcept that may have specific input/output
-- constraints. It also has attributes like derivation, source, etc.
data InstanceModel = IM { _mk :: ModelKinds
                        , _imInputs :: Inputs
                        , _inCons :: InputConstraints
                        , _imOutput :: Output
                        , _outCons :: OutputConstraints
                        , _ref :: [Reference]
                        , _deri :: Maybe Derivation
                        ,  lb :: ShortName
                        ,  ra :: String
                        , _notes :: [Sentence]
                        }
makeLenses ''InstanceModel

lens_mk :: forall a. Lens' QDefinition a -> Lens' RelationConcept a -> Lens' InstanceModel a
lens_mk lq lr = lens g s
    where g :: InstanceModel -> a
          g i = elim_mk lq lr (i ^. mk)
          s :: InstanceModel -> a -> InstanceModel
          s i x = set mk (set_mk (i ^. mk) lq lr x) i

instance HasUID             InstanceModel where uid = lens_mk uid uid
instance NamedIdea          InstanceModel where term = lens_mk term term
instance Idea               InstanceModel where getA = elim_mk (to getA) (to getA) . view mk
instance Definition         InstanceModel where defn = lens_mk defn defn
instance ConceptDomain      InstanceModel where cdom = elim_mk (to cdom) (to cdom) . view mk
instance ExprRelat          InstanceModel where relat = elim_mk (to relat) (to relat) . view mk
instance HasDerivation      InstanceModel where derivations = deri
instance HasReference       InstanceModel where getReferences = ref
instance HasShortName       InstanceModel where shortname = lb
instance HasRefAddress      InstanceModel where getRefAdd = ra
instance HasAdditionalNotes InstanceModel where getNotes = notes
instance HasSymbol          InstanceModel where symbol = symbol . view imOutput -- ???
instance HasSpace           InstanceModel where typ = imOutput . typ
instance Quantity           InstanceModel where
instance MayHaveUnit        InstanceModel where getUnit = getUnit . view imOutput
instance CommonIdea         InstanceModel where abrv _ = abrv inModel
instance Referable          InstanceModel where
  refAdd      = getRefAdd
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

-- | Smart constructor for instance models with everything defined
im :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> [Reference] -> Maybe Derivation -> String -> [Sentence] -> InstanceModel
im rcon _ _  _ _  [] _  _  = error $ "Source field of " ++ rcon ^. uid ++ " is empty"
im rcon i ic o oc r der sn = 
  IM (OthModel rcon) i ic o oc r der (shortname' sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models; no derivation
imNoDeriv :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> [Reference] -> String -> [Sentence] -> InstanceModel
imNoDeriv rcon _ _  _ _ [] _  = error $ "Source field of " ++ rcon ^. uid ++ " is empty"
imNoDeriv rcon i ic o oc r sn =
  IM (OthModel rcon) i ic o oc r Nothing (shortname' sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models; no references
imNoRefs :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> Maybe Derivation -> String -> [Sentence] -> InstanceModel
imNoRefs rcon i ic o oc der sn = 
  IM (OthModel rcon) i ic o oc [] der (shortname' sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models; no derivations or references
imNoDerivNoRefs :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> String -> [Sentence] -> InstanceModel
imNoDerivNoRefs rcon i ic o oc sn = 
  IM (OthModel rcon) i ic o oc [] Nothing (shortname' sn) (prependAbrv inModel sn)

-- | Get equational models from a list of instance models
getEqMod :: [InstanceModel] -> [QDefinition]
getEqMod = mapMaybe isEqMod . map (view mk)
  where
    isEqMod (EquationalModel f) = Just f
    isEqMod _                   = Nothing
