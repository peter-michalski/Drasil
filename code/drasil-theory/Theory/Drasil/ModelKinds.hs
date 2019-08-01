{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module Theory.Drasil.ModelKinds where

import Language.Drasil

import Control.Lens (Getter, Setter', (^.), set)

data ModelKinds =
    EquationalModel QDefinition
  | DEModel RelationConcept
  | OthModel RelationConcept

elim_mk :: (Getter QDefinition a) -> (Getter RelationConcept a) -> ModelKinds -> a
elim_mk l _ (EquationalModel q) = q ^. l
elim_mk _ l (DEModel q)         = q ^. l
elim_mk _ l (OthModel q)        = q ^. l

set_mk :: ModelKinds -> Setter' QDefinition a -> Setter' RelationConcept a -> a -> ModelKinds
set_mk (EquationalModel q) f _ x = EquationalModel $ set f x q
set_mk (DEModel q)         _ g x = DEModel $ set g x q
set_mk (OthModel q)        _ g x = OthModel $ set g x q
