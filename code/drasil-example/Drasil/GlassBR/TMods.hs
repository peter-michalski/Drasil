module Drasil.GlassBR.TMods (tMods, pbIsSafe, lrIsSafe) where

import Language.Drasil
import Theory.Drasil (TheoryModel, tm)
import Utils.Drasil

import Control.Lens ((^.))

import Drasil.GlassBR.Concepts (lResistance)
import Drasil.GlassBR.References (astm2009)
import Drasil.GlassBR.Unitals (tmDemand, demandq, isSafeProb, isSafeLoad, tmLRe, pbTolfail, probFail)

{--}

tMods :: [TheoryModel]
tMods = [pbIsSafe, lrIsSafe]

-- FIXME: This is a hack to see if TheoryModel printing will work. This chunk
-- needs to be updated properly.
-- this is the new function but it still uses the lrIsSafeQD,
-- so basically we have to combine the old function with the new function
-- glass_concept :: [ConceptInstance]
-- glass_concept = []


lrIsSafe :: TheoryModel
lrIsSafe = tm (cw lrIsSafeQD)
   [qw isSafeLoad, qw tmLRe, qw tmDemand] ([] :: [ConceptChunk])
   [lrIsSafeQD] [sy isSafeLoad $= sy tmLRe $> sy tmDemand] [] [makeCite astm2009] 
   "isSafeLoad" [lrIsSafeDesc]

lrIsSafeQD :: QDefinition
lrIsSafeQD = fromEqn' "safetyLoad" (nounPhraseSP "Safety Load")
  EmptyS (eqSymb isSafeLoad) Boolean (sy tmLRe $> sy tmDemand)

lrIsSafeDesc :: Sentence
lrIsSafeDesc = tModDesc isSafeLoad s ending
  where 
    s = ch isSafeProb +:+ sParen (S "from" +:+ makeRef2S pbIsSafe) `sAnd` ch isSafeLoad
    ending = short lResistance `isThe` phrase lResistance +:+ 
      sParen (S "also called capacity") `sC` ch tmDemand +:+ sParen (S "also referred as the" +:+ 
      titleize demandq) `isThe` (demandq ^. defn)

pbIsSafe :: TheoryModel
pbIsSafe = tm (cw pbIsSafeQD) 
  [qw isSafeProb, qw probFail, qw pbTolfail] ([] :: [ConceptChunk])
  [pbIsSafeQD] [sy isSafeProb $= sy probFail $< sy pbTolfail] [] [makeCite astm2009]
  "isSafeProb" [pbIsSafeDesc]

pbIsSafeQD :: QDefinition
pbIsSafeQD = fromEqn' "safetyProbability" (nounPhraseSP "Safety Probability")
  EmptyS (eqSymb isSafeProb) Boolean (sy probFail $< sy pbTolfail)

pbIsSafeDesc :: Sentence
pbIsSafeDesc = tModDesc isSafeProb s ending
  where 
    s = ch isSafeProb `sAnd` ch isSafeLoad +:+ sParen (S "from" +:+
      makeRef2S lrIsSafe)
    ending = (ch probFail `isThe` phrase probFail) `sC` ch pbTolfail `isThe` phrase pbTolfail

tModDesc :: QuantityDict -> Sentence -> Sentence -> Sentence
tModDesc main s ending = foldlSent [S "If", ch main `sC` S "the glass is" +:+.
  S "considered safe", s +:+. S "are either both True or both False", ending]
