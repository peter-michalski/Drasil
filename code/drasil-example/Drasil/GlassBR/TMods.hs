module Drasil.GlassBR.TMods (tMods, pbIsSafe, lrIsSafe) where

import Language.Drasil
import Theory.Drasil (TheoryModel, tm)

import Drasil.GlassBR.References (astm2009)
import Drasil.GlassBR.Unitals (isSafeLoad, isSafeProb, pbTolfail, probFail, tmDemand, tmLRe)

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
lrIsSafeDesc = tModDesc isSafeLoad

pbIsSafe :: TheoryModel
pbIsSafe = tm (cw pbIsSafeQD) 
  [qw isSafeProb, qw probFail, qw pbTolfail] ([] :: [ConceptChunk])
  [pbIsSafeQD] [sy isSafeProb $= sy probFail $< sy pbTolfail] [] [makeCite astm2009]
  "isSafeProb" [pbIsSafeDesc]

pbIsSafeQD :: QDefinition
pbIsSafeQD = fromEqn' "safetyProbability" (nounPhraseSP "Safety Probability")
  EmptyS (eqSymb isSafeProb) Boolean (sy probFail $< sy pbTolfail)

pbIsSafeDesc :: Sentence
pbIsSafeDesc = tModDesc isSafeProb

tModDesc :: QuantityDict -> Sentence
tModDesc main = S "If" +:+. (ch main `sC` S "the structure is considered safe")
