-- | Defines common software products.
module Data.Drasil.Software.Products where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators

import Data.Drasil.Concepts.Documentation (game, video, open, source)
import Data.Drasil.Concepts.Computation (computer)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.Domains (progLanguage)


prodtcon :: [NamedChunk]
prodtcon = [sciCompS, videoGame, openSource, compPro]

matlab :: CI
matlab     = commonIdeaWithDict "matlab" (pn' "MATLAB programming language")       "MATLAB"  [progLanguage]

sciCompS :: NamedChunk
sciCompS   = nc "sciCompS"       (cn' "scientific computing software")

videoGame, openSource, compPro :: NamedChunk
videoGame   = compoundNC video game
openSource  = compoundNC open source
compPro     = compoundNC computer program
