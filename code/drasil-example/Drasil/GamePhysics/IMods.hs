module Drasil.GamePhysics.IMods (iMods, transMot, rotMot, col2D, instModIntro) where

import Language.Drasil
import Theory.Drasil (InstanceModel, eqModel, imNoDerivNoRefs)
import Utils.Drasil

import Drasil.GamePhysics.Assumptions (assumpOT, assumpOD, assumpAD, assumpCT, assumpDI,
  assumpCAJI)
import Drasil.GamePhysics.Goals (linearGS, angularGS)
import Drasil.GamePhysics.Unitals (accI, forceI, transMotLegTerms, 
  rotMotLegTerms, col2DLegTerms, massA, massI, normalVect, timeC, torqueI, velA,
  velI)
import Drasil.GamePhysics.DataDefs (ctrOfMassDD, linDispDD, linVelDD, linAccDD,
  angDispDD, angVelDD, angAccelDD, impulseDD)

import Data.Drasil.Concepts.Documentation (goal)
import qualified Data.Drasil.Concepts.Physics as CP (rigidBody)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration,
  angularAccel, force, gravitationalAccel, momentOfInertia, angularVelocity, 
  time, impulseS)

iMods :: [InstanceModel]
iMods = [transMot, rotMot, col2D]

{-- Force on the translational motion  --}
transMot :: InstanceModel
transMot = eqModel transMotQD [qw velI, qw QP.time, qw QP.gravitationalAccel, qw forceI, qw massI] 
  [sy velI $> 0, sy QP.time $> 0, sy QP.gravitationalAccel $> 0, sy forceI $> 0, sy massI $> 0]
  (qw accI) [] [] Nothing "transMot" [transMotDesc]

transMotQD :: QDefinition
transMotQD = fromEqn' "transMotQD" transMotNP (transMotDesc +:+ transMotLeg)
  (eqSymb accI) Real transMotEqn

transMotNP :: NP
transMotNP = nounPhraseSP "Force on the translational motion of a set of 2d rigid bodies"

-- FIXME: should this "equation" be a series of equations?
transMotEqn :: Expr
transMotEqn = (deriv (apply1 velI QP.time) QP.time)
  $= (sy QP.gravitationalAccel) + ((apply1 forceI QP.time) / (sy massI))

-- FIXME: need referencing
transMotDesc, transMotLeg :: Sentence
transMotDesc = foldlSent [S "The above equation expresses the total",
  phrase QP.acceleration, S "of the", phrase CP.rigidBody,
  makeRef2S assumpOT, makeRef2S assumpOD, S "i as the sum of",
  phrase QP.gravitationalAccel,
  S "(GD3) and", phrase QP.acceleration, S "due to applied",
  phrase QP.force, S "Fi(t) (T1). The resultant outputs are",
  S "then obtained from this equation using", makeRef2S linDispDD,
  makeRef2S linVelDD +:+. makeRef2S linAccDD, S" It is currently",
  S "assumed that there is no damping", makeRef2S assumpDI,
  S "or constraints", makeRef2S assumpCAJI +:+. S "involved", makeRef2S ctrOfMassDD]

transMotLeg = foldlSent_ $ map defList transMotLegTerms

{-- Rotational Motion --}

rotMot :: InstanceModel
rotMot = eqModel rotMotQD [qw QP.angularVelocity, qw QP.time, qw torqueI, qw QP.momentOfInertia]
  [sy QP.angularVelocity $> 0, sy QP.time $> 0, sy torqueI $> 0, sy QP.momentOfInertia $> 0] 
  (qw QP.angularAccel) [sy QP.angularAccel $> 0] [] Nothing "rotMot" [rotMotDesc]

rotMotQD :: QDefinition
rotMotQD = fromEqn' "rotMotQD" rotMotNP (rotMotDesc +:+ rotMotLeg)
  (eqSymb QP.angularAccel) Real rotMotEqn

rotMotNP :: NP
rotMotNP =  nounPhraseSP "Force on the rotational motion of a set of 2D rigid body"

-- FIXME: should this "equation" be a series of equations?
rotMotEqn :: Expr
rotMotEqn = deriv (apply1 QP.angularVelocity QP.time) QP.time
  $= (apply1 torqueI QP.time / sy QP.momentOfInertia)

--fixme: need referencing
rotMotDesc, rotMotLeg :: Sentence
rotMotDesc = foldlSent_ [S "The above equation for the total angular acceleration",
  S "of the rigid body", makeRef2S assumpOT, makeRef2S assumpOD,
  S "i is derived from T5, and the resultant outputs",
  S "are then obtained from this equation using", makeRef2S angDispDD,
  makeRef2S angVelDD +:+. makeRef2S angAccelDD, S "It is",
  S "currently assumed that there is no damping", makeRef2S assumpDI,
  S "or constraints", makeRef2S assumpCAJI +:+. S "involved", makeRef2S assumpAD]

rotMotLeg = foldlSent_ $ map defList rotMotLegTerms

{-- 2D Collision --}

col2D :: InstanceModel
col2D = imNoDerivNoRefs col2DRC [qw QP.time, qw QP.impulseS, qw massA, qw normalVect] 
  [sy QP.time $> 0, sy QP.impulseS $> 0, sy massA $> 0, sy normalVect $> 0]
  (qw timeC) [sy velA $> 0, sy timeC $> 0] "col2D"
  [col2DDesc]

col2DRC :: RelationConcept
col2DRC = makeRC "col2DRC" col2DNP (col2DDesc +:+ col2DLeg) col2DEqn

col2DNP :: NP
col2DNP = nounPhraseSP "Collisions on 2D rigid bodies"

col2DEqn {-, im3Eqn2, im3Eqn3, im3Eqn4 -} :: Expr -- FIXME: add proper equation
col2DEqn = apply1 velA timeC $= apply1 velA QP.time +
  (sy QP.impulseS / sy massA) * sy normalVect

--im3Eqn2 = (apply1 velB timeC) $= (apply1 velB QP.time) -
--  ((sy QP.impulseS) / (sy massB)) * (sy normalVect)


--fixme: these two need to use cross product and parametrized dispUnit symbol
--im3Eqn3 = (apply1 angVelA timeC) $= (apply1 angVelA QP.time) +
--  ((sy dispUnit) * ((sy QP.impulseS) * (sy normalVect))) / (sy QP.momentOfInertia)

--im3Eqn4 = (apply1 angVelB timeC) $= (apply1 angVelB QP.time) -
--  ((sy dispUnit) * ((sy QP.impulseS) * (sy normalVect))) / (sy QP.momentOfInertia)

--fixme: need referencing
col2DDesc, col2DLeg :: Sentence
col2DDesc = foldlSent_ [S "This instance model is based on our assumptions",
  S "regarding rigid body", makeRef2S assumpOT, makeRef2S assumpOD,
  S "collisions", makeRef2S assumpCT, S "Again, this does not take",
  S "damping", makeRef2S assumpDI, S "or constraints",
  makeRef2S assumpCAJI +:+. S "into account" +:+. makeRef2S assumpAD,
  makeRef2S ctrOfMassDD, makeRef2S impulseDD]


{--S "Ik is the moment of inertia of the k-th rigid body (kg m2)",
  S "t is a point in time, t0 denotes the initial time" `sC` 
  S "and tc denotes the time at collision (s)",
  S "P is the point of collision (m)"
--}

defList :: (Quantity a, MayHaveUnit a) => a -> Sentence
defList thing = foldlSent [ch thing, S "is the", phrase thing,
  sParen (fmtU EmptyS thing)]

col2DLeg = foldlSent_ $ map defList col2DLegTerms
  

{--displaceVectBtw  = cvR (ddcWDS "dispBtwVect" (compoundPhrase' 
  (QP.displacement ^. term) (cn "vector between the centre of mass of the k-th
  body and point P"))) (sub (QP.displacement ^. symbol) ) 
--}

{- Intro -}

instModIntro :: Sentence
instModIntro = foldlSent [S "The", phrase goal, makeRef2S linearGS, 
  S "is met by" +:+. (makeRef2S transMot `sAnd` makeRef2S col2D),
  S "The", phrase goal, makeRef2S angularGS, S "is met by",
  makeRef2S rotMot `sAnd` makeRef2S col2D]
