module Drasil.SSP.Unitals where --export all of it

import Language.Drasil
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.ShortHands
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.NounPhrase.Combinators as NP
import qualified Language.Drasil.Sentence.Combinators as S

import Drasil.SSP.Defs (fsConcept)

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (degree, metre, m_3, newton, pascal, specificWeight)

import Data.Drasil.Units.Physics (forcePerMeterU)

import Data.Drasil.Concepts.Math (cartesian, xCoord, xDir, yCoord, yDir,
  zCoord, zDir)
import Data.Drasil.Concepts.Physics (gravity)

import Data.Drasil.Quantities.Math (area, pi_, unitVectj)
import Data.Drasil.Quantities.PhysicalProperties (density, mass, specWeight, 
  vol)
import Data.Drasil.Quantities.Physics (acceleration, displacement, distance,
  force,  gravitationalAccel, height, moment2D, pressure, subX, subY, subZ, 
  supMax, supMin, torque, weight, positionVec)


symbols :: [DefinedQuantityDict]
symbols = map dqdWr inputs ++ map dqdWr outputs ++ 
  map dqdWr units ++ map dqdWr unitless

---------------------------
-- Imported UnitalChunks --
---------------------------
{-
SM.mobShear, SM.shearRes <- currently not used
SM.poissnsR, SM.elastMod <- Used to make UncertQ
-}
genericF = force
genericA = area
genericM = moment2D

-- FIXME: These need to be imported here because they are used in generic TMs/GDs that SSP also imports. Automate this?
genericV = vol
genericW = weight
genericSpWght = specWeight
accel = acceleration
genericMass = mass
gravAccel = gravitationalAccel
dens = density
genericH = height
genericP = pressure
genericR = displacement
genericT = torque
posVec = positionVec

-------------
-- HELPERS --
-------------
wiif :: String
wiif  = "without the influence of interslice forces"

--------------------------------
-- START OF CONSTRAINEDCHUNKS --
--------------------------------

constrained :: [ConstrainedChunk]
constrained = map cnstrw inputsWUncrtn ++ map cnstrw outputs

inputsWUncrtn :: [UncertQ]
inputsWUncrtn = [slopeDist, slopeHght, waterDist, waterHght, xMaxExtSlip, 
  xMaxEtrSlip, xMinExtSlip, xMinEtrSlip, yMaxSlip, yMinSlip, effCohesion, 
  fricAngle, dryWeight, satWeight, waterWeight]

inputsNoUncrtn :: [DefinedQuantityDict]
inputsNoUncrtn = [constF]

inputs :: [DefinedQuantityDict]
inputs = map dqdWr inputsWUncrtn ++ map dqdWr inputsNoUncrtn

outputs :: [ConstrConcept]
outputs = [fs, coords]

{-
monotonicIn :: [Constraint]  --FIXME: Move this?
monotonicIn = [physc $ \_ -> -- FIXME: Hack with "index" !
  (idx xi (sy index) $< idx xi (sy index + 1) $=> idx yi (sy index) $< idx yi (sy index + 1))]
-}

slopeDist, slopeHght, waterDist, waterHght, xMaxExtSlip, xMaxEtrSlip, 
  xMinExtSlip, xMinEtrSlip, yMaxSlip, yMinSlip, effCohesion, fricAngle, 
  dryWeight, satWeight, waterWeight :: UncertQ


{-Intput Variables-}
--FIXME: add (x,y) when we can index or make related unitals
--FIXME: add constraints to coordinate unitals when that is possible (constraints currently in the Notes section of the crtSlpId IM instead)

slopeDist = uq (constrained' (makeUCWDS "x_slope,i"
  (nounPhraseSent $ plural xCoord `S.of_` S "the slope")
  (plural xCoord `S.of_` S "points on the soil slope")
  (sub (vec lX) lSlope) metre) [] (exactDbl 0)) defaultUncrt

slopeHght = uq (constrained' (makeUCWDS "y_slope,i"
  (nounPhraseSent $ plural yCoord `S.of_` S "the slope")
  (plural yCoord `S.of_` S "points on the soil slope")
  (sub (vec lY) lSlope) metre) [] (exactDbl 0)) defaultUncrt

waterDist = uqc "x_wt,i" (nounPhraseSent $ plural xCoord `S.of_` S "the water table")
  "x-positions of the water table"
  (sub (vec lX) lWatTab) metre Real [] (exactDbl 0) defaultUncrt

waterHght = uqc "y_wt,i" (nounPhraseSent $ plural yCoord `S.of_` S "the water table")
  "heights of the water table"
  (sub (vec lY) lWatTab) metre Real [] (exactDbl 0) defaultUncrt

xMaxExtSlip = uq (constrained' (makeUCWDS "x_slip^maxExt"
  (nounPhraseSent $ S "maximum exit" +:+ phrase xCoord)
  (S "the maximum potential" +:+ phrase xCoord +:+ S "for the exit point of a slip surface")
  (sup (sub lX lSlip) lMaxExt) metre) [] (exactDbl 100)) defaultUncrt

xMaxEtrSlip = uq (constrained' (makeUCWDS "x_slip^maxEtr" 
  (nounPhraseSent $ S "maximum entry" +:+ phrase xCoord)
  (S "the maximum potential" +:+ phrase xCoord +:+ S "for the entry point of a slip surface")
  (sup (sub lX lSlip) lMaxEtr) metre) [] (exactDbl 20)) defaultUncrt
  
xMinExtSlip = uq (constrained' (makeUCWDS "x_slip^minExt"
  (nounPhraseSent $ S "minimum exit" +:+ phrase xCoord)
  (S "the minimum potential" +:+ phrase xCoord +:+ S "for the exit point of a slip surface")
  (sup (sub lX lSlip) lMinExt) metre) [] (exactDbl 50)) defaultUncrt

xMinEtrSlip = uq (constrained' (makeUCWDS "x_slip^minEtr"
  (nounPhraseSent $ S "minimum entry" +:+ phrase xCoord)
  (S "the minimum potential" +:+ phrase xCoord +:+ S "for the entry point of a slip surface")
  (sup (sub lX lSlip) lMinEtr) metre) [] (exactDbl 0)) defaultUncrt

yMaxSlip = uq (constrained' (makeUCWDS "y_slip^max"
  (nounPhraseSent $ S "maximum" +:+ phrase yCoord)
  (S "the maximum potential" +:+ phrase yCoord `S.of_` S "a point on a slip surface")
  (supMax (sub lY lSlip)) metre) [] (exactDbl 30)) defaultUncrt

yMinSlip = uq (constrained' (makeUCWDS "y_slip^min"
  (nounPhraseSent $ S "minimum" +:+ phrase yCoord)
  (S "the minimum potential" +:+ phrase yCoord `S.of_` S "a point on a slip surface")
  (supMin (sub lY lSlip)) metre) [] (exactDbl 0)) defaultUncrt

effCohesion = uqc "c'" (cn "effective cohesion")
  "the internal pressure that sticks particles of soil together"
  (prime $ variable "c") pascal Real [gtZeroConstr] (exactDbl 10000) defaultUncrt

fricAngle = uqc "varphi'" (cn "effective angle of friction")
  ("the angle of inclination with respect to the horizontal axis of " ++
  "the Mohr-Coulomb shear resistance line") --http://www.geotechdata.info
  (prime vPhi) degree Real [physc $ Bounded (Exc, exactDbl 0) (Exc, exactDbl 90)]
  (exactDbl 25) defaultUncrt

dryWeight = uqc "gamma" (cn "soil dry unit weight")
  "the weight of a dry soil/ground layer divided by the volume of the layer"
  (sub lGamma lDry) specificWeight Real [gtZeroConstr]
  (exactDbl 20000) defaultUncrt

satWeight = uqc "gamma_sat" (cn "soil saturated unit weight")
  "the weight of saturated soil/ground layer divided by the volume of the layer"
  (sub lGamma lSat) specificWeight Real [gtZeroConstr]
  (exactDbl 20000) defaultUncrt

waterWeight = uqc "gamma_w" (cn "unit weight of water")
  "the weight of one cubic meter of water"
  (sub lGamma lW) specificWeight Real [gtZeroConstr]
  (exactDbl 9800) defaultUncrt

constF :: DefinedQuantityDict
constF = dqd' (dcc "const_f" (nounPhraseSP "decision on f") 
  ("a Boolean decision on which form of f the user desires: constant if true," ++
  " or half-sine if false")) (const (variable "const_f")) Boolean Nothing

{-Output Variables-} --FIXME: See if there should be typical values
fs, coords  :: ConstrConcept
fs = constrained' (dqd' fsConcept (const $ sub cF lSafety) Real Nothing)
  [gtZeroConstr] (exactDbl 1)

fsMin :: DefinedQuantityDict -- This is a hack to remove the use of indexing for 'min'.
fsMin = dqd' (dcc "fsMin" (cn "minimum factor of safety") 
  "the minimum factor of safety associated with the critical slip surface")
  (const $ supMin (eqSymb fs)) Real Nothing 
-- Once things are converted to the new style of instance models, this will
-- be removed/fixed.

coords = constrainedNRV' (makeUCWDS "(x,y)" (cn "cartesian position coordinates")
  (P lY +:+ S "is considered parallel to the direction of the force of" +:+
   phrase gravity `S.and_` P lX +:+ S "is considered perpendicular to" +:+ P lY)
  lCoords metre) []

---------------------------
-- START OF UNITALCHUNKS --
---------------------------

units :: [UnitalChunk]
units = map ucw [accel, genericMass, genericF, genericA, genericM, genericV,
  genericW, genericSpWght, gravAccel, dens, genericH, genericP, genericR, 
  genericT, nrmShearNum, nrmShearDen, slipHght, xi, yi, zcoord, critCoords, 
  slipDist, mobilizedShear, resistiveShear, mobShrI, shrResI, shearFNoIntsl, 
  shearRNoIntsl, slcWght, watrForce, intShrForce, baseHydroForce, 
  surfHydroForce, totNrmForce, nrmFSubWat, surfLoad, baseAngle, surfAngle, 
  impLoadAngle, baseWthX, baseLngth, surfLngth, midpntHght, 
  porePressure, sliceHght, sliceHghtW, fx, fy, fn, ft, nrmForceSum, watForceSum, 
  sliceHghtRight, sliceHghtLeft, intNormForce, shrStress, totNormStress, tangStress,
  effectiveStress, effNormStress, dryVol, satVol, rotForce, momntArm, posVec]

accel, genericMass, genericF, genericA, genericM, genericV, genericW, 
  genericSpWght, gravAccel, dens, genericH, genericP, genericR, genericT, 
  nrmShearNum, nrmShearDen, slipDist, slipHght, xi, yi, zcoord, critCoords, 
  mobilizedShear, mobShrI, sliceHght, sliceHghtW, shearFNoIntsl, shearRNoIntsl,
  slcWght, watrForce, resistiveShear, shrResI, intShrForce, baseHydroForce, 
  surfHydroForce, totNrmForce, nrmFSubWat, surfLoad, baseAngle, surfAngle, 
  impLoadAngle, baseWthX, baseLngth, surfLngth, midpntHght, fx, fy, fn, ft, 
  nrmForceSum, watForceSum, sliceHghtRight, sliceHghtLeft, porePressure, 
  intNormForce, shrStress, totNormStress, tangStress, effectiveStress, 
  effNormStress, dryVol, satVol, rotForce, momntArm, posVec :: UnitalChunk
  
{-FIXME: Many of these need to be split into term, defn pairs as
         their defns are mixed into the terms.-}

intNormForce = makeUCWDS "G_i" (cn "interslice normal forces")
  (S "the forces per meter" `S.inThe` phrase zDir +:+
   S "exerted between each pair of adjacent slices")
  (vec cG) forcePerMeterU

slipHght = uc' "y_slip,i" (nounPhraseSent $ plural yCoord +:+ S "of the slip surface")
  "heights of the slip surface"
  (sub (vec lY) lSlip) metre

slipDist = makeUCWDS "x_slip,i" (nounPhraseSent $ plural xCoord +:+ S "of the slip surface")
  (plural xCoord `S.of_` S "points on the slip surface")
  (sub (vec lX) lSlip) metre

xi     = makeUCWDS "x_i" (nounPhraseSent $ phrase xCoord)
  (phraseNP (NP.the (xCoord `inThe` cartesian))) lX metre

yi     = makeUCWDS "y_i" (nounPhraseSent $ phrase yCoord)
  (phraseNP (NP.the (yCoord `inThe` cartesian))) lY metre

zcoord = makeUCWDS "z"   (nounPhraseSent $ phrase zCoord)
  (phraseNP (NP.the (zCoord `inThe` cartesian))) lZ metre

-- FIXME: the 'symbol' for this should not have { and } embedded in it.
-- They have been removed now, but we need a reasonable notation.
critCoords = makeUCWDS "(xcs,ycs)" (cn "critical slip surface coordinates")
  (S "the set" `S.of_` pluralNP (xCoord `and_PP` yCoord) +:+
   S "that describe the vertices of the critical slip surface")
  (Concat [sub (vec lX) lCSlip, label ",", sub (vec lY) lCSlip]) metre

mobilizedShear = uc' "mobilizedShear" (cn' "mobilized shear force")
  "the shear force in the direction of potential motion" cS newton

resistiveShear = makeUCWDS "resistiveShear" (cn' "resistive shear force")
  (S "the Mohr Coulomb frictional force that describes the limit" `S.of_`
    phrase mobilizedShear +:+ S "that can be withstood before failure")
  cP newton

mobShrI = makeUCWDS "mobShrFs" (cn' "mobilized shear force")
  (phraseNP (the mobilizedShear) +:+ S "per meter" `S.inThe` phrase zDir +:+
   S "for each slice")
  (vec cS) forcePerMeterU --FIXME: DUE TO ID THIS WILL SHARE THE SAME SYMBOL AS CSM.mobShear
              -- This is fine for now, as they are the same concept, but when this
              -- symbol is used, it is usually indexed at i. That is handled in
              -- Expr.

shrResI = makeUCWDS "shrRes" (cn "resistive shear forces")
  (S "the Mohr Coulomb frictional forces per meter" `S.inThe` phrase zDir +:+
   S "for each slice that describe the limit" `S.of_` phrase mobilizedShear +:+
   S "the slice can withstand before failure")
  (vec cP) forcePerMeterU --FIXME: DUE TO ID THIS WILL SHARE THE SAME SYMBOL AS CSM.shearRes
              -- This is fine for now, as they are the same concept, but when this
              -- symbol is used, it is usually indexed at i. That is handled in
              -- Expr.

shearFNoIntsl = makeUCWDS "T_i" (cn ("mobilized shear forces " ++ wiif)) 
  (pluralNP (the mobilizedShear) +:+ S "per meter" +:+ S wiif `S.inThe`
   phrase zDir +:+  S "for each slice")
  (vec cT) forcePerMeterU

shearRNoIntsl = makeUCWDS "R_i" (cn ("resistive shear forces " ++ wiif))
  (pluralNP (the resistiveShear) +:+ S "per meter" +:+ S wiif `S.inThe`
   phrase zDir +:+ S "for each slice")
  (vec cR) forcePerMeterU

slcWght = makeUCWDS "W_i" (cn "weights")
  (S "the downward force per meter" `S.inThe` phrase zDir +:+
   S "on each slice caused by" +:+ phrase gravity)
  (vec cW) forcePerMeterU

watrForce = makeUCWDS "H_i" (cn "interslice normal water forces") 
  (S "the normal water forces per meter" `S.inThe` phrase zDir +:+
   S "exerted" `S.inThe` phrase xDir +:+ S "between each pair of adjacent slices")
  (vec cH) forcePerMeterU

intShrForce = makeUCWDS "X_i" (cn "interslice shear forces") 
  (S "the shear forces per meter" `S.inThe` phrase zDir +:+ S "exerted between adjacent slices")
  (vec cX) forcePerMeterU

baseHydroForce = makeUCWDS "U_b,i" (cn "base hydrostatic forces")
  (S "the forces per meter" `S.inThe` phrase zDir +:+ S "from water pressure within each slice")
  (sub (vec cU) lBase) forcePerMeterU

surfHydroForce = makeUCWDS "U_t,i" (cn "surface hydrostatic forces")
  (S "the forces per meter" `S.inThe` phrase zDir +:+ S "from water pressure acting" +:+
   S "into each slice from standing water on the slope surface")
  (sub (vec cU) lSurface) forcePerMeterU

totNrmForce = makeUCWDS "N_i" (cn "normal forces")
  (S "the total reactive forces per meter" `S.inThe` phrase zDir +:+
   S "for each slice of a soil surface subject to a body resting on it")
  (vec cN) forcePerMeterU

nrmFSubWat = makeUCWDS "N'_i" (cn "effective normal forces")
  (S "the forces per meter" `S.inThe` phrase zDir +:+ S "for each slice of a soil surface" `sC`
   S "subtracting pore water reactive force from total reactive force") 
  (vec (prime $ variable "N")) forcePerMeterU

surfLoad = makeUCWDS "Q_i" (cn "external forces") 
  (S "the forces per meter" `S.inThe` phrase zDir +:+
   S "acting into the surface from the midpoint of each slice")
  (vec cQ) forcePerMeterU

baseAngle = uc' "alpha_i" (cn "base angles")
  "the angles between the base of each slice and the horizontal"
  (vec lAlpha) degree

surfAngle = uc' "beta_i" (cn "surface angles")
  "the angles between the surface of each slice and the horizontal"
  (vec lBeta) degree

impLoadAngle = uc' "omega_i" (cn "imposed load angles")
  "the angles between the external force acting into the surface of each slice and the vertical"
  (vec lOmega) degree

baseWthX = makeUCWDS "b_i" (cn "base width of slices")
  (S "the width of each slice" `S.inThe` phrase xDir)
  (vec lB) metre

baseLngth = uc' "l_b,i" (cn "total base lengths of slices") 
  "the lengths of each slice in the direction parallel to the slope of the base"
  (sub (vec cL) lB) metre

surfLngth = uc' "l_s,i" (cn "surface lengths of slices")
  "the lengths of each slice in the direction parallel to the slope of the surface"
  (sub (vec cL) lS) metre

midpntHght = makeUCWDS "h_i" (nounPhraseSent $ phrase yDir +:+ S "heights of slices")
  (S "the heights" `S.inThe` phrase yDir +:+ S "from the base of each slice" `S.toThe`
   S "slope surface, at the" +:+ phrase xDir +:+ S "midpoint of the slice")
  (vec lH) metre

porePressure = uc' "u" (cn "pore pressure")
  "the pressure that comes from water within the soil" lU pascal
  
shrStress = uc' "tau_i" (cn "shear strength")
  "the strength of a material against shear failure" (sup lTau (label "f")) pascal

sliceHght = makeUCWDS "h_z,i" (cn "heights of interslice normal forces")
  (pluralNP (height `inThePS` yDir) `S.the_ofThe` S "interslice normal forces on each slice")
  (subZ (vec lH)) metre

sliceHghtW = makeUCWDS "h_z,w,i" (cn "heights of the water table")
  (S "the heights" `S.inThe` phrase yDir +:+ S "from the base of each slice to the water table")
  (sub (vec lH) lHeights) metre

nrmShearNum = uc' "C_num,i" (cn "proportionality constant numerator")
  ("values for each slice that sum together to form the numerator of the " ++
  "interslice normal to shear force proportionality constant")
  (sub (vec cC) lNum) newton
  
nrmShearDen = uc' "C_den,i" (cn "proportionality constant denominator")
  ("values for each slice that sum together to form the denominator of the " ++
  "interslice normal to shear force proportionality constant")
  (sub (vec cC) lDen) newton

fx = makeUCWDS "fx" (nounPhraseSent $ phrase xCoord +:+ S "of the force")
  (S "the force acting" `S.inThe` phrase xDir) (subX cF) newton

fy = makeUCWDS "fy" (nounPhraseSent $ phrase yCoord +:+ S "of the force")
  (S "the force acting" `S.inThe` phrase yDir) (subY cF) newton

fn = uc' "F_n" (cn "total normal force") "component of a force in the normal direction"
  (sub cF (label "n")) newton

ft = uc' "F_t" (cn "tangential force") "component of a force in the tangential direction"
  (sub cF (label "t")) newton

nrmForceSum = uc' "F_x^G" (cn "sums of the interslice normal forces") 
  "the sums of the normal forces acting on each pair of adjacent interslice boundaries"
  (sup (subX (vec cF)) lNorm) newton

watForceSum = uc' "F_x^H" (cn "sums of the interslice normal water forces") 
  "the sums of the normal water forces acting on each pair of adjacent interslice boundaries"
  (sup (subX (vec cF)) lNormWat) newton

sliceHghtRight = uc' "h^R" (cn "heights of the right side of slices") 
  "the heights of the right side of each slice, assuming slice surfaces have negative slope"
  (sup (vec lH) lRight) metre

sliceHghtLeft = uc' "h^L" (cn "heights of the left side of slices") 
  "the heights of the left side of each slice, assuming slice surfaces have negative slope"
  (sup (vec lH) lLeft) metre

totNormStress = uc' "sigma" (cn' "total normal stress")
  "the total force per area acting on the soil mass" lSigma pascal

tangStress = uc' "tau" (cn' "tangential stress")
  "the shear force per unit area" lTau pascal

effectiveStress = uc' "sigma'" (cn' "effective stress")
  ("the stress in a soil mass that is effective in causing volume changes " ++
   "and mobilizes the shear strength arising from friction; represents the " ++
   "average stress carried by the soil skeleton")
  (prime lSigma) pascal

effNormStress = uc' "sigmaN'" (cn' "effective normal stress")
  ("the normal stress in a soil mass that is effective in causing volume " ++
   "changes; represents the average normal stress carried by the soil skeleton")
  (prime $ sub lSigma cN) pascal

dryVol = uc' "V_dry" (cn "volumes of dry soil")
  "the amount of space occupied by dry soil for each slice"
  (sub (vec cV) lDry) m_3

satVol = uc' "V_sat" (cn "volumes of saturated soil")
  "the amount of space occupied by saturated soil for each slice"
  (sub (vec cV) lSat) m_3

rotForce = uc' "F_rot" (cn "force causing rotation") 
  "a force in the direction of rotation" (sub cF lRot) newton
  
momntArm = uc' "r" (cn' "length of the moment arm") 
  "the distance between a force causing rotation and the axis of rotation"
  lR metre

----------------------
-- Unitless Symbols --
----------------------

unitless :: [DefinedQuantityDict]
unitless = [earthqkLoadFctr, normToShear, scalFunc, numbSlices, minFunction, 
  mobShrC, shrResC, index, pi_, varblV, fsMin, unitVectj]

earthqkLoadFctr, normToShear, scalFunc, numbSlices,
  minFunction, mobShrC, shrResC, index, varblV :: DefinedQuantityDict

earthqkLoadFctr = dqd' (dcc "K_c" (nounPhraseSP "seismic coefficient")
  ("the proportionality factor of force that weight pushes outwards; " ++
   "caused by seismic earth movements"))
  (const $ sub cK lCoeff) Real Nothing 

normToShear = dqd' (dcc "lambda" (nounPhraseSP "proportionality constant")
  "the ratio of the interslice normal to the interslice shear force")
  (const lLambda) Real Nothing

scalFunc = dqd' (dccWDS "f_i" 
  (nounPhraseSP "interslice normal to shear force ratio variation function")
  (S "a function" `S.of_` phraseNP (distance `inThe` xDir) +:+
   S "that describes the variation of the interslice normal to shear ratio"))
  (const (vec lF)) Real Nothing 

numbSlices = dqd' (dcc "n" (nounPhraseSP "number of slices")
  "the number of slices into which the slip surface is divided")
  (const lN) Natural Nothing

minFunction = dqd' (dcc "Upsilon" (nounPhraseSP "minimization function")
  "generic minimization function or algorithm")
  (const cUpsilon) Real Nothing

mobShrC = dqd' (dcc "Psi"
  (nounPhraseSP "second function for incorporating interslice forces into shear force")
  ("the function for converting mobile shear " ++ wiif ++
   ", to a calculation considering the interslice forces"))
  (const (vec cPsi)) Real Nothing

shrResC = dqd' (dcc "Phi"
  (nounPhraseSP "first function for incorporating interslice forces into shear force")
  ("the function for converting resistive shear " ++ wiif ++
   ", to a calculation considering the interslice forces"))
  (const (vec cPhi)) Real Nothing

--------------------
-- Index Function --
--------------------

varblV = dqd' (dcc "varblV" (nounPhraseSP "local index")
  "used as a bound variable index in calculations")
  (const lV) Natural Nothing

index = dqd' (dcc "index" (nounPhraseSP "index")
  "a number representing a single slice")
  (const lI) Natural Nothing 

-- FIXME: move to drasil-lang
indx1 :: (ExprC r, LiteralC r, Quantity a) => a -> r
indx1 a = idx (sy a) (int 1)

indxn :: (ExprC r, Quantity a) => a -> r
indxn a = idx (sy a) (sy numbSlices)

inxi, inxiP1, inxiM1 :: (ExprC r, LiteralC r, Quantity e) => e -> r
inxiP1 e = inx e 1
inxi   e = inx e 0
inxiM1 e = inx e (-1)

inx :: (ExprC r, LiteralC r, Quantity e) => e -> Integer -> r
inx e n 
  | n < 0     = idx (sy e) (sy index $- int (-n))
  | n == 0    = idx (sy e) (sy index)
  | otherwise = idx (sy e) (sy index `addI` int n)

sum1toN :: (ExprC r, LiteralC r) => r -> r
sum1toN = defsum (eqSymb index) (int 1) (sy numbSlices)

-- Labels

lBase, lCoeff, lCoords, lCSlip, lDen, lDry, lHeights, lLeft, lMaxEtr, lMaxExt,
  lMinEtr, lMinExt, lNorm, lNormWat, lNum, lRight, lRot, lSafety, lSat, lSlip,
  lSlope, lSurface, lWatTab :: Symbol
lBase    = label "b"
lCoeff   = label "c"
lCoords  = label "(x,y)"
lCSlip   = label "cs"
lDen     = label "den"
lDry     = label "dry"
lHeights = label "z,w"
lLeft    = label "L"
lMaxEtr  = label "maxEtr"
lMaxExt  = label "maxExt"
lMinEtr  = label "minEtr"
lMinExt  = label "minExt"
lNorm    = label "G"
lNormWat = label "H"
lNum     = label "num"
lRight   = label "R"
lRot     = label "rot"
lSafety  = label "S"
lSat     = label "sat"
lSlip    = label "slip"
lSlope   = label "slope"
lSurface = label "g"
lWatTab  = label "wt"
