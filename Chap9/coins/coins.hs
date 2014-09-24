import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let
    (firtsCoin, newGen) = random gen
    (sndCoin, newGen') = random newGen
    (thirdCoin, _) = random newGen'
  in
   (firtsCoin, sndCoin, thirdCoin)

