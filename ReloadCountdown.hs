module Main where

import Test.HUnit

stoppedIsTrueAtInitialState =
  True ~?= stopped (startCountdown 0)

startCountdown seconds = seconds

decrease seconds amount 
  | amount > seconds = 0 
  | otherwise = remaining
  where
    remaining = seconds - amount
    
  
stopped seconds
  | seconds == 0 = True
  | otherwise = False

gun seconds 
  | seconds == 0 = "loaded"
  | otherwise = "unloaded"
  
canFire gun 
  | gun == "loaded" = True
  | otherwise = False
      
fire gun = "unloaded"

makeNewGun = gun (startCountdown 0)

stoppedIsFalseIfTheCountdownHasBeenStarted =
  False ~?= stopped (startCountdown 10)
  
stoppedIsFalseIfTheCountdownHasBeenInitializeBy10AndDecreasedBy5 =
  False ~?= stopped (decrease (startCountdown 10) 5)
  
stoppedIsTrueIfTheCountdownHasBeenInitializeBy5AndDecreasedBy6 =
  True ~?= stopped (decrease (startCountdown 5) 6)

weCanFireIfWeGotAGun =
  True ~?= canFire makeNewGun

weCantFireIfTheGunIsLoaded =
  False ~?= canFire (fire makeNewGun)

weCantFireIfTheGunIsLoading =
  False ~?= canFire (gun ( decrease  (startCountdown 10) 5))
  
tests = TestList [
  stoppedIsTrueAtInitialState,
  stoppedIsFalseIfTheCountdownHasBeenStarted,
  stoppedIsFalseIfTheCountdownHasBeenInitializeBy10AndDecreasedBy5,
  stoppedIsTrueIfTheCountdownHasBeenInitializeBy5AndDecreasedBy6,
  weCanFireIfWeGotAGun,
  weCantFireIfTheGunIsLoaded,
  weCantFireIfTheGunIsLoading
  
  ]


main = runTestTT tests
