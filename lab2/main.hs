module Main where

import Control.Monad
import Control.Concurrent
import Data.Time.Clock.POSIX
import Data.Bits
import Control.Parallel.Strategies
import Prelude
import Debug.Trace


integrate :: (Double->Double) -> Double -> Double -> Double -> Double
integrate func a b eps = searchIntegral func (a, b, eps, 1)


searchIntegral :: (Double->Double) -> (Double, Double, Double, Integer) -> Double
searchIntegral func params = do
  let (a, b, eps, lastSteps)  = params
  let currValues = parMap rdeepseq (integrateParallelPacked func) [
        (a, b, lastSteps),
        (a, b, (lastSteps+1))]

  if (abs((currValues !! 0) - (currValues !! 1)) < eps)
    then (currValues !! 1)
    else  searchIntegral func (a, b, eps, lastSteps+1)

integrateParallelPacked:: (Double->Double) -> (Double, Double, Integer) -> Double
integrateParallelPacked func params = do 
  let (a, b, totalSteps)  = params
  integrateParallel func a b totalSteps

integrateParallel :: (Double->Double) -> Double -> Double -> Integer -> Double
integrateParallel func a b totalSteps = do
  let parts = 4
  let part = (b-a)/parts
  let p = parMap rdeepseq (integratePacked func) [
        (a + part*0, a + part*1, totalSteps),
        (a + part*1, a + part*2, totalSteps),
        (a + part*2, a + part*3, totalSteps),
        (a + part*3, a + part*4, totalSteps)]
  --print p
  sum p

integratePacked :: (Double->Double) -> (Double, Double, Integer) -> Double
integratePacked func params = do
  let (a, b, totalSteps)  = params
  integratePart func a b totalSteps



integratePart :: (Double -> Double) -> Double -> Double -> Integer -> Double
integratePart f xmin xmax intervals =
    let step = (xmax - xmin) / fromInteger intervals
        x1 = xmin + step
        x2 = x1   + step
        s = sum $ map f [x1,x2 .. xmax]
    in  s * step


f :: Double -> Double
f x = x ** 2

main :: IO ()
main = do
    -- ghc .\main.hs & .\main.exe  
    print "Integrating"
    print $ integrate f 0.0 1.0 0.001

    