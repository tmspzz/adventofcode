#!/usr/bin/env stack
-- stack --system-ghc --resolver lts-14.16 script

import Control.Applicative ((<**>))
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = Text.readFile "input.txt" 
    <**> return (((map $ read . Text.unpack) . Text.lines) :: Text.Text -> [Integer]) 
    >>= \ints -> do
        let totalMassBeforeAccounting = foldl (\acc mass -> fuelForModuleWithMass mass + acc) 0 ints
        putStrLn $ "Total fuel before accounting for fuel mass: " ++ show totalMassBeforeAccounting
        let totalMassAfterAccounting = foldl (\acc mass -> fuelForModuleWithMassAccountingForFuelMass mass + acc) 0 ints
        putStrLn $ "Total fuel after accounting for fuel mass: " ++ show totalMassAfterAccounting

fuelForModuleWithMass :: Integer -> Integer
fuelForModuleWithMass mass = (mass `div` 3) - 2

fuelForModuleWithMassAccountingForFuelMass :: Integer -> Integer
fuelForModuleWithMassAccountingForFuelMass mass
                                            | mass < 0 = 0
                                            | otherwise = let fuelMass = fuelForModuleWithMass mass in
                                                            fuelMass + fuelForModuleWithMassAccountingForFuelMass fuelMass   
