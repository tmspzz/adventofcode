#!/usr/bin/env stack
-- stack --system-ghc --resolver lts-14.16 script

{-# LANGUAGE OverloadedStrings #-} 

import Control.Applicative ((<**>))

import Data.List
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = Text.readFile "input.txt"
    <**> return (((map $ read . Text.unpack) . (concatMap $ Text.splitOn ",") . Text.lines) :: Text.Text -> [Integer])         
    >>= \program -> do
            let restoredMemory = setAt 2 2 $ setAt 1 12 program
            putStrLn $ "Restored Result: " ++ show (head $ runProgram restoredMemory restoredMemory)
        
            let (noun, verb) = head $ dropWhile (\(noun, verb) ->
                                                    let restoredMemory = setAt 1 noun $ setAt 2 verb program in
                                                    head (runProgram restoredMemory restoredMemory) /= 19690720
                                                ) ((,) <$> [0..99] <*> [0..99])
            
            putStrLn $ "100 * " ++ show noun ++ " + " ++ show verb ++ " is: " ++ show (100 * noun + verb)

        

test :: [Integer]
test = let program = [1,9,10,3,2,3,11,0,99,30,40,50]
           memory = program
           in runProgram program memory

runProgram :: [Integer] -> [Integer] -> [Integer]
runProgram [] memory = memory
runProgram program@(opcode:tail) memory =
    case opcode of
        1 ->
            let [parameter1Index, parameter2Index, storeIndex] = take 3 tail
                parameter1 = memory `genericIndex` parameter1Index
                parameter2 = memory `genericIndex` parameter2Index
                alteredMemory = setAt storeIndex (parameter1 + parameter2) memory 
                remainingProgram = drop 3 tail
                in runProgram remainingProgram alteredMemory 
        2 -> 
            let [parameter1Index, parameter2Index, storeIndex] = take 3 tail
                parameter1 = memory `genericIndex` parameter1Index
                parameter2 = memory `genericIndex` parameter2Index
                alteredMemory = setAt storeIndex (parameter1 * parameter2) memory 
                remainingProgram = drop 3 tail
                in runProgram remainingProgram alteredMemory 
        99 ->
            memory
        n -> error $ "Unrecognized opcode " ++ show n


setAt :: Integer -> a -> [a] -> [a]
setAt index value list = genericTake index list  ++ value:genericDrop (index + 1) list
