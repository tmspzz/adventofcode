#!/usr/bin/env stack
-- stack --system-ghc --resolver lts-14.16 script

{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative                      ( (<$>) )

import           Data.Char                                ( isDigit
                                                          , toUpper
                                                          )
import qualified Text.ParserCombinators.ReadP  as P
import qualified Text.Read                     as R
                                                          ( readP_to_Prec )
import           Data.List
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Set                      as Set
import qualified Text.Read                     as R
import qualified Text.Read.Lex                 as R

import           Debug.Trace

data Direction = R | L | U | D deriving (Show, Eq, Read)
data WirePath = WirePath { direction :: Direction,  amount :: Integer} deriving (Show, Eq)

-- All credit for this instace to @jaspervdj https://gist.github.com/jaspervdj/86a0179f44e083234d4c2377103a357a
instance R.Read WirePath where
  readPrec = R.readP_to_Prec $ const $ do
    c <- P.get
    s <- P.munch1 isDigit
    return $ WirePath { direction = read [toUpper c], amount = read s }

data Point = Point { x :: Integer, y :: Integer } deriving (Show, Eq)
data Segment = Segment { p1 :: Point, p2 :: Point } deriving (Show, Eq)

main :: IO ()
main = test --solvePathsFromFile "example.txt"


solvePathsFromFile :: String -> IO ()
solvePathsFromFile file = do
  withPaths@[wirePath1, wirePath2] <-
    (map (map (read . Text.unpack)) :: [[Text.Text]] -> [[WirePath]])
    .   map (Text.splitOn ",")
    .   Text.lines
    <$> (Text.readFile file)
  let [segs1, segs2] = wirePathsToSegments <$> withPaths
  let crossings = map (\i -> (i, taxiCabDistanceFromOrigin i))
                      (tail . concat $ intersections <$> segs1 <*> segs2)

  let
    lengthsToIntersections = map
      (\intersection ->
        let
          w1PathWithOutEndSegment =
            segs1 `pathWithoutEndSegmentBefore` fst intersection
          w1PathLastSegment =
            head $ drop (length w1PathWithOutEndSegment) segs1
          w1LastSteps = w1PathLastSegment `stepsToPoint` fst intersection
          w2PathWithOutEndSegment =
            segs2 `pathWithoutEndSegmentBefore` fst intersection
          w2PathLastSegment =
            head $ drop (length w2PathWithOutEndSegment) segs2
          w2LastSteps = w2PathLastSegment `stepsToPoint` fst intersection
        in
          ( intersection
          , sum (stepsForSegment <$> w1PathWithOutEndSegment) + w1LastSteps
          , sum (stepsForSegment <$> w2PathWithOutEndSegment) + w2LastSteps
          )
      )
      crossings

  print
    $ ((\(_, dx, dy) -> dx + dy) :: ((Point, Integer), Integer, Integer)
        -> Integer
      )
        (minInList lengthsToIntersections)

 where
  pathWithoutEndSegmentBefore segments point =
    takeWhile (\s -> not $ s `crossesPoint` point) segments

applyWirePathDisplacementToPoint :: Point -> WirePath -> Point
applyWirePathDisplacementToPoint (Point x y) (WirePath direction amount) =
  case direction of
    R -> Point (x + amount) y
    L -> Point (x - amount) y
    U -> Point x (y + amount)
    D -> Point x (y - amount)

wirePathsToSegments :: [WirePath] -> [Segment]
wirePathsToSegments (p : ps) = reverse $ foldl
  (\acc element ->
    let (Segment _ endPoint) = head acc
    in  Segment endPoint (applyWirePathDisplacementToPoint endPoint element)
          : acc
  )
  [Segment (Point 0 0) (applyWirePathDisplacementToPoint (Point 0 0) p)]
  ps

intersections :: Segment -> Segment -> [Point]
intersections s1@(Segment (Point s1StartX s1StartY) (Point s1EndX s1EndY)) s2@(Segment (Point s2StartX s2StartY) (Point s2EndX s2EndY))
  = case (xIntersections, yIntersections) of
    (_  , [] ) -> []
    ([] , _  ) -> []
    (xs , [y]) -> foldl (\acc x -> Point x y : acc) [] xs
    ([x], ys ) -> foldl (\acc y -> Point x y : acc) [] ys
    a          -> error $ "Inputs don't follow challenge spec: " ++ show a
 where
  s1XSet         = Set.fromList [min s1StartX s1EndX .. max s1StartX s1EndX]
  s1YSet         = Set.fromList [min s1StartY s1EndY .. max s1StartY s1EndY]
  s2XSet         = Set.fromList [min s2StartX s2EndX .. max s2StartX s2EndX]
  s2YSet         = Set.fromList [min s2StartY s2EndY .. max s2StartY s2EndY]
  xIntersections = Set.toList $ s1XSet `Set.intersection` s2XSet
  yIntersections = Set.toList $ s1YSet `Set.intersection` s2YSet

taxiCabDistanceFromOrigin :: Point -> Integer
taxiCabDistanceFromOrigin (Point x y) = abs x + abs y

minInList
  :: [((Point, Integer), Integer, Integer)]
  -> ((Point, Integer), Integer, Integer)
minInList (triplet@(_, distanceX, distanceY) : rest) = findMin triplet rest
 where
  findMin currentMin@(_, minDistanceX, minDistanceY) (triplet@(_, distanceX, distanceY) : rest)
    | distanceX + distanceY >= minDistanceX + minDistanceY
    = findMin currentMin rest
    | otherwise
    = findMin triplet rest
  findMin currentMin [] = currentMin
minInList [] = error "Inputs don't follow challenge spec."

crossesPoint :: Segment -> Point -> Bool
crossesPoint s@(Segment (Point startX startY) (Point endX endY)) (Point x y) =
  let xs = Set.fromList [min startX endX .. max startX endX]
      ys = Set.fromList [min startY endY .. max startY endY]
  in  x `Set.member` xs && y `Set.member` ys

stepsToPoint :: Segment -> Point -> Integer
stepsToPoint s@(Segment (Point startX startY) _) (Point x y) =
  abs (startX - x) + abs (startY - y)

stepsForSegment :: Segment -> Integer
stepsForSegment s1@(Segment (Point startX startY) (Point endX endY)) =
  abs (startX - endX) + abs (startY - endY)

test :: IO ()
test =
  solvePathsFromFile "example.txt"
    <* solvePathsFromFile "test1.txt"
    <* solvePathsFromFile "test2.txt"
    <* solvePathsFromFile "input.txt"

