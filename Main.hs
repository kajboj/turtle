import Control.Applicative
import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.Vector (rotateV)
import Graphics.Gloss.Geometry.Angle (degToRad)

import Command
import Sierpinski

screenRes = (1366, 768)::(Int, Int)
proportion = (2, 2)
windowSize = let (w, h) = screenRes
                 (a, b) = proportion
               in (w `div` a, h `div` b)

type Line = (G.Point, G.Point)

type Turtle = (Float, G.Point)

main =
  let points = turtle (Rot (-90):sierp 7)
    in do
      putStrLn $ show points
      G.display (G.InWindow "Turtle" windowSize (0, 0))
        G.white $ G.line points

trans :: G.Picture -> G.Picture
trans pic = let (_, height) = windowSize
              in G.Translate 0 (fromIntegral (-height)/2) $ pic

drawLines :: [Line] -> G.Picture
drawLines = G.Pictures . map (G.line . toList)

toList :: (a, a) -> [a]
toList (x, y) = [x, y]

turtle :: [Command] -> [G.Point]
turtle = fst . foldl move ([initialPosition], initialTurtle)
  where
    initialTurtle = (0, initialPosition)
    initialPosition = (0, 0)

move :: ([G.Point], Turtle) -> Command -> ([G.Point], Turtle)
move (points, (angle, position)) (Rot n) = (points, (angle+n, position))
move (points, (angle, position)) (Fwd n) = (newPosition:points, (angle, newPosition))
  where
    newPosition = position `add` rotate angle (0, n)

rotate :: Float -> G.Vector -> G.Vector
rotate radians point = rotateV (degToRad radians) point

add :: G.Vector -> G.Vector -> G.Vector
add (x1, y1) (x2, y2) = (x1+x2, y1+y2)
