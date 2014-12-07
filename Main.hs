import Control.Applicative
import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.Vector (rotateV)
import Graphics.Gloss.Geometry.Angle (degToRad)

screenRes = (1366, 768)::(Int, Int)
proportion = (2, 2)
windowSize = let (w, h) = screenRes
                 (a, b) = proportion
               in (w `div` a, h `div` b)

type Line = (G.Point, G.Point)

type Turtle = (Float, G.Point)
data Command = Fwd Float | Rot Float

main = do
  let lines = turtle [Fwd 100, Rot 90, Fwd 100]
    in
      G.display (G.InWindow "Nice Window" windowSize (0, 0))
        G.white $ drawLines lines

trans :: G.Picture -> G.Picture
trans pic = let (_, height) = windowSize
              in G.Translate 0 (fromIntegral (-height)/2) $ pic

drawLines :: [Line] -> G.Picture
drawLines = G.Pictures . map (G.line . toList)

toList :: (a, a) -> [a]
toList (x, y) = [x, y]

turtle :: [Command] -> [Line]
turtle = fst . foldl move ([], initialTurtle)
  where
    initialTurtle = (0, (0, 0))

move :: ([Line], Turtle) -> Command -> ([Line], Turtle)
move (lines, (angle, position)) (Rot n) = (lines, (angle+n, position))
move (lines, (angle, position)) (Fwd n) = (line:lines, (angle, newPosition))
  where
    line = (position, newPosition)
    newPosition = position `add` rotate angle (0, n)

rotate :: Float -> G.Vector -> G.Vector
rotate radians point = rotateV (degToRad radians) point

add :: G.Vector -> G.Vector -> G.Vector
add (x1, y1) (x2, y2) = (x1+x2, y1+y2)

