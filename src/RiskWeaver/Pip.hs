module RiskWeaver.Pip where

newtype Polygon = Polygon [(Double, Double)] deriving (Show, Eq)

newtype Point = Point (Double, Double) deriving (Show, Eq)

-- | When given a polygon and a point, returns True if the point is inside the polygon.
pointInPolygon :: Polygon -> Point -> Bool
pointInPolygon (Polygon polygon) (Point point) =
  let points = zip polygon (tail polygon)
      wn = flip map points $ \(vi, vii) ->
        if ((snd vi <= snd point) && (snd vii > snd point))
          then
            let vt = (snd point - snd vi) / (snd vii - snd vi)
             in if (fst point < (fst vi + (vt * (fst vii - fst vi))))
                  then 1
                  else 0
          else
            if ((snd vi > snd point) && (snd vii <= snd point))
              then
                let vt = (snd point - snd vi) / (snd vii - snd vi)
                 in if (fst point < (fst vi + (vt * (fst vii - fst vi))))
                      then -1
                      else 0
              else 0
   in (foldl (+) 0 wn) /= (0 :: Int)
