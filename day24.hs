import Data.Maybe
import qualified Data.Map as M

data Colour = Black | White deriving (Eq, Show)
type Tile = (Int, Int)
type Tiles = M.Map Tile Colour

-- helper functions
east y x = if odd y then x+1 else x
west y x = if even y then x-1 else x

e (x,y) = (x+1, y)
w (x,y) = (x-1, y)
ne (x,y) = (east y x, y-1)
nw (x,y) = (west y x, y-1)
se (x,y) = (east y x, y+1)
sw (x,y) = (west y x, y+1)

-- flip initial tiles

flip' :: Tile -> String -> Tiles -> Tiles
flip' (x,y) [] tiles =
    if M.member (x,y) tiles
    then M.delete (x,y) tiles
    else M.insert (x,y) Black tiles
flip' xy (d1:dirs) tiles
    | d1 == 'e' = flip' (e xy) dirs tiles
    | d1 == 'w' = flip' (w xy) dirs tiles
    | otherwise = let xy' = case [d1, head dirs] of
                                "ne" -> ne xy
                                "nw" -> nw xy
                                "se" -> se xy
                                "sw" -> sw xy
      in flip' xy' (tail dirs) tiles

-- conway

addNbrs :: Tile -> Colour -> Tiles -> Tiles
addNbrs xy White tiles = tiles
addNbrs xy Black tiles = foldr addWhite tiles nbrs
  where
    nbrs = map ($xy) [e,w,ne,nw,se,sw]
    addWhite nbr tiles =
        if nbr `M.member` tiles then tiles
        else M.insert nbr White tiles

evolve :: Tiles -> Tile -> Colour -> Colour
evolve tiles xy c
    | c == Black = if b == 0 || b > 2 then White else Black
    | c == White = if b == 2 then Black else White
  where
    b = length.filter (==Black)
        .mapMaybe (flip M.lookup tiles)
        $ map ($xy) [e,w,ne,nw,se,sw]

evolveAll :: Tiles -> Tiles
evolveAll tiles = let tiles' = M.foldrWithKey addNbrs tiles tiles
    in M.mapWithKey (evolve tiles') tiles'


main = interact $ show
    .M.size.M.filter (==Black)
    .(!!100).iterate evolveAll
    .foldr (flip' (0,0)) M.empty
    .filter (not.null).lines

