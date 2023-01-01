-- | Tree Fractal.
--	Based on ANUPlot code by Clem Baker-Finch.
--	
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import qualified Data.Set as S


data Pos = Pos 
	{
		x :: Float,
		y :: Float,
		z :: Float,
	}

data GamePos = AlignedPos Pos | UnalignedPos Pos
pos :: GamePos -> Pos
pos (AlignedPos p) = p
pos (UnalignedPos p) = p

add :: Pos -> Pos -> Pos
add (Pos x1 y1 z1) (Pos x2 y2 z2) = Pos (x1+x2) (y1+y2) (z1+z2)


data TracerWorld = TracerWorld
	{
		playerPos :: GamePos,
		keys :: S.Set Key
	}

main = play (InWindow "Test" (640, 480) (20,  20))
		black
		60
		TracerWorld { playerPos = (UnalignedPos Pos {x=0, y=0}), keys=S.empty }
		renderWorld
		handleEvent
		stepWorld


renderWorld :: TracerWorld -> Picture
renderWorld TracerWorld { playerPos = UnalignedPos p } = 
	Translate (x p) (y p) $ Color red $ circleSolid 20
renderWorld _ = Blank

handleEvent :: Event -> TracerWorld -> TracerWorld
handleEvent (EventKey k Down _ _)  w = w { keys = S.insert k (keys w) }
handleEvent (EventKey k Up _ _)  w = w { keys = S.delete k (keys w) }
handleEvent _ w = w
	

stepWorld :: Float -> TracerWorld -> TracerWorld
stepWorld f w =
	let vx = (if S.member (SpecialKey KeyLeft) (keys w) then -1 else 0) +
		 (if S.member (SpecialKey KeyRight) (keys w) then 1 else 0)
	    vy = (if S.member (SpecialKey KeyUp) (keys w) then 1 else 0) +
		 (if S.member (SpecialKey KeyDown) (keys w) then -1 else 0) in
	let currentPlayerPos = pos $ playerPos w in
	let newPlayerPos = currentPlayerPos { 
		x = x currentPlayerPos + vx, 
		y = y currentPlayerPos + vy
	} in
	w {	playerPos = UnalignedPos newPlayerPos }