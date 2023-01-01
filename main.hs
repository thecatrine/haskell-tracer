-- | Ray Tracer.
-- Modified Heavily from the example below

-- Tree Fractal.
--	Based on ANUPlot code by Clem Baker-Finch.
--	
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import qualified Data.Set as S
import qualified Data.ByteString
import Data.Word (Word8)
import qualified Data.Bits


data Pos = Pos 
	{
		x :: Float,
		y :: Float,
		z :: Float
	}

data GamePos = AlignedPos Pos | UnalignedPos Pos
pos :: GamePos -> Pos
pos (AlignedPos p) = p
pos (UnalignedPos p) = p

add :: Pos -> Pos -> Pos
add (Pos x1 y1 z1) (Pos x2 y2 z2) = Pos (x1+x2) (y1+y2) (z1+z2)


data Ray = Ray
	{
		loc :: Pos,
		dir :: Pos
	}
normalized :: Ray -> Ray
normalized ray = 
	let lensqr = sqrt((x $ dir ray) * (x $ dir ray) + (y $ dir ray) * (y $ dir ray) + (z $ dir ray) * (z $ dir ray)) in
	ray { 
			dir = Pos {
				x = (x $ dir ray) / lensqr * 3, 
				y = (y $ dir ray) / lensqr * 3, 
				z = (z $ dir ray) / lensqr * 3
			} 
		}


data TracerWorld = TracerWorld
	{
		playerPos :: GamePos,
		keys :: S.Set Key,
		offset :: Pos,
		offsetV :: Float
	}

main = play (InWindow "Test" (640, 480) (20,  20))
		black
		30
		TracerWorld { 
			playerPos = (UnalignedPos Pos {x=0, y=0, z=0}), 
			keys=S.empty,
			offset = Pos {x=0, y=0, z=10},
			offsetV = 0.1
		}
		renderWorld
		handleEvent
		stepWorld


pixelTest :: Int -> Int -> [Word8]
pixelTest x y = 
	let wx :: Word8 = fromIntegral x in
	let wy :: Word8 = fromIntegral y in
	[
		fromIntegral $ Data.Bits.xor wx wy, 
		fromIntegral $ Data.Bits.xor wx wy, 
		fromIntegral $ Data.Bits.xor wx wy, 
		255
	]


subCastRay :: Ray -> Pos -> [Word8]
subCastRay ray offset = 
	if (z $ loc ray) < 50 - (z offset) then
		subCastRay Ray { loc = add (loc ray) (dir ray), dir = dir ray } offset
	else
		let hit = loc ray in
		let offsetX = x hit - x offset in
		let offsetY = y hit + y offset in
		if offsetX > -256 &&
			offsetX < 256 && 
			offsetY > -256 && 
			offsetY < 256 then
			pixelTest (round offsetX) (round offsetY)
		else
			[0, 0, 0, 255]

castRay :: Int -> Int -> Pos -> [Word8]
castRay y x offset = 
	if y == 240 && x == (round $ z offset) then [255, 0, 0, 255] else
	let ray = normalized Ray { loc = Pos {x=0, y=0, z=0-10}, dir = Pos {x=fromIntegral x-320, y=fromIntegral y - 240, z=10} } in
	subCastRay ray offset
	


pixelData :: Pos -> [Word8]
pixelData offset = concat [castRay y x offset | y <- [0..479], x <- [0..639]]

renderWorld :: TracerWorld -> Picture
renderWorld TracerWorld { playerPos = UnalignedPos p, offset = o} =
	let bitmapData = Data.ByteString.pack $ pixelData o in
	bitmapOfByteString	
		640 480 (BitmapFormat TopToBottom PxRGBA) 
		bitmapData False
renderWorld _ = Blank

handleEvent :: Event -> TracerWorld -> TracerWorld
handleEvent (EventKey k Down _ _)  w = w { keys = S.insert k (keys w) }
handleEvent (EventKey k Up _ _)  w = w { keys = S.delete k (keys w) }
handleEvent _ w = w
stepWorld :: Float -> TracerWorld -> TracerWorld
stepWorld f w =
	let speed = 3 in
	let vx = (if S.member (SpecialKey KeyLeft) (keys w) then -speed else 0) +
		 (if S.member (SpecialKey KeyRight) (keys w) then speed else 0)
	    vy = (if S.member (SpecialKey KeyUp) (keys w) then speed else 0) +
		 (if S.member (SpecialKey KeyDown) (keys w) then -speed else 0) in
	let currentPlayerPos = pos $ playerPos w in
	let newPlayerPos = currentPlayerPos { 
		x = x currentPlayerPos + vx, 
		y = y currentPlayerPos + vy
	} in
	let currentV = offsetV w in
	w {	
		playerPos = UnalignedPos newPlayerPos,
		offset = Pos {x=(x currentPlayerPos), y=(y currentPlayerPos), z=10 + 25 * (sin $ currentV / 2)},
		offsetV = currentV + f
	  }
