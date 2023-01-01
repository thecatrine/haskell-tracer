-- | Tree Fractal.
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


data TracerWorld = TracerWorld
	{
		playerPos :: GamePos,
		keys :: S.Set Key
	}

main = play (InWindow "Test" (640, 480) (20,  20))
		black
		60
		TracerWorld { playerPos = (UnalignedPos Pos {x=0, y=0, z=0}), keys=S.empty }
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

pixelData :: [Word8]
pixelData = concat [pixelTest x y | y <- [0..479], x <- [0..639]]

renderWorld :: TracerWorld -> Picture
renderWorld TracerWorld { playerPos = UnalignedPos p } = 
	let bitmapData = Data.ByteString.pack $ pixelData in
	bitmapOfByteString	
		640 480 (BitmapFormat TopToBottom PxRGBA) 
		bitmapData False
renderWorld _ = Blank

handleEvent :: Event -> TracerWorld -> TracerWorld
handleEvent (EventKey k Down _ _)  w = w { keys = S.insert k (keys w) }
handleEvent (EventKey k Up _ _)  w = w { keys = S.delete k (keys w) }
handleEvent _ w = w



bitmapTest :: Picture
bitmapTest = 
	let bitmapData = Data.ByteString.pack $ (map fromIntegral pixelData) in
	bitmapOfByteString	
		256 256 (BitmapFormat TopToBottom PxRGBA) 
		bitmapData False
	

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
