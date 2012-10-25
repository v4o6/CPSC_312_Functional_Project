-- Myron Yao
-- solfalling.lunarising@hotmail.com

type OskaBoard = [String]
type Turn = {w, b}
type Coordinate = (Int, Int, Char)
type Movetype = {advance, jump}
type Direction = {L, R}
type Move = (Coordinate, Movetype, Direction)


oska_v4o6 :: OskaBoard -> Turn -> Int -> OskaBoard
oska_v4o6 board turn depth	= first (oska_helper board turn depth)


-- is recursive
oska_helper :: OskaBoard -> Turn -> Int -> (OskaBoard, Int)
oska_helper board turn depth
	| depth == 0	= board
	| depth == 1	= minmax nextMovesGoodVals (otherturn turn)
	| otherwise		= minmax [ oska_helper board0 | board0 <- nextMoves ] (otherturn turn)
	where		nextMoves = move_gen OskaBoard turn
			nextMovesGoodVals = evaluate nextMoves

evaluate :: [OskaBoard] -> [(OskaBoard, Int)]
evaluate boards	= map evaluateOneBoard boards

-- our heuristic function
evaluateOneBoard :: OskaBoard -> (OskaBoard, Int)


-- returns the other turn
otherturn :: Turn -> Turn


-- returns largest tuple if black, smallest tuple if white
minmax :: [(OskaBoard, Int)] -> Turn -> (OskaBoard, Int)


move_gen :: OskaBoard -> Turn -> [OskaBoard]
move_gen board turn		= 
	where		pawns = find_pawns board turn
			moves = concat (map (find_legal_moves pawns))
			newBoards = [ doMove board move | move <- moves ]


find_pawns :: OskaBoard -> Turn -> [Coordinate]


find_legal_moves :: OskaBoard -> Coordinate -> [Move]


-- advances a pawn
advance_pawn :: OskaBoard -> Coordinate -> Direction -> OskaBoard

-- jumps a pawn
jump_pawn :: OskaBoard -> Coordinate -> Direction -> Oskaboard


doMove :: Oskaboard -> Move -> OskaBoard
doMove board (c, m, d)
	| m == advance		= advance_pawn board c d
	| otherwise			= jump_pawn board c d
