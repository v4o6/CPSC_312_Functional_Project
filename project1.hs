-- Myron Yao (80226046) solfalling.lunarising@hotmail.com
-- Xi Yuan Kevin Liu (11304128) x.y.kevin.liu@gmail.com
-- UBC CPSC 312 Functional Project
-- Nov 4th 2012

-- TODO correct type declaration
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

-- TODO
-- our heuristic function
evaluateOneBoard :: OskaBoard -> (OskaBoard, Int)

-- TODO
-- returns the other turn
otherturn :: Turn -> Turn

-- TODO
-- returns largest tuple if black, smallest tuple if white
minmax :: [(OskaBoard, Int)] -> Turn -> (OskaBoard, Int)

-- TODO
move_gen :: OskaBoard -> Turn -> [OskaBoard]
move_gen board turn		= 
	where		pawns = find_pawns board turn
			moves = concat (map (find_legal_moves pawns))
			newBoards = [ doMove board move | move <- moves ]

-- TODO
find_pawns :: OskaBoard -> Turn -> [Coordinate]

-- TODO
find_legal_moves :: OskaBoard -> Coordinate -> [Move]





-- By Kevin:
-- advances a pawn according to the paramters and return the board after that 
-- move.
-- @param board OskaBoard the current state of the broad
-- @param coord Coordinate the pawn that is being moved
-- @param drtn Direction the character that indicates wheather the pawn should
--                       move to left/right along the diagonal
-- @output the oska board after the move
advance_pawn :: OskaBoard -> Coordinate -> Direction -> OskaBoard
advance_pawn board (cur_row cur_col turn) drtn
	| turn == 'W'	= advance_place_pawn_W board cur_row cur_col next_row next_col
	| otherwise	= advance_place_pawn_B board cur_row cur_col next_row next_col
	where next_row = get_next_row cur_row turn (length board)
	      next_col = get_next_col cur_col drtn (length board)






-- jumps a pawn
jump_pawn :: OskaBoard -> Coordinate -> Direction -> Oskaboard


doMove :: Oskaboard -> Move -> OskaBoard
doMove board (c, m, d)
	| m == advance		= advance_pawn board c d
	| otherwise		= jump_pawn board c d



--------------------------------------------------------------------------------
-- Supplemental Helper Functions:

-- By Kevin
-- To replace the n-th character in str by rplc_char
replace :: Char -> String -> Int -> String
replace rplc_char str n
	| n == 0	= rplc_char : (tail str)
	| otherwise	= (head str) : (replace rplc_char (tail str) (n -1))

-- Generate the row number of next move
get_next_row :: Int -> Char -> Int -> Int
get_next_row cur_row turn total_row
	--when W can't move any further
	| turn == 'W' && cur_row >= (total_row - 1)	= cur_row
	--when B can't move any further
	| turn == 'B' && cur_row >= (total_row - 1)	= cur_row
	--general cases:
	| turn == 'W'					= cur_row + 1
	| otherwise					= cur_row - 1

-- Generate the colum number of next move
get_next_col :: Int -> Char -> Int -> Int
get_next_col cur_col drtn total_col
	--the following two special cases may be redundant, so I leave it for now
	--(it is done by find_legal_move)
	--when the pawn can't move to left
	--when the pawn can't move to right
	--general cases:
	| drtn == 'L'	= cur_col - 1
	| drtn == 'R'	= cur_col

-- Get the number colum of a specific row of a board
get_width_of_row :: [String] -> Int -> Int
get_width_of_row board row
	| row == 0	= length (head board)
	| otherwise	= get_width_of_row (tail board) (row -1)

-- Extract the nth string in a list of string
find_n_row :: [String] -> Int -> String
find_n_row board n
	| n == 0	= head board
	| otherwise	= find_n_row (tail board) (n - 1)

-- Helper functions for W to advance
-- @param board OskaBoard the board of current state
-- @param cur_row Int the current row position of the pawn
-- @param cur_col Int the current colum poisition of the pawn
-- @param to_row Int the row position the pawn is moving to
-- @param to_col Int the column position that pawn is moving to
-- @output the OskaBoard representing the state after the advance move
advance_place_pawn_W :: OskaBoard -> Int -> Int -> Int -> Int-> OskaBoard
advance_place_pawn_W board cur_row cur_col to_row to_col
	| cur_row == 0 && to_row /= 0	= (replace '_' (head board) cur_col) : 
					  (advance_place_pawn_W (tail board) cur_row cur_col (to_row - 1) to_col)
	| cur_row == 0 && to_row == 0	= (replace 'W' (head board) to_col) : (tail board)
	| otherwise			= (head board) : 
					  (advance_place_pawn_W (tail board) (cur_row - 1) cur_col (to_row -1) to_col)

-- Helper function for B to advance
-- @param board OskaBoard the board of current state
-- @param cur_row Int the current row position of the pawn
-- @param cur_col Int the current colum poisition of the pawn
-- @param to_row Int the row position the pawn is moving to
-- @param to_col Int the column position that pawn is moving to
-- @output the OskaBoard representing the state after the advance move
advance_place_pawn_B :: [String] -> Int -> Int -> Int -> Int -> [String]
advance_place_pawn_B board cur_row cur_col to_row to_col
	| to_row == 0 && cur_row /= 0	= (replace 'B' (head board) to_col) :
					  (place_pawn_B (tail board) (cur_row - 1) cur_col to_row to_col)
	| to_row == 0 && cur_row == 0	= (replace '_' (head board) cur_col) : (tail board)
	| otherwise			= (head board) :
					  (place_pawn_B (tail board) (cur_row - 1) cur_col (to_row - 1) to_col)




