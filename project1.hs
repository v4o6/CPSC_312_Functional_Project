-- Myron Yao (80226046) solfalling.lunarising@hotmail.com
-- Xi Yuan Kevin Liu (11304128) x.y.kevin.liu@gmail.com
-- UBC CPSC 312 Functional Project
-- Nov 4th 2012

type OskaBoard = [String]

data Turn = W | B			-- white | black
data Movetype = A | J			-- advance | jump
data Direction = L | R			-- left | right
type Coordinate = (Int, Int, Turn)
type Move = (Coordinate, Movetype, Direction)


oska_v4o6 :: OskaBoard -> Turn -> Int -> OskaBoard
oska_v4o6 board turn depth	= first (oska_helper board turn depth)


-- is recursive
oska_helper :: OskaBoard -> Turn -> Int -> (OskaBoard, Int)
oska_helper board turn depth
	| depth == 0	= board
	| depth == 1	= minmax nextMovesGoodVals (opponent turn)
	| otherwise	= minmax [ oska_helper board0 | board0 <- nextMoves ] (opponent turn)
	where		nextMoves = move_gen board turn
			nextMovesGoodVals = evaluate_list nextMoves

evaluate_list :: [OskaBoard] -> [(OskaBoard, Int)]
evaluate_list boardlist	= map static_board_evaluator boardlist

-- TODO
-- our heuristic function
static_board_evaluator :: OskaBoard -> (OskaBoard, Int)


-- returns the other turn
opponent :: Turn -> Turn
opponent turn
	| turn == W		= B
	| otherwise		= W


-- returns tuple with highest value if white, lowest value if black
minmax :: [(OskaBoard, Int)] -> Turn -> (OskaBoard, Int)
minmax moves turn
	| turn == w		= max (map second moves)
	| otherwise		= min (map second moves)

max :: [(OskaBoard, Int)] -> (OskaBoard, Int)
max []	= []
max [b]	= [b]
max [(b,val):bs)
	| val > maxrest		= (b,val)
	| otherwise		= maxrest
	where		maxrest = max bs

min :: [(OskaBoard, Int)] -> (OskaBoard, Int)
min []	= []
min [b]	= [b]
min [(b,val):bs)
	| val < minrest		= (b,val)
	| otherwise		= minrest
	where		minrest = min bs


-- move generator for new Oska states
move_gen :: OskaBoard -> Turn -> [OskaBoard]
move_gen board turn		= [ doMove board m | m <- moves ]
	where		moves = concat (map (find_legal_moves pawns))
			pawns = find_pawns board turn


-- TODO
-- Format for Coordinate: let the first row/column be 0 (tentative)
find_pawns :: OskaBoard -> Turn -> [Coordinate]


-- TODO
find_legal_moves board (r, c, h, t)
	| t == w	= white_check_row r c b_length (take 3 (drop r board))
	| otherwise	= black_check_row r c b_length (take 3 (drop r board))
	where 	b_length = length board

white_check_row :: Int -> Int -> Int -> Int -> [String] -> [Move]
white_check_row r c b_length rows
	| null row1		= []
	| otherwise		= white_check_helper wNoJ wR wL
	where
		wNoJ = null row2
		wR = (c == 0)
		wL = ((c - 1) == length row0)

		white_check_helper
			| wNoJ
				| wR && (wR1 == '_')	= ((r,c,W), A, R)

		white_advance_check
			| wr1 == '_'
		white_check_right
			| wr1 == '_'			= ((r,c,h,w), advance, R)
			| wr1 == 'b' && wr2 == '_'	= ((r,c,h,w), jump, R)
			| otherwise				= Nothing
		white_check_left
			| wl1 == '_'			= ((r,c,h,w), advance, L)
			| wl1 == 'b' && wl2 == '_'	= ((r,c,h,w), jump, L)
			| otherwise				= Nothing
		row1 = head rows
		row2 = head (tail rows)
		wr1 == head (drop (r + h) row1)
		wr2 == head (drop (c + 1) row2)
		wl1 == head (drop (c - 1) row1)
		wl2 == head (drop (c - 2) row2)


black_check_row :: Int -> Int -> OskaBoard -> [Move]






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


-- By Kevin:
-- jumps a pawn based on the parameter provided.
-- @param board OskaBoard the current state of the broad
-- @param coord Coordinate the pawn that is being moved
-- @param drtn Direction the character that indicates wheather the pawn should
--                       move to left/right along the diagonal
-- @output the oska board after the move
jump_pawn :: [String] -> (Int, Int, Char) -> Char -> [String]
jump_pawn board (cur_row, cur_col, turn) drtn
	| turn == 'W'	= jump_pawn_W board cur_row cur_col next_row next_col take_row take_col
	| otherwise	= jump_pawn_B board cur_row cur_col next_row next_col take_row take_col
	where next_row = get_jump_row cur_row turn (length board)
	      next_col = get_jump_col cur_col turn drtn (length board)
	      --the coordinate of opponent's pawn can be determined by get_next_row and get_next_col
	      take_row = get_next_row cur_row turn (length board)
	      take_col = get_next_col cur_col drtn (length board)


doMove :: Oskaboard -> Move -> OskaBoard
doMove board (c, m, d)
	| m == advance		= advance_pawn board c d
	| otherwise		= jump_pawn board c d



-----------------------------------------------------------------------------------------------------------------------
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
	--the following two special cases may be redundant, so I comment them out
	--for now as it may be done by find_legal_move
	--when W can't move any further
	-- | turn == 'W' && cur_row >= (total_row - 1)	= cur_row
	--when B can't move any further
	-- | turn == 'B' && cur_row >= (total_row - 1)	= cur_row
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
	| otherwise	= cur_col

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

-- Helper function of finding the row position that is jumping to
get_jump_row :: Int -> Char -> Int -> Int
get_jump_row cur_row turn total_row
	--special case is when the pawn cannot jump, but this should be done by
	--find_legal_move
	--general cases:
	| turn == 'W'	= cur_row + 2
	| otherwise	= cur_row - 2

-- Helper function of finding the column position that is jumping to
get_jump_col :: Int -> Char -> Char -> Int -> Int
get_jump_col cur_col turn drtn total_row
	--special case is when the pawn cannot jump, but this should be done by
	--find_legal_move
	--general case:
	| turn == 'W' && drtn == 'L'	= cur_col - 2
	| turn == 'W' && drtn == 'R'	= cur_col
	| turn == 'B' && drtn == 'L'	= cur_col - 1
	| otherwise			= cur_col + 1

-- Helper function for W to jump
-- @param board OskaBoard the board of current state
-- @param cur_row Int the current row position of the pawn
-- @param cur_col Int the current colum poisition of the pawn
-- @param jump_row Int the row position the pawn is moving to
-- @param jump_col Int the column position that pawn is moving to
-- @param take_row Int the row position of opponent's pawn being take down
-- @param take_col Int the column position of opponent's pawn being take down
-- @output the OskaBoard representing the state after the advance move
jump_pawn_W :: [String] -> Int -> Int -> Int -> Int -> Int -> Int -> [String]
jump_pawn_W board cur_row cur_col jump_row jump_col take_row take_col
	| cur_row == 0 && take_row /= 0
	  && jump_row /= 0		= (replace '_' (head board) cur_col) :
					  (jump_pawn_W (tail board) cur_row cur_col (jump_row - 1) jump_col (take_row - 1) take_col)
	| cur_row == 0 && take_row == 0
	  && jump_row /= 0		= (replace '_' (head board) take_col) :
					  (jump_pawn_W (tail board) cur_row cur_col (jump_row - 1) jump_col take_row take_col)
	| cur_row == 0 && take_row == 0
	  && jump_row == 0		= (replace 'W' (head board) jump_col) : (tail board)
	| otherwise			= (head board) : 
					  (jump_pawn_W (tail board) (cur_row -1) cur_col (jump_row - 1) jump_col (take_row - 1) take_col)

-- Helper function for B to jump
-- @param board OskaBoard the board of current state
-- @param cur_row Int the current row position of the pawn
-- @param cur_col Int the current colum poisition of the pawn
-- @param jump_row Int the row position the pawn is moving to
-- @param jump_col Int the column position that pawn is moving to
-- @param take_row Int the row position of opponent's pawn being take down
-- @param take_col Int the column position of opponent's pawn being take down
-- @output the OskaBoard representing the state after the advance move
jump_pawn_B :: [String] -> Int -> Int -> Int -> Int -> Int -> Int -> [String]
jump_pawn_B board cur_row cur_col jump_row jump_col take_row take_col
	| jump_row == 0 && take_row /= 0
	  && cur_row /= 0		= (replace 'B' (head board) jump_col) :
					  (jump_pawn_B (tail board) (cur_row - 1) cur_col jump_row jump_col (take_row - 1) take_col)
	| jump_row == 0 && take_row == 0
	  && cur_row /= 0		= (replace '_' (head board) take_col) :
					  (jump_pawn_B (tail board) (cur_row - 1) cur_col jump_row jump_col take_row take_col)
	| jump_row == 0 && take_row == 0
	  && cur_row == 0		= (replace '_' (head board) cur_col) : (tail board)
	| otherwise			= (head board) :
					  (jump_pawn_B (tail board) (cur_row - 1) cur_col (jump_row - 1) jump_col (take_row - 1) take_col)


