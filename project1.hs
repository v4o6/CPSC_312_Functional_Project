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
evaluateOneBoard board turn
	| turn == 'W'	= (board, (((length board) * (length (head board))) - (0.8 * fromIntegral(sum_of_dist - sum_of_dist_oppo))))
	| otherwise	= (board, (negate ((((length board) * (length (head board))) - (0.8 * fromIntegral(sum_of_dist - sum_of_dist_oppo))))))
	where dest_row = find_dest_row board turn
	      dest_row_oppo = find_dest_row board (opponent turn)
	      pawns = find_pawns board turn
	      oppo_pawns = find_pawns board (opponent turn)
	      sum_of_dist = find_sum_of_dist pawns dest_row
	      sum_of_dist_oppo = find_sum_of_dist oppo_pawns dest_row_oppo
	      --locally defined function
	      find_dest_row :: OskaBoard -> Char -> Int
	      find_dest_row board turn
		| turn == 'W'	= (length board) - 1
		| otherwise	= 0



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


-- Finding the coordinate of all pawns
-- @param board OskaBoard the current state of the board
-- @param turn Turn which color of pawn is being looked for
-- @output all the coordinate associate with the pawn in the current board
find_pawns :: OskaBoard -> Turn -> [Coordinate]
find_pawns board turn
	| turn == W		= scan_rows board W 0
	| otherwise		= scan_rows (reverse board) B 0

-- recursive helper function: scans a list of rows
scan_rows :: OskaBoard -> Turn -> Int -> [Coordinate]
scan_rows rows turn i
	| null rows		= []
	| otherwise		= (scan_row row_i turn i 0) ++ (scan_rows (tail rows) turn (i + 1))
	where	row_i	= head rows

-- recursive helper function: scans a row
scan_row :: String -> Turn -> Int -> Int -> [Coordinate]
scan_row row turn i j
	| null row		= []
	| (head row) == turn	= (i, j, turn) : scan_row (tail row) turn i (j + 1)
	| otherwise		= scan_row (tail row) turn i (j + 1)


find_legal_moves board (r, c, turn)
	| turn == w	= white_check_row r c b_length (take 3 (drop r board))
	| otherwise	= black_check_row r c b_length (take 3 (drop r (reverse board)))
	where 	b_length = length board

white_check_row :: Int -> Int -> Int -> [String] -> [Move]
white_check_row r c b_length rows
	| null row1		= []
	| otherwise		= w_check
	where	wNoJ = null row2
		wR = (c == 0)
		wL = ((c - 1) == length row0)

		w_check		| wNoJ		= wNoJ_check
				| wR		= wR_check : []
				| wL		= wL_check : []
				| otherwise	= wR_check : wL_check : []

		wNoJ_check	| wR		= wRA_check : []
				| wL		= wLA_check : []
				| otherwise	= wRA_check : wLA_check : []
		wR_check	| wRA		= ((r,c,W), A, R)
				| wRJ		= ((r,c,W), J, R)
				| otherwise	= Nothing
		wL_check	| wLA		= ((r,c,W), A, L)
				| wLJ		= ((r,c,W), J, L)
				| otherwise	= Nothing

		wRA_check	| wRA		= ((r,c,W), A, R)
				| otherwise	= Nothing
		wLA_check	| wLA		= ((r,c,W), A, L)
				| otherwise	= Nothing

		wRA = (wR1 == '_')
		wLA = (wL1 == '_')
		wRJ = (wR1 == 'b') && (wR2 == '_')
		wLJ = (wR1 == 'b') && (wL2 == '_')

		wR1	| r10diff < 0 		= head (drop r row1)
			| otherwise		= head (drop (r + 1) row1)
		wR2	| r20diff < 0		= head (drop r row2)
			| r20diff == 0		= head (drop (r + 1) row2)
			| otherwise		= head (drop (r + 2) row2)
		wL1	| r10diff < 0 		= head (drop (r - 1) row1)
			| otherwise		= head (drop r row1)
		wL2	| r20diff < 0		= head (drop (r - 2) row2)
			| r20diff == 0		= head (drop (r - 1) row2)
			| otherwise		= head (drop r row2)

		r10diff = (length row1) - (length row0)
		r20diff = (length row2) - (length row0)

		row0 = head rows
		row1 = head (tail rows)
		row2 = last rows


black_check_row :: Int -> Int -> Int -> [String] -> [Move]
black_check_row r c b_length rows
	| null row1		= []
	| otherwise		= b_check
	where	bNoJ = null row2
		bR = (c == 0)
		bL = ((c - 1) == length row0)

		b_check
			| bNoJ		= bNoJ_check
			| bR		= bR_check : []
			| bL		= bL_check : []
			| otherwise	= bR_check : bL_check : []

		bNoJ_check
			| bR		= bRA_check : []
			| bL		= bLA_check : []
			| otherwise	= bRA_check : bLA_check : []

		bR_check
			| bRA		= ((r,c,b), A, R)
			| bRJ		= ((r,c,b), J, R)
			| otherwise	= Nothing
		bL_check
			| bLA		= ((r,c,b), A, L)
			| bLJ		= ((r,c,b), J, L)
			| otherwise	= Nothing

		bRA_check
			| bRA		= ((r,c,b), A, R)
			| otherwise	= Nothing
		bLA_check
			| bLA		= ((r,c,b), A, L)
			| otherwise	= Nothing

		bRA = (bR1 == '_')
		bLA = (bL1 == '_')
		bRJ = (bR1 == 'b') && (bR2 == '_')
		bLJ = (bR1 == 'b') && (bL2 == '_')

		bR1	| r10diff < 0 		= head (drop r row1)
			| otherwise		= head (drop (r + 1) row1)
		bR2	| r20diff < 0		= head (drop r row2)
			| r20diff == 0		= head (drop (r + 1) row2)
			| otherwise		= head (drop (r + 2) row2)
		bL1	| r10diff < 0 		= head (drop (r - 1) row1)
			| otherwise		= head (drop r row1)
		bL2	| r20diff < 0		= head (drop (r - 2) row2)
			| r20diff == 0		= head (drop (r - 1) row2)
			| otherwise		= head (drop r row2)

		r10diff = (length row1) - (length row0)
		r20diff = (length row2) - (length row0)

		row0 = head rows
		row1 = head (tail rows)
		row2 = last rows


-- advances a pawn according to the paramters and return the board after that 
-- move.
-- @param board OskaBoard the current state of the broad
-- @param coord Coordinate the pawn that is being moved
-- @param d Direction whether the pawn should move left/right along the diagonal
-- @output the oska board after the move advances a pawn
advance_pawn :: OskaBoard -> Coordinate -> Direction -> OskaBoard
advance_pawn board (r, c, t) d
	| t == W		= advance_pawn_helper board t r c d
	| otherwise		= reverse (advance_pawn_helper (reverse board) t r c d)

-- helper function
advance_pawn_helper :: OskaBoard -> Turn -> Int -> Int -> Direction -> OskaBoard
advance_pawn_helper rows t r c d	= prefix_rows ++ new_row0 : new_row1 : suffix_rows
	where		(prefix_rows, remainder) = splitAt r rows
			row0 = head remainder
			row1 = head (tail remainder)
			suffix_rows = drop 2 remainder

			r10diff = (length row1) - (length row0)
			d_shift	| d == R	= 0
					| d == L	= -1

			(r0_prefix, r0_suffix) = splitAt c row0
			new_row0 = r0_prefix ++ '_' : (tail r0_suffix)
			
			(r1_prefix, r1_suffix)
				| r10diff < 0	= splitAt (c + d_shift) row1
				| otherwise		= splitAt (c + d_shift + 1) row1
			new_row1 = r1_prefix ++ t : (tail r1_suffix)


-- jumps a pawn based on the parameter provided.
-- @param board OskaBoard the current state of the broad
-- @param coord Coordinate the pawn that is being moved
-- @param drtn Direction whether the pawn should move left/right along the diagonal
-- @output the oska board after the move
jump_pawn :: OskaBoard -> Coordinate -> Direction -> Oskaboard
jump_pawn board (r, c, t) d
	| t == W		= jump_pawn_helper board t r c d
	| otherwise		= reverse (jump_pawn_helper (reverse board) t r c d)

-- helper function
jump_pawn_helper :: OskaBoard -> Turn -> Int -> Int -> Direction -> OskaBoard
jump_pawn_helper rows t r c d	= prefix_rows ++ new_row0 : new_row1 : new_row2 : suffix_rows
	where	
		(prefix_rows, remainder) = splitAt r rows
		row0 = head remainder
		row1 = head (tail remainder)
		row2 = head (drop 2 remainder)
		suffix_rows = drop 3 remainder

		r10diff = (length row1) - (length row0)
		r20diff = (length row2) - (length row1)
		d_shift	| d == R	= 0
				| d == L	= -1

		(r0_prefix, r0_suffix) = splitAt c row0
		new_row0 = r0_prefix ++ '_' : (tail r0_suffix)
			
		(r1_prefix, r1_suffix)
			| r10diff < 0	= splitAt (c + d_shift) row1
			| otherwise		= splitAt (c + d_shift + 1) row1
		new_row1 = r1_prefix ++ '_' : (tail r1_suffix)

		(r2_prefix, r2_suffix)
			| r20diff < 0	= splitAt (c + 2*d_shift) row2
			| r20diff == 0	= splitAt (c + 2*d_shift + 1) row2
			| otherwise		= splitAt (c + 2*d_shift + 2) row2
		new_row2 = r2_prefix ++ t : (tail r2_suffix)


doMove :: Oskaboard -> Move -> OskaBoard
doMove board (c, m, d)
	| m == advance		= advance_pawn board c d
	| otherwise		= jump_pawn board c d

