-- as the name suggests, never mind this file.




advance_pawn :: [String] -> (Int, Int, Char) -> Char -> [String]
advance_pawn board (cur_row, cur_col, turn) drtn
	| turn == 'W'	= place_pawn_W board cur_row cur_col next_row next_col
	| otherwise	= place_pawn_B board cur_row cur_col next_row next_col
	where next_row = get_next_row cur_row turn (length board)
	      next_col = get_next_col cur_col drtn (length board)

get_width_of_row :: [String] -> Int -> Int
get_width_of_row board row
	| row == 0	= length (head board)
	| otherwise	= get_width_of_row (tail board) (row -1)


find_n_row :: [String] -> Int -> String
find_n_row board n
	| n == 0	= head board
	| otherwise	= find_n_row (tail board) (n - 1)



sample_board_0 = ["WWWW",--0
		  "___", --1
		  "__",  --2
		  "___", --3
		  "BBBB"]--4

sample_board_1 = ["WW_W",--0
		  "_W_", --1
		  "__",  --2
		  "__B", --3
		  "BBB_"]--4

sample_board_2 = ["WW_W",--0
		  "___", --1
		  "W_",  --2
		  "B_B", --3
		  "_BB_"]--4

sample_board_3 = ["____",--0
		  "_B_", --1
		  "W_",  --2
		  "___", --3
		  "_W__"]--4

sample_board_4 = ["____",--0
		  "W__", --1
		  "_B",  --2
		  "B_W", --3
		  "____"]--4

sample_board_5 = ["WW_W",--0
		  "__B", --1
		  "W_",  --2
		  "B__", --3
		  "_BB_"]--4

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

get_next_col :: Int -> Char -> Int -> Int
get_next_col cur_col drtn total_col
	--the following two special cases may be redundant, so I leave it for now
	--(it is done by find_legal_move)
	--when the pawn can't move to left
	--when the pawn can't move to right
	--general cases:
	| drtn == 'L'	= cur_col - 1
	| otherwise	= cur_col


replace :: Char -> String -> Int -> String
replace rplc_char str n
	| n == 0	= rplc_char : (tail str)
	| otherwise	= (head str) : (replace rplc_char 
						(tail str) 
						(n -1))


get_jump_row :: Int -> Char -> Int -> Int
get_jump_row cur_row turn total_row
	--special case is when the pawn cannot jump, but this should be done by
	--find_legal_move
	--general cases:
	| turn == 'W'	= cur_row + 2
	| otherwise	= cur_row - 2

get_jump_col :: Int -> Char -> Char -> Int -> Int
get_jump_col cur_col turn drtn total_row
	--special case is when the pawn cannot jump, but this should be done by
	--find_legal_move
	--general case:
	| turn == 'W' && drtn == 'L'	= cur_col - 2
	| turn == 'W' && drtn == 'R'	= cur_col
	| turn == 'B' && drtn == 'L'	= cur_col - 1
	| otherwise			= cur_col + 1

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

jump_pawn_B :: [String] -> Int -> Int -> Int -> Int -> Int -> Int -> [String]
jump_pawn_B board cur_row cur_col to_row to_col take_row take_col
	| to_row == 0 && take_row /= 0
	  && cur_row /= 0		= (replace 'B' (head board) to_col) :
					  (jump_pawn_B (tail board) (cur_row - 1) cur_col to_row to_col (take_row - 1) take_col)
	| to_row == 0 && take_row == 0
	  && cur_row /= 0		= (replace '_' (head board) take_col) :
					  (jump_pawn_B (tail board) (cur_row - 1) cur_col to_row to_col take_row take_col)
	| to_row == 0 && take_row == 0
	  && cur_row == 0		= (replace '_' (head board) cur_col) : (tail board)
	| otherwise			= (head board) :
					  (jump_pawn_B (tail board) (cur_row - 1) cur_col (to_row - 1) to_col (take_row - 1) take_col)

place_pawn_W :: [String] -> Int -> Int -> Int -> Int-> [String]
place_pawn_W board cur_row cur_col to_row to_col
	| cur_row == 0 && to_row /= 0	= (replace '_' (head board) cur_col) : 
					  (place_pawn_W (tail board) cur_row cur_col (to_row - 1) to_col)
	| cur_row == 0 && to_row == 0	= (replace 'W' (head board) to_col) : (tail board)
	| otherwise			= (head board) : 
					  (place_pawn_W (tail board) (cur_row - 1) cur_col (to_row -1) to_col)

place_pawn_B :: [String] -> Int -> Int -> Int -> Int -> [String]
place_pawn_B board cur_row cur_col to_row to_col
	| to_row == 0 && cur_row /= 0	= (replace 'B' (head board) to_col) :
					  (place_pawn_B (tail board) (cur_row - 1) cur_col to_row to_col)
	| to_row == 0 && cur_row == 0	= (replace '_' (head board) cur_col) : (tail board)
	| otherwise			= (head board) :
					  (place_pawn_B (tail board) (cur_row - 1) cur_col (to_row - 1) to_col)
jump_pawn :: [String] -> (Int, Int, Char) -> Char -> [String]
jump_pawn board (cur_row, cur_col, turn) drtn
	| turn == 'W'	= jump_pawn_W board cur_row cur_col next_row next_col take_row take_col
	| otherwise	= jump_pawn_B board cur_row cur_col next_row next_col take_row take_col
	where next_row = get_jump_row cur_row turn (length board)
	      next_col = get_jump_col cur_col turn drtn (length board)
	      --the coordinate of opponent's pawn can be determined by get_next_row and get_next_col
	      take_row = get_next_row cur_row turn (length board)
	      take_col = get_next_col cur_col drtn (length board)








