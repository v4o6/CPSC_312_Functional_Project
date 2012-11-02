-- as the name suggests, never mind this file.

sample_board_0 = ["WWWW",
		  "___",
		  "__",
		  "___",
		  "BBBB"]
sample_board_1 = ["WW_W",
		  "_W_",
		  "__",
		  "__B",
		  "BBB_"]
sample_board_2 = ["WW_W",
		  "___",
		  "W_",
		  "B_B",
		  "_BB_"]


replace :: Char->String->Int->String
replace rplc_char str n
	| n == 0	= rplc_char : (tail str)
	| otherwise	= (head str) : (replace rplc_char 
						(tail str) 
						(n -1))

get_next_row :: Int->Char->Int->Int
get_next_row cur_row turn total_row
	--when W can't move any further
	| turn == 'W' && 
	cur_row >= (total_row - 1)	= cur_row
	--when B can't move any further
	| turn == 'B' &&
	cur_row >= 0			= cur_row
	--general cases:
	| turn == 'W'			= cur_row + 1
	| turn == 'B'			= cur_row - 1

get_next_col :: Int->Char->Int->Int
get_next_col cur_col drtn total_col
	--the following two special cases may be redundant, so I leave it for now
	--when the pawn can't move to left
	--when the pawn can't move to right
	--general cases:
	| drtn == 'L'	= cur_col - 1
	| drtn == 'R'	= cur_col	 + 1

get_width_of_row :: [String]->Int->Int
get_width_of_row board row
	| row == 0	= length (head board)
	| otherwise	= get_width_of_row (tail board) (row -1)









