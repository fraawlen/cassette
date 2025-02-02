if exists("b:current_syntax")
	finish
endif

" Filler tokens

syntax match C_Filler /:=\|=/

" Substitution tokens

syntax match   C_Swap /==\|!=\|>=\|<=\|>\|<\|\$\$\|\$\|%\|\*\|\!\|\/\|+\|-/
syntax keyword C_Swap VAR PARAM ITER JOIN STREQ SQRT CBRT ABS CEIL FLOOR ROUND COS SIN TAN ACOS ASIN ATAN COSH SINH LN LOG MOD POW BIG SMALL ITRPL LIMIT CITRPL RGB RGBA PI E TRUE FALSE TIME RAND

" Sequence leads tokens

syntax keyword C_Lead SECTION SECTION_ADD SECTION_DEL LET LET_ENUM LET_APPEND LET_PREPEND LET_MERGE FOR_EACH FOR_END INCLUDE SEED_OVERRIDE RESTRICT DEBUG_PRINT

" Comments

syntax match C_Comment /\/\/.*\|EOL.*/

" Brackets and parentheses

syntax match C_Brackets /[()\[\]]/

" Numbers

syntax match C_Number /\<\d\+\>/
syntax match C_Number /\(\(^\|\W\)\zs#[0-9A-Fa-f]\+\)/

" Strings

syntax region C_String start=+\"+ end=+\"+
syntax region C_String start=+'+  end=+'+

" Links

hi def link C_Swap     Keyword
hi def link C_Filler   Delimiter
hi def link C_Lead     Function
hi def link C_Comment  Comment
hi def link C_String   String
hi def link C_Number   Number
hi def link C_Brackets Delimiter

let b:current_syntax = "ccfg"

