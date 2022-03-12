
* PRELIMINARY WORK

** GOAL: REVIEW GFX ROUTINES / FILES
[ ] ???


** GOAL: ADDRESS MISSING / EXTRANEOUS SOURCE FILES

[ ] Baseball - recreate missing source files
	[X] SCHEDULE.BAS
		used for AUTO-SCHEDULER (Option 4)
		was not in /src, but referenced by in menus
		found / renamed SCH.BAS, which appears to serve this functionality
		compared to compiled game to confirm
	
	[-] LOOKY.BAS
		used to VIEW COMPILED STATS (Option 7)
		referenced by HELLO.BAS, installed by game
		similar file exists for each game but is unique to game
		essentially, this file loads and outputs stats
		therefore it requires intimate knowledge of this data / output

		+ template file created which should be a navigatable menu
		+ it will not, however, be very functional

** GOAL: COMPILE FULL GAMES FROM SOURCES
*** From WWW - "FULL COUNT BASEBALL 6.53"
*** From Code - "FULL COUNT BASEBALL 6.51"

[ ] Re-create dependent functions with no QB64 equivalent in native BASIC
	[X] ClearEOL
	[X] FCopy
	[X] MAMenu
	[X] MMenuVert
	[ ] MPaintBox
	[X] QPrint/MQPrint	
	[X] ReadFile + FCount
	[X] Text In
	[X] YesNoB
	
[ ] Replace dependent functions with QB64 equivalents

[ ] Make code compatible with QB64
	[ ] Remove unused DECLARE / DIM
	[ ] Replace RND(1) with RND
