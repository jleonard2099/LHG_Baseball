
# PHASE 1 - PRELIMINARY WORK

## GOAL: ADDRESS GFX ROUTINES / FILES

- [X] Inventory routines used for graphics
- [X] Determine equivalent functions or re-create as needed


## GOAL: ADDRESS MISSING / EXTRANEOUS SOURCE FILES

- [X] Baseball - recreate missing source files
	- [X] SCHEDULE.BAS
		used for AUTO-SCHEDULER (Option 4)
		was not in /src, but referenced by in menus
		found / renamed SCH.BAS, which appears to serve this functionality
		compared to compiled game to confirm
	
	- [X] LOOKY.BAS
		used to VIEW COMPILED STATS (Option 7)
		referenced by HELLO.BAS, installed by game
		similar file exists for each game but is unique to game
		found in different path than expected

## GOAL: COMPILE FULL GAMES FROM SOURCES

- [X] Re-create dependent functions with no QB64 equivalent in native BASIC
	- [X] ClearEOL
	- [X] FCopy
	- [X] MAMenu
	- [X] MMenuVert
	- [X] MScrnSave/MScrnRest
	- [X] QPrint/MQPrint	
	- [X] ReadFile + FCount
	- [X] Text In
	- [X] YesNoB
	
- [X] Replace dependent functions with QB64 equivalents

- [X] Remove obsolete code
	Make sure there are no more disk prompts
	References to SortStr are not needed
		They are all used on file lists
		My function sorts the list in shell

- [X] Make code compatible with QB64

- [X] Attempt first compilation of each file
