[ ] Paths / folders
	- use _CWD$ function to set as defaults if not existent
	- add "Boxes" subfolder to structure and adapt code
	- add "Schedules" subfolder to structure and adapt code
[ ] SCHEDULE.BAS
	- menu needs to come down some
	- playing games should return back to the menu, not DOS
	- Box Score editor disabled because (uses QEdit); just remove it!
[ ] Integrate team file selection, stat file name functions
- College BB already has "STATCONVERT" subroutine
[ ] Title routine to take parameters
- 1 for the text to use
- 1 for the actual window title
[ ] "Does this team have saved stats"??? - update to just detect it!!!
[ ] Develop "Select File" function (for LDR/LIB/etc)
- should only take a file as a parameter
- set as function to just return the file name
[ ] Bad file extension - .BAT file --> .BTR (for Batters)
[ ] BINPUT.BAS - Team Management is a redundant option
[ ] Black screen menus --> convert to blue vertical menus
[ ] LOADER.BAS
	- Lineup (batters) changes
	- Press 1-9 to Change
		1) press a lineup # to change
		2) press a key for the new person to go there
		3) press a key for the position to fill
		however it does not show the position slots for this last option
		we could find a way to show this; however some lineups also take up the whole screen
