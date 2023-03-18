[ ] Paths / folders
- add "Boxes" subfolder to structure and adapt code
- add "Schedules" subfolder to structure and adapt code

[ ] Consistency of TITLE screens with other games

[ ] SCHEDULE.BAS
- menu needs to come down some
- playing games should return back to the menu, not DOS
- Box Score editor disabled because (uses QEdit); just remove it!

[ ] Integrate team file selection, stat file name functions
- College BB already has "STATCONVERT" subroutine
- Team file routines already in progress but not tested

[ ] Review all file selection routines
- Are they overly wordy?
- Are they "centered" reasonably?

[ ] "Does this team have saved stats"??? - update to just detect it!!!

[ ] Develop "Select File" function (for LDR/LIB/etc)
- should only take a file as a parameter
- set as function to just return the file name

[ ] Bad file extension - .BAT file --> .BTR (for Batters)

[ ] Change name of major routines
- LEAGCMPL --> CMPLEAG

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

[ ] AutoPlay global variable
- If AP=1 and u5 = 6 or u5...
- Use this conditional once to set a flag
	- Use that flag later in the game to make decisions
	- Give the flag a user-friendly name like silentPBP

[ ] Popup Menus
There are no separate menus for Batter, Pitcher
- I've added the option in for grins
- However neither works right (would have to change to screen 0

Pop-Up Batter doesn't draw it's own box
- Update routines so that each box is drawn in its own routine
- Each call needs to take a keypress before continuing to gfx

[ ] Update RECORDS file(s) using real-world data
