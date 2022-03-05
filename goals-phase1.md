
** PHASE 1 - BASIC FIXES / UI IMPROVEMENT

[ ] Make menu wording consistent
	There are slight variations between games

[ ] Improve DECLARE statements
	Adjust inconsistent in variable names
		DECLARE FUNCTION EXIST (FILENAME$)
			vs
		DECLARE FUNCTION EXIST% (FILENAME$)
	Migrate to DECLARE.BAS file for project

[ ] Inclusion of Color Constants

[ ] Create "DEFAULT" files, by default!

[ ] Fix teams disk prompts
	when prompted for a disk ID, hitting ENTER shouldn't create a blank file
	instead, it should return to the prompt

[ ] Fix data entry menus
	When "saving" changes for Football, hit "ENTER" to proceed through data entry menus
	This would be preferred over having to know a number
	The only downside could be you may skip a screen before you intend
	However it can still have a check for whether things are 100%

[ ] Add support for more colors for teams (if possible)
