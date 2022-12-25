## GENERAL ##
- Deleting box schedules when there are none Crashes one of my routines in QPProEqu
- If you say there are stats for a team, and there's not, there is no error checking


	
## Game Functions ##

#### Autoscheduler ####
Generate Season Schedule
- has issues displaying all teams in generated schedule
- not clear exactly what the code is meant to do
- doesn't appear to work quite right in original either

#### Input New Team ####
- If you hit ESC with blank data, it tries to move forward
- This will eventually cause a crash when it tries to write the file
- Add data checking at first screen to make sure Disk ID / Name are not null

#### View Team Rosters / Ratings ####
- ViewHittingFielding
- BAVG
	- Acts funny with value of 1000
	- Problem in original game too
	
#### View Results for League ####
- Throws an error if ATBATS is 0
- Original game does not throw error but acts odd
- Need to simply accommodate these situations
