Declare Sub ChooseTeam (yearNumber$, numberTeams, teamIdx)
Declare Sub SelectTeam (selectedIdx)
Declare Sub ReadTeam (yearNumber$, teamIdx)
Declare Function nameStatFile$ (teamName$)
Declare Function ChooseFile$(fileSpec$, fileDesc$)

Const TEAM_SIZE_BYTES = 655
Const TEAM_FILE_NAME = "BASK"