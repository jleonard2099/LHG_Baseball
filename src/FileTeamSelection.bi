Declare Sub ChooseTeam (yearNumber$, numberTeams, teamIdx)
Declare Sub SelectTeam (selectedIdx)
Declare Sub ReadTeam (yearNumber$, teamIdx)
Declare Sub ReadExtraData (teamYear$, targetTeam$, teamIdx)
Declare Function nameStatFile$ (teamName$)
Declare Function ChooseFile$(fileSpec$, fileDesc$)

Const TEAM_SIZE_BYTES = 4342
Const DATA_SIZE_BYTES = 4108
Const TEAM_FILE_NAME$ = "FCTEAMS"
Const DATA_FILE_NAME$ = "LR"

Dim previousYear$
