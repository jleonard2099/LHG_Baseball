Declare Sub ChooseTeam (yearNumber$, numberTeams, teamIdx)
Declare Sub SelectTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%)
Declare Sub ReadTeam (teamYear$, targetTeam$, teamIdx, dataOK)
Declare Sub ReadExtraData (teamYear$, targetTeam$, teamIdx)
Declare Sub UserFilePrompt (reportFile$)

Declare Sub ReadTeamStats (teamName$, statFile$)
Declare Sub ReadScheduleStats (statFile$, numberGames)
Declare Sub ReadSeasonRecords (recordFile$)
Declare Sub ReadGameRecords (recordFile$)
Declare Sub ReadLeagueStats (statFile$, statType)

Declare Function nameStatFile$ (teamName$)
Declare Function CheckString% (targetString$)
Declare Function ChooseFile$(fileSpec$, fileDesc$)

Const TEAM_SIZE_BYTES = 4342
Const DATA_SIZE_BYTES = 4108
Const MGR_SIZE_BYTES = 2000
Const TEAM_FILE_NAME$ = "FCTEAMS"
Const DATA_FILE_NAME$ = "LR"
Const MGR_FILE_NAME$ = "MGR"

Dim previousYear$

'-- transition away from this
Dim Shared fileLength&

Dim CK

Dim Manager$, teamAbbrev$, Stadium$

Dim batterRatings(0 To 22, 0 To 79), pitcherRatings(21, 88)
Dim teamRatings(1 To 13)

'These are used for the LR. file
'Presumably this was the new way to load
' LEFTY / RIGHTY, since the game code references that
' it will perform those calculations the "old way"
Dim teamSA(24), teamTS(11)

Dim batterNames$(0 To 23), pitcherNames$(0 To 21)
