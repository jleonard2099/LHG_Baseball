Const SS_B_Bats = 0 '-1=Left, 0=Switch, 1=Right'

Const SS_B_G = 3
Const SS_B_AB = 4
Const SS_B_R = 5
Const SS_B_H = 6
Const SS_B_2B = 7
Const SS_B_3B = 8
Const SS_B_HR = 9
Const SS_B_RBI = 10
Const SS_B_W = 11
Const SS_B_K = 12
Const SS_B_SB = 13
Const SS_B_CS = 14
Const SS_B_Arm = 15
Const SS_B_Run = 16
Const SS_B_Range = 17
Const SS_B_GndPct = 18
Const SS_B_FldPct = 19
Const SS_B_LgBAvg = 20
'21 is unknown (INJURY?)'
Const SS_B_Pos1 = 22
Const SS_B_Pos2 = 23
Const SS_B_Pos3 = 24
Const SS_B_Pos4 = 25
Const SS_B_FldPct1 = 26
Const SS_B_FldPct2 = 27
Const SS_B_FldPct3 = 28
Const SS_B_FldPct4 = 29
Const SS_B_Bunt = 30
'31 is unknown (INJURY?)'
'32 is INJURY?'

Const SS_B_Arm1   = 33
Const SS_B_Range1 = 34
Const SS_B_Arm2   = 35
Const SS_B_Range2 = 36
Const SS_B_Arm3   = 37
Const SS_B_Range3 = 38
Const SS_B_Arm4   = 39
Const SS_B_Range4 = 40

' 41-79 could be SPLITS - at least 41-55 are '
Const SS_B_LHP_AB = 42
Const SS_B_LHP_H = 43
'44 is unknown'
'45 is unknown'
'46 is unknown'
'47 is unknown'
'48 is unknown'
Const SS_B_RHP_AB = 49
Const SS_B_RHP_H = 50
'51 is unknown'
'52 is unknown'
'53 is unknown'
'54 is unknown'
'55 is unknown'

'80 is unknown'

Const SS_P_Throws = 0
Const SS_P_W = 1
Const SS_P_L = 2
Const SS_P_Sv = 3
Const SS_P_G = 4
Const SS_P_GS = 5
Const SS_P_IP = 6
Const SS_P_H = 7
Const SS_P_BB =  8
Const SS_P_K = 9
Const SS_P_ERA100 = 10 'ERA*100
Const SS_H_BAvg = 11
Const SS_P_GndPct = 12
Const SS_P_HR = 13
Const SS_P_LgBAvg = 14
Const SS_P_WP = 15
Const SS_P_CG = 16
Const SS_P_FldPct = 17
Const SS_P_HoldRunner = 18

Const SS_P_H_G  = 19
Const SS_P_H_AB = 20
Const SS_P_H_R  = 21
Const SS_P_H_H  = 22
Const SS_P_H_2B = 23
Const SS_P_H_3B = 24
Const SS_P_H_HR = 25
Const SS_P_H_RBI= 26
Const SS_P_H_BB = 27
Const SS_P_H_SO = 28
Const SS_P_H_SB = 29
Const SS_P_H_CS = 30
Const SS_P_Range= 31
Const SS_P_Bunt = 32
Const SS_P_SH   = 33

Const SS_P_Rest = 35

' 37-88 could be SPLITS'

Const SS_P_LHP_AB = 51 
Const SS_P_LHP_H = 52

Const SS_P_RHP_AB = 58
Const SS_P_RHP_H = 59


Const LG_B_AB  = 0
Const LG_B_H   = 1
Const LG_B_R   = 2
Const LG_B_RBI = 3
Const LG_B_Dbl = 4
Const LG_B_Trp = 5
Const LG_B_HR  = 6
Const LG_B_W   = 7
Const LG_B_K   = 8
Const LG_B_SB  = 9
Const LG_B_CS  = 10
Const LG_B_E   = 11
Const LG_B_HS  = 12
Const LG_B_LS  = 13
Const LG_B_G   = 14
Const LG_B_PO  = 16
Const LG_B_A   = 17

Const LG_P_IP3  = 0
Const LG_P_H    = 1
Const LG_P_R    = 2
Const LG_P_ER   = 3
Const LG_P_BB   = 4
Const LG_P_K    = 5
Const LG_P_W    = 6
Const LG_P_L    = 7
Const LG_P_G    = 8
Const LG_P_GS   = 9
Const LG_P_CG   = 10
Const LG_P_Sv   = 12
Const LG_P_HR   = 14
Const LG_P_RInh = 17
Const LG_P_BSv  = 19
Const LG_P_Hold = 22

Const LG_P_E    = 34 '???'

Const LIVE_BAvg=0
Const LIVE_2B = 1
Const LIVE_3B = 2
Const LIVE_HR = 3
Const LIVE_K  = 4
Const LIVE_BB = 5
Const LIVE_L_HR=6
Const LIVE_R_HR=7


Const TM_BAVG   = 1
Const TM_K      = 2
Const TM_BB     = 3
Const TM_HR     = 4
Const TM_XX     = 5
Const TM_2B     = 6
Const TM_3B     = 7
Const TM_FOUL   = 8
Const TM_OUTDOORS= 9
Const TM_GRASS  = 10
Const TM_FG     = 11
Const TM_BG     = 12
Const TM_PARK_ID= 13

Const BASE_EMPTY= 0
Const BASE_1    = 1
Const BASE_2    = 2
Const BASE_3    = 3
Const BASE_1_2  = 4
Const BASE_1_3  = 5
Const BASE_2_3  = 6
Const BASE_FULL = 7

Const INF_NORMAL = 0
Const INF_CORNERS_IN = 1
Const INF_INFIELD_IN = 2
Const INF_GUARD_LINES = 3
' Const INF_SHIFT = 4
' Const INF_PULL = 5

Const POS_P  = 1
Const POS_C  = 2
Const POS_1B = 3
Const POS_2B = 4
Const POS_3B = 5
Const POS_SS = 6
Const POS_LF = 7
Const POS_CF = 8
Const POS_RF = 9    
Const POS_DH = 0

Const MGR_CLOSE_STRATEGY     = 581
Const MGR_INN_SAVE_OPP       = 582
Const MGR_INN_AHEAD          = 583
Const MGR_INN_TIED_AHEAD     = 584
Const MGR_TIRED_AHEAD        = 585
Const MGR_TIRED_TIED_AHEAD   = 586
Const MGR_SAVE_OPP_OCCURS    = 587
Const MGR_INN_CLOSE_STRATEGY = 588
Const MGR_USE_BOTH_SETUP_CLOSER = 589

Const EVENT_HPB   = 1
Const EVENT_BB_OR_INTERFERENCE= 2
Const EVENT_BALK  =  4
Const EVENT_PB    =  5
Const EVENT_WP    =  6
Const EVENT_1B    =  7
Const EVENT_2B    =  8
Const EVENT_3B    =  9
Const EVENT_HR    = 10
Const EVENT_PSBL_SINGLE = 12
Const EVENT_DP          = 15
Const EVENT_FLY_DEEP    = 17
Const EVENT_FLY_REGULAR = 18
Const EVENT_FLY_SHALLOW = 19
Const EVENT_INF_POPUP   = 20
Const EVENT_RUNNER_DBLD = 21
Const EVENT_BUNT        = 23
