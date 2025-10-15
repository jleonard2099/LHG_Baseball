'$Include: 'Variables.bi'
Dim Shared parkDesc$

Open "parkdata.csv" For Append As #11

Print #11, "Park Name,LF,LCF,CF,RCF,RF,Turf,Dome,Wind,";
Print #11, "AprBaseTemp,AprDeltaTemp,AprWindRL,AprWindLR,AprWindFromINL,AprWindFromINC,AprWindFromINR,AprWindToOUTL,AprWindToOUTC,AprWindToOUTR,AprWS0-9,AprWS10-14,AprWS15-19,AprWS20+,AprPrecip,";
Print #11, "MayBaseTemp,MayDeltaTemp,MayWindRL,MayWindLR,MayWindFromINL,MayWindFromINC,MayWindFromINR,MayWindToOUTL,MayWindToOUTC,MayWindToOUTR,MayWS0-9,MayWS10-14,MayWS15-19,MayWS20+,MayPrecip,";
Print #11, "JuneBaseTemp,JuneDeltaTemp,JuneWindRL,JuneWindLR,JuneWindFromINL,JuneWindFromINC,JuneWindFromINR,JuneWindToOUTL,JuneWindToOUTC,JuneWindToOUTR,JuneWS0-9,JuneWS10-14,JuneWS15-19,JuneWS20+,JunePrecip,";
Print #11, "JulyBaseTemp,JulyDeltaTemp,JulyWindRL,JulyWindLR,JulyWindFromINL,JulyWindFromINC,JulyWindFromINR,JulyWindToOUTL,JulyWindToOUTC,JulyWindToOUTR,JulyWS0-9,JulyWS10-14,JulyWS15-19,JulyWS20+,JulyPrecip,";
Print #11, "AugBaseTemp,AugDeltaTemp,AugWindRL,AugWindLR,AugWindFromINL,AugWindFromINC,AugWindFromINR,AugWindToOUTL,AugWindToOUTC,AugWindToOUTR,AugWS0-9,AugWS10-14,AugWS15-19,AugWS20+,AugPrecip,";
Print #11, "SeptBaseTemp,SeptDeltaTemp,SeptWindRL,SeptWindLR,SeptWindFromINL,SeptWindFromINC,SeptWindFromINR,SeptWindToOUTL,SeptWindToOUTC,SeptWindToOUTR,SeptWS0-9,SeptWS10-14,SeptWS15-19,SeptWS20+,SeptPrecip"

For X = 1 To 100

    GetStadiumWeather (X)
    Print #11, parkDesc$; ",";
    Print #11, parkLF; ","; parkLCF; ",";
    Print #11, parkCF; ","; parkRCF; ",";
    Print #11, parkRF; ","; turfType; ",";
    Print #11, DOME; ","; windRating; ",";
    For I = 0 To 5
        For I1 = 0 To 14
            Print #11, monthlyRatings(I, I1); ",";
        Next
    Next
    Print #11,

Next X

Close #11



Sub GetStadiumWeather (parkID)

    Shared monthlyRatings()

    Shared avgAttendance&

    Shared parkCF
    Shared parkLF, parkLCF
    Shared parkRF, parkRCF
    Shared DOME, turfType, windRating

    'The layout of the PARKS.DB file is as follows:
    'Park Name (40 chars)
    'Attendance
    'Park Ratings - LF, LCF, CF, RCF, RF, GRASS, DOME, windRating
    '6 Months Data, 15 weather factors
    '   0 = Base Temp
    '   1 = Delta Temp
    '   2 = Wind R->L
    '   3 = Wind L->R
    '   4 = Wind from INL (infield, L)
    '   5 = Wind from INC (infield, C)
    '   6 = Wind from INR (infield, R)
    '   7 = Wind to OUTL (outfield, L)
    '   8 = Wind to OUTC (outfield, C)
    '   9 = Wind to OUTR (outfield, R)
    '   10 = WS09 (windspeed 0-9)
    '   11 = WS1014 (windspeed 10-14)
    '   12 = WS1519 (windspeed 15-19)
    '   13 = WS20   (windseed 20+)
    '   14 = PRECIP

    Open "PARKS.DB" For Random As #1 Len = 240

    Field #1, 40 As Q3$(0), 4 As Q3$(1), 2 As Q3$(2), 2 As Q3$(3), 2 As Q3$(4), 2 As Q3$(5), 2 As Q3$(6), 2 As Q3$(7), 2 As Q3$(8), 2 As Q3$(9)

    For I = 0 To 5
        For I1 = 0 To 14
            Field #1, 60 + I * 30 + I1 * 2 As X$, 2 As Q3$(10 + I1 + I * 15)
        Next
    Next

    Get #1, parkID

    parkDesc$ = Q3$(0)
    avgAttendance& = CVL(Q3$(1))
    parkLF = CVI(Q3$(2))
    parkLCF = CVI(Q3$(3))
    parkCF = CVI(Q3$(4))
    parkRCF = CVI(Q3$(5))
    parkRF = CVI(Q3$(6))
    turfType = CVI(Q3$(7))
    DOME = CVI(Q3$(8))
    windRating = CVI(Q3$(9))

    'April(0) - September(5)
    For I = 0 To 5
        For I1 = 0 To 14
            monthlyRatings(I, I1) = CVI(Q3$(10 + I1 + I * 15))
        Next
    Next

    Close #1

End Sub
