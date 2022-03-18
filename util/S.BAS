'$INCLUDE: 'KeyInput.BI'

6DIM S$(1),A$(1),L%(1,13),T1(1),B%(1,22,41),P%(1,21,36),B$(1,22),P$(1,21),P8%(1,21,41),H$(4)

7DIM B8%(1,22,21),P6%(1),P1%(1),S8%(1,1),B1(1),B0%(3,2),B1%(3,1),P2(1),S1%(1),B9%(1,10),NN$(1)

8DIM S%(1,26),B3%(1,9),B7%(1,9),O%(1),S6%(1,2),B1$(4),B4%(1,7,9),X$(12),C$(10),F$(10)

9DIM V(1),V1(1),D3%(1),M%(9),P2%(1),K9(1),A5%(4),ZB%(1,6),U$(4),DP%(1),EJ$(20),IJ$(20)

10DIM X0%(1,2,23),D0%(1),P5%(1,21,41),B5%(1,22,21),TP%(11,2),L0%(1),T3%(22)
DIM T%(22),P4%(22),B2%(1,22),P3%(1,21),P0%(21,41),B6%(22,21),ft%(6),fot%(5,6)
DIM H%(162),HT$(162),V%(162),PL$(162),W$(162),L$(162),ING%(162),PH%(22,1)
DIM NG%(18),YN$(1),Q$(23),H0$(1),TM$(4),SC$(4),PC$(4),N$(1),D$(2),M$(1)
DIM H0%(1),CZ%(1),BP(1),BT%(1,9,9),CF%(9,2),LB%(9,1),SO%(5,2),UM%(1,9,4)
DIM LD%(9),DL%(1,9,3),OH%(9),LL%(9,3)

DATA first,second,third,short
for i=3 to 6:read f$(i):next

11 H0$(0)=" ":H0$(1)="H"
	TM$(1)="HOT":TM$(2)="WARM":TM$(3)="COOL":TM$(4)="COLD"
	SC$(1)="CLEAR":SC$(2)="PARTLY CLOUDY":SC$(3)="OVERCAST":SC$(4)="DOME"
	PC$(1)="THUNDERSTORMS":PC$(2)="SHOWERS":PC$(3)="DRIZZLE":PC$(4)="NONE"
	GOTO 9414

65 GOSUB 220:GOTO 5000

99 I=0

100 COLOR 15,0:CLS:GOSUB 115:F$=MID$(N$(I),4,QQ):R$=F$+"."+LEFT$(N$(I),2)

102 ON ERROR GOTO 110

103 OPEN D$(2)+R$ FOR INPUT AS #1

104 INPUT #1,A$:INPUT #1,W2%:INPUT #1,L2%

105 FOR I1=0 TO 22:INPUT #1,K$:INPUT #1,K%:NEXT I1

106 FOR I1=0 TO 21:INPUT #1,K$:INPUT #1,K%:NEXT I1

107 FOR I1=0 TO 22:FOR I2=0 TO 21:INPUT #1,B5%(I,I1,I2):NEXT:NEXT

108 FOR I1=0 TO 21:FOR I2=0 TO 41:INPUT #1,P5%(I,I1,I2):NEXT:NEXT

109 CLOSE #1:GOTO 111

110 CLOSE #1:RESUME 111

111 ON ERROR GOTO 0

112 IF I=1 THEN RETURN

113 I=1:GOTO 100

114 RETURN

115 N$(I)=A$(I):FOR XX=4 TO 11:IF MID$(N$(I),XX,1)=" " OR MID$(N$(I),XX,1)="." THEN MID$(N$(I),XX,1)="_"

116 NEXT XX:QQ=8:FOR XX=11 TO 4 STEP -1:IF MID$(N$(I),XX,1)="_" THEN QQ=XX-4:GOTO 118

117 GOTO 119

118 NEXT XX

119 RETURN

135 S8%(P9,0)=INT(((P%(P9,P1%(P9),8)+P%(P9,P1%(P9),7))/P%(P9,P1%(P9),4))+.5)
	IF S8%(P9,0)<3 THEN S8%(P9,0)=3

136 S8%(P9,1)=3

138 IF P6%(P9)>1 THEN S8%(P9,1)=0

139 GOSUB 156:RETURN

140 CLS

141 LOCATE 1,1
	COLOR 14:PRINT "#";:COLOR L%(P9,11),L%(P9,12):PRINT A$(P9);:COLOR 14,0
	LOCATE, 16:PRINT "B   POS    G  AB 2B 3B HR RBI  BB  SO SRAB  SB CS G%  BA"
	FOR I=2 TO 13:LOCATE I,1:PRINT STRING$(78,32):NEXT:LOCATE 2,1
	IF PG=0 THEN AA=0:BB=12
	IF PG=1 THEN AA=13:BB=22
	FOR I=AA TO BB:LOCATE, 1

142 IF B%(P9,I,21)<>0 THEN 150
	COLOR 2,0

143 IF I<10 THEN PRINT USING "#";I;

144 IF I>9 THEN PRINT X$(I-10);
	COLOR 15,0

145 PRINT TAB(3); B$(P9,I); TAB(17); B1$(B%(P9,I,0)+2);
	FOR XX=20 TO 23
	PRINT TAB(XX);:IF B%(P9,I,XX+2)<>-1 THEN PRINT USING "#";B%(P9,I,XX+2);
	NEXT

147 PRINT TAB(24);

148 PRINT USING " ### ### ## ## ## ### ### ### #";B%(P9,I,3),B%(P9,I,4),B%(P9,I,7),B%(P9,I,8),B%(P9,I,9),B%(P9,I,10),B%(P9,I,11),B%(P9,I,12),B%(P9,I,16);
	PRINT USING "#";B%(P9,I,17);:PRINT USING "#";B%(P9,I,15);:PRINT USING "# ";B%(P9,I,30);

149 PRINT USING "### ## ## ";B%(P9,I,13),B%(P9,I,14),B%(P9,I,18);:PRINT TAB(69); ".";
	IF B%(P9,I,4)>0 THEN PRINT USING "###";INT(((B%(P9,I,6)/B%(P9,I,4))+.0005)*1000):GOTO 150
	PRINT

150 NEXT I:RETURN

151 FOR I=1 TO 9

152 IF B7%(P9,I)=1 THEN 154

153 B%(P9,B3%(P9,I),21)=1

154 NEXT I

155 RETURN

156 IF I%<=5 AND S6%(P9,0)-S6%(1-P9,0)>0 THEN P2%(P9)=P1%(P9)

157 RETURN

160 FOR I=0 TO 1:FOR I1=1 TO 9:B3%(I,I1)=-1:B7%(I,I1)=-1:NEXT I1:NEXT I

162 RETURN

165 IF UCASE$(I$)="A" THEN I$="10"
	IF UCASE$(I$)="B" THEN I$="11"
	IF UCASE$(I$)="C" THEN I$="12"
	IF UCASE$(I$)="D" THEN I$="13"
	IF UCASE$(I$)="E" THEN I$="14"
	IF UCASE$(I$)="F" THEN I$="15"
	IF UCASE$(I$)="G" THEN I$="16"
	IF UCASE$(I$)="H" THEN I$="17"
	IF UCASE$(I$)="I" THEN I$="18"
	IF UCASE$(I$)="J" THEN I$="19"
	IF UCASE$(I$)="K" THEN I$="20"
	IF UCASE$(I$)="L" THEN I$="21"
	IF UCASE$(I$)="M" THEN I$="22"

177 RETURN

200 CLS
	COLOR 14:PRINT "#";:COLOR L%(P9,11),L%(P9,12):PRINT A$(P9);:COLOR 14,0
	LOCATE,18: PRINT "T  IP   H   G GS CG SV HR  BB  SO  W- L   ERA  H G%"
	AA=0:BB=21

201 FOR I=AA TO BB:IF P%(P9,I,15)>0 THEN 206
	COLOR 2,0

202 IF I<=9 THEN PRINT USING "#";I;

203 IF I>9 THEN PRINT X$(I-10);
	COLOR 15,0

204 LOCATE,3:PRINT P$(P9,I);:LOCATE,18:PRINT B1$(P%(P9,I,0)+2);
	PRINT USING " ### ### ### ## ## ## ## ### ### ##";P%(P9,I,6),P%(P9,I,7),P%(P9,I,4),P%(P9,I,5),P%(P9,I,16),P%(P9,I,3),P%(P9,I,13),P%(P9,I,8),P%(P9,I,9),P%(P9,I,1);:PRINT "-";
	PRINT USING "##";P%(P9,I,2);
	LOCATE,59:PRINT USING"##.## # ##";P%(P9,I,10)/100,P%(P9,I,18),P%(P9,I,12)

206 NEXT I:IF I%>0 THEN 217

217 RETURN

220 ON ERROR GOTO 0
	RANDOMIZE TIMER
	CLS:PRINT "Please Wait...":PRINT:PRINT "Calculating Pitcher/Batter Park Effects...":FOR I=0 TO 1:I1=1-I

221 I6=L%(I1,3)/1000
	I2=P%(I1,P1%(I1),8)/(((P%(I1,P1%(I1),6)*3)*.94)+P%(I1,P1%(I1),7)+P%(I1,P1%(I1),8))

222 FOR I3=V(I) TO V1(I)
	IF B7%(I,I3)=1 AND P%(I,P1%(I),11)<>999 THEN I7=.037:GOTO 223
	IF B7%(I,I3)=1 AND P%(I,P1%(I),27)+P%(I,P1%(I),20)=0 THEN I7=0:GOTO 223
	IF B7%(I,I3)=1 THEN I7=P%(I,P1%(I),27)/(P%(I,P1%(I),27)+P%(I,P1%(I),20)):GOTO 223
	IF B%(I,B3%(I,I3),4)+B%(I,B3%(I,I3),11)=0 THEN I7=0:GOTO 223
	I7=B%(I,B3%(I,I3),11)/(B%(I,B3%(I,I3),4)+B%(I,B3%(I,I3),11))

223 GOSUB 285:B4%(I,5,I3)=I8

224 NEXT I3:GOSUB 280

225 I6=P%(I1,P1%(I1),14)/1000:I7=P%(I1,P1%(I1),7)/(((P%(I1,P1%(I1),6)*3)*.94)+P%(I1,P1%(I1),7)):I2=L%(I1,1)/1000:GOSUB 285:I9=I8/1000

226 FOR I3=V(I) TO V1(I)
	IF B7%(I,I3)=1 AND P%(I,P1%(I),11)<>999 THEN I7=P%(I,P1%(I),11)/1000:I6=P%(I,P1%(I),14)/1000:GOTO 228
	IF B7%(I,I3)=1 AND P%(I,P1%(I),20)=0 THEN I7=0:GOTO 227
	IF B7%(I,I3)=1 THEN I7=P%(I,P1%(I),22)/P%(I,P1%(I),20)

227 IF B7%(I,I3)=1 THEN I6=P%(I,P1%(I),14)/1000:GOTO 228
	IF B%(I,B3%(I,I3),4)=0 THEN I7=0:GOTO 230
	I7=B%(I,B3%(I,I3),6)/B%(I,B3%(I,I3),4)

230 I6=B%(I,B3%(I,I3),20)/1000

228 I2=L%(I,1)/1000:GOSUB 285:I7=I8/1000:I6=L%(I,1)/1000:I2=I9:GOSUB 285:B4%(I,0,I3)=I8

229 NEXT I3

231 FOR I3=V(I) TO V1(I)
	IF B7%(I,I3)=1 AND P%(I,P1%(I),11)<>999 THEN B4%(I,1,I3)=(130*(I9*1000))/L%(I1,1):B4%(I,2,I3)=(19*(I9*1000))/L%(I1,1):B4%(I,3,I3)=37:GOTO 235
	E2=(I9*1000)/L%(I1,1)
	IF B7%(I,I3)=1 AND P%(I,P1%(I),22)=0 THEN B4%(I,1,I3)=0:B4%(I,2,I3)=0:B4%(I,3,I3)=0:GOTO 235
	IF B7%(I,I3)=1 THEN B4%(I,1,I3)=INT((P%(I,P1%(I),23)/P%(I,P1%(I),22))*1000)*E2*.95
	IF B7%(I,I3)=1 THEN B4%(I,2,I3)=INT((P%(I,P1%(I),24)/P%(I,P1%(I),22))*1000)*E2
	IF B7%(I,I3)=1 THEN B4%(I,3,I3)=INT((P%(I,P1%(I),25)/P%(I,P1%(I),22))*1000):GOTO 235
	if b%(i,b3%(i,i3),6)=0 then b4%(i,1,i3)=0:b4%(i,2,i3)=0:b4%(i,3,i3)=0:goto 235
	B4%(I,1,I3)=INT((B%(I,B3%(I,I3),7)/B%(I,B3%(I,I3),6))*1000)*E2*.95
	B4%(I,2,I3)=INT((B%(I,B3%(I,I3),8)/B%(I,B3%(I,I3),6))*1000)*E2
	B4%(I,3,I3)=INT((B%(I,B3%(I,I3),9)/B%(I,B3%(I,I3),6))*1000)

235 I6=L%(I1,4)/1000:I7=B4%(I,3,I3)/1000:I2=P%(I1,P1%(I1),13)/P%(I1,P1%(I1),7):GOSUB 285:B4%(I,3,I3)=I8

239 NEXT I3

240 I4=P%(I1,P1%(I1),0)

241 FOR I3=V(I) TO V1(I)
	I2=B%(I,B3%(I,I3),0)
	IF B7%(I,I3)=1 AND P%(I,P1%(I),0)=-1 AND P%(I1,P1%(I1),0)=1 THEN A=1.04:A1=.88:GOSUB 260

242 IF B7%(I,I3)=1 AND P%(I,P1%(I),0)=-1 AND P%(I1,P1%(I1),0)=-1 THEN A=.93:A1=1.16:GOSUB 260

243 IF B7%(I,I3)=1 AND P%(I,P1%(I),0)=1 AND P%(I1,P1%(I1),0)=-1 THEN A=1.02:A1=0.98:GOSUB 260

244 IF B7%(I,I3)=1 AND P%(I,P1%(I),0)=1 AND P%(I1,P1%(I1),0)=1 THEN A=.97:A1=1.10:GOSUB 260

245 IF B7%(I,I3)<>1 AND I2=-2 AND I4=1 THEN A=1.04:A1=.96:GOSUB 260

246 IF B7%(I,I3)<>1 AND I2=-2 AND I4=-1 THEN A=.78:A1=1.22:GOSUB 260

247 IF B7%(I,I3)<>1 AND I2=2 AND I4=1 THEN A=.78:A1=1.22:GOSUB 260

248 IF B7%(I,I3)<>1 AND I2=2 AND I4=-1 THEN A=1.04:A1=.96:GOSUB 260

249 IF B7%(I,I3)<>1 AND I2=-1 AND I4=-1 THEN A=.93:A1=1.16:GOSUB 260

250 IF B7%(I,I3)<>1 AND I2=-1 AND I4=1 THEN A=1.04:A1=0.88:GOSUB 260

251 IF B7%(I,I3)<>1 AND I2=1 AND I4=1 THEN A=.97:A1=1.10:GOSUB 260

252 IF B7%(I,I3)<>1 AND I2=1 AND I4=-1 THEN A=1.02:A1=0.98:GOSUB 260
	IF B7%(I,I3)<>1 AND I2=0 AND I4=-1 THEN A=1.01:A1=.86:GOSUB 260
	IF B7%(I,I3)<>1 AND I2=0 AND I4=1 THEN A=0.99:A1=.93:GOSUB 260

253 B4%(I,6,I3)=B4%(I,3,I3):B4%(I,7,I3)=B4%(I,3,I3):NEXT I3

255 FOR I3=V(I) TO V1(I)

256 IF I=1 THEN B4%(I,0,I3)=B4%(I,0,I3)+5

257 IF I<>1 THEN B4%(I,0,I3)=B4%(I,0,I3)-5

258 NEXT I3

259 GOTO 262

260 FOR I6=0 TO 3
	B4%(I,I6,I3)=B4%(I,I6,I3)*A
	NEXT I6
	B4%(I,5,I3)=B4%(I,5,I3)*A:' NEWLY ADDED

261 B4%(I,4,I3)=B4%(I,4,I3)*A1
	RETURN

262 IF N=2 THEN 273

263 FOR I2=V(I) TO V1(I):IF I=1 THEN B4%(I,1,I2)=INT(B4%(I,1,I2)*(L%(I,6)/100)+.5):B4%(I,2,I2)=INT(B4%(I,2,I2)*(L%(I,7)/100)+.5)

264 IF I<>1 THEN B4%(I,1,I2)=INT(B4%(I,1,I2)*(L%(I1,6)/100)*((200-L%(I,6))/100)+.5):B4%(I,2,I2)=INT(B4%(I,2,I2)*(L%(I1,7)/100)*((200-L%(I,7))/100)+.5)

265 IF K9(I)>1000 THEN I3=100:I6=INT(K9(I)/1000)*WF:I7=(K9(I)-INT((K9(I)/1000))*1000)*WF

266 IF K9(I1)>1000 THEN I4=100:I8=INT(K9(I1)/1000)*WF:I9=(K9(I1)-INT((K9(I1)/1000))*1000)*WF

267 IF K9(I)<1000 THEN I3=K9(I)*WF:I6=I3:I7=I3

268 IF K9(I1)<1000 THEN I4=K9(I1)*WF:I8=I4:I9=I4

269 IF I=1 THEN B4%(I,3,I2)=INT((B4%(I,3,I2)*(I3/100))+.5):B4%(I,6,I2)=INT((B4%(I,6,I2)*(I6/100))+.5):B4%(I,7,I2)=INT((B4%(I,7,I2)*(I7/100))+.5)

270 IF I<>1 THEN B4%(I,3,I2)=INT((B4%(I,3,I2)*(I4/100)*((200-I3)/100))+.5):B4%(I,6,I2)=INT((B4%(I,6,I2)*(I8/100)*((200-I6)/100))+.5):B4%(I,7,I2)=INT((B4%(I,7,I2)*(I9/100)*((200-I7)/100))+.5)

271 NEXT I2

272 NEXT I

273 RETURN

275 RN=INT(RND(1)*100)+1:RETURN

280 I2=P%(I1,P1%(I1),9)/((P%(I1,P1%(I1),6)*3)*.94):I6=L%(I1,2)/1000

281 FOR I3=V(I) TO V1(I)
	IF B7%(I,I3)=1 AND P%(I,P1%(I),11)<>999 THEN I7=.42:GOTO 282
	IF B7%(I,I3)=1 AND (P%(I,P1%(I),20)-P%(I,P1%(I),22)=0 OR P%(I,P1%(I),22)=0) THEN I7=0:GOTO 282
	IF B7%(I,I3)=1 THEN I7=P%(I,P1%(I),28)/(P%(I,P1%(I),20)-P%(I,P1%(I),22)):GOTO 282
	IF B%(I,B3%(I,I3),4)=0 OR B%(I,B3%(I,I3),4)-B%(I,B3%(I,I3),6)=0 THEN I7=0:GOTO 282
	I7=B%(I,B3%(I,I3),12)/(B%(I,B3%(I,I3),4)-B%(I,B3%(I,I3),6))

282 GOSUB 285:B4%(I,4,I3)=I8

283 NEXT I3:RETURN

285 I0=(I7*I2*(1-I6)):I8=I0/(I0+((1-I7)*(1-I2)*I6)):I8=INT((I8*1000)+.0005):RETURN

299 '

300 COLOR ,0:CLS
	FOR X=5 TO 25:LOCATE X,46:COLOR ,2:PRINT STRING$(34,32);:NEXT:COLOR ,0:LOCATE 1,1
	X%=1:Y%=9
	IF I%>9 THEN X%=10:Y%=18
	IF I%>18 THEN X%=19:Y%=27
	PRINT TAB(10);
	FOR X=0 TO 2:PRINT USING "## ## ##  ";X%+3*X,X%+3*X+1,X%+3*X+2;:NEXT
	PRINT " R  H  E ";:COLOR 1:PRINT CHR$(179);:COLOR 15:PRINT " BALL:  ";:COLOR 14:PRINT BC%;:COLOR 1:PRINT CHR$(179);:COLOR 15:PRINT"WIND: ";:COLOR 14:PRINT WI$
	FOR I=0 TO 1:LOCATE I+2:PRINT LEFT$(A$(I),3)NN$(I);
	FOR I2=X% TO Y%
	IF P=0 AND I=0 AND I2>I%-1 AND S%(I,I%)=0 OR P=0 AND I=0 AND I2>I% AND S%(I,I%)>0 OR P=1 AND I2>I% AND I=0 THEN 310
	IF P=0 AND I=1 AND I2>I%-1 OR P=1 AND I=1 AND S%(I,I%)=0 AND I2>I%-1 OR P=1 AND I=1 AND S%(I,I%)>0 AND I2>I% THEN 310
	GOSUB 390
	NEXT I2

310 LOCATE,40:PRINT USING "## ## ## ";S6%(I,0),S6%(I,1),S6%(I,2);:COLOR 1:PRINT CHR$(179);:COLOR 15
	IF I=0 THEN PRINT " STRIKE:";:COLOR 14:PRINT U%;:COLOR 1:PRINT CHR$(179);:COLOR 15:PRINT "SKY:  ";:COLOR 14:PRINT SC$(SC%)
	IF I=1 THEN PRINT " OUT:   ";:COLOR 14:PRINT O%(0);:COLOR 1:PRINT CHR$(179);:COLOR 15:PRINT "TEMP:";:COLOR 14:PRINT TP%
	NEXT I
	COLOR 1:PRINT STRING$(20,196)CHR$(194)STRING$(23,196)CHR$(194)STRING$(3,196)CHR$(193)STRING$(11,196)CHR$(193)STRING$(19,196)
	FOR X=5 TO 12:LOCATE X,21:PRINT CHR$(179);:NEXT
	FOR X=5 TO 25:LOCATE X,45:PRINT CHR$(179);:NEXT
	COLOR 15,2
	LOCATE 5,58:COLOR L%(D,11),L%(D,12):PRINT LEFT$(B$(D,B9%(D,8)),10):COLOR 15,2
	LOCATE 6,59:PRINT USING "A:# R:#";B%(D,B9%(D,8),15),B%(D,B9%(D,8),17)
	LOCATE 7,59:PRINT USING "FA:####";B%(D,B9%(D,8),19)
	LOCATE 6,46:COLOR L%(D,11),L%(D,12):PRINT LEFT$(B$(D,B9%(D,7)),10);
	LOCATE ,70:PRINT LEFT$(B$(D,B9%(D,9)),10):COLOR 15,2
	LOCATE 7,47:PRINT USING "A:# R:#";B%(D,B9%(D,7),15),B%(D,B9%(D,7),17);
	LOCATE ,71:PRINT USING "A:# R:#";B%(D,B9%(D,9),15),B%(D,B9%(D,9),17)
	LOCATE 8,47:PRINT USING "FA:####";B%(D,B9%(D,7),19);
	LOCATE ,71:PRINT USING "FA:####";B%(D,B9%(D,9),19)
	LOCATE 10,49:COLOR L%(D,11),L%(D,12):PRINT LEFT$(B$(D,B9%(D,6)),10);
	LOCATE ,68:PRINT LEFT$(B$(D,B9%(D,4)),10):COLOR 15,2
	LOCATE 11,50:PRINT USING "R:#";B%(D,B9%(D,6),17);
	LOCATE ,69:PRINT USING "R:#";B%(D,B9%(D,4),17)
	LOCATE 12,50:PRINT USING "FA:####";B%(D,B9%(D,6),19);:LOCATE ,62
	PRINT CHR$(219);
	LOCATE ,69:PRINT USING "FA:####";B%(D,B9%(D,4),19)
	LOCATE 14,46:COLOR L%(D,11),L%(D,12):PRINT LEFT$(B$(D,B9%(D,5)),10);
	LOCATE ,70:PRINT LEFT$(B$(D,B9%(D,3)),10):COLOR 15,2
	LOCATE 15,47:PRINT USING "R:#";B%(D,B9%(D,5),17);:LOCATE 17,49
	PRINT CHR$(219);
	LOCATE ,78:PRINT CHR$(219);:LOCATE 15,71:PRINT USING "R:#";B%(D,B9%(D,3),17)
	LOCATE 16,47:PRINT USING "FA:####";B%(D,B9%(D,5),19);
	LOCATE ,71:PRINT USING "FA:####";B%(D,B9%(D,3),19)
	LOCATE 17,55:COLOR L%(D,11),L%(D,12):PRINT "["B1$(P%(D,P1%(D),0)+2)"] "LEFT$(P$(D,P1%(D)),10):COLOR 15,2
	LOCATE 18,59:PRINT USING "H:#";P%(D,P1%(D),18)
	LOCATE 19,59:PRINT USING "FA:####";P%(D,P1%(D),17)
	LOCATE 21,62:PRINT CHR$(219)
	LOCATE 23,58:COLOR L%(D,11),L%(D,12):PRINT LEFT$(B$(D,B9%(D,2)),10):COLOR 15,2
	LOCATE 24,59:PRINT USING "   A:#";B%(D,B9%(D,2),15);
	LOCATE 25,59:PRINT USING "FA:####";B%(D,B9%(D,2),19);
	COLOR 7,0:FOR X=5 TO 25:LOCATE X,1:PRINT STRING$(44,32);:NEXT
	COLOR 1:LOCATE 4,1:PRINT STRING$(20,196)CHR$(194)STRING$(23,196)CHR$(194)
	LOCATE 14,1:PRINT STRING$(20,196)CHR$(193)STRING$(23,196)CHR$(180)
	RETURN

315 IF P%(P,P1%(P),0)<0 THEN LOCATE 21,64
	IF P%(P,P1%(P),0)>0 THEN LOCATE 21,51
	PRINT X$:RETURN

320 IF B%(P,J,0)<0 OR B%(P,J,0)=0 AND P%(D,P1%(D),0)>0 THEN LOCATE 21,64
	X=LEN(X$):IF B%(P,J,0)>0 OR B%(P,J,0)=0 AND P%(D,P1%(D),0)<0 THEN LOCATE 21,51+10-X
	PRINT X$:RETURN

323 COLOR 14,0:I2=I%:I=P:LOCATE I+2:GOSUB 325:LOCATE, 40:PRINT USING"##";S6%(I,0);:LOCATE D+2:LOCATE,46:PRINT USING "##";S6%(D,2);:COLOR 15,0:RETURN

324 COLOR 14,0:S6%(P,1)=S6%(P,1)+1:LOCATE P+2,43:PRINT USING "##";S6%(P,1);:COLOR 15,0:RETURN

325 IF S%(P,I%)>0 THEN GOSUB 390

326 RETURN

327 LOCATE 17,77:COLOR 15,2:PRINT H0$(H0%):COLOR ,0:RETURN

330 GOSUB 4096
	LOCATE 21,48:COLOR 15,2:PRINT TAB(62)CHR$(219)TAB(79):COLOR 7,0
	LOCATE 10,1:COLOR L%(D,11),L%(D,12):PRINT P$(D,P1%(D));" ";B1$(P%(D,P1%(D),0)+2):COLOR 15,0:GOSUB 355
	COLOR 1:FOR X=5 TO 13:LOCATE X,21:PRINT CHR$(179);:NEXT
	LOCATE 11,1:COLOR 2,0:PRINT "W-L ERA ";:COLOR 15,0
	PRINT USING "##";P%(D,P1%(D),1);:PRINT "-";:PRINT USING "##";P%(D,P1%(D),2);
	PRINT " ";:PRINT USING "##.##";P%(D,P1%(D),10)/100
	COLOR 2,0
	PRINT "  IP  H  BB  K GAME":COLOR 15,0
	IF P8%(D,P1%(D),0)=0 THEN PRINT" 0.0";:GOTO 338
	PRINT USING"##";INT(P8%(D,P1%(D),0)/3);
	PRINT ".";:PRINT USING "#";P8%(D,P1%(D),0)-(INT(P8%(D,P1%(D),0)/3)*3);

338 PRINT USING " ##  ## ##";P8%(D,P1%(D),1),P8%(D,P1%(D),4),P8%(D,P1%(D),5)
	LOCATE 5,1
	J=B3%(P,B1(P)):COLOR L%(P,11),L%(P,12)
	IF B7%(P,B1(P))=1 THEN X$=P$(P,P1%(P)):PRINT X$;" ";B1$(P%(P,P1%(P),0)+2);:GOSUB 315:GOTO 335
	X$=B$(P,J):PRINT X$;" ";B1$(B%(P,J,0)+2);:GOSUB 320

335 LOCATE 6,1:COLOR 2,0:PRINT " AVG HR  BB   K":COLOR 15,0
	IF B7%(P,B1(P))=1 THEN 336
	IF B%(P,J,4)=0 THEN PRINT ".000  0   0   0";:GOTO 337
	PRINT USING ".### ## ### ###";B%(P,J,6)/B%(P,J,4),B%(P,J,9),B%(P,J,11),B%(P,J,12):GOTO 337

336 IF P%(P,P1%(P),11)<>999 THEN PRINT USING ".###";P%(P,P1%(P),11)/1000:GOTO 337
	IF P%(P,P1%(P),20)=0 THEN PRINT ".000 0";:GOTO 337
	PRINT USING ".### ## ### ###";P%(P,P1%(P),22)/P%(P,P1%(P),20),P%(P,P1%(P),25),P%(P,P1%(P),27),P%(P,P1%(P),28)

337 LOCATE 8,2:IF B7%(P,B1(P))=1 THEN 332
	PRINT B8%(P,B3%(P,B1(P)),1)"-"B8%(P,B3%(P,B1(P)),0);:GOTO 333

332 PRINT P8%(P,P1%(P),24)"-"P8%(P,P1%(P),23);

333 PRINT " TODAY"

342 COLOR 14,0:LOCATE 3,58:PRINT O%(0):LOCATE 2,58:PRINT U%:LOCATE 1,58:PRINT BC%

345 COLOR 15,2:LOCATE 22,46:PRINT "INFIELD:"
	LOCATE 23,46
	IF I5%=0 THEN PRINT "NORMAL    "
	IF I5%=1 THEN PRINT "CORNERS IN"
	IF I5%=2 THEN PRINT "INFIELD IN"

347 GOSUB 327
	BK$="        "
	LOCATE 13,58:COLOR 15,2:PRINT TAB(68):LOCATE 14,58:PRINT BK$:LOCATE 13,58
	I2=2:X=58:GOSUB 360
	IF O%(0)<>3 THEN MO=0
	FOR WK=1 TO 3
	IF B0%(WK,0)>0 THEN MO=MO+1
	NEXT

348 LOCATE 18,47:COLOR 15,2:PRINT TAB(57):LOCATE 19,47:PRINT BK$:LOCATE 18,47
	I2=3:X=47:GOSUB 360
	LOCATE 18,69:COLOR 15,2:PRINT TAB(79):LOCATE 19,70:PRINT BK$:LOCATE 18,69
	I2=1:X=70:GOSUB 360

354 COLOR 15,0:RETURN

355 IF I%>5 AND S8%(D,0)<P8%(D,P1%(D),4)+P8%(D,P1%(D),1) AND S8%(D,1)<P8%(D,P1%(D),2) THEN LOCATE 10,17:COLOR 30,0:PRINT "-T-":COLOR 15,0

357 LOCATE 5,1
	FOR X=1 TO 9:COLOR 3,0:LOCATE X+4,22
	GOSUB 380
	LOCATE,22:IF B7%(P,X)<>1 THEN 358
	PRINT B1$(P%(P,P1%(P),0)+2); TAB(25); LEFT$(P$(P,P1%(P)),10);" p" TAB(39);
	IF P%(P,P1%(P),11)<>999 THEN PRINT USING " .###";P%(P,P1%(P),11)/1000:GOTO 359
	IF P%(P,P1%(P),20)=0 THEN PRINT " .000":GOTO 359
	IF P%(P,P1%(P),20)=P%(P,P1%(P),22) THEN PRINT "1.000":GOTO 359
	PRINT USING " .###";P%(P,P1%(P),22)/P%(P,P1%(P),20)
	GOTO 359

358 PRINT B1$(B%(P,B3%(P,X),0)+2); TAB(25); LEFT$(B$(P,B3%(P,X)),10);" ";C$(B7%(P,X)); TAB(39);
	IF B%(P,B3%(P,X),4)=0 THEN PRINT " .000":GOTO 359
	IF B%(P,B3%(P,X),6)=B%(P,B3%(P,X),4) THEN PRINT "1.000":GOTO 359
	PRINT USING " .###";B%(P,B3%(P,X),6)/B%(P,B3%(P,X),4)

359 NEXT:COLOR 15,0
	RETURN

360 J=B0%(I2,0):IF J=0 THEN 363

361 COLOR L%(P,11),L%(P,12)
	IF B7%(P,J)=1 AND P%(P,P1%(P),11)<>999 THEN PRINT LEFT$(P$(P,P1%(P)),10):COLOR 15,2:LOCATE ,X:PRINT "1/  0":GOTO 363
	IF B7%(P,J)=1 THEN PRINT LEFT$(P$(P,P1%(P)),10):LOCATE ,X:COLOR 15,2:PRINT USING "#";P%(P,P1%(P),31);:PRINT "/";:PRINT USING "###";P%(P,P1%(P),29):GOTO 363

362 PRINT LEFT$(B$(P,B3%(P,J)),10):LOCATE ,X:COLOR 15,2:PRINT USING "#";B%(P,B3%(P,J),16);:PRINT "/";:PRINT USING "###";B%(P,B3%(P,J),13)

363 COLOR 15,0:RETURN

380 IF B1(P)<>X THEN 382

381 COLOR 11,0

382 RETURN

390 IF I2<4 THEN LOCATE,3*I2+7

391 IF I2>3 AND I2<7 THEN LOCATE,3*I2+8

392 IF I2>6 AND I2<10 THEN LOCATE,3*I2+9

393 IF I2>9 AND I2<13 THEN LOCATE,3*(I2-9)+7

394 IF I2>12 AND I2<16 THEN LOCATE,3*(I2-9)+8

395 IF I2>15 AND I2<19 THEN LOCATE,3*(I2-9)+9

396 IF I2>18 AND I2<22 THEN LOCATE,3*(I2-18)+7

397 IF I2>21 AND I2<25 THEN LOCATE,3*(I2-18)+8

398 IF I2>24 AND I2<28 THEN LOCATE,3*(I2-18)+9

399 PRINT USING "##";S%(I,I2);:RETURN

400 '

500 J=B3%(P,B1(P))
	LOCATE 3,3:COLOR L%(P,11),L%(P,12):PRINT " ";CHR$(201);STRING$(71,205);CHR$(187);" "
	FOR X=1 TO 7:LOCATE ,3:PRINT " ";CHR$(186);STRING$(71,32);CHR$(186);" ":NEXT
	LOCATE ,3:PRINT " ";CHR$(200);STRING$(71,205);CHR$(188);" "
	LOCATE 4,6:PRINT "Stats for ";
	IF B7%(P,B1(P))=1 AND P%(P,P1%(P),11)<>999 THEN PRINT P$(P,P1%(P)):GOTO 510
	IF B7%(P,B1(P))=1 THEN PRINT P$(P,P1%(P)):LOCATE ,6:PRINT "BATS: "B1$(P%(P,P1%(P),0)+2)"    ";:PRINT USING "G/A%: ##       BUNT: #       RUN: #";52,P%(P,P1%(P),32),P%(P,P1%(P),31):GOTO 510
	PRINT B$(P,J):LOCATE ,6:PRINT "BATS: "B1$(B%(P,J,0)+2)"    ";:PRINT USING "G/A%: ##       BUNT: #       RUN: #";B%(P,J,18),B%(P,J,30),B%(P,J,16)

510 LOCATE ,3:PRINT " ";CHR$(204);STRING$(71,205);CHR$(185);" "
	LOCATE ,6:PRINT "          AVG   OBP   G  AB   R   H 2B 3B HR RBI  BB  SO  SB CS HS LS"
	IF B7%(P,B1(P))=1 AND P%(P,P1%(P),11)<>999 THEN LOCATE,6:PRINT "SEASON";:LOCATE ,15:PRINT USING ".###";P%(P,P1%(P),11)/1000:GOTO 520
	IF B7%(P,B1(P))=1 THEN 517
	QS%=1:WK=P:M=J:GOSUB 739:QS%=0:GOTO 621

517 LOCATE ,6:PRINT "SEASON";:LOCATE ,15
	PRINT USING ".###  .### ### ### ### ### ## ";P%(P,P1%(P),22)/P%(P,P1%(P),20),(P%(P,P1%(P),22)+P%(P,P1%(P),27))/(P%(P,P1%(P),27)+P%(P,P1%(P),20)),P%(P,P1%(P),19),P%(P,P1%(P),20),P%(P,P1%(P),21),P%(P,P1%(P),22),P%(P,P1%(P),23);
	PRINT USING "## ## ### ### ### ### ##";P%(P,P1%(P),24),P%(P,P1%(P),25),P%(P,P1%(P),26),P%(P,P1%(P),27),P%(P,P1%(P),28),P%(P,P1%(P),29),P%(P,P1%(P),30)

520 LOCATE ,6:PRINT "LEAGUE";:J=P1%(P)
	IF P5%(P,J,23)=0 OR P5%(P,J,24)=0 THEN LOCATE ,15:PRINT ".000";:GOTO 521
	IF P5%(P,J,23)=P5%(P,J,24) THEN LOCATE ,14:PRINT "1.000";:GOTO 521
	LOCATE ,15:PRINT USING ".###";P5%(P,J,24)/P5%(P,J,23);

521 IF P5%(P,J,23)+P5%(P,J,30)=0 THEN LOCATE ,21:PRINT ".000";:GOTO 522
	IF P5%(P,J,24)+P5%(P,J,30)=P5%(P,J,23)+P5%(P,J,30) THEN LOCATE ,20:PRINT "1.000";:GOTO 522
	LOCATE ,21:PRINT USING ".###";(P5%(P,J,24)+P5%(P,J,30))/(P5%(P,J,23)+P5%(P,J,30));

522 LOCATE ,26:PRINT USING "### ### ### ### ## ## ## ### ### ### ### ## ## ##";P5%(P,J,8),P5%(P,J,23),P5%(P,J,25),P5%(P,J,24),P5%(P,J,27),P5%(P,J,28),P5%(P,J,29),P5%(P,J,26),P5%(P,J,30),P5%(P,J,31),P5%(P,J,32),P5%(P,J,33),P5%(P,J,35),P5%(P,J,36)
	LOCATE ,6:PRINT "GAME";
	IF P8%(P,J,23)=0 OR P8%(P,J,24)=0 THEN LOCATE ,15:PRINT ".000";:GOTO 523
	IF P8%(P,J,23)=P8%(P,J,24) THEN LOCATE ,14:PRINT "1.000";:GOTO 523
	LOCATE ,15:PRINT USING ".###";P8%(P,J,24)/P8%(P,J,23);

523 IF P8%(P,J,23)+P8%(P,J,30)=0 THEN LOCATE ,21:PRINT ".000";:GOTO 524
	IF P8%(P,J,24)+P8%(P,J,30)=P8%(P,J,23)+P8%(P,J,30) THEN LOCATE ,20:PRINT "1.000";:GOTO 524
	LOCATE ,21:PRINT USING ".###";(P8%(P,J,24)+P8%(P,J,30))/(P8%(P,J,23)+P8%(P,J,30));

524 LOCATE ,26:PRINT USING "### ### ### ### ## ## ## ### ### ### ### ## ## ##";P8%(P,J,8),P8%(P,J,23),P8%(P,J,25),P8%(P,J,24),P8%(P,J,27),P8%(P,J,28),P8%(P,J,29),P8%(P,J,26),P8%(P,J,30),P8%(P,J,31),P8%(P,J,32),P8%(P,J,33),P8%(P,J,35),P8%(P,J,36)

621 QS%=1:WK=D:M=P1%(D):GOSUB 809:QS%=0

650 I$=GetKeyPress$
655 COLOR 7,0:RETURN

700 CLS
	COLOR 12:PRINT TAB(20); "SELECT TEAM TO VIEW DETAILED STATS FOR:":PRINT:PRINT
	COLOR 14,0:PRINT "(0) ";:COLOR L%(0,11),L%(0,12):PRINT A$(0):PRINT
	COLOR 14,0:PRINT "(1) ";:COLOR L%(1,11),L%(1,12):PRINT A$(1):PRINT
	COLOR 14,0:PRINT "(2) ";:COLOR 15:PRINT "RETURN TO PLAY"

701 I$=GetKeyPress$
702 WK=VAL(I$):IF WK<0 OR WK>2 THEN 701

703 IF WK=2 THEN RETURN

705 CLS
	COLOR 14:PRINT "(1) ";:COLOR 15:PRINT "VIEW PITCHERS":PRINT
	COLOR 14:PRINT "(2) ";:COLOR 15:PRINT "VIEW HITTERS":PRINT
	COLOR 14:PRINT "(3) ";:COLOR 15:PRINT "RETURN TO PLAY"

706 I$=GetKeyPress$
707 Q=VAL(I$):IF Q<1 OR Q>3 THEN 706

708 IF Q=3 THEN RETURN

710 IF Q=1 THEN 800

725 CLS
	COLOR 12:PRINT "VIEW BATTERS (AVAILABLE PLAYERS HIGHLIGHTED)"
	FOR I=0 TO 10:COLOR 3,0
	IF B%(WK,I,21)=0 THEN COLOR 11
	LOCATE I+5,5:PRINT USING "##";I;:PRINT " "B$(WK,I); TAB(40);
	COLOR 3
	IF B%(WK,I+12,21)=0 THEN COLOR 11
	PRINT USING "##";I+12;:PRINT " "B$(WK,I+12):COLOR 3:NEXT
	IF B%(WK,11,21)=0 THEN COLOR 11
	LOCATE ,5:PRINT USING "##";11;:PRINT " "B$(WK,11)
	COLOR 3

730 COLOR 15:LOCATE 20,1:PRINT "# TO VIEW (0-22) (23 TO CONTINUE) ";:INPUT M

731 IF M<0 OR M>23 THEN 730

732 IF M=23 THEN 700

734 LOCATE 3,3:COLOR L%(WK,11),L%(WK,12):PRINT " ";CHR$(201);STRING$(71,205);CHR$(187);" "
	FOR X=1 TO 7:LOCATE ,3:PRINT " ";CHR$(186);STRING$(71,32);CHR$(186);" ":NEXT
	LOCATE ,3:PRINT " ";CHR$(200);STRING$(71,205);CHR$(188);" "
	LOCATE 4,6:PRINT "Stats for ";
	PRINT B$(WK,M):LOCATE ,6:PRINT "BATS: "B1$(B%(WK,M,0)+2)"    ";:PRINT USING "G/A%: ##       BUNT: #       RUN: #";B%(WK,M,18),B%(WK,M,30),B%(WK,M,16)
	LOCATE ,3:PRINT " ";CHR$(204);STRING$(71,205);CHR$(185);" "
	LOCATE ,6:PRINT "          AVG   OBP   G  AB   R   H 2B 3B HR RBI  BB  SO  SB CS HS LS"

739 LOCATE ,6:PRINT "SEASON";:LOCATE ,15
	PRINT USING ".###  .### ### ### ### ### ## ## ## ### ### ### ";B%(WK,M,6)/B%(WK,M,4),(B%(WK,M,6)+B%(WK,M,11))/(B%(WK,M,4)+B%(WK,M,11)),B%(WK,M,3),B%(WK,M,4),B%(WK,M,5),B%(WK,M,6),B%(WK,M,7),B%(WK,M,8),B%(WK,M,9),B%(WK,J,10);
	PRINT USING "### ### ### ##";B%(WK,M,11),B%(WK,M,12),B%(WK,M,13),B%(WK,M,14)
	LOCATE ,6:PRINT "LEAGUE";
	IF B5%(WK,M,0)=0 OR B5%(WK,M,1)=0 THEN LOCATE ,15:PRINT ".000";:GOTO 735
	IF B5%(WK,M,0)=B5%(WK,M,1) THEN LOCATE ,14:PRINT "1.000";:GOTO 735
	LOCATE ,15:PRINT USING ".###";B5%(WK,M,1)/B5%(WK,M,0);

735 IF B5%(WK,M,0)+B5%(WK,M,7)=0 THEN LOCATE ,21:PRINT ".000";:GOTO 736
	IF B5%(WK,M,1)+B5%(WK,M,7)=B5%(WK,M,0)+B5%(WK,M,7) THEN LOCATE ,20:PRINT "1.000";:GOTO 736
	LOCATE ,21:PRINT USING ".###";(B5%(WK,M,1)+B5%(WK,M,7))/(B5%(WK,M,0)+B5%(WK,M,7));

736 LOCATE ,26:PRINT USING "### ### ### ### ## ## ## ### ### ### ### ## ## ##";B5%(WK,M,14),B5%(WK,M,0),B5%(WK,M,2),B5%(WK,M,1),B5%(WK,M,4),B5%(WK,M,5),B5%(WK,M,6),B5%(WK,M,3),B5%(WK,M,7),B5%(WK,J,8),B5%(WK,M,9),B5%(WK,M,10),B5%(WK,M,12),B5%(WK,M,_

13)
	LOCATE ,6:PRINT "GAME";
	IF B8%(WK,M,0)=0 OR B8%(WK,M,1)=0 THEN LOCATE ,15:PRINT ".000";:GOTO 737
	IF B8%(WK,M,0)=B8%(WK,M,1) THEN LOCATE ,14:PRINT "1.000";:GOTO 737
	LOCATE ,15:PRINT USING ".###";B8%(WK,M,1)/B8%(WK,M,0);

737 IF B8%(WK,M,0)+B8%(WK,M,7)=0 THEN LOCATE ,21:PRINT ".000";:GOTO 738
	IF B8%(WK,M,1)+B8%(WK,M,7)=B8%(WK,M,0)+B8%(WK,M,7) THEN LOCATE ,20:PRINT "1.000";:GOTO 738
	LOCATE ,21:PRINT USING ".###";(B8%(WK,M,1)+B8%(WK,M,7))/(B8%(WK,M,0)+B8%(WK,M,7));

738 LOCATE ,26:PRINT USING "### ### ### ### ## ## ## ### ### ### ### ## ## ##";B8%(WK,M,14),B8%(WK,M,0),B8%(WK,M,2),B8%(WK,M,1),B8%(WK,M,4),B8%(WK,M,5),B8%(WK,M,6),B8%(WK,M,3),B8%(WK,M,7),B8%(WK,J,8),B8%(WK,M,9),B8%(WK,M,10),B8%(WK,M,12),B8%(WK,M,_

13)
	IF QS%=1 THEN RETURN

790 I$=GetKeyPress$
791 COLOR 7,0:GOTO 725

800 CLS
	COLOR 12:PRINT "VIEW PITCHERS (AVAILABLE PLAYERS HIGHLIGHTED)"
	FOR I=0 TO 10:COLOR 3,0
	IF P%(WK,I,15)=0 THEN COLOR 11
	LOCATE I+5,5:PRINT USING "##";I;:PRINT " "P$(WK,I); TAB(40);
	COLOR 3
	IF P%(WK,I+11,15)=0 THEN COLOR 11
	PRINT USING "##";I+11;:PRINT " "P$(WK,I+11):COLOR 3:NEXT

801 COLOR 15:LOCATE 20,1:PRINT "# TO VIEW (0-21) (22 TO CONTINUE) ";:INPUT M

802 IF M<0 OR M>22 THEN 801

803 IF M=22 THEN 700

809 LOCATE 14,3::COLOR L%(WK,11),L%(WK,12):PRINT " ";CHR$(201);STRING$(71,205);CHR$(187);" "
	FOR X=1 TO 7:LOCATE ,3:PRINT " ";CHR$(186);STRING$(71,32);CHR$(186);" ":NEXT
	LOCATE ,3:PRINT " ";CHR$(200);STRING$(71,205);CHR$(188);" "
	LOCATE 15,6:PRINT "Stats for ";P$(WK,M)
	LOCATE ,6:PRINT "THROWS: ";B1$(P%(WK,M,0)+2)"    ";:PRINT USING "G/A%: ##     HOLD: #";P%(WK,M,12),P%(WK,M,18)
	LOCATE ,3:PRINT " ";CHR$(204);STRING$(71,205);CHR$(185);" "
	LOCATE ,6:PRINT "          W  L SV   ERA  G GS CG   IP    H  BB  SO   R  ER HR"
	LOCATE ,6:PRINT "SEASON   ";:PRINT USING"## ## ## ##.## ## ## ## ###   ### ";P%(WK,M,1),P%(WK,M,2),P%(WK,M,3),P%(WK,M,10)/100,P%(WK,M,4),P%(WK,M,5),P%(WK,M,16),P%(WK,M,6),P%(WK,M,7);
	PRINT USING "### ###         ##";P%(WK,M,8),P%(WK,M,9),P%(WK,M,13)
	LOCATE ,6:PRINT "LEAGUE   ";:PRINT USING"## ## ## ";P5%(WK,M,6),P5%(WK,M,7),P5%(WK,M,12);
	IF P5%(WK,M,3)=0 OR P5%(WK,M,0)=0 THEN PRINT " 0.00";:GOTO 805
	PRINT USING "##.##";P5%(WK,M,3)/(P5%(WK,M,0)/3)*9;

805 PRINT USING " ## ## ## ";P5%(WK,M,8),P5%(WK,M,9),P5%(WK,M,10);
	IF P5%(WK,M,0)=0 THEN PRINT"  0.0";:GOTO 806
	PRINT USING"###";INT(P5%(WK,M,0)/3);
	PRINT ".";:PRINT USING "#";P5%(WK,M,0)-(INT(P5%(WK,M,0)/3)*3);

806 PRINT USING " ### ### ### ### ### ##";P5%(WK,M,1),P5%(WK,M,4),P5%(WK,M,5),P5%(WK,M,2),P5%(WK,M,3),P5%(WK,M,14)
	LOCATE ,6:PRINT "GAME     ";:PRINT USING"## ## ## ";P8%(WK,M,6),P8%(WK,M,7),P8%(WK,M,12);
	IF P8%(WK,M,3)=0 OR P8%(WK,M,0)=0 THEN PRINT " 0.00";:GOTO 807
	PRINT USING "##.##";P8%(WK,M,3)/(P8%(WK,M,0)/3)*9;

807 PRINT USING " ## ## ## ";P8%(WK,M,8),P8%(WK,M,9),P8%(WK,M,10);
	IF P8%(WK,M,0)=0 THEN PRINT"  0.0";:GOTO 808
	PRINT USING"###";INT(P8%(WK,M,0)/3);
	PRINT ".";:PRINT USING "#";P8%(WK,M,0)-(INT(P8%(WK,M,0)/3)*3);

808 PRINT USING " ### ### ### ### ### ##";P8%(WK,M,1),P8%(WK,M,4),P8%(WK,M,5),P8%(WK,M,2),P8%(WK,M,3),P8%(WK,M,14)

810 IF QS%=1 THEN RETURN

830 I$=GetKeyPress$
831 COLOR 7,0:GOTO 800

840 ' *** POPUP LINEUPS FOR TEAMS ***
	J=11
	LOCATE 8,7:COLOR L%(1,11),L%(1,12):PRINT " Ö"; STRING$(62,196);"· ";
	FOR II=1 TO J:LOCATE 8+II,7:PRINT " º";STRING$(62,32);"º ";:NEXT
	LOCATE 9+J,7:PRINT " Ó";STRING$(62,196); "½ ";
	LOCATE 9,14:PRINT A$(0); TAB(40); A$(1)
	FOR J=0 TO 1
	FOR X=1 TO 9:LOCATE X+9,9+J*26
	GOSUB 845
	LOCATE,11+J*26
	IF B7%(J,X)<>1 THEN 841
	PRINT B1$(P%(J,P1%(J),0)+2); TAB(14+J*26) LEFT$(P$(J,P1%(J)),10);" p" TAB(28+J*26);
	IF P%(J,P1%(J),11)<>999 THEN PRINT USING " .###";P%(J,P1%(J),11)/1000:GOTO 842
	IF P%(J,P1%(J),20)=0 THEN PRINT " .000":GOTO 842
	IF P%(J,P1%(J),20)=P%(J,P1%(J),22) THEN PRINT "1.000":GOTO 842
	PRINT USING " .###";P%(J,P1%(J),22)/P%(J,P1%(J),20)
	GOTO 842

841 PRINT B1$(B%(J,B3%(J,X),0)+2); TAB(14+J*26) LEFT$(B$(J,B3%(J,X)),10);" ";C$(B7%(J,X)); TAB(28+J*26);
	IF B%(J,B3%(J,X),4)=0 THEN PRINT " .000":GOTO 842
	IF B%(J,B3%(J,X),6)=B%(J,B3%(J,X),4) THEN PRINT "1.000":GOTO 842
	PRINT USING " .###";B%(J,B3%(J,X),6)/B%(J,B3%(J,X),4)

842 NEXT:NEXT:COLOR 15,0

843 I$=GetKeyPress$
	RETURN

845 IF B1(J)<>X THEN 846
	PRINT "->";

846 RETURN

850 '
	LOCATE 8,8:COLOR L%(1,11),L%(1,12):PRINT " Ö"; STRING$(61,196);"· ";:FOR II=1 TO J:LOCATE 8+II,8:PRINT " º";STRING$(61,32);"º ";:NEXT
	LOCATE 9+J,8:PRINT " Ó";STRING$(61,196); "½ ";:RETURN

1000

1150 IF S6%(P,0)-S6%(D,0)=1 THEN P2%(D)=B0%(I,1):P2%(P)=P1%(P):S1%(1)=-1:S1%(0)=-1:GOTO 1155

1152 IF S6%(P,0)-S6%(D,0)=0 AND S1%(D)<>-1 THEN I5=P1%(D):I6=19:GOSUB 8020:S1%(D)=-1

1155 RETURN

1200 RI=0:IF S4%=0 THEN 1220

1201 IF S4%=1 OR S4%=2 OR S4%=3 THEN RI=1

1202 IF S4%=4 OR S4%=5 OR S4%=6 THEN RI=2

1203 IF S4%=7 THEN RI=3

1205 P5%(D,P1%(D),17)=P5%(D,P1%(D),17)+RI:P8%(D,P1%(D),17)=P8%(D,P1%(D),17)+RI

1220 RETURN

1800 I3=S6%(D,0)-S6%(P,0):S1%(D)=-1:IF I3<1 THEN 1807

1801 IF S4%=0 AND I3<=2 THEN S1%(D)=P1%(D)

1802 IF S4%>0 AND S4%<4 AND I3<=3 THEN S1%(D)=P1%(D)

1803 IF S4%>3 AND S4%<7 AND I3<=4 THEN S1%(D)=P1%(D)

1804 IF S4%=7 AND I3<=5 THEN S1%(D)=P1%(D)

1805 IF S4%=0 AND O%(0)=0 AND I3<=3 THEN S1%(D)=P1%(D)

1806 IF I%>5 AND I3>0 AND I%<8 THEN S1%(D)=P1%(D)

1807 GOTO 1200

1810 I3=0:IF S6%(1,0)-S6%(0,0)>0 THEN I3=1

1811 IF P2%(I3)<> P1%(I3) AND S1%(I3)=-1 AND P8%(I3,P1%(I3),0)>=9 AND P8%(I3,P1%(I3),0)<=12 AND P6%(I3)>0 AND P8%(I3,P1%(I3),3)<3 THEN S1%(I3)=P1%(I3)

1812 RETURN

1815 IF B7%(P,B0%(I,0))=1 THEN I5=P1%(P):I6=25:GOSUB 8009

1816 IF B7%(P,B0%(I,0))<>1 THEN I5=B3%(P,B0%(I,0)):I6=2:GOSUB 8010

1817 RETURN

1820 GOSUB 4220

1821 IF PQ=8 THEN 1825

1822 IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=26:GOSUB 8009

1823 IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=3:GOSUB 8010

1824 IF F%=1 THEN PQ=8

1825 RETURN

1840 I4=0:IF P2=2 THEN I4=-5

1841 IF I5%=2 THEN SN=70:I4=I4+15

1842 IF U%=2 THEN I4=I4-15

1843 IF I5%=1 THEN SN=20

1844 IF I%>5 AND S8%(D,0)<=P8%(D,P1%(D),4)+P8%(D,P1%(D),1) AND S8%(D,1)<=P8%(D,P1%(D),2) THEN I4=I4+40

1845 IF P=0 THEN I4=I4-2

1846 IF P=1 THEN I4=I4+2

1847 RETURN

1999 '

2000 GOSUB 4095:H6%=INT(RND(1)*100)+1
	IF H6%=1 THEN PRINT B$(D,B9%(D,2))" is arguing ball and strike calls":GOSUB 6095
	IF H6%=1 THEN PRINT U$(1)" has heard enough":GOSUB 6095
	IF H6%=1 THEN PRINT B$(D,B9%(D,2))" has been ejected!!":X$=B$(D,B9%(D,2)):GOSUB 2200:GOSUB 6095:po%=2:GOSUB 2700:GOTO 5052
	IF H6%<=3 THEN PRINT P$(D,P1%(D)) " steps off the mound"
	IF H6%<=3 THEN PRINT "after that last call":GOSUB 6095:PRINT "He is shouting at "U$(1):GOSUB 6095
	IF H6%<=3 THEN PRINT U$(1) " does not appreciate what he hears...":GOSUB 6095:PRINT P$(D,P1%(D)) " has been ejected!!"
	if h6%<=3 then X$=P$(D,P1%(D)):GOSUB 2200:GOSUB 6095:GOSUB 2750:GOTO 5052
	IF H6%=4 AND B7%(P,B1(P))<>1 THEN PRINT Q$(0) " is unhappy with that last call":gosub 6095:print "He is venting his opinion to "U$(1):gosub 6095:print "Thats it!! "u$(1)" has ejected "q$(0)"!!":X$=Q$(0):GOSUB 2200:GOSUB 6095:gosub 2800
	if h6%=4 and b7%(p,b1(p))<>1 then 5052
	IF H6%=4 THEN PRINT Q$(0) " did not like that":PRINT "last call at all!!":gosub 6095:goto 5052
	IF H6%>5 THEN 2005
	PRINT Q$(0) " hits a slow roller to first":gosub 6095:print b$(d,b9%(d,3))" fields it cleanly":gosub 6095:print "He flips it to "p$(d,p1%(d)) " covering just in time!":gosub 6095:print "Wait... "u$(2)" rules safe!!":gosub 6095
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010:I6=1:GOSUB 8010
	I5=P1%(D):I6=1:GOSUB 8020
	IF B7%(P,B1(P))=1 THEN I5=p1%(p):I6=23:GOSUB 8009:I6=24:GOSUB 8009
	FOR I=0 TO 3:A5%(I)=1:NEXT I:GOSUB 3800:GOSUB 324
	GOSUB 4095:print m$(d)" charges from the dugout":gosub 6095
	print M$(D)", "p$(d,p1%(d))","
	PRINT b$(d,b9%(d,2))", and "b$(d,b9%(d,3))
	PRINT "are going at it with "u$(2)
	gosub 6095:gosub 6095:gosub 6095
	print u$(3) " and "u$(1)" come in to help out":gosub 6095
	print "This argument is out of control now!!":gosub 6095
	print"What is this!!":gosub 6095
	print "All of them have been ejected!!!"
	X$=P$(D,P1%(D)):GOSUB 2200
	X$=B$(D,B9%(D,2)):GOSUB 2200
	X$=B$(D,B9%(D,3)):GOSUB 2200
	X$=M$(D):GOSUB 2200
	gosub 6095:po%=2:gosub 2700:po%=3:gosub 2700:gosub 2750:goto 3900

2005 if h6%>7 then 2007
	print q$(0)" sends a long one"
	print "down the left field line":gosub 6095
	print "It may go...":gosub 6095:print "HOME RUN!!!":GOSUB 6095
	PRINT "Wait!!! ";:gosub 6095:print u$(4) " is calling it foul!!":gosub 6095:gosub 4095
	print q$(0)" is arguing vehemently"
	print "with "u$(1)" and "u$(4):gosub 6095
	print m$(p)" charges from the dugout":gosub 6095
	print "He and "q$(0)" are really going"
	print "at it with the umpires!!":gosub 6095:gosub 6095
	print q$(0)" has just been tossed!!":X$=Q$(0):GOSUB 2200:gosub 6095
	gosub 2800:goto 5052

2007 IF H6%=8 THEN PRINT U$(1)" has just ejected "p$(d,p1%(d))
	IF H6%=8 THEN PRINT "for wetting the ball!!":GOSUB 2200:EJ$(EJ%)=P$(D,P1%(D)):gosub 6095:gosub 6095:gosub 2750:goto 5052
	IF H6%=9 then print "Here's the 3-2 pitch...":gosub 6095:print "Time called...":print u$(1)" is calling it ball 4":gosub 6095:print"He says "p$(d,p1%(d))" went to his mouth":gosub 6095:goto 3240
	if h6%>15 then 2006
	PRINT Q$(0)" hits the deck on a high"
	print "and tight pitch from "p$(d,p1%(d)):gosub 6095:gosub 6095
	print q$(0)" charges the mound!":gosub 6095
	PRINT "Both dugouts have emptied...":GOSUB 6095
	PRINT "It's a bench-clearing brawl!":gosub 6095
	gosub 4095:print "It will take a few minutes to clean this up":gosub 6095:i=int(rnd(1)*3)+1
	if i=1 then print p$(d,p1%(d))" has been ejected":X$=P$(D,P1%(D)):GOSUB 2200:gosub 6095:gosub 2750:goto 5052
	IF I=2 THEN  PRINT Q$(0)" has been ejected":X$=Q$(0):GOSUB 2200:gosub 6095:gosub 2800:goto 5052
	PRINT Q$(0)" and "P$(D,P1%(D))" have been ejected":X$=Q$(0):GOSUB 2200:X$=P$(D,P1%(D)):GOSUB 2200:gosub 6095:gosub 2750:gosub 2800:goto 5052

2006 IF H6%>21 THEN 2010

2008 GOSUB 4095:IF RD=0 AND (PC%=0 OR PC%=4) THEN PRINT Q$(0)" fouls the pitch out of play":gosub 6095:goto 5052
	tm=int(rnd(1)*59)+2:color 0,7:PRINT "The game has been halted by rain":for i=1 to 3:gosub 6095:next
	print "Rain delay lasted"tm" minutes":gosub 6095:gosub 6095
	RD%=1
	if tm>28 then print "Pitchers have lost effectiveness during delay":gosub 6095:print "Their arms have stiffened up":gosub 6095:gosub 6095:for i=0 to 1:s8%(i,0)=s8%(i,0)-2:next
	goto 5052

2010 IF H6%>24 THEN 2015
	IF S4%=0 and L%(1,13)=14 THEN PRINT Q$(0)" steps out of the box as a jet passes over":gosub 6095:goto 5052
	IF S4%=0 THEN PRINT U$(1)" calls time to get some new baseballs":gosub 6095:goto 5052
	PRINT B$(D,B9%(D,2))" rifles the ball to ";
	IF S4%=1 THEN PRINT "1st...":gosub 6095:i=2:goto 2012
	if s4%=2 or s4%=4 then print "2nd base...":gosub 6095:i=3:goto 2012
	print "3rd base...":gosub 6095:i=4

2012 PRINT Q$(I-1);" is picked off base!":A5%(I-1)=10:O%(0)=O%(0)+1:O%(1)=O%(1)+1:W%=2:GOSUB 3400:GOSUB 3800:GOSUB 3410:I5=P1%(D):I6=0:GOSUB 8020:IF O%(0)>=3 THEN B1(P)=B1(P)-1:GOTO 3900
	GOTO 5051

2015 IF H6%>29 then 2020
	print q$(0)" pops it up near the 3rd base stands":gosub 6095
	print "A fan interferes...":GOSUB 6095
	PRINT b$(d,b9%(d,6))" makes a great catch!!":W%=6:GOSUB 3420
	O%(0)=O%(0)+1:O%(1)=O%(1)+1:GOSUB 6095
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010
	I5=P1%(D):I6=0:GOSUB 8020
	IF B7%(P,B1(P))=1 THEN I5=p1%(p):I6=23:GOSUB 8009
	GOSUB 6095:GOTO 3900

2020 IF H6%<=31 THEN print q$(0)" pops it up foul near the stands":gosub 6095
	if H6%<=30 then print b$(d,b9%(d,2))" runs into the stands!!":po%=2:goto 2099
	IF H6%<=31 THEN print b$(d,b9%(d,3))" gives chase and runs into the stands!!":po%=3:goto 2099
	if h6%=32 then print q$(0)" pops it up foul down the rf line":gosub 6095:print b$(d,b9%(d,4))" crashes into the wall going for it":po%=4:goto 2099
	if h6%=33 then print q$(0)" pops it up foul by"
	if h6%=33 then print "the 3rd base dugout":gosub 6095
	if h6%=33 then print b$(d,b9%(d,5))" falls in trying to make the catch!!":gosub 6095:po%=5:goto 2099
	if h6%=34 then print q$(0)" pops it up foul"
	if h6%=34 then print "down the lf line":gosub 6095
	if h6%=34 then print b$(d,b9%(d,6))" tumbles over the railing"
	if h6%=34 then print "trying to make the catch!":gosub 6095:gosub 6095:po%=6:goto 2099
	if h6%>37 then 2023
	PRINT Q$(0)" lines one to deep left field":gosub 6095:print b$(d,b9%(d,7))" races back...":gosub 6095:print "He slams into the wall and is down!!":gosub 6095
	op%=7:print b$(d,b9%(d,8))" comes over and gets the ball in":GOSUB 6095

2021 i5=p1%(d):i6=1:gosub 8020
	if b7%(p,b1(p))<>1 then i5=b3%(p,b1(p)):i6=0:gosub 8010:i6=1:gosub 8010:i6=4:gosub 8010
	if b7%(p,b1(p))=1 then i5=p1%(p):i6=23:gosub 8009:i6=24:gosub 8009:i6=27:gosub 8009
	for i=0 to 3:a5%(i)=2:next i:a5%(3)=1:a5%(1)=3:gosub 3800:gosub 324:gosub 4095:po%=op%:goto 2098

2023 if h6%>40 then 2025
	PRINT Q$(0)" drives a sinking liner"
	print "into deep center":gosub 6095
	print b$(d,b9%(d,8))" gets a good jump on it":gosub 6095
	PRINT "He crashes into the wall and"
	print "the ball bounces away!!":GOSUB 6095
	print "The runners are circling the bases":gosub 6095
	print b$(d,b9%(d,9))" retrieves the ball":gosub 6095
	i5=p1%(d):i6=1:gosub 8020:I6=14:GOSUB 8020:W%=8
	if b7%(p,b1(p))<>1 then i5=b3%(p,b1(p)):i6=0:gosub 8010:i6=1:gosub 8010:i6=6:gosub 8010
	if b7%(p,b1(p))=1 then i5=p1%(p):i6=23:gosub 8009:i6=24:gosub 8009:i6=29:gosub 8009
	a5%(0)=4:a5%(1)=3:a5%(2)=2:a5%(3)=1:gosub 3800:print "It's an inside the park HOME RUN!!!":gosub 6095:gosub 6095:gosub 324:gosub 4095:po%=8:goto 2098

2025 if h6%>43 then 2027
	PRINT Q$(0)" lifts a deep drive into right":gosub 6095:print b$(d,b9%(d,9))" races back...":gosub 6095:print "He slams into the wall and is down!!":gosub 6095
	i5=p1%(d):i6=1:gosub 8020:W%=9:print b$(d,b9%(d,8))" comes over to get the ball back in":gosub 6095
	if b7%(p,b1(p))<>1 then i5=b3%(p,b1(p)):i6=0:gosub 8010:i6=1:gosub 8010:i6=5:gosub 8010
	if b7%(p,b1(p))=1 then i5=p1%(p):i6=23:gosub 8009:i6=24:gosub 8009:i6=28:gosub 8009
	a5%(0)=3:a5%(1)=3:a5%(2)=2:a5%(3)=1:gosub 3800:print "Triple!!":gosub 6095:gosub 6095:gosub 324:gosub 4095:po%=9:goto 2098

2027 if h6%>44 then 2030
	i5=p1%(d):i6=1:gosub 8020:w%=int(rnd(1)*3)+7:print q$(0)" hits a sinking liner into "c$(w%):gosub 6095
	print b$(d,b9%(d,w%))" dives for the catch...":gosub 6095
	print "the ball rolls free!":gosub 6095
	print b$(d,b9%(d,w%-1))" gets the loose ball in":gosub 6095
	op%=w%:goto 2021

2030 if h6%=45 then print q$(0)" is hit by a pitch and injured":gosub 6095:gosub 2800:goto 3201
	if h6%<=49 then print b$(d,b9%(d,2))" is injured by a foul tip!":gosub 6095:po%=2:goto 2099
	if h6%>58 then 2037
	i=int(rnd(1)*3)+7
	if i=9 then hi1=3:hi2=9:hi3=8
	if i=8 then hi1=4:hi2=8:hi3=7
	if i=7 then hi1=6:hi2=7:hi3=8
	print q$(0)" hits a weak fly into short "c$(i):gosub 6095
	print b$(d,b9%(d,hi1))" goes out":gosub 6095
	print b$(d,b9%(d,hi2))" is charging in":gosub 6095
	print "They collide and the ball rolls free!!":gosub 6095
	print b$(d,b9%(d,hi3))" gets it back in"
	GOSUB 6095
	i5=p1%(d):i6=1:gosub 8020
	if b7%(p,b1(p))<>1 then i5=b3%(p,b1(p)):i6=0:gosub 8010:i6=1:gosub 8010:i6=4:gosub 8010
	if b7%(p,b1(p))=1 then i5=p1%(p):i6=23:gosub 8009:i6=24:gosub 8009:i6=27:gosub 8009
	for i=0 to 3:a5%(i)=2:next i:a5%(3)=1:a5%(1)=3:gosub 3800:gosub 324
	gosub 4095:po%=hi1:gosub 2097:po%=hi2:gosub 2097:goto 3900

2037 if h6%<=61 then print q$(0)" fouls the pitch off his foot":gosub 6095:print "He is injured":gosub 6095:gosub 2800:goto 5052
	if h6%>64 then 2040
	PRINT Q$(0)" pops it up in foul along the 3b line":gosub 6095:print "It drifts into foul territory":gosub 6095
	print b$(d,b9%(d,5))" and "p$(d,p1%(d))" chase after it and collide!!":gosub 6095
	print b$(d,b9%(d,5))" is down and injured":GOSUB 6095:po%=5:goto 2099

2040 if h6%>70 then 2045
	PRINT Q$(0);:i5=p1%(d):i6=1:gosub 8020:W%=8:GOSUB 8030
	PRINT B$(D,B9%(D,7))" collides with "b$(d,b9%(d,8))"!!":GOSUB 6095
	print "both fielders are down... ":GOSUB 6095
	PRINT b$(d,b9%(d,6))" gets the ball back in":GOSUB 6095
	if b7%(p,b1(p))<>1 then i5=b3%(p,b1(p)):i6=0:gosub 8010:i6=1:gosub 8010:i6=4:gosub 8010
	if b7%(p,b1(p))=1 then i5=p1%(p):i6=23:gosub 8009:i6=24:gosub 8009:i6=27:gosub 8009
	a5%(0)=2:a5%(1)=3:a5%(2)=2:a5%(3)=1:gosub 3800:print "Double!!":gosub 324:gosub 4095:po%=7:gosub 2097:po%=8:gosub 2097:goto 3900

2045 if h6%>85 then 2055
	print q$(0)" lines it off ";
	i=int(rnd(1)*5)+1:on i goto 2046,2047,2047,2047,2047

2046 print "the leg of "p$(d,p1%(d)):gosub 6095
	if b7%(p,b1(p))<>1 then i5=b3%(p,b1(p)):i6=0:gosub 8010:i6=1:gosub 8010
	if b7%(p,b1(p))=1 then i5=p1%(p):i6=23:gosub 8009:i6=24:gosub 8009
	I5=P1%(D):I6=1:GOSUB 8020
	for i=0 to 3:a5%(i)=1:next:gosub 3800:gosub 324:gosub 4095
	print "He reaches on a single":gosub 6095:print p$(d,p1%(d))" is down and in pain!":GOSUB 6095
	gosub 2750:goto 3900

2047 po%=i+1:print "of "b$(d,b9%(d,po%))" for a single":gosub 6095:gosub 6095
	if b7%(p,b1(p))<>1 then i5=b3%(p,b1(p)):i6=0:gosub 8010:i6=1:gosub 8010
	if b7%(p,b1(p))=1 then i5=p1%(p):i6=23:gosub 8009:i6=24:gosub 8009
	I5=P1%(D):I6=1:GOSUB 8020
	for i=0 to 3:a5%(i)=1:next:gosub 3800:gosub 324:gosub 4095
	print b$(d,b9%(d,po%))" is down and injured":gosub 6095
	gosub 2097:goto 3900

2055 if h6%<=89 then print p$(d,p1%(d))" is motioning for "m$(d)" to come out":gosub 6095:print "It appears he has a blister":gosub 6095:print "He will have to leave the game":gosub 6095:gosub 2750:goto 5052

2056 if h6%=90 then print q$(0)" takes a mighty swing!":gosub 6095:print "It looks like he injured himself":gosub 6095:gosub 2800:goto 5052

2060 if h6%=91 then print "Time is called":GOSUB 6095:PRINT "Morganna strikes again!!":gosub 6095:goto 5052

2061 PRINT U$(1)" calls time to dust off the plate":gosub 6095:goto 5052

2097 gosub 2116:return

2098 gosub 2116:goto 3900

2099 gosub 2116:goto 5052

2100 '*** Determine Injury Length ***

2102 if b%(d,b9%(d,po%),3)=162 then print " remainder of game":goto 2115

2103 if b%(d,b9%(d,po%),3)=161 then print " this game and next game":il=1:goto 2115

2104 il=int(rnd(1)*(162-b%(d,b9%(d,po%),3)))+1:goto 2114

2114 print il" games"

2115 b%(d,b9%(d,po%),32)=b%(d,b9%(d,po%),32)+il+1:return

2116 GOSUB 2205:IJ$(IJ%)=b$(d,b9%(d,po%)):gosub 6095:print b$(d,b9%(d,po%))" is injured for":gosub 2100:gosub 6095:gosub 6095:gosub 2700:return

2200 EJ%=EJ%+1:EJ$(EJ%)=X$:RETURN

2205 IJ%=IJ%+1:RETURN

2700 '*** Defensive Player Ejected/Injured ***

2702 if u6=2 or u6=1 and p<>u9 then 2704

2703 print "You must replace "b$(d,b9%(d,po%)):GOSUB 6095:goto 2749

2704 FOR I2=1 TO 9:IF B7%(D,I2)=PO% THEN 2705
	NEXT

2705 FOR I=0 TO 22:IF B%(D,I,21)>0 OR B$(D,I)="XXX" THEN 2708

2706 IF B7%(D,I2)<>B%(D,I,22) AND B7%(D,I2)<>B%(D,I,23) and b7%(d,I2)<>b%(d,i,24) and b7%(d,I2)<>b%(d,i,25) THEN 2708

2707 print B$(D,B9%(D,PO%))" will be replaced by "b$(D,i):gosub 6095
	D0%(D)=D0%(D)+1
	X0%(D,0,D0%(D))=I
	X0%(D,1,D0%(D))=B7%(D,I2)
	X0%(D,2,D0%(D))=(I2*10)+(D0%(D)-9)
	B%(D,I,21)=1
	B3%(D,I2)=I
	I7%=D:I8%=I2:I9%=I
	GOSUB 7100

2711 B9%(D,B7%(D,I2))=I
	P9=D:W5=P9:W6=P9:V(P9)=po%:V1(P9)=po%:GOSUB 220:GOSUB 300:GOSUB 330:LOCATE 15,1
	goto 2749

2708 NEXT I
	for i=0 to 22:if b%(D,i,21)>0 or b$(D,i)="XXX" then 2710
	goto 2707

2710 next i

2749 return

2750 '*** Defensive Pitcher Ejected/Injured ***

2752 if u6=2 or u6=1 and p<>u9 then 2755

2753 print "You must replace "p$(d,p1%(d)):GOSUB 6095:goto 2799

2755 ep=1:gosub 8860:ep=0

2799 return

2800 '*** Offensive Player Ejected/Injured ***

2802 if u6=2 or u6=1 and p=u9 then 2805

2803 print "You must replace "q$(0)" with a pinch-hitter":GOSUB 6095:goto 2849

2805 FOR I=0 TO 22:IF B%(P,I,21)>0 OR B$(P,I)="XXX" THEN 2808

2806 IF B7%(P,B1(P))<>B%(P,I,22) AND B7%(P,B1(P))<>B%(P,I,23) and B7%(P,B1(P))<>B%(P,I,24) AND B7%(P,B1(P))<>B%(P,I,25) THEN 2808

2807 print q$(0)" will be replaced by "b$(p,i):gosub 6095
	D0%(P)=D0%(P)+1:X0%(P,0,D0%(P))=I:X0%(P,1,D0%(P))=B7%(P,B1(P)):X0%(P,2,D0%(P))=(B1(P)*10)+(D0%(P)-9)
	B%(P,I,21)=1:B3%(P,B1(P))=I:B9%(P,B7%(P,B1(P)))=I:IF B7%(P,B1(P))=1 OR B7%(P,B1(P))=10 THEN B9%(P,1)=99:B9%(P,0)=B1(P):B7%(P,B1(P))=10
	P9=P:W5=P9:W6=P9:V(P9)=B1(P):V1(P9)=B1(P):GOSUB 220:GOSUB 300:GOSUB 330:LOCATE 15,1
	Q$(0)=B$(P,B3%(P,B1(P))):IF B7%(P,B1(P))=1 THEN Q$(0)=P$(P,P1%(D))
	goto 2849

2808 NEXT i
	for i=0 to 22:if b%(p,i,21)>0 or b$(p,i)="XXX" then 2810
	goto 2807

2810 next i

2849 return

3050 gosub 275
	rp%=0
	if (((PC%=1 OR PC%=2 OR PC%=3) AND RN=2) OR (rd=1 and rn<=5)) and i%>=2 and rd%=0 then 2008
	H6%=INT(RND(1)*1000)+1
	IF H6%<=3 THEN 2000
	'IF H6%<=14 THEN 4600
	'IF H6%<=25 THEN 4500
	SN=0:H6%=INT(RND(1)*1000)+1
	IF O%(0)=0 AND S4%=4 AND I5%<2 AND H6%=768 OR O%(0)=0 AND S4%=4 AND I5%<2 AND H6%=123 OR O%(0)=0 AND S4%=7 AND I5%<2 AND H6%=245 OR O%(0)=0 AND S4%=7 AND I5%<2 AND H6%=381 THEN 3250
	H7%=INT(RND(1)*7500)+1

3051 GOSUB 4095:IF H6%<=4 THEN 3200

3052 IF H6%<=6 THEN 3210

3053 IF H6%<=11 OR H6%<=200 AND B%(D,B9%(D,2),22)<>2 AND B%(D,B9%(D,2),23)<>2 AND B%(D,B9%(D,2),24)<>2 AND B%(D,B9%(D,2),25)<>2 THEN 3220

3054 IF H6%<=19 THEN 3230
	IF H7%=27 THEN 3244
	IF H7%=28 THEN 3246

3055 GOTO 6600

3056 H6%=INT(RND(1)*1000)+1:H7%=INT(RND(1)*1000)+1:GOSUB 1840
	GOSUB 4095:IF H7%<=SN THEN PRINT"It gets through the drawn-in infield!":GOSUB 6095:GOTO 3063
	IF H6%>B4%(P,0,B1(P))+I4 THEN 3600

3057 H6%=INT(RND(1)*1000)+1:IF H6%<=B4%(P,1,B1(P)) AND P2<>2 OR H6%<=B4%(P,1,B1(P))*.5 AND P2=2 THEN S2%=8:GOTO 3500

3058 IF H6%<=B4%(P,1,B1(P))+B4%(P,2,B1(P)) AND P2<>2 OR H6%<=B4%(P,1,B1(P))*.5+B4%(P,2,B1(P))*.5 AND P2=2 THEN S2%=9:GOTO 3520

3059 GOSUB 7050
	I3=B4%(P,1,B1(P))+B4%(P,2,B1(P))

3060 E2%=I3+B4%(P,3,B1(P)):IF W%=8 AND H6%<=E2% AND P2<>2 OR W%=8 AND H6%<=E2%*.5 AND P2=2 THEN S2%=10:GOTO 3530

3061 E2%=I3+B4%(P,6,B1(P)):IF W%=7 AND H6%<=E2% AND P2<>2 OR W%=7 AND H6%<=E2%*.5 AND P2=2 THEN S2%=10:GOTO 3530

3062 E2%=I3+B4%(P,7,B1(P)):IF W%=9 AND H6%<=E2% AND P2<>2 OR W%=9 AND H6%<=E2%*.5 AND P2=2 THEN S2%=10:GOTO 3530

3063 H6%=INT(RND(1)*100)+1:S2%=7:IF B7%(P,B1(P))=1 OR H7%<=SN THEN 3065

3064 IF H6%<=(B%(P,B3%(P,B1(1)),16)*2.5) THEN S2%=12:GOSUB 7010:GOSUB 4400:IF F%=1 THEN 3900

3065 '*** single ***
	GOSUB 7030
	GOSUB 4095
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010:I6=1:GOSUB 8010

3067 I5=P1%(D):I6=1:GOSUB 8020

3068 IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009:I6=24:GOSUB 8009
	GOSUB 8011
	FOR I=0 TO 3
	A5%(I)=1:NEXT I
	IF S2%=12 OR B0%(2,0)=0 THEN 3075

3070 H6%=INT(RND(1)*100)+1
	BR%=B%(P,B3%(P,B0%(2,0)),16)
	IF B7%(P,B0%(2,0))=1 THEN BR%=P%(P,P1%(P),31)

3072 IF O%(0)=2 THEN I3=55+BR%*5

3073 IF O%(0)<2 THEN I3=40+BR%*5

3074 IF H6%<=I3 OR P2=2 AND S2%<>12 THEN A5%(2)=2

3075 IF B0%(1,0)=0 OR S2%=12 OR B0%(2,0)>0 AND A5%(2)=1 THEN 3092

3077 H6%=INT(RND(1)*100)+1
	BR%=B%(P,B3%(P,B0%(1,0)),16)
	IF B7%(P,B0%(1,0))=1 THEN BR%=P%(P,P1%(P),31)

3078 IF O%(0)=2 AND W%=9 THEN I3=46+BR%*3

3079 IF O%(0)=2 AND W%=8 THEN I3=28+BR%*3

3080 IF O%(0)=2 AND W%=7 THEN I3=10+BR%*3

3081 IF O%(0)<2 AND W%=9 THEN I3=34+BR%*3

3082 IF O%(0)<2 AND W%=8 THEN I3=14+BR%*3

3083 IF O%(0)<2 AND W%=7 THEN I3=-2+BR%*3

3091 IF H6%<=I3 OR P2=2 AND S2%<>12 THEN A5%(1)=2

3092 GOSUB 3800

3093 IF S2%=12 THEN GOSUB 324:GOTO 5050

3094 F%=0:PQ=0:GOSUB 4400

3095 IF F%=0 THEN GOSUB 3300

3096 GOSUB 324:GOTO 3900

3100 '*** ADD FOUL OUT ROUTINE HERE ***
	gosub 275
	if rn<=ft%(1) then OQ%=1
	A5%(0)=0:A5%(1)=0:A5%(2)=0:A5%(3)=0
	if rn<=ft%(2) then w%=2:goto 3105
	if rn<=ft%(3) then w%=3:goto 3105
	if rn<=ft%(4) then w%=5:goto 3105
	if rn<=ft%(5) then w%=4:goto 3105
	w%=6

3105 gosub 7024:gosub 275:gosub 4095:PRINT Q$(0)" pops it up foul ";
	if w%=6 and rn<=50 or w%=5 and rn<=25 then print "down the lf line"
	if w%=5 and rn>25 and rn<=50 then print "down the 3b line"
	if w%=6 and rn>50 or w%=5 and rn>50 and rn<=75 then print "behind 3b"
	if w%=5 and rn>75 or w%=2 and rn>70 and rn<=85 then print "near the 3b stands"
	if w%=2 and rn<=70 then print "behind the plate"
	if w%=3 and rn<=25 or w%=4 and rn<=50 then print "behind 1b"
	if w%=3 and rn>50 and rn<=75 or w%=2 and rn>85 then print "near the 1b stands"
	if w%=3 and rn>25 and rn<=50 then print "down the 1b line"
	if w%=3 and rn>75 or w%=4 and rn>50 then print "down the rf line"
	gosub 6095:print q$(4)" goes after it...":GOSUB 6095
	IF OQ%=1 THEN OQ%=0:PRINT "It's out of play":gosub 6095:goto 5052
	goto 4011

3200 '*** hit by pitch ***
	S2%=1:GOSUB 4095
	gosub 8060
	GOSUB 275
	if rn<=16 then print "it HITS him":gosub 6095:print q$(0)" trots down to first, unhurt"
	if rn>16 and rn<=32 then print "and "q$(0)" is BEANED":gosub 6095:print "Oh, my...":gosub 6095:print "He's up and shakes it off!"
	if rn>32 and rn<=48 then PRINT Q$(0)" is hit on the shoulder":gosub 6095:print "He rubs it off and trots to first"
	if rn>48 and rn<=64 then print "low and inside":gosub 6095:print "it hits "q$(0)" on the foot!"
	if rn>64 and rn<=80 then print Q$(0)" is nailed on the arm!"
	if rn>80 then PRINT P$(D,P1%(D));" hits ";Q$(0)
	GOSUB 6095

3201 A5%(0)=1:A5%(1)=0:A5%(2)=0:A5%(3)=0:IF B0%(1,0)>0 THEN A5%(1)=1

3202 IF B0%(1,0)>0 AND B0%(2,0)>0 THEN A5%(2)=1

3203 IF B0%(1,0)>0 AND B0%(2,0)>0 AND B0%(3,0)>0 THEN A5%(3)=1:IF W%=2 AND S2%>2 THEN A5%(3)=11

3204 GOSUB 3800:GOTO 5050

3210 '*** balk ***
	IF S4%=0 THEN 3050

3211 S2%=4:GOSUB 4095
	gosub 275
	UN=INT(RND(1)*4)+1
	if rn<=33 then print "a balk has been called"
	if rn>33 and rn<=66 then print "and a BALK is called":gosub 6095:print p$(d,p1%(d))" dropped the ball"
	if rn>66 then PRINT U$(UN);" calls a balk on ";P$(D,P1%(D))
	GOSUB 6095:GOTO 3223

3220 '*** passed ball ***
	IF S4%=0 AND SO<>1 THEN 3100

3221 S2%=5:GOSUB 4095
	gosub 8060
	gosub 275
	if rn<=50 then PRINT "pitch gets by ";B$(D,B9%(D,2)):GOSUB 6095
	if rn>50 then print "off "B$(D,B9%(D,2))"'s glove":gosub 6095:print "it gets away from him"
	PRINT "It's ruled a passed ball":gosub 6095
	IF B0%(1,0)=0 AND I=9 AND SO=1 OR O%(0)>1 AND I=9 AND SO=1 THEN O%(1)=O%(1)+1

3222 I8=INT(RND(1)*10)+1
	IF S4%=1 AND P2=2 AND I8<B%(P,B3%(P,B0%(1,0)),16) OR S4%=5 AND P2=2 AND I8<B%(P,B3%(P,B0%(1,0)),16) THEN A5%(1)=2:A5%(3)=1:A5%(0)=0:A5%(2)=2:GOSUB 3800:GOTO 5051

3223 IF B0%(1,0)=0 AND I=9 AND SO=1 OR O%(0)>1 AND I=9 AND SO=1 THEN A5%(0)=1:A5%(1)=1:A5%(2)=1:A5%(3)=1:GOSUB 3800:GOTO 3900

3224 A5%(1)=1:A5%(2)=1:A5%(3)=1:A5%(0)=0:GOSUB 3800:GOTO 5051

3230 '*** wild pitch ***
	IF S4%=0 AND SO<>1 THEN 3100

3231 S2%=6
	GOSUB 275
	IF RN<=16 THEN PRINT "and it gets away from "B$(D,B9%(D,2))
	if rn>16 and rn<=32 then print "pitch is WAY high, and "B$(D,B9%(D,2))" can't get it"
	if rn>32 and rn<=48 then print "It's in the dirt":gosub 6095:print "The runners move up 90 ft."
	if rn>48 and rn<=64 then print "It bounces in front of the plate":gosub 6095:print "it's past the catcher":GOSUB 6095:PRINT "the runners advance"
	if rn>64 and rn<=80 then print "it's over "B$(D,B9%(D,2))"'s head"
	if rn>67 then PRINT "wild pitch by ";P$(D,P1%(D)):GOSUB 6095:goto 3235
	gosub 275
	gosub 6095
	if rn<=33 then print "It's ruled a wild pitch"
	if rn>33 and rn<=66 then print "score it a wild pitch"
	if rn>66 then print "credit "p$(d,p1%(d))" with a wild pitch"
	gosub 6095

3235 GOTO 3222

3240 '*** base on balls ***
	S2%=2
	GOSUB 4095:BV=INT(RND(1)*100)+1:GOSUB 275
	IF D2=2 and bv<=50 THEN PRINT Q$(0)" is walked intentionally..."
	if d2=2 and bv>50 then print q$(0)" is given a purpose pass..."
	if d2=2 then S8%(D,0)=S8%(D,0)+1:gosub 6095:goto 3241
	IF RN<=30 THEN PRINT "Full count...":gosub 6095
	gosub 8060
	Y%=INT(RND(1)*26)+271:GOSUB 3990:GOSUB 9800
	print Y$
	gosub 6095
	Y%=INT(RND(1)*21)+321:GOSUB 3990:GOSUB 9800
	print Y$

3241 GOSUB 6095
	I5=P1%(D):I6=4:GOSUB 8020
	IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=30:GOSUB 8009:GOTO 3243
	I5=B3%(P,B1(P)):I6=7:GOSUB 8010

3243 GOTO 3201

3244 S2%=2:GOSUB 4095:PRINT B$(D,B9%(D,2));" is called for interference!!":GOSUB 6095
	PRINT Q$(0);" trots to first base":GOSUB 6095

3245 GOTO 3201

3246 GOSUB 4095:PRINT Q$(0)" is called out for obstruction!":GOSUB 6095
	W%=2:GOSUB 3420:O%(0)=O%(0)+1:O%(1)=O%(1)+1

3247 IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010:GOTO 3248
	I5=P1%(P):I6=23:GOSUB 8009

3248 I5=P1%(D):I6=0:GOSUB 8020
	GOTO 3900

3250 GOSUB 4095:Q$(4)=B$(D,B9%(D,5))
	PRINT "grounder to ";Q$(4);" @ 3B":GOSUB 6095
	PRINT Q$(2);" OUT @ 3B":GOSUB 6095
	PRINT Q$(1);" OUT @ 2B":GOSUB 6095
	PRINT Q$(0);" OUT @ 1B":GOSUB 6095:
	PRINT "--- TRIPLE PLAY !!! ---":GOSUB 6095:GOSUB 6095
	O%(0)=3

3251 I5=P1%(D):I6=0:FOR I=1 TO 3:GOSUB 8020:NEXT I
	IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009

3252 IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010

3253 GOTO 3900

3300 I8=0:H6%=INT(RND(1)*100)+1:IF S2%=8 AND B0%(3,0)>0 AND H6%<=B%(P,B3%(P,B0%(3,0)),16)*4 THEN I8=4

3301 IF S2%=8 AND B0%(2,0)>0 AND I8=0 AND H6%<6 AND B0%(3,0)=0 AND B%(P,B3%(P,B0%(2,0)),8)>0 THEN I8=3

3302 IF S2%=9 AND B0%(3,0)>0 AND H6%<=2 AND B%(P,B3%(P,B0%(3,0)),9)>0 THEN I8=4

3303 IF S2%=7 AND B0%(3,0)>0 AND P2=2 AND H6%>=6 AND H6%<=B%(P,B3%(P,B0%(3,0)),16) THEN I8=4

3304 IF S2%=7 AND B0%(3,0)>0 AND Z2%=B0%(3,0) AND P2<>2 AND H6%<=B%(P,B3%(P,B0%(3,0)),16)*12 THEN I8=4

3305 E2%=10+B%(P,B3%(P,B0%(2,0)),16)
	IF S2%=7 AND B0%(2,0)>0 AND P2<>2 AND H6%<=E2% AND B0%(3,0)=0 AND I8=0 THEN I8=3

3306 IF S2%=7 AND B0%(1,0)>0 AND B0%(2,0)=0 AND I8=0 AND H6%<=B%(P,B3%(P,B0%(2,0)),16) AND B%(P,B3%(P,B0%(1,0)),7)>0 THEN I8=2

3307 IF I8=0 THEN 3385

3308 IF U6=2 OR U6=1 AND P=U9 then 3310
	PRINT Q$(I8-1);
	IF S2%<10 THEN PRINT " try for ";H$(I8);" (YN) "

3309 IF S2%>10 THEN PRINT " tag up (YN) "
	PRINT "against ";Q$(4)

3310 ';" Arm:";B%(D,B9%(D,W%),15)
	I$="N":A1=S6%(P,0)-S6%(D,0)
	IF (U6=2 OR U6=1 AND P=U9) AND A1>-2 AND A1<2 AND S2%<>19 THEN I$="Y":GOTO 3312
	IF (U6=2 OR U6=1 AND P=U9) AND A1<0 AND I%>=7 OR S2%=19 THEN I$="N":GOTO 3312
	IF S2%=8 AND O%(0)=2 AND I8=3 AND (U6=2 OR U6=1 AND P=U9) THEN I$="N"
	IF U6=2 OR U6=1 AND P=U9 THEN 3312

3311 I$=GetKeyPress$
3312 IF UCASE$(I$)<>"Y" THEN I9=1:GOTO 3385

3313 PRINT Q$(I8-1)" is ";:if s2%<10 then print "going for ";h$(i8)
	if s2%>10 then print "tagging"
	gosub 6095
	IF S2%=7 AND I8=2 OR S2%>10 AND S4%=3 THEN 3317

3314 IF U6=2 OR U6=1 AND D=U9 then 3322
	PRINT Q$(4)" throw to ";H$(I8);" (YN) "

3322 IF U6=2 OR U6=1 AND D=U9 THEN I$="Y":GOTO 3316

3315 I$=GetKeyPress$
3316 I9=0:IF UCASE$(I$)<>"Y" THEN I9=1:print "No throw is made":gosub 6095:GOTO 3380

3317 GOSUB 4095:PRINT "Here comes the throw by ";Q$(4)
	FOR I=1 TO 3:PRINT".";:GOSUB 6095:NEXT I:PRINT" "
	GOSUB 4400:IF F%=1 THEN 3385
	I4=INT(RND(1)*1000)+1:A=B%(P,B3%(P,B0%(I8-1,0)),16)
	IF B7%(P,B0%((I8-1),0))=1 THEN A=p%(p,p1%(p),31)
	A=A-5:A=A*50
	IF S2%=17 THEN B=((B%(D,B9%(D,W%),15)-5)*25):I3=800+A-B

3319 IF S2%=18 THEN B=((B%(D,B9%(D,W%),15)-5)*25):I3=650+A-B

3320 IF S2%=19 or s2%<10 THEN B=((B%(D,B9%(D,W%),15)-5)*25):I3=500+A-B

3321 IF I4<=I3 THEN 3380

3370 A5%(I8-1)=11:O%(0)=O%(0)+1:O%(1)=O%(1)+1:I5=P1%(D):I6=0:GOSUB 8020
	gosub 275
	if rn<=4 and i8=4 then PRINT q$(i8-1)" collides with "B$(D,B9%(D,2)):gosub 6095:print b$(d,b9%(d,2))" hangs on!!":GOTO 3377
	if rn<=50 and i8<>4 then print Q$(I8-1)" doesn't make it!":GOTO 3377
	if rn<=80 then print "throw is perfect!":GOTO 3377
	if rn<=90 then print "This will be close":GOTO 3377
	print "Close play.....":gosub 6095
	IF I8<4 THEN print u$(i8+1);
	IF I8=4 THEN PRINT U$(1);
	PRINT " makes the call..."

3377 GOSUB 6095:GOSUB 3400:GOSUB 3800:GOSUB 3410:w%=po%
	IF O%(0)=3 OR S2%>10 THEN 3385

3371 I5=INT(RND(1)*100)+1
	A1=S6%(P,0)-S6%(D,0)
	IF A1<-2 OR A1<0 AND I%>8 THEN 3376

3372 '*** RUNNER ON 2ND GUNNED AT 3RD ON THROW HOME ***
	IF I8-1>2 AND I5<=15 AND B0%(2,0)>0 THEN GOSUB 3400:print "runner is trying for 3rd...":gosub 6095:A5%(2)=11:O%(0)=O%(0)+1:O%(1)=O%(1)+1:W%=5:GOSUB 3420:I5=P1%(D):I6=0:GOSUB 8020
	I5=INT(RND(1)*100)+1

3373 '*** RUNNER ON 1ST GUNNED AT 2ND ON THROW TO HOME OR 3RD ***
	IF I8-1>1 AND I5<=15 AND B0%(1,0)>0 AND A5%(2)<>11 THEN GOSUB 3400:W%=6:GOSUB 3420:print "runner is trying for 2nd...":gosub 6095:A5%(1)=11:O%(0)=O%(0)+1:O%(1)=O%(1)+1:I5=P1%(D):I6=0:GOSUB 8020
	I5=INT(RND(1)*100)+1

3374 IF I8-1>2 AND A5%(2)<>11 AND I5>(80-B%(P,B3%(P,B0%(2,0)),16)*2) AND B0%(2,0)>0 AND I9=0 THEN A5%(2)=1:print "runner is trying for 3rd...":gosub 6095:print "He's safe!":gosub 6095
	I5=INT(RND(1)*10)+1
	IF A5%(1)<>11 AND (S4%=4 OR S4%=7 AND I8-1>1 AND I5>(10-B%(P,B3%(P,B0%(1,0)),16)) AND B0%(1,0)>0 AND I9=0) THEN A5%(1)=1:print "runner is trying for 2nd...":gosub 6095:print "He's in there!":gosub 6095:GOTO 3376
	I5=INT(RND(1)*100)+1

3375 IF I8-1>1 AND I5>(80-B%(P,B3%(P,B0%(1,0)),16)*2) AND B0%(1,0)>0 AND I9=0 AND A5%(1)<>11 THEN print "runner safe at 2nd on the throw":gosub 6095:A5%(1)=1

3376 GOSUB 3800:I9=0:GOTO 3385

3380 A5%(I8-1)=1:if i9=1 then 3379
	gosub 275

3378 if rn<=4 and i8=4 then PRINT q$(i8-1)" collides with "B$(D,B9%(D,2)):gosub 6095:print "He knocks the ball loose!!":GOTO 3379
	if rn<=50 and i8<>4 then print Q$(I8-1)" beats the throw!":GOTO 3379
	IF RN<=60 THEN PRINT Q$(I8-1)" is ahead of the throw":goto 3379
	if rn<=80 then print "throw is off line":GOTO 3379
	if rn<=90 then print Q$(I8-1)" slides under the tag":GOTO 3379
	print "Close play...":gosub 6095
	if I8<>4 then print u$(i8+1);
	if I8=4 then print u$(1);
	PRINT " makes the call..."

3379 gosub 6095:print Q$(I8-1)" is safe @ ";H$(I8):gosub 6095
	if i8=4 then sah%=1

3381 GOSUB 3386:IF S2%<10 THEN 3371

3382 IF S2%>10 AND B7%(P,B1(P))=1 THEN P8%(P,P1%(P),23)=P8%(P,P1%(P),23)-1:P5%(P,P1%(P),23)=P5%(P,P1%(P),23)-1:I5=P1%(P):I6=41:GOSUB 8009:I5=P1%(D):I6=16:GOSUB 8020

3383 IF S2%>10 AND B7%(P,B1(P))<>1 THEN B8%(P,B3%(P,B1(P)),0)=B8%(P,B3%(P,B1(P)),0)-1:B5%(P,B3%(P,B1(P)),0)=B5%(P,B3%(P,B1(P)),0)-1:I5=B3%(P,B1(P)):I6=19:GOSUB 8010:I5=P1%(D):I6=16:GOSUB 8020:GOTO 3371

3385 RETURN

3386 IF S2%=7 AND I8=2 AND B7%(P,B1(P))=1 THEN I5=p1%(p):I6=27:GOSUB 8009:GOTO 3394

3387 IF S2%=7 AND I8=2 AND B7%(P,B1(P))<> 1 THEN I5=B3%(P,B1(P)):I6=4:GOSUB 8010:GOTO 3394

3388 IF S2%=8 AND I8=3 AND B7%(P,B1(P))=1 THEN I5=p1%(p):I6=28:GOSUB 8009:p8%(P,p1%(p),27)=p8%(P,p1%(p),27)-1:p5%(P,p1%(p),27)=p5%(P,p1%(p),27)-1:goto 3394

3389 IF S2%=8 AND I8=3 AND B7%(P,B1(P))<>1 THEN I3=4:I4=5:I5=B3%(P,B1(P))

3390 IF S2%=9 AND I8=4 THEN I3=5:I4=6:I5=B3%(P,B1(P))

3391 IF S2%=8 AND I8=3 OR S2%=9 AND I8=4 THEN 3393

3392 GOTO 3394

3393 B8%(P,I5,I3)=B8%(P,I5,I3)-1:B5%(P,I5,I3)=B5%(P,I5,I3)-1:B8%(P,I5,I4)=B8%(P,I5,I4)+1:B5%(P,I5,I4)=B5%(P,I5,I4)+1

3394 RETURN

3400 '*** CREDIT ASSISTS ***

3405 IF W%=1 THEN I5=P1%(D):I6=39:GOSUB 8020:GOTO 3408

3407 I5=B9%(D,W%):I6=17:GOSUB 8008

3408 RETURN

3410 '*** CREDIT INFIELD PUTOUTS ***

3411 PO%=I3+2: ' 1=1st 2=2nd 3=3rd 4=home

3412 IF PO%=6 THEN PO%=2:goto 3417

3413 IF PO%=3 AND fb2p%=1 tHEN fb2p%=0:PO%=1:goto 3415
	if po%=3 then 3417

3414 IF PO%=4 AND ast%=4 THEN PO%=6:goto 3417
	IF PO%=4 AND ast%=6 THEN PO%=4:goto 3417
	goto 3417

3415 IF PO%=1 THEN I5=p1%(d):I6=38:GOSUB 8020:GOTO 3418

3417 ast%=0:I5=B9%(D,PO%):I6=16:GOSUB 8008

3418 RETURN

3420 '*** CREDIT REGULAR PUTOUT  ***

3425 IF W%=1 THEN I5=p1%(d):I6=38:GOSUB 8020:GOTO 3429

3427 I5=B9%(D,W%):I6=16:GOSUB 8008

3429 RETURN

3499

3500 '*** double s2%=8 ***
	GOSUB 7030
	I5=P1%(D):I6=1:GOSUB 8020:GOSUB 4095
	gosub 8060
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010:I6=1:GOSUB 8010:I6=4:GOSUB 8010:GOTO 3503
	I5=p1%(p):I6=23:GOSUB 8009:I6=24:GOSUB 8009:I6=27:GOSUB 8009

3503 GOSUB 275
	IF RN<=25 THEN GOSUB 8030:HW%=1:GOSUB 3780:GOTO 3502
	IF RN>25 AND RN<=50 THEN PRINT Q$(0);:gosub 3550:PRINT "one to the gap in ";:gosub 3560:GOTO 3502
	if rn>50 and rn<=75 then print Q$(0)" hits a sinking liner into ";:gosub 3560:gosub 6095:print "It goes to the wall":GOTO 3502
	if rn>75 and rn<=85 and (w%=7 or w%=9) then PRINT Q$(0);:gosub 3550:print "one down the "c$(w%)" line":GOSUB 6095:print "It rolls into the corner":GOTO 3502
	if rn>75 and rn<=85 and w%=8 then PRINT Q$(0);:gosub 3550:print "one between the fielders":GOSUB 6095:print q$(4)" cuts it off...":GOTO 3502
	IF RN>85 AND RN<=92 THEN PRINT Q$(0);:GOSUB 3550:PRINT "a blue darter into "c$(w%):GOTO 3502
	if rn>92 then print Q$(0)" lines one to ";:gosub 3560:GOSUB 6095:print "It's down and to the wall!"

3502 HW%=0:GOSUB 6095
	if int(rnd(1)*100)+1<=80 then PRINT "Double!!" else print "2-Bagger!!"
	GOSUB 6095

3505 FOR I=0 TO 3:A5%(I)=2:NEXT I:A5%(3)=1

3506 IF B0%(1,0)=0 THEN 3511

3507 H6%=INT(RND(1)*100)+1
	BR%=B%(P,B3%(P,B0%(1,0)),16)
	IF B7%(P,B0%(1,0))=1 THEN BR%=P%(P,P1%(P),31)

3508 IF O%(0)=2 THEN I3=22+BR%*5

3509 IF O%(0)<2 THEN I3=8+BR%*5

3510 IF H6%<=I3 OR P2=2 THEN A5%(1)=3

3511 GOSUB 3800:F%=0:GOSUB 4400

3512 IF F%=0 THEN GOSUB 3300

3513 GOSUB 324:GOTO 3900

3520 '*** TRIPLE S2%=9 ***
	GOSUB 7040
	GOSUB 8060

3521 GOSUB 4095
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010:I6=1:GOSUB 8010:I6=5:GOSUB 8010:GOTO 3522
	I5=p1%(p):I6=23:GOSUB 8009:I6=24:GOSUB 8009:I6=28:GOSUB 8009

3522 I5=P1%(D):I6=1:GOSUB 8020
	gosub 275
	if rn<=10 and w%<>8 then 3570

3524 GOSUB 8030
	ad$="wall":if w%=7 and l%(1,13)=27 then ad$="screen"
	BV=INT(RND(1)*100)+1:GOSUB 275
	if rn<=20 then PRINT "It one-hops the "ad$"..."
	if rn<=40 and rn>20 then PRINT "It's off the top of the "ad$"..."
	if rn<=60 and rn>40 then PRINT "It bounces fair":gosub 6095:print "down into the corner..."
	if rn<=80 and rn>60 then PRINT "It's in the gap":gosub 6095:print "It rolls to the "ad$
	if rn<=99 and rn>80 then PRINT "It hits at the base of the "ad$"..."
	if rn>99 then PRINT "The outfielders cross"
	gosub 6095
	GOSUB 275
	if rn<=90 then print "Triple!!"
	IF RN>90 THEN print "3-Bagger!"
	GOSUB 6095

3525 A5%(3)=1:A5%(0)=3:A5%(1)=3:A5%(2)=2

3526 GOSUB 3800:F%=0:GOSUB 4400

3527 IF F%=0 THEN GOSUB 3300

3528 GOSUB 324:GOTO 3900

3530 '*** HOME RUN S2%=10 ***
	GOSUB 4095
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010:I6=1:GOSUB 8010:I6=6:GOSUB 8010

3532 I5=P1%(D):I6=1:GOSUB 8020

3533 IF B7%(P,B1(P))=1 THEN I5=p1%(p):I6=23:GOSUB 8009:I6=24:GOSUB 8009:I6=29:GOSUB 8009

3534 GOSUB 275:IF RN<=80 THEN GOSUB 8030:GOTO 3535
	GOSUB 275
	IF RN<=20 AND (W%=7 OR W%=9) THEN PRINT q$(0)" hits it down the "c$(w%)" line":gosub 6095:print "It's long enough if it's fair...":gosub 6095:goto 3536
	PRINT q$(0)" swings...":gosub 6095
	IF RN>20 AND RN<=40 THEN print "NO DOUBT ABOUT THAT ONE!!":GOSUB 6095:PRINT "IT'S WAY OUT OF HERE!!"
	IF RN>40 AND RN<=60 THEN PRINT Q$(4)" CAN ONLY LOOK UP!!"
	IF RN>60 AND RN<=80 THEN PRINT P$(D,P1%(D))" WATCHES IT SAIL OUT!!"
	IF RN>80 AND RN<=90 THEN PRINT "GONE!!!"
	IF RN>90 THEN PRINT "SAINTS AND HAYRAKES, LOOK AT 'ER SAIL!!"

3536 GOSUB 6095:PRINT "HOME RUN!!":GOTO 3543

3535 gosub 275
	IF RN<=5 AND L%(1,13)=8 AND W%=7 THEN PRINT "THAT'S OUT OF THE PARK":PRINT"ON WAVELAND AVENUE!!!":GOSUB 6095:PRINT "HOME RUN...HOLY COW!!!":GOTO 3543
	IF RN<=15 AND L%(1,13)=8 AND W%=7 THEN PRINT "IT'S GONE":GOSUB 6095:PRINT"LAST SEEN BOUNCING DOWN WAVELAND AVENUE!!!":GOSUB 6095:GOTO 3543
	IF RN<=25 THEN 3580

3545 GOSUB 275
	IF RN<=7 THEN PRINT "GOODBYE!! IT'S A HOME RUN!!"
	IF RN>7 AND RN<=14 THEN PRINT "ADIOS!! IT'S A HOME RUN !!!"
	IF RN>14 AND RN<=21 THEN print "SAY GOOD-BYE TO THAT ONE!!!"
	IF RN>21 AND RN<=28 THEN PRINT "TOUCH 'EM ALL!!"
	if rn>28 and rn<=35 then print "SEE ";:GOSUB 6095:PRINT "YOU ";:GOSUB 6095:PRINT "LATER!!"
	IF RN>35 AND RN<=42 THEN print "GOOD-BYE MR. SPALDING!! HOME RUN!!!"
	if rn>42 and rn<=49 then print "HOME RUN!! MY OH MY!!"
	if rn>49 and rn<=56 then print "IT'S OVER THE WALL...OH DOCTOR!!!"
	IF RN>56 AND RN<=63 THEN PRINT "LONG, LONG, AND GONE!!"
	IF RN>63 AND RN<=70 THEN PRINT "HEY! HEY!...IT'S A HOME RUN !!!"
	IF RN>70 AND RN<=77 THEN PRINT "IT'S LONG, IT'S LONG, IT'S GONE!!!"
	IF RN>77 AND RN<=84 THEN PRINT "HOLY COW!! IT'S A HOME RUN!!!"
	IF RN>84 AND RN<=89 THEN PRINT "OPEN THE WINDOW AUNT MINNIE...HERE SHE COMES":GOSUB 6095
	IF RN>89 AND RN<=95 THEN PRINT "WAAYYY BACK...":GOSUB 6095:PRINT "YOU CAN PUT IT ON THE BOARD, YYESSS!!":GOSUB 6095
	IF RN>95 THEN PRINT "IT'S WAY OUT OF HERE!! HOME RUN!!!":GOSUB 6095:PRINT "HOW 'BOUT THAT!!!"

3543 GOSUB 6095:A5%(3)=1:A5%(2)=2:A5%(1)=3:A5%(0)=4
	I5=P1%(D):I6=14:GOSUB 8020

3544 GOSUB 3800:GOSUB 324:GOTO 3900

3550 GOSUB 275
	IF RN<=20 THEN X$="rips"
	if rn>20 and rn<=40 then x$="hammers"
	if rn>40 and rn<=60 then X$="lashes"
	if rn>60 and rn<=80 then X$="lines"
	if rn>80 then X$="drives"

3555 PRINT " "X$" ";:return

3560 GOSUB 275
	IF W%=7 OR W%=8 AND RN>50 THEN PRINT "lcf"
	IF W%=9 OR W%=8 AND RN<=50 THEN PRINT "rcf"
	return

3570 gosub 275:x$="1st"
	if w%=7 then x$="3rd"
	if rn<=50 then print Q$(0)" hits a hot smash"
	if rn>50 then print Q$(0)" lines one"
	gosub 6095:print "Fair past "x$"!!":gosub 6095:print "It rolls into the "C$(W%)" corner"
	gosub 6095:PRINT "It's a triple!!":gosub 6095:goto 3525

3580 IF L%(1,13)=21 AND W%=7 THEN PRINT "IT CLEARS THE GREEN MONSTER!!":GOSUB 6095:PRINT "HOME RUN!!!":GOTO 3543
	IF L%(1,13)=27 AND W%=7 THEN PRINT "IT CLEARS THE SCREEN!!":GOSUB 6095:PRINT "HOME RUN!!!":GOTO 3543
	IF L%(1,13)=33 AND P=1 THEN PRINT "ADIOS!! THAT'S A HOME RUN!!":GOSUB 6095:PRINT "THE BUSCH EAGLE FLAPS ITS WINGS":GOSUB 6095:GOTO 3543
	IF L%(1,13)=25 AND P=1 THEN PRINT "IT'S OUT OF HERE FOR A HOMER!!!":GOSUB 6095:PRINT "THERE GOES THE EXPLODING SCOREBOARD!":GOTO 3543
	IF L%(1,13)=7 AND W%=9 THEN PRINT "IT CLEARS THE WAGON GATE!!":GOSUB 6095:PRINT "THAT'S A HOME RUN!!":GOTO 3543
	IF L%(1,13)=7 AND P=1 THEN PRINT "ADIOS!! THAT'S A HOME RUN!!":GOSUB 6095:PRINT "THERE GO THE CANNONS!!":GOTO 3543
	IF L%(1,13)=8 OR L%(1,13)=40 THEN PRINT "IT CLEARS THE IVY FOR A HOME RUN!!":GOTO 3543
	IF L%(1,13)=23 AND (W%=7 OR W%=9) THEN PRINT "IT'S LONG, IT'S LONG, IT'S GONE!!":GOSUB 6095:PRINT "THAT ONE'S IN THE UPPER DECK":GOTO 3543
	GOTO 3545

3600 '*** strikeout ***
	H6%=INT(RND(1)*1000)+1
	IF H6%>B4%(P,4,B1(P)) THEN 3610

3601 GOSUB 4095:GOSUB 3762:W%=2:GOSUB 3420:O%(0)=O%(0)+1:O%(1)=O%(1)+1:SO=1

3602 IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010:I6=8:GOSUB 8010:IF P2=2 THEN U%=3

3603 I5=P1%(D):I6=5:GOSUB 8020:I6=0:GOSUB 8020

3604 IF B7%(P,B1(P))=1 THEN I5=p1%(p):I6=23:GOSUB 8009:I6=31:GOSUB 8009:IF P2=2 THEN U%=3

3605 IF P2=2 AND O%(0)<3 THEN SO=7:GOTO 4800

3606 I=INT(RND(1)*600)+1
	IF B0%(1,0)=0 AND I=9 AND SO=1 OR O%(0)=3 AND I=9 THEN O%(0)=O%(0)-1:O%(1)=O%(1)-1:I5=P1%(D):P8%(D,I5,0)=P8%(D,I5,0)-1:P5%(D,I5,0)=P5%(D,I5,0)-1:B8%(D,B9%(D,2),16)=B8%(D,B9%(D,2),16)-1:B5%(D,B9%(D,2),16)=B5%(D,B9%(D,2),16)-1:GOTO 3608

3607 GOTO 3609

3608 I1=INT(RND(1)*100)+1:IF I1<77 THEN 3230 ELSE 3220

3609 GOTO 3900

3610 '*** ground ball for out ***
	H6%=INT(RND(1)*100)+1
	E2%=B%(P,B3%(P,B1(P)),18)+P%(D,P1%(D),12)-52
	E3%=23+P%(D,P1%(D),12)
	E4%=0:IF H6%>E2% AND B7%(P,B1(P))<>1 THEN E4%=1
	IF H6%>E3% AND B7%(P,B1(P))=1 THEN E4%=1
	IF E4%=1 THEN 4000

3611 GOSUB 7010:GOSUB 4095:S2%=13
	gosub 8060
	'PRINT Q$(0);
	GOSUB 3750
	IF W%=2 THEN 3612
	IF W%=1 THEN PRINT "the pitcher":goto 3612
	print f$(w%)
	'PRINT Q$(4);" at ";f$(W%)

3612 gosub 6095
	IF (S4%=1 OR S4%=4) AND O%(0)<=1 THEN GOSUB 3755
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010:GOTO 3614

3613 I5=p1%(p):I6=23:GOSUB 8009

3614 GOSUB 4400:IF F%=1 THEN 3900
	'*** 2 outs, bases empty, runner 2nd, runner 3rd, or ?? ***

3615 IF O%(0)=2 OR S4%=0 OR S4%=2 OR S4%=3 OR S4%=6 THEN 3650

3616 H6%=INT(RND(1)*1000)+1:E2%=((L%(D,5)-80)*4)
	E3%=((5-B%(P,B3%(P,B1(P)),16))*9)
	IF W%=2 AND H6%>250+E2%+E3% THEN 3650
	IF H6%>490+E2%+E3% THEN 3650

3617 IF P2=2 THEN 3650

3619 S2%=13:IF S4%=4 AND W%=5 AND INT(RND(1)*3)+1=1 THEN 3633

3620 IF S4%=4 AND W%=2 THEN GOSUB 3400:GOTO 3633

3621 IF S4%=7 AND W%=1 THEN GOSUB 3400:TB=9:W%=2:print "He throws home":gosub 6095:GOTO 3631

3622 iF S4%=5 AND S6%(P,0)-S6%(D,0)>=0 AND I%>7 AND O%(0)=0 AND I5%>0 AND W%=5 OR S4%=5 AND S6%(P,0)-S6%(D,0)>=0 AND I%>7 AND O%(0)=0 AND I5%>0 AND W%=3 THEN 3650

3623 IF S4%=7 AND I5%>0 AND (W%=5 OR W%=3) THEN GOSUB 3400:TB=9:W%=2:print "He fires it home":gosub 6095:GOTO 3631

3624 IF S4%=7 AND I5%=2 AND W%=4 THEN GOSUB 3400:TB=9:W%=2:print "He throws home for the force":gosub 6095:GOTO 3631

3625 IF S4%=7 AND I5%=2 AND W%=6 THEN GOSUB 3400:TB=9:W%=2:print "He goes home with it":gosub 6095:GOTO 3631

3626 IF W%<=4 THEN GOSUB 3400:W%=6:PRINT "He throws to "B$(D,B9%(D,6)):gosub 6095:goto 3628

3627 IF W%>4 THEN GOSUB 3400:W%=4:PRINT "He throws to "B$(D,B9%(D,4)):gosub 6095

3628 A5%(1)=11:A5%(2)=0:A5%(3)=0:A5%(0)=0
	IF S4%=7 AND W%=2 OR S4%=7 AND W%=1 THEN A5%(3)=11:A5%(2)=1:A5%(1)=1:A5%(0)=1

3629 GOTO 3634

3631 A5%(3)=11:A5%(1)=0:A5%(2)=0:A5%(0)=0:GOTO 3634

3633 A5%(3)=0:A5%(2)=11:A5%(1)=0:A5%(0)=0

3634 I5=P1%(D):I6=0:GOSUB 8020:O%(0)=O%(0)+1:O%(1)=O%(1)+1
	Q$(4)=B$(D,B9%(D,W%)):GOSUB 3800:GOSUB 3420:S2%=15:GOSUB 4400
	IF F%=1 THEN 3900

3640 GOSUB 3400:IF W%=5 AND O%(0)<2 AND INT(RND(1)*10)+1<=B%(P,B3%(P,B0%(1,0)),16) THEN 3643

3641 A5%(0)=11:A5%(1)=1:A5%(2)=0:A5%(3)=0:IF O%(0)<2 THEN A5%(2)=1:A5%(3)=1

3642 GOTO 3644

3643 A5%(0)=1:A5%(1)=11:A5%(2)=0:A5%(3)=0:IF O%(0)<2 THEN A5%(2)=1:A5%(3)=1

3644 O%(0)=O%(0)+1:O%(1)=2:I5=P1%(D):I6=0:gosub 3715:GOSUB 8020:GOSUB 3800
	GOSUB 3410
	gosub 275
	IF rn<=80 then PRINT " --- DOUBLE PLAY ---"
	if rn>80 then print " --- TWIN KILLING ---"
	DP%(D)=DP%(D)+1:D3%(D)=D3%(D)+1
	GOSUB 4230:TB=0:GOSUB 3645:GOSUB 6095:GOTO 3900

3645 IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=15:GOSUB 8010:GOTO 3646
	I5=P1%(P):I6=37:GOSUB 8009

3646 I5=P1%(D):I6=13:GOSUB 8020

3647 IF MO>0 THEN MO=MO-1

3648 RETURN

3650 IF O%(0)<2 THEN 3690

3651 I=INT(RND(1)*10)+1:S2%=13

3652 IF W%=5 AND I<=5 AND (S4%=4 or s4%=7) THEN GOSUB 3667:print q$(4)" gloves it and heads for 3rd":gosub 6095:GOTO 3670

3656 IF W%=5 and rp%<>1 THEN GOSUB 3667:gosub 3950:gosub 3960:GOTO 3672

3657 IF W%=6 AND I<=8 AND (S4%=1 OR S4%=4 OR S4%=5 OR S4%=7) THEN GOSUB 3667:gosub 3950:gosub 3970:ast%=6:GOTO 3671

3661 IF W%=6 THEN GOSUB 3667:gosub 3950:gosub 3960:GOTO 3672

3662 IF W%=4 AND I<=5 AND (S4%=1 OR S4%=4 OR S4%=5 OR S4%=7) THEN GOSUB 3667:gosub 3950:gosub 3970:ast%=4:GOTO 3671
	GOSUB 275
	if w%=3 AND RN<=94 then gosub 3667:gosub 3950:print "He takes it to the bag unassisted":fbu%=1:goto 3672
	IF W%=3 THEN GOSUB 3667:gosub 3950:w%=1:gosub 3667:print "He flips it to "q$(4)" covering":fb2p%=1:w%=3:goto 3672

3666 GOSUB 3667:gosub 3950:gosub 3955:GOTO 3672

3667 IF W%=1 THEN Q$(4)=P$(D,P1%(D))

3668 IF W%<>1 THEN Q$(4)=B$(D,B9%(D,W%))

3669 RETURN

3670 A5%(0)=0:A5%(1)=0:A5%(2)=11:A5%(3)=0:GOTO 3684

3671 A5%(0)=0:A5%(1)=11:A5%(2)=0:A5%(3)=0:GOTO 3684

3672 A5%(0)=11:A5%(1)=0:A5%(2)=0:A5%(3)=0

3684 GOSUB 6095
	if fbu%=0 then GOSUB 3400
	fbu%=0
	GOSUB 3800:GOSUB 3410:O%(0)=O%(0)+1:O%(1)=O%(1)+1:I5=P1%(D):I6=0:GOSUB 8020:GOTO 3900

3690 I=INT(RND(1)*100)+1:IF I<=90 AND P2<>2 or s4%=0 THEN 3720

3691 IF I<=10 AND P2=2 THEN 3720

3692 S2%=13:A5%(1)=1:A5%(2)=1:A5%(3)=1:A5%(0)=11
	I=INT(RND(1)*100)+1
	IF S4%=1 OR S4%=4 OR S4%=5 OR S4%=7 THEN 3713

3693 A5%(1)=0:A5%(2)=0:A5%(3)=0
	IF W%=5 AND I<=25 AND I5%<2 THEN A5%(2)=1

3696 IF W%=5 AND I<=50 AND I5%=0 THEN A5%(3)=1

3697 IF W%=5 AND I<=10 AND I5%>0 THEN A5%(3)=1

3698 IF W%=5 AND I<5 AND I5%<2 AND S4%=2 AND S6%(P,0)-S6%(D,0)>-2 OR W%=5 AND I<5 AND I5%<2 AND S4%=6 AND A5%(3)=1 AND S6%(P,0)-S6%(D,0)>-2 THEN A5%(2)=11:A5%(0)=1:print "NO, he's going to third":gosub 6095

3699 IF W%=6 AND I<=20 AND I5%<2 THEN A5%(2)=1

3700 IF W%=6 AND I<=92 AND I5%<2 THEN A5%(3)=1

3701 IF W%=6 AND I<=5 AND I5%=2 THEN A5%(3)=1

3702 IF W%=6 AND I<6 AND I5%<2 AND S4%=2 AND S6%(P,0)-S6%(D,0)>-2 OR W%=6 AND I<6 AND I5%<2 AND S4%=6 AND A5%(3)=1 AND S6%(P,0)-S6%(D,0)>-2 THEN A5%(2)=11:A5%(0)=1:print "NO, he's going to third":gosub 6095

3703 IF W%=4 AND I<=90 AND I5%<2 THEN A5%(2)=1

3704 IF W%=4 AND I<=90 AND I5%<2 THEN A5%(3)=1

3705 IF W%=4 AND I<=65 AND I5%=2 THEN A5%(2)=1

3706 IF W%=4 AND I<=5 AND I5%=2 THEN A5%(3)=1

3707 IF W%=3 AND I<=95 THEN A5%(2)=1

3708 IF W%=3 AND I<=60 AND I5%=0 THEN A5%(3)=1

3709 IF W%=3 AND I<=15 AND I5%>0 THEN A5%(3)=1

3710 IF W%=1 AND I<=30 THEN A5%(2)=1

3711 IF W%=1 AND I<=10 THEN A5%(3)=1

3712 IF W%=2 AND I<=45 THEN A5%(2)=1

3713 GOTO 3684

3715 gosub 275
	if rn<=10 then print "Give me the Hoover!":goto 3716
	if a5%(0)<>11 then 3716
	gosub 275
	if rn<=8 then print "over to 1st..."
	if rn>8 and rn<=16 then print "throw to 1st..."
	if rn>16 and rn<=24 then print "on to first..."
	if rn>24 and rn<=30 then print q$(4)" pivots and throws"
	if rn>30 and rn<=36 then print q$(4)" gets the throw off"
	if rn>36 and rn<=44 then print q$(4)" is hit as he throws"
	if rn>44 and rn<=50 then print q$(4)" leaps and throws"
	if rn>50 and rn<=56 then print q$(4)" jumps over the sliding "q$(1)
	if rn>56 and rn<=64 then print "the relay to first..."
	if rn>64 and rn<=72 then print "quick turn by "q$(4)
	if rn>72 and rn<=80 then print "relay to 1st..."
	if rn>80 and rn<=88 then print "to 1st..."
	if rn>88 and a5%(0)=11 then print "over to 1st..."

3716 gosub 6095:return

3720 S2%=14
	IF S4%=0 OR S4%=2 OR S4%=3 OR S4%=6 THEN A5%(0)=11:gosub 3744:GOTO 3693

3721 I=INT(RND(1)*100)+1

3722 A5%(0)=1:A5%(1)=1:A5%(2)=1:A5%(3)=1:IF I5%=0 AND W%=2 THEN A5%(3)=0

3723 IF S4%=5 OR S4%=7 OR S4%=4 THEN 3725

3724 A5%(1)=11:GOTO 3743

3725 IF S4%=4 OR S4%=7 THEN 3732

3726 IF S6%(P,0)-S6%(D,0)>=0 AND I%>6 AND O%(0)=0 AND I5%>0 AND W%=5 OR S6%(P,0)-S6%(D,0)>=0 AND I%>6 AND O%(0)=0 AND I5%>0 AND W%=3 THEN A5%(3)=11:GOTO 3743

3727 IF I5%=0 AND W%<>2 THEN A5%(1)=11:GOTO 3743

3728 IF I5%>0 AND W%=5 OR I5%>0 AND W%=3 THEN A5%(3)=0:A5%(1)=11:GOTO 3743

3729 IF I5%>0 AND W%=1 OR W%=3 THEN A5%(3)=0:A5%(1)=1:A5%(0)=11:GOTO 3743

3730 IF I5%=2 AND W%=4 OR I5%=2 AND W%=6 AND I<=90 THEN A5%(3)=0:A5%(1)=11:GOTO 3743

3731 A5%(1)=11:GOTO 3743

3732 IF W%=6 AND I<6 AND I5%<2 AND I%<9 OR W%=5 AND I<6 AND I5%<2 AND I%<9 THEN A5%(2)=11:GOTO 3684

3733 IF S4%=7 THEN 3740

3734 I=INT(RND(1)*10)+1:A5%(0)=1:A5%(1)=11:A5%(2)=1:A5%(3)=0

3735 IF W%=5 AND I<=5 THEN A5%(1)=1:A5%(2)=11:A5%(3)=0

3736 IF W%=1 AND I<=3 THEN A5%(1)=1:A5%(2)=11:A5%(3)=0

3737 IF W%=2 AND B%(P,B3%(P,B0%(2,0)),16)<7 AND B%(P,B3%(P,B0%(2,0)),16)>0 THEN A5%(1)=1:A5%(2)=11:A5%(3)=0:GOTO 3743

3738 IF W%=2 AND B%(P,B3%(P,B0%(2,0)),16)>6 THEN A5%(1)=1:A5%(2)=1:A5%(0)=11

3739 GOTO 3743

3740 I=INT(RND(1)*100)+1:A5%(1)=11:A5%(2)=1:A5%(3)=1:A5%(0)=1

3741 IF I5%>0 AND W%=5 OR I5%>0 AND W%=3 OR I5%=2 AND W%=4 OR I5%=2 AND W%=6 OR W%=2 OR W%=1 THEN A5%(3)=11:A5%(0)=1:A5%(1)=1:A5%(2)=1:GOTO 3743

3742 IF W%=5 AND I<=5 OR W%=1 OR W%=2 THEN A5%(3)=11:A5%(2)=1:A5%(1)=1:A5%(0)=1

3743 GOTO 3684

3744 gosub 3667
	gosub 3950
	gosub 275
	if w%=3 AND RN<=94 then print "he takes it to the bag unassisted":fbu%=1:goto 3745
	IF W%=3 THEN w%=1:gosub 3667:print "He flips it to "q$(4)" covering":fb2p%=1:w%=3
	if w%=4 then gosub 3955
	if w%=5 or w%=6 then gosub 3960

3745 return

3750 BV=INT(RND(1)*100)+1
	if rp%=1 then 3751
	IF W%=2 THEN 3760
	if bv<=15 and w%=1 then print "tapped back to ";:return
	if bv<=30 and w%=1 then print q$(0)" taps it to ";:return
	IF BV<=40 AND W%=1 THEN PRINT "Come back Little Sheba":gosub 6095:print "taken by ";:return
	GOSUB 275
	y%=int(rnd(1)*58)+171
	IF RN<=80 THEN y%=int(rnd(1)*58)+171:GOTO 3752
	IF RN>80 AND RN<=85 AND (W%=3 OR W%=4) THEN y%=int(rnd(1)*3)+246:GOTO 3752
	IF RN>80 AND RN<=85 AND (W%=5 OR W%=6) THEN y%=int(rnd(1)*3)+251:GOTO 3752
	IF RN>85 AND RN<=90 AND (W%=4 OR W%=6 or w%=1) THEN y%=int(rnd(1)*3)+256:GOTO 3752

3752 GOSUB 3990:GOSUB 9800:PRINT Y$" ";

3751 return

3755 GOSUB 275
	IF RN<=20 THEN PRINT "may be a double play ball"
	IF RN>20 AND RN<=40 THEN PRINT "they might turn two"
	if rn>40 and rn<=60 then print "they could get two"
	if rn>60 and rn<=80 then print "could be two"
	if rn>80 then print "could be a double play"

3759 GOSUB 6095:RETURN

3760 '*** in front of plate ***
	gosub 275
	if rn<=16 then print "weak roller to "B$(D,B9%(D,2))"'s left"
	if rn>16 and rn<=32 then print "dribbler in front"
	if rn>32 and rn<=48 then print "topped off the plate"
	if rn>48 and rn<=64 then print "swinging bunt right in front"
	if rn>64 and rn<=80 then PRINT q$(0)" hits a weak roller"
	if rn>80 then print q$(0) " taps one in front"

3761 RETURN

3762 '*** strikeout pbp ***
	IF U%=3 THEN BV=20:GOTO 3770

3765 BV=INT(RND(1)*100):gosub 275
	if rn<=20 then print "Full count...":gosub 6095
	gosub 8060
	if bv=1 then print "Strike Three!!":print q$(0)" turns to "u$(1):gosub 6095:print "he says he tees them up higher than that":print "at the country club!!":gosub 6095:GOTO 3774
	if bv=2 then PRINT "Strike three...":gosub 6095:print q$(0)" stands there like a house":print "by the side of the road":gosub 6095:gosub 6095:GOTO 3774
	if bv>2 and bv<=5 then PRINT Q$(0)" looks at a called strike three":GOSUB 6095:PRINT"and turns to glare at "U$(1):GOSUB 6095:PRINT M$(P)" is barking from the dugout!!":GOTO 3774
	y%=int(rnd(1)*28)+101:gosub 3990:GOSUB 9800
	PRINT Y$:GOSUB 6095

3770 y%=int(rnd(1)*12)+151:gosub 3990:GOSUB 9800
	PRINT Y$

3774 gosub 6095
	gosub 275
	if rn<=20 then print "Number"P8%(D,p1%(d),5)+1"for "p$(d,p1%(d))
	if rn>20 and rn<=40 then print "That's"P8%(D,p1%(d),5)+1"for "p$(d,p1%(d))
	gosub 6095:RETURN

3780 '*** doubles off of a wall ***
	BV=INT(RND(1)*100)+1:rn=int(rnd(1)*100)+1

3781 IF BV<=40 AND HW%=0 THEN PRINT "It's in the gap and to the wall":GOTO 3785
	if bv<=65 and rn<=25 and (l%(1,13)=8 or l%(1,13)=40) THEN PRINT "It's off the ivy!":goto 3785
	if bv<=65 and rn<=50 and l%(1,13)=21 and w%=7 then print "It hits off the Green Monster!":goto 3785
	if bv<=65 and l%(1,13)=27 and w%=7 then print "It hits off the screen!":goto 3785
	IF BV<=65 THEN PRINT "It's off the wall":GOTO 3785
	IF BV<=85 AND HW%=0 THEN 3788
	IF BV<=90 AND HW%=0 THEN PRINT "It's to the wall":gosub 6095:print q$(0) " slides into second":PRINT "with a stand-up double!!":GOTO 3785
	if rn<=25 and l%(1,13)=21 then print "It's off the Green Monster!":goto 3785
	if l%(1,13)=27 and w%=7 then PRINT "It's off the screen...":GOTO 3785
	PRINT "It's off the top of the wall..."

3785 RETURN

3788 IF W%=8 THEN BV=39:GOTO 3781

3789 PRINT "It's past "q$(4):GOSUB 6095:PRINT "It goes into the corner...":GOTO 3785

3800 IF B0%(3,0)=0 OR A5%(3)=0 THEN 3805

3801 I=3

3802 IF A5%(3)=10 THEN I3=3:GOSUB 3840

3803 IF A5%(3)>=1 AND A5%(3)<=4 THEN GOSUB 3845

3804 IF A5%(3)=11 THEN I3=4:GOSUB 3840

3805 IF B0%(2,0)=0 OR A5%(2)=0 THEN 3812

3806 I=2

3807 IF A5%(2)=10 THEN I3=2:GOSUB 3840

3808 IF A5%(2)=11 THEN I3=3:GOSUB 3840

3809 IF A5%(2)=12 AND B0%(3,0)=0 THEN I3=4:GOSUB 3840

3810 IF A5%(2)=1 AND B0%(3,0)=0 THEN GOSUB 3850

3811 IF A5%(2)>=2 AND A5%(2)<=4 AND B0%(3,0)=0 THEN GOSUB 3845

3812 IF B0%(1,0)=0 OR A5%(1)=0 THEN 3821

3813 I=1:IF A5%(1)=8 AND B0%(2,0)=0 THEN GOSUB 3850

3814 IF A5%(1)=10 THEN I3=1:GOSUB 3840

3815 IF A5%(1)=11 THEN I3=2:GOSUB 3840

3816 IF A5%(1)=12 AND B0%(3,0)=0 AND B0%(2,0)=0 THEN I3=3:GOSUB 3840

3817 IF A5%(1)=13 AND B0%(2,0)=0 AND B0%(3,0)=0 THEN I3=4:GOSUB 3840

3818 IF A5%(1)=1 AND B0%(2,0)=0 THEN GOSUB 3850

3819 IF A5%(1)=2 AND B0%(2,0)=0 AND B0%(3,0)=0 THEN GOSUB 3851

3820 IF A5%(1)>=3 AND A5%(1)<=4 AND B0%(2,0)=0 AND B0%(3,0)=0 THEN GOSUB 3845

3821 I=0:B0%(0,0)=B1(P):B0%(0,1)=P1%(D):IF A5%(0)=0 THEN 3835

3822 IF A5%(0)>0 AND A5%(0)<4 THEN I3=A5%(0):GOSUB 3852

3825 IF A5%(0)=4 THEN I3=A5%(0):GOSUB 3845

3826 IF A5%(0)=11 THEN I3=1:GOSUB 3840

3827 IF A5%(0)=12 THEN I3=2:GOSUB 3840

3828 IF A5%(0)=13 THEN I3=3:GOSUB 3840

3829 IF A5%(0)=14 THEN I3=4:GOSUB 3840

3835 IF F%=0 AND O%(1)<3 THEN GOSUB 6800

3836 FOR I=0 TO 3:A5%(I)=0:NEXT I

3837 RETURN

3840 PRINT Q$(I);" out @ ";H$(I3):B0%(I,0)=0:B0%(I,1)=0:B0%(I,2)=0:Q$(I)="":GOSUB 6095:RETURN

3845 if sah%=0 then PRINT Q$(I);" scores"
	sah%=0
	I5=B0%(I,1):I6=2:GOSUB 8020
	IF P1%(D)<>B0%(I,1) THEN I5=P1%(D):I6=18:GOSUB 8020
	IF P6%(P)=1 THEN P8%(P,P1%(P),20)=P8%(P,P1%(P),20)+1

3846 GOSUB 1815:GOSUB 1820:S%(P,I%)=S%(P,I%)+1:S6%(P,0)=S6%(P,0)+1:GOSUB 1150:B0%(I,0)=0:B0%(I,1)=0:B0%(I,2)=0:Q$(I)="":GOSUB 6095
	if s%(p,i%)=4 then print "They're tearin' up the peapatch!":gosub 6095

3847 IF I%>=9 AND P=1 AND S6%(P,0)-S6%(D,0)=1 AND F%=0 AND O%(1)<3 AND S2%<>10 THEN GOSUB 6845

3848 IF I%>=9 AND P=1 AND S6%(P,0)-S6%(D,0)=1 AND S2%<>10 THEN A5%(0)=0:A5%(1)=0:A5%(2)=0:A5%(3)=0

3849 RETURN

3850 FOR I4=0 TO 2:B0%(I+1,I4)=B0%(I,I4):B0%(I+1,2)=0:B0%(I,I4)=0:NEXT I4:Q$(I+1)=Q$(I):Q$(I)="":RETURN

3851 FOR I4=0 TO 2:B0%(I+2,I4)=B0%(I,I4):B0%(I+1,2)=0:B0%(I,I4)=0:NEXT I4:Q$(I+2)=Q$(I):Q$(I)="":RETURN

3852 B0%(I3,0)=B1(P):B0%(I3,1)=P1%(D):Q$(I3)=Q$(0):B0%(I3,2)=0

3853 B0%(0,0)=0:RETURN

3860

3900 rp%=0:H0%=0:I5%=0:F%=0:PQ=0:H0%(0)=0:IF O%(0)=3 THEN 5000

3901 GOTO 5050

3950 '*** grounders to infielders ***
	if rp%=1 then return
	x$=q$(4)
	if w%=2 then 3953
	Y%=INT(RND(1)*39)+1:GOSUB 3990:GOSUB 9800
	print x$" "Y$
	gosub 6095
	return

3953 ' grounders in front of the plate
	Y%=INT(RND(1)*7)+91:GOSUB 3990:GOSUB 9800
	IF LEFT$(Y$,2)="QQ" THEN Y$=X$+RIGHT$(Y$,LEN(Y$)-2)
	print Y$
	gosub 6095
	return

3955 '*** throw to 1st from 2nd ***
	y%=int(rnd(1)*10)+51:gosub 3990:GOSUB 9800
	print y$" to first"
	gosub 6095
	return

3960 '*** throw to 1st from 3rd/ss ***
	Y%=INT(RND(1)*13)+71:GOSUB 3990:GOSUB 9800
	PRINT Y$" to first"
	gosub 6095
	return

3970 '*** double play pivot ***
	if w%=4 then ww%=6
	if w%=6 then ww%=4
	if w%=3 then ww%=1
	y%=int(rnd(1)*4)+261:gosub 3990:GOSUB 9800
	PRINT Y$" "B$(D,B9%(D,ww%)):gosub 6095:return

4000 I=INT(RND(1)*100)+1:A5%(0)=0:A5%(1)=0:A5%(2)=0:A5%(3)=0
	IF I<=73 THEN 4020

4001 IF I<=89 THEN 4010

4002 S2%=21:GOSUB 7010:GOSUB 4095
	gosub 8060
	y%=int(rnd(1)*11)+436:gosub 3990:GOSUB 9800
	PRINT Y$
	gosub 6095
	y%=int(rnd(1)*11)+451:gosub 3990:GOSUB 9800
	PRINT Y$
	gosub 6095
	IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009:GOTO 4004
	I5=B3%(P,B1(P)):I6=0:GOSUB 8010

4004 GOSUB 6095:GOSUB 4400:IF F%=1 THEN 3900

4005 O%(0)=O%(0)+1:O%(1)=O%(1)+1:GOSUB 3420:I5=P1%(D):I6=0:GOSUB 8020
	IF O%(0)=3 THEN 3900

4006 I=INT(RND(1)*100)+1
	IF (S4%=1 OR S4%=5) AND I>=80 OR (S4%=1 OR S4%=5) AND P2=2 THEN A5%(1)=10:O%(0)=O%(0)+1:O%(1)=O%(1)+1:GOSUB 8020:D3%(D)=D3%(D)+1:DP%(D)=DP%(D)+1:PRINT " --- DOUBLE PLAY ---":I3=1:GOSUB 3410:IF W%<>3 THEN GOSUB 3400

4007 IF (S4%=4 OR S4%=2) AND I>=80 OR S4%=4 AND P2=2 THEN A5%(2)=10:O%(0)=O%(0)+1:O%(1)=O%(1)+1:GOSUB 8020:D3%(D)=D3%(D)+1:DP%(D)=DP%(D)+1:PRINT " --- DOUBLE PLAY ---":I3=2:GOSUB 3410:GOSUB 3400

4009 GOSUB 3800:GOTO 3900

4010 GOSUB 7010:S2%=20:GOSUB 4095:GOSUB 9500
	y%=int(rnd(1)*14)+466:gosub 3990:GOSUB 9800
	PRINT Y$
	GOSUB 6095
	IF (W%=2 OR W%=5 OR W%=3) AND INT(RND(1)*100)+1<=30 THEN PRINT "it's in foul territory":gosub 6095:fo%=1:goto 4011
	gosub 4030:gosub 6095:GOTO 4013

4011 if w%=2 then print q$(4)" discards the mask":gosub 6095

4013 IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009:GOTO 4015

4014 I5=B3%(P,B1(P)):I6=0:GOSUB 8010

4015 GOSUB 6095:IF S4%=4 THEN 4017
	IF INT(RND(1)*100)+1<=25 AND (L%(1,13)=11 OR L%(1,13)=8) THEN PRINT "fights the wind...":gosub 6095
	if int(rnd(1)*100)+1<=25 and (l%(1,13)=2 or l%(1,13)=4 or l%(1,13)=13 or l%(1,13)=24) THEN PRINT "The dome lights are giving him a problem...":gosub 6095

4016 if fo%<>1 then GOSUB 4400:IF F%=1 THEN 3900
	fo%=0
	PRINT q$(4)" makes the catch":gosub 6095

4017 GOSUB 3420:O%(0)=O%(0)+1:O%(1)=O%(1)+1:I5=P1%(D):I6=0:GOSUB 8020:GOTO 3900

4020 GOSUB 7030
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010:GOTO 4022

4021 I5=P1%(P):I6=23:GOSUB 8009

4022 I=INT(RND(1)*100)+1:IF I<=17 THEN 4050

4023 IF I<=33 THEN 4060

4024 '*** regular fly out ***
	S2%=18:GOSUB 4095
	x$=c$(w%)
	if int(rnd(1)*100)<=25 and w%=7 then x$="lcf"
	if int(rnd(1)*100)<=25 and w%=9 then x$="rcf"
	gosub 8060
	y%=int(rnd(1)*40)+481:gosub 3990:GOSUB 9800
	PRINT Y$" "X$
	GOSUB 6095:GOSUB 4400:IF F%=1 THEN 3900

4025 GOSUB 3420:GOSUB 4300:O%(0)=O%(0)+1:O%(1)=O%(1)+1:I5=P1%(D):I6=0:GOSUB 8020:IF O%(0)=3 THEN 3900

4026 I=INT(RND(1)*100)+1
	IF S2%=19 AND S4%=1 AND I<=33 AND P2=2 OR S2%=19 AND S4%=5 AND I<=33 THEN A5%(1)=10:O%(0)=O%(0)+1:O%(1)=O%(1)+1:GOSUB 8020:D3%(D)=D3%(D)+1:DP%(D)=DP%(D)+1:GOSUB 3400:GOTO 4009

4027 IF B0%(3,0)>0 THEN I8=4:GOSUB 3308

4028 IF I9=1 THEN I9=0:GOTO 4074

4029 GOTO 4066

4030 gosub 275
	if w%<>2 then 4031
	if rn<=50 then print "it's in front of the plate"
	if rn>50 then print "it's behind the plate":fo%=1
	gosub 6095:print q$(4)" tosses the mask away":return

4031 if (w%=3 or w%=4) and rn<=50 then print "it's on the right side"
	if (w%=5 or w%=6) and rn<=50 then print "it's on the left side"
	if w%=1 or rn>50 then print"it's on the infield"
	gosub 6095:gosub 275
	print q$(4);
	if rn<=25 then print " calls for it"
	if rn>25 and rn<=50 then print " is under it"
	if rn>50 and rn<=75 then print " drifts under it"
	if rn>75 and rn<=90 then print " moves under it"
	if rn>90 then print " backs under it"
	return

4050 '*** shallow fly ball ***
	S2%=19
	GOSUB 4095
	x$=c$(w%)
	if int(rnd(1)*100)<=25 and w%=7 then x$="lcf"
	if int(rnd(1)*100)<=25 and w%=9 then x$="rcf"
	gosub 8060
	y%=int(rnd(1)*19)+531:gosub 3990:GOSUB 9800
	PRINT Y$" "X$
	GOSUB 6095:GOSUB 4400:IF F%=1 THEN 3900

4051 GOTO 4025

4060 '*** deep fly out ***
	S2%=17:GOSUB 4095:GOSUB 8030:GOSUB 4400

4061 IF F%=1 THEN 3900

4062 GOSUB 3420:GOSUB 4350
	O%(0)=O%(0)+1:O%(1)=O%(1)+1:I5=P1%(D):I6=0:GOSUB 8020:IF O%(0)=3 THEN 3900

4063 IF B0%(3,0)=0 THEN 4066

4064 A5%(3)=1:I5=P1%(D):I6=16:GOSUB 8020
	IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8007:I5=P1%(P):I6=41:GOSUB 8009:GOTO 4066

4065 I5=B3%(P,B1(P)):I6=0:GOSUB 8006:I5=B3%(P,B1(P)):I6=19:GOSUB 8010

4066 IF B0%(2,0)=0 THEN 4072

4067 I4=(B%(P,B3%(P,B0%(2,0)),16)-5)*25:ZM=(B%(D,B9%(D,W%),15)-5)*25
	I4=I4-ZM
	I=INT(RND(1)*1000)+1

4068 IF W%=9 AND S2%=17 AND I<=800+I4 AND P2<>2 OR W%=9 AND S2%=18 AND I<=400+I4 AND P2<>2 OR W%=8 AND S2%=17 AND I<=700+I4 AND P2<>2 OR W%=8 AND S2%=18 AND I<=350+I4 AND P2<>2 OR W%=7 AND S2%=17 AND I<=250+I4 AND P2<>2 THEN A5%(2)=1

4069 IF P2=2 AND B%(P,B3%(P,B0%(2,0)),16)>6 AND W%=9 AND S2%=17 AND I<=400+I4 OR P2=2 AND B%(P,B3%(P,B0%(2,0)),16)>6 AND W%=8 AND S2%=17 AND I<=400+I4 THEN A5%(2)=1

4070 IF A5%(2)<>1 AND S2%=18 AND W%=9 AND P2<>2 AND S4%<6 AND I>800 OR A5%(2)<>1 AND S2%=18 AND W%=8 AND P2<>2 AND S4%<6 AND I>840 OR A5%(2)<>1 AND W%=7 AND P2<>2 AND S4%<6 AND I>880 THEN I8=3:GOSUB 3308

4072 IF B0%(1,0)=0 THEN 4074

4073 I=INT(RND(1)*1000)+1:A5%(1)=0:I3=(B%(P,B3%(P,B0%(1,0)),16)-5)*25:I4=(B%(D,B9%(D,W%),15)-5)*25:J5=I3-I4:IF S2%=17 AND P2<> 2 AND S4%=1 AND I<=100+J5 THEN I8=2:GOSUB 3308

4074 GOSUB 3800:GOTO 3900

4091 IF AP=1 AND U6=2 THEN RETURN

4092 LOCATE 24,1:PRINT "Hit any key to continue";

4093 I$=GetKeyPress$
4094 RETURN

4095 FOR I5=15 TO 24:LOCATE,1:LOCATE I5:GOSUB 4097:NEXT I5:LOCATE 15:LOCATE,1:RETURN

4096 FOR I5=5 TO 13:LOCATE I5,1:GOSUB 4097:NEXT I5:LOCATE 5,1:RETURN

4097 PRINT "                                            ";:RETURN

4100 '*** BUNT ROUTINE ***
	GOSUB 4095:FOR I=0 TO 3:A5%(I)=0:NEXT:SH=2:S2%=23
	IF (S4%=3 OR S4%>4) AND O%(0)<2 THEN GOSUB 4240
	GOSUB 275
	IF B7%(P,B1(P))=1 THEN BR=P%(P,P1%(P),32)
	IF B7%(P,B1(P))<>1 THEN BR=B%(P,B3%(P,B1(P)),30)
	IF I5%>0 THEN BR=BR-2
	IF BR<=1 THEN BR=1
	FOR I=1 TO 9:IF RN<=BT%(L%(1,10),BR,I) THEN 4107
	NEXT

4107 WW%=INT(RND(1)*10)
	IF WW%<=2 THEN W%=5
	IF WW%>2 AND WW%<=4 THEN W%=1
	IF WW%>4 AND WW%<=7 THEN W%=3
	IF WW%>7 THEN W%=2
	GOSUB 7024
	GOSUB 275
	IF RN<=52 THEN 4175
	ON I GOTO 4110,4120,4130,4140,4150,4160,4170,4180,4175

4110 'PU
	PRINT Q$(0)" bunts it in the air...":GOSUB 6095
	GOSUB 4400:IF F%=1 THEN 3900
	PRINT Q$(4)" makes the catch":GOSUB 6095
	GOSUB 4270
	O%(0)=O%(0)+1:O%(1)=O%(1)+1:GOSUB 3420
	IF O%(0)=3 OR SH=2 THEN 3900

4111 IF W%<>5 THEN GOSUB 3400:W%=5:GOSUB 3420:Q$(5)=Q$(4):GOSUB 7024:goto 4115
	GOSUB 3400:W%=6:GOSUB 3420:Q$(5)=Q$(4):GOSUB 7024

4115 PRINT Q$(5)" throws to "Q$(4)" at 3rd":GOSUB 6095
	A5%(3)=10:GOSUB 3800
	O%(0)=O%(0)+1:O%(1)=O%(1)+1:I5=P1%(D):I6=0:GOSUB 8020
	IF O%(0)<3 AND SH=1 THEN 5051
	IF O%(0)>=3 AND SH=1 THEN B1(P)=B1(P)-1
	GOTO 3900

4120 'DP
	IF S4%=1 OR S4%=4 OR S4%=5 OR S4%=7 THEN 4125
	PRINT Q$(0)" lays down the bunt...":GOSUB 6095
	GOSUB 4400:IF F%=1 THEN 3900
	PRINT Q$(4)" makes a nice fielding play":GOSUB 6095
	GOSUB 3400:A5%(0)=11

4121 IF SH=1 AND (S4%=3 OR S4%=6) THEN A5%(3)=11:FOR I=0 TO 2:A5%(I)=1:NEXT

4122 GOSUB 4270
	O%(0)=O%(0)+1:O%(1)=O%(1)+1:GOSUB 3800:GOSUB 3410
	GOTO 3900

4125 PRINT Q$(0)" lays down a good bunt":GOSUB 6095
	GOSUB 4400:IF F%=1 THEN 3900
	PRINT Q$(4)" fields it cleanly":GOSUB 6095
	IF SH=1 AND (S4%=5 OR S4%=7) THEN 4127
	IF S4%=7 AND O%(0)<2 THEN 4128
	PRINT "He throws to 2nd":GOSUB 6095:GOSUB 3400:A5%(1)=11
	GOSUB 4270
	O%(0)=O%(0)+1:O%(1)=O%(1)+1:GOSUB 3800:GOSUB 3410
	IF O%(0)>=3 THEN 3900
	A5%(0)=11:GOSUB 3400:A5%(2)=1
	PRINT "over to first...":GOSUB 6095:GOSUB 3800:PRINT "DOUBLE PLAY!!":GOSUB 6095
	GOSUB 3410:O%(0)=O%(0)+1:O%(1)=O%(1)+1:I5=P1%(D):I6=0:GOSUB 8020
	GOTO 3900

4127 IF W%=2 THEN PRINT "and catches "Q$(3)" heading home!":GOSUB 3420
	IF W%<>2 THEN PRINT "and throws home to nail "Q$(3)"!":GOSUB 3400:W%=2:GOSUB 3420
	A5%(3)=11:FOR I=0 TO 2:A5%(0)=1:NEXT:GOTO 4122

4128 A5%(3)=11
	IF W%=2 THEN PRINT "He steps on home for one...":GOSUB 3420:GOSUB 3400:GOSUB 6095
	IF W%<>2 THEN PRINT "He throws home for the force":GOSUB 3400:W%=2:GOSUB 3420:GOSUB 3400:GOSUB 6095
	GOSUB 3800
	PRINT "Back to first...":A5%(0)=11:GOSUB 6095:GOSUB 3800:GOSUB 3410:PRINT "DOUBLE PLAY!!":GOSUB 6095
	O%(0)=O%(0)+2:O%(1)=O%(1)+2
	GOSUB 4270
	I5=P1%(D):I6=0:GOSUB 8020
	IF O%(0)>=3 THEN 3900
	A5%(2)=1:A5%(1)=1:GOSUB 3800:GOTO 3900

4130 'LDRNRCTDN
	IF S4%=7 OR S4%=6 OR S4%=5 OR S4%=3 THEN A5%(3)=11:FOR I=0 TO 2:A5%(I)=1:NEXT
	IF S4%=2 OR S4%=4 THEN A5%(2)=11:A5%(0)=1:A5%(1)=1
	IF S4%=1 THEN A5%(1)=11:A5%(0)=1
	IF S4%=0 THEN A5%(0)=11
	IF PLB=0 THEN PRINT Q$(0)" bunts it hard!":GOSUB 6095
	IF PLB=0 THEN PRINT Q$(4)" makes an easy play":GOSUB 6095
	PLB=0:O%(0)=O%(0)+1:O%(1)=O%(1)+1
	GOSUB 4270
	GOSUB 3400:GOSUB 3800:GOSUB 3410:GOTO 3900

4140 'SHGOOD
	PRINT Q$(0)" lays down a good bunt...":GOSUB 6095
	GOSUB 4400:IF F%=1 THEN 3900
	PRINT Q$(4)" makes a nice play":GOSUB 6095
	IF S4%=0 THEN S2%=12:GOTO 3065

4141 I5=P1%(D):I6=15:GOSUB 8020
	IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=40:GOSUB 8009
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=18:GOSUB 8010
	I=INT(RND(1)*100)+1
	IF S4%=5 AND SH=2 AND I>98 THEN S2%=12:GOTO 3065
	IF I>97 THEN S2%=12:GOTO 3065

4142 A5%(0)=11
	PRINT "The only play is to first":GOSUB 6095
	I5=P1%(D):I6=0:GOSUB 8020
	O%(0)=O%(0)+1:O%(1)=O%(1)+1
	GOSUB 3400:GOSUB 3800:GOSUB 3410
	IF O%(0)>=3 THEN 3900
	FOR I=1 TO 3:A5%(I)=1:NEXT:GOSUB 3800
	GOTO 3900

4150 'C@3
	IF SH=1 THEN 4160
	GOSUB 275
	IF B7%(P,B1(P))=1 THEN RR=2
	IF B7%(P,B1(P))<>1 THEN RR=B%(P,B3%(P,B1(P)),16)
	IF I5%>0 THEN RR=RR-2
	IF RR<=2 THEN RR=2
	FOR I=0 TO 2
	IF RN<=CF%(RR,I) THEN 4155
	NEXT

4155 ON I+1 GOTO 4156,4157,4158

4156 IF S4%>0 THEN 4157
	PRINT Q$(0)" drops it right down the line":GOSUB 6095
	PRINT Q$(4)" lets it roll...":GOSUB 6095
	PRINT Q$(0)" is trying for 2nd!!":GOSUB 6095
	PRINT Q$(4)" fields and throws too late!!":GOSUB 6095
	PRINT "You won't see that play very often":GOSUB 6095
	A5%(0)=2:I5=P1%(D):I6=1:GOSUB 8020
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=1:GOSUB 8010:I6=4:GOSUB 8010:I6=0:GOSUB 8010
	IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009:I6=24:GOSUB 8009:I6=27:GOSUB 8009
	GOSUB 3800:GOSUB 324:GOTO 3900

4157 S2%=12
	PRINT Q$(0)" lays down a great bunt...":GOSUB 6095
	GOSUB 4400:IF F%=1 THEN 3900
	PRINT Q$(4)" fields... but too late!":GOSUB 6095
	GOTO 3065

4158 GOTO 4140

4160 'PLYLDBS
	IF B7%(P,B1(P))=1 THEN RR=2
	IF B7%(P,B1(P))<>1 THEN RR=B%(P,B3%(P,B1(P)),16)
	IF I5%>0 THEN RR=RR-2
	IF RR<=2 THEN RR=2
	IF S4%=0 THEN 4150
	IF S4%=1 THEN I3=1:J3$="2nd"
	IF S4%=2 OR S4%=4 THEN I3=2:J3$="3rd"
	IF S4%=3 OR S4%>4 THEN I3=3:J3$="home"
	PRINT Q$(0)" gets the bunt down...":GOSUB 6095
	GOSUB 4400:IF F%=1 THEN 3900
	PRINT Q$(4)" fields it quickly...":GOSUB 6095
	PRINT "Looks like a close play at "J3$"!!":GOSUB 6095
	PRINT "1- Throw to "J3$" 2- Throw to 1st"
	IF U6=2 OR U6=1 AND P<>U9 THEN I$="1":GOTO 4162

4161 I$=GetKeyPress$
	I5=VAL(I$):IF I5<1 OR I5>2 THEN 4161

4162 IF I5=2 THEN 4141
	GOSUB 275
	FOR I=0 TO 1
	IF RN<=LB%(RR,I) THEN 4163
	NEXT

4163 IF I=0 THEN PLB=1:GOTO 4130
	PRINT Q$(4)" throws to "J3$"... SAFE!!":GOSUB 6095
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010
	IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009
	FOR I=0 TO 3:A5%(I)=1:NEXT:GOSUB 3800:GOTO 3900

4170 'FLKSO
	GOSUB 275
	BR=P%(D,P1%(D),9)/P%(D,P1%(D),6)
	IF BR<=4 THEN PR=5
	IF BR>4 AND BR<=5.4 THEN PR=4
	IF BR>5.4 AND BR<=6.9 THEN PR=3
	IF BR>6.9 AND BR<=8.4 THEN PR=2
	IF BR>8.4 AND BR<=11.9 THEN PR=1
	IF BR>11.9 THEN PR=0
	FOR I=0 TO 2
	IF RN<=SO%(PR,I) THEN 4171
	NEXT

4171 ON I+1 GOTO 4172,4175,4140

4172 GOTO 3601

4175 U%=U%+1:IF U%=3 THEN 3601
	LOCATE 2,58:PRINT U%:GOSUB 4095
	GOSUB 275
	PRINT Q$(0)" ";
	IF RN<=13 or sh=1 and rn<=42 THEN PRINT "misses the pitch"
	IF RN>13 AND RN<=42 and sh<>1 THEN PRINT "takes a called strike"
	IF RN>42 THEN PRINT "bunts it foul"
	GOSUB 6095
	if sh=1 and rn<=42 then w%=2:gosub 7024:goto 4111
	PRINT "He has"U%"strikes":GOSUB 6095
	IF S4%=4 OR S4%=2 THEN I5%=0
	GOTO 5080

4180 'TFPLY
	PRINT Q$(0)" lays down the bunt...":GOSUB 6095
	PRINT Q$(4)" has a tough play":GOSUB 6095
	GOSUB 4400:IF F%=1 THEN 3900
	IF W%=1 THEN RR=3
	IF W%<>1 THEN RR=B%(D,B9%(D,W%),17)
	IF I5%>0 AND S4%<>0 THEN RR=RR+1+L%(1,10)
	IF I5%>0 AND S4%=0 THEN RR=RR+L%(1,10)
	IF I5%=0 AND S4%=0 THEN RR=RR-1
	IF RR>11 THEN RR=11
	IF RR<0 THEN RR=0
	GOSUB 275
	FOR I=0 TO 2
	IF RN<=TP%(RR,I) THEN 4182
	NEXT

4182 ON I+1 GOTO 4183,4184,4185

4183 S2%=12:GOTO 3065

4184 PLB=1:GOTO 4130

4185 GOTO 4141

4220 IF F%=1 AND O%(0)=2 THEN PQ=8:RETURN

4221 IF F%=1 AND W%<=3 AND S2%<>23 OR F%=1 AND W%=5 AND S2%<>23 THEN PQ=8:RETURN

4222 IF F%=1 AND W%=4 AND I5%=2 THEN PQ=8:RETURN

4223 IF F%=1 AND W%=6 AND I5%=2 THEN PQ=8:RETURN

4224 IF S2%>=4 AND S2%<=6 THEN PQ=8:RETURN

4225 RETURN

4230 IF O%(0)=3 OR TB=9 THEN RETURN

4231 IF S4%<5 THEN RETURN

4232 IF S4%=6 THEN RETURN

4235 RETURN

4240 '*** SQUEEZE ***
	IF U6=2 OR U6=1 AND P=U9 THEN SH=2:GOTO 4242
	LOCATE 15,1:PRINT "1 - SQUEEZE PLAY":PRINT "2 - BUNT FOR SACRIFICE/HIT"

4241 I$=GetKeyPress$
	SH=VAL(I$):IF SH<1 OR SH>2 THEN 4241

4242 RETURN

4270 IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010

4272 IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009

4274 I5=P1%(D):I6=0:GOSUB 8020

4276 RETURN

4277 IF INT(RND(1)*100)<80 THEN RETURN

4278 PRINT "AND HERE COMES ";M$(1-P):gosub 6095:
	PRINT"HE'S REALLY GOING AT IT WITH ";U$(2):gosub 6095
	PRINT "AND WITH ";U$(3):GOSUB 6095:GOSUB 6095
	PRINT "NOW HE FINALLY STORMS BACK TO THE DUGOUT":GOSUB 6095:GOSUB 6095:RETURN

4280 IF S4%=5 AND SH=2 THEN A5%(3)=0:A5%(1)=1:RETURN

4282 RETURN

4290 IF I=4 THEN A5%(3)=1

4291 IF S4%<>4 THEN RETURN

4292 GOSUB 4095:LOCATE 15,1:PRINT "WHICH RUNNER DO YOU WANT TO GO AFTER"
	PRINT "1 - "B$(P,B3%(P,B0%(2,0)))
	IF B7%(P,B0%(1,0))<>1 THEN PRINT "2 - "B$(P,B3%(P,B0%(1,0)))

4293 IF B7%(P,B0%(1,0))=1 THEN PRINT "2 - "P$(P,P1%(P))

4294 IF U6=2 OR U6=1 AND P<>U9 THEN D5=INT(RND(1)*2)+1:I$=STR$(D5):GOTO 4296

4295 I$=GetKeyPress$
4296 D5=VAL(I$):GOSUB 4095:LOCATE 15,1:IF D5=2 THEN I=2:D5=3:I2=B3%(P,B0%(1,0)):IF B7%(P,B0%(1,0))=1 THEN Q$(I-1)=P$(P,P1%(P))

4297 'IF D5=3 THEN GOSUB 4809

4298 D5=0:RETURN

4300 GOTO 4350

4350 GOSUB 275
	IF I=5 THEN 4361
	IF RN<=96 THEN I=1
	IF RN=97 THEN I=2
	IF RN=98 THEN I=3
	IF RN=99 THEN I=4
	if rn=100 then i=6

4351 ON I GOTO 4352,4354,4356,4358,4361,4362

4352 if s2%=17 then 4353
	y%=int(rnd(1)*15)+551:gosub 3990:GOSUB 9800
	PRINT Q$(4)" "Y$
	gosub 6095

4353 if s2%=17 and l%(1,13)=37 then print "he climbs the terrace":gosub 6095
	y%=int(rnd(1)*10)+571:gosub 3990:GOSUB 9800
	PRINT Y$
	goto 4359

4354 PRINT Q$(4)" makes a sliding catch":GOSUB 6095:PRINT"What a play!!!":GOTO 4359

4356 PRINT Q$(4)" makes a leaping grab":GOSUB 6095:PRINT"What a catch!!!":GOTO 4359

4358 PRINT Q$(4)" makes a diving catch!!!"

4359 GOSUB 6095

4361 RETURN

4362 print q$(4)" makes an incredible catch!!":goto 4359

4400 F%=0:IF W%=1 THEN I3=P%(D,P1%(D),17)+30
	IF W%=1 AND SB%=1 THEN I3=975

4401 IF W%<>1 THEN GOSUB 4450

4402 IF I3>=999 THEN I6=1:GOTO 4408

4404 IF S2%>=12 AND S2%<=15 THEN I6=1009-I3

4405 IF S2%>=7 AND S2%<=9 THEN I6=1006-I3

4406 IF S2%=16 OR S2%>=22 THEN I6=1000-I3

4407 IF S2%>=17 AND S2%<=21 THEN I6=(1000 - I3)*.05

4408 IF W%=7 THEN I6=I6-4
	IF W%=8 THEN I6=I6-10
	IF W%=9 THEN I6=I6-15
	IF W%=5 THEN I6=I6-15
	H6%=INT(RND(1)*1000)+1:IF H6%>I6 THEN 4421

4409 F%=1:I=INT(RND(1)*2)+1:I1=2:GOSUB 4430
	IF S2%<>12 THEN 4410
	IF S2%=12 THEN GOSUB 8011
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010:I6=1:GOSUB 8010
	I5=P1%(D):I6=1:GOSUB 8020
	IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009:I6=24:GOSUB 8009

4410 IF S2%>=17 AND S2%<=21 THEN I1=3
	if s2%>=17 and s2%<=20 then gosub 4440
	IF S2%=8 OR S2%=9 OR S2%>=22 OR S2%=16 THEN I1=1

4411 IF S2%>=12 AND S2%<=14 AND I=1 OR S2%=15 THEN I1=1
	GOSUB 275
	IF I1=1 AND W%=1 AND S2%=16 THEN PRINT "*** WILD PICK OFF THROW BY";:GOTO 4415

4412 IF I1=1 AND RN<=50 THEN PRINT "*** THROWING ERROR ON";:GOTO 4415
	if i1=1 and rn>50 and rn<=55 then print "*** BALL KICKED BY";:GOTO 4415
	IF I1=1 THEN PRINT "*** "Q$(4)" THROWS IT AWAY ***":GOTO 4403

4413 IF I1=2 AND RN<=30 THEN PRINT "*** FIELDING ERROR ON";:GOTO 4415
	if i1=2 and rn>30 and rn<=50 then print "*** OFF THE GLOVE OF";:GOTO 4415
	IF I1=2 AND RN>50 AND RN<=75 THEN PRINT "*** BALL IS BOOTED BY";:GOTO 4415
	IF I1=2 AND RN>75 AND RN<=95 THEN PRINT "*** BALL IS BOBBLED BY";:GOTO 4415
	IF I1=2 AND RN>95 THEN PRINT "*** BALL IS THROUGH THE LEGS OF";:GOTO 4415

4414 IF I1=3 AND RN<=33 THEN PRINT "*** BALL DROPPED BY";:GOTO 4415
	IF I1=3 AND RN>33 AND RN<=66 THEN PRINT "*** OFF THE GLOVE OF";:GOTO 4415
	IF I1=3 THEN PRINT "*** BALL IS MISPLAYED BY";

4415 PRINT " "Q$(4)" ***"

4403 IF S2%=16 OR S2%=22 THEN I8=INT(RND(1)*10)+1:IF I8<5 THEN A5%(1)=2:A5%(2)=2:A5%(3)=1:A5%(0)=2:GOSUB 6095:GOTO 4418

4416 I=INT(RND(1)*100)+1:A5%(1)=1:A5%(2)=1:A5%(3)=1:A5%(0)=1:GOSUB 6095:IF S2%>17 AND S2%<=21 THEN 4418

4417 IF I<=17 AND S2%<>8 or s2%=12 THEN A5%(1)=2:A5%(2)=2:A5%(0)=2

4418 GOSUB 4424:IF I1>0 AND S2%<>7 AND S2%<>8 AND S2%<>9 AND S2%<>16 AND S2%<>22 THEN O%(1)=O%(1)+1

4419 IF S2%=16 OR S2%=7 OR S2%=8 OR S2%=9 OR S2%=22 THEN A5%(0)=0:IF I8>4 AND W%<>2 THEN A5%(1)=1:A5%(2)=1:A5%(3)=1

4420 S6%(D,2)=S6%(D,2)+1:GOSUB 3800

4421 RETURN

4424 IF S2%>=17 AND S2%<=20 AND O%(0)=2 OR S2%>=23 OR S2%=16 AND W%=2 THEN A5%(2)=2:A5%(1)=2:A5%(0)=2:A5%(3)=1

4425 IF S2%>=17 AND S2%<=19 AND I<=33 AND O%(0)<2 AND B0%(1,0)=0 THEN A5%(0)=2

4426 IF S2%=16 OR S2%=22 THEN A5%(0)=0

4427 RETURN

4430 IF W%=1 THEN I5=P1%(D):I6=34:GOSUB 8020

4431 IF W%<>1 THEN I5=B9%(D,W%):I6=11:GOSUB 8008

4432 RETURN

4440 '*** outfielders in trouble ***
	gosub 275
	if rn<=20 then print q$(4)" can't find it"
	if rn>20 and rn<=40 then print q$(4)" misjudges it"
	if rn>40 and rn<=60 then print q$(4)" turns the wrong way"
	if rn>60 and rn<=80 then print q$(4)" loses it"
	if rn>80 then print q$(4)" doesn't see it"
	gosub 6095:return

4450 IF B%(D,B9%(D,W%),19)>=999 THEN I3=999:GOTO 4454

4451 IF W%>=7 THEN I3=B%(D,B9%(D,W%),19):GOTO 4454

4452 IF W%=3 OR W%=2 THEN I3=(B%(D,B9%(D,W%),19))*.96:GOTO 4454

4453 'IF W%<>1 AND W%<>4 AND W%<>6 THEN I3=(B%(D,B9%(D,W%),19))*.985

4454 I4=B%(D,B9%(D,W%),22):I5=B%(D,B9%(D,W%),23):I6=B%(D,B9%(D,W%),24):I7=B%(D,B9%(D,W%),25)

4455 IF W%=I4 OR W%=I5 OR W%=I6 OR W%=I7 THEN 4462

4456 IF W%=4 OR W%>6 THEN I3=982

4457 IF W%=2 THEN I3=987

4458 IF W%=3 THEN I3=980

4459 IF W%=6 OR W%=5 THEN I3=964

4460 IF W%>=7 THEN I3=I3-10:GOTO 4462

4461 I3=I3-20:IF W%=2 OR W%>=4 AND W%<=6 THEN I3=I3-80:GOTO 4464

4462 IF W%=4 OR W%=6 THEN I3=(B%(D,B9%(D,W%),19))*.995

4463 IF W%=5 THEN I3=(B%(D,B9%(D,W%),19))*.985

4464 RETURN

4500 '*** IFR CHRT ***
	GOSUB 4095:GOSUB 275
	IF RN<=10 THEN W%=1
	IF RN>10 AND RN<=25 THEN W%=3
	IF RN>25 AND RN<=40 THEN W%=5
	IF RN>40 AND RN<=70 THEN W%=6
	IF RN>70 THEN W%=4
	GOSUB 7024
	GOSUB 275:IF W%=1 THEN FR=3
	IF W%<>3 THEN FR=B%(D,B9%(D,W%),17)
	IF W%=1 OR (W%=4 OR W%=6) AND RN>42 AND RN<=75 THEN I=1:GOTO 4505
	IF (W%=3 OR W%=5) AND RN<=50 OR (W%=4 OR W%=6) AND RN>17 AND RN<=42 THEN I=2:GOTO 4505
	IF (W%=3 OR W%=5) AND RN>67 AND RN<=83 OR (W%=4 OR W%=6) AND RN<=17 THEN I=3:GOTO 4505
	IF (W%=3 OR W%=5) AND RN>50 AND RN<=67 THEN I=4:GOTO 4505
	I=5

4505 ON I GOTO 4510,4530,4550,4570,4590

4510 PRINT Q$(0)" hits a hard grounder up the middle"

4511 'PRINT "to "Q$(4)" @ "C$(W%)
	GOSUB 6095
	GOSUB 275
	FOR I=1 TO 4
	IF RN<=UM%(L%(1,10),FR,I) THEN 4512
	NEXT
	I=INT(RND(1)*4)+1

4512 ON I GOTO 4513,4514,4516,4518

4513 '*** CLOSE ***
	RN=INT(RND(1)*9)+1
	PRINT "tough chance for "Q$(4):GOSUB 6095
	IF B7%(P,B1(P))=1 THEN RR=P%(P,P1%(P),31) ELSE RR=B%(P,B3%(P,B1(P)),16)
	IF RN>RR THEN S2%=13:rp%=1:GOTO 3612
	S2%=12:GOTO 3065

4514 '*** DIVE ***
	PRINT Q$(4)" makes a diving stop!":GOSUB 6095:print "He gets to his feet":gosub 6095:rp%=1:GOTO 3612

4516 '*** KNOCK ***
	PRINT q$(4) " knocks it down...":GOSUB 6095:PRINT "but has no play!":GOSUB 6095:S2%=12:GOTO 3065

4518 '*** THRU ***
	PRINT "It's through for a hit!":GOSUB 6095

4519 A5%(0)=1:A5%(1)=2:A5%(2)=2:A5%(3)=1:GOSUB 3800
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010:I6=1:GOSUB 8010
	IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009:I6=24:GOSUB 8009
	I5=P1%(D):I6=1:GOSUB 8020:GOSUB 324
	GOTO 3900

4530 PRINT Q$(0)" hits a hot smash in the hole":GOTO 4511

4550 PRINT Q$(0)" hits a slow roller":GOTO 4511

4570 PRINT Q$(0)" rips one down the line":GOTO 4511

4590 PRINT Q$(0)" lines a bullet":PRINT "to "Q$(4)" @ "C$(W%):GOSUB 6095
	GOSUB 275:IF RN>LD%(FR) THEN 4592
	PRINT "He leaps and catches it for the out!!":GOSUB 6095
	IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010
	GOTO 4005

4592 PRINT "It's over his glove for a base hit!":GOSUB 6095
	GOTO 4519

4600 '*** OFR ***
	GOSUB 4095:GOSUB 275
	IF RN<=50 THEN W%=8
	IF RN>50 AND RN<=75 THEN W%=7
	IF RN>75 THEN W%=9
	GOSUB 7043
	FR=B%(D,B9%(D,W%),17):GOSUB 275
	IF W%=8 THEN 4605
	IF RN<=14 THEN I=1
	IF RN>14 AND RN<=28 THEN I=2
	IF RN>28 AND RN<=56 THEN I=3
	IF RN>56 THEN I=4
	GOTO 4610

4605 IF RN<=43 THEN I=2
	IF RN>43 AND RN<=72 THEN I=3
	IF RN>72 THEN I=4

4610 ON I GOTO 4620,4640,4660,4680

4620 DL%=1:PRINT Q$(0)" drives one down the "C$(W%)" line":GOSUB 6095

4621 GOSUB 275
	FOR I=1 TO 3
	IF RN<=DL%(L%(1,10),FR,I) THEN 4622
	NEXT
	I=INT(RND(1)*3)+1

4622 ON I GOTO 4623,4625,4628

4623 S2%=18:I=4:IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010
	GOTO 4025

4625 PRINT "It drops in for a hit...":GOSUB 6095:PRINT Q$(4)" quickly cuts it off":GOSUB 6095:GOTO 4519

4628 IF DL%=1 THEN PRINT "It stays fair and rolls into the corner!":GOSUB 6095
	IF IG%=1 THEN PRINT "It falls in and rolls to the wall!!":GOSUB 6095
	DL%=0:IG%=0
	A5%(0)=2:A5%(1)=3:A5%(2)=2:A5%(3)=1:GOSUB 3800
	PRINT "Double!!":GOSUB 6095
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010:I6=1:GOSUB 8010:I6=4:GOSUB 8010
	IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009:I6=24:GOSUB 8009:I6=27:GOSUB 8009
	I5=P1%(D):I6=1:GOSUB 8020:GOSUB 324
	GOTO 3900

4640 IG%=1:PRINT Q$(0)" lines one to the gap in ";
	GOSUB 275
	IF W%=7 OR W%=8 AND RN>50 THEN PRINT "lcf"
	IF W%=9 OR W%=8 AND RN<=50 THEN PRINT "rcf"
	GOSUB 6095:GOTO 4621

4660 PRINT Q$(0)" drives one to deep "C$(W%):GOSUB 6095
	PRINT Q$(4)" races back...":GOSUB 6095
	GOSUB 275:IF RN>OH%(FR) THEN 4662
	PRINT "He makes a great running catch!!":GOSUB 6095
	S2%=17:I=5:IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010
	GOTO 4025

4662 PRINT "It's over his head and off the wall!":GOSUB 6095
	A5%(0)=2:IF W%=8 THEN A5%(0)=3
	A5%(1)=3:A5%(2)=2:A5%(3)=1:GOSUB 3800
	if W%<>8 THEN PRINT "Double!!":GOSUB 6095
	if W%=8 THEN PRINT "Triple!!":GOSUB 6095
	IF W%<>8 AND B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010:I6=1:GOSUB 8010:I6=4:GOSUB 8010
	IF W%<>8 AND B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009:I6=24:GOSUB 8009:I6=27:GOSUB 8009
	IF W%=8 AND B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010:I6=1:GOSUB 8010:I6=5:GOSUB 8010
	IF W%=8 AND B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009:I6=24:GOSUB 8009:I6=28:GOSUB 8009
	I5=P1%(D):I6=1:GOSUB 8020:GOSUB 324
	GOTO 3900

4680 PRINT Q$(0)" hits a sinking liner to "C$(W%):GOSUB 6095
	GOSUB 275
	FOR I=1 TO 3
	IF RN<=LL%(FR,I) THEN 4682
	NEXT
	I=INT(RND(1)*3)+1

4682 ON I GOTO 4683,4685,4688

4683 S2%=19:I=5:PRINT Q$(4)" makes a great shoestring catch!!":GOSUB 6095
	IF B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009
	IF B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010
	GOTO 4025

4685 PRINT Q$(4)" dives for the ball and misses!!":GOSUB 6095
	PRINT "It rolls to the wall!!":GOSUB 6095
	A5%(0)=2:IF L%(1,10)=1 THEN A5%(0)=3
	A5%(1)=3:A5%(2)=2:A5%(3)=1:GOSUB 3800
	if L%(1,10)=0 THEN PRINT "Double!!":GOSUB 6095
	if L%(1,10)=1 THEN PRINT "Triple!!":GOSUB 6095
	IF L%(1,10)=0 AND B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010:I6=1:GOSUB 8010:I6=4:GOSUB 8010
	IF L%(1,10)=0 AND B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009:I6=24:GOSUB 8009:I6=27:GOSUB 8009
	IF L%(1,10)=1 AND B7%(P,B1(P))<>1 THEN I5=B3%(P,B1(P)):I6=0:GOSUB 8010:I6=1:GOSUB 8010:I6=5:GOSUB 8010
	IF L%(1,10)=1 AND B7%(P,B1(P))=1 THEN I5=P1%(P):I6=23:GOSUB 8009:I6=24:GOSUB 8009:I6=28:GOSUB 8009
	I5=P1%(D):I6=1:GOSUB 8020:GOSUB 324
	GOTO 3900

4688 GOTO 4625

4795 IF P2<>2 THEN GOSUB 4095:SB%=1:GOSUB 4400:SB%=0:IF F%=1 THEN B1(P)=B1(P)-1:GOSUB 6020:GOTO 3900
	GOTO 4804

4800 SH%=0:S2%=16:W%=1:Q$(4)=P$(D,P1%(D)):IF S4%=0 THEN 5051

4801 IF S4%=1 OR S4%=5 THEN I=2:GOTO 4795

4802 IF S4%=2 OR S4%=4 THEN I=3:GOTO 4795

4803 IF S4%=3 OR S4%>=6 THEN I=4

4804 I2=B3%(P,B0%(I-1,0)):FOR I3=1 TO 2:A5%(I3)=1:A5%(3)=0:NEXT I3

4805 GOSUB 4850

4806 IF B7%(P,B0%(I-1,0))=1 THEN I1=0

4808 IF I=4 THEN I1=I1*.05:' *** REDUCTION FOR STEALING HOME ***

4809 IF B%(P,I2,14)>0 THEN Q0=INT((B%(P,I2,13)/(B%(P,I2,13)+B%(P,I2,14)))*1000):GOTO 4813

4810 IF B%(P,I2,13)>31 THEN Q0=700+((B%(P,I2,13)-30)*2):GOTO 4813

4811 IF B%(P,I2,13)>10 THEN Q0=600+((B%(P,I2,13)-10)*10):GOTO 4813

4812 IF B%(P,I2,13)>0 THEN Q0=400+((B%(P,I2,13)*20))
	Q0=100

4813 IF B7%(P,B0%(I-1,0))=1 THEN Q0=100

4814 GOSUB 4870:REM*** SUCCESS OF 3B STEAL ***

4815 IF P%(D,P1%(D),0)=1 AND I=2 THEN I1=I1+25:Q0=Q0+10

4816 IF P%(D,P1%(D),0)=-1 AND I=2 THEN I1=I1-50:Q0=Q0-30

4817 IF P2=2 THEN 4838

4818 B0%(I-1,2)=B0%(I-1,2)+1:I4=INT(RND(1)*100)+1:IF I4>B0%(I-1,2) THEN 4825

4819 S2%=22:W%=1:Q$(4)=P$(D,P1%(D))

4820 GOSUB 4095:PRINT Q$(I-1);" caught off base in a rundown!":GOSUB 6095:A5%(1)=0:A5%(2)=0:A5%(3)=0:A5%(I-1)=11:O%(0)=O%(0)+1:O%(1)=O%(1)+1:GOSUB 4822:GOSUB 3800:GOSUB 3410:I5=P1%(D):I6=0:GOSUB 8020:IF O%(0)>=3 THEN B1(P)=B1(P)-1:GOTO 3900

4821 I5%=0:GOTO 5051

4822 IF B7%(P,B0%(I-1,0))=1 THEN I5=P1%(P):I6=33:GOSUB 8009

4823 IF B7%(P,B0%(I-1,0))<>1 THEN I5=B3%(P,B0%(I-1,0)):I6=10:GOSUB 8010

4824 RETURN

4825 B0%(I-1,2)=B0%(I-1,2)+17:I9=INT(RND(1)*1000)+1:IF I9>I1*1.388 AND D5<>3 THEN GOSUB 4095:GOSUB 4840:GOTO 5060

4826 GOSUB 4095
	bv=int(rnd(1)*100)+1
	if bv<=25 then PRINT Q$(I-1);" has a good lead":gosub 6095:print "He's going!"
	if bv>25 and bv<=50 then PRINT Q$(I-1);" breaks for "h$(i)"!"
	if bv>50 and bv<=75 then PRINT Q$(I-1);" is going!"
	if bv>75 then print Q$(I-1);" with a good jump":gosub 6095:print "He's going!"
	GOSUB 6095:GOSUB 4880:GOTO 4884

4827 GOSUB 4290:I9=INT(RND(1)*1000)+1:Q0=Q0-((B%(D,B9%(D,2),15)-5)*25)

4828 rn=int(rnd(1)*100)+1
	if i=4 then 4837
	if rn<=20 then PRINT B$(D,B9%(D,2))" rifles it to "h$(i)
	if rn>20 and rn<=40 then PRINT B$(D,B9%(D,2))" fires it to "h$(i)
	if rn>40 and rn<=60 then PRINT B$(D,B9%(D,2))" unloads it"
	if rn>60 and rn<=80 then PRINT B$(D,B9%(D,2))" throws quickly"
	if rn>80 then PRINT B$(D,B9%(D,2)) " throws"
	gosub 6095

4837 IF B0%(I-1,2)=18 AND I9>Q0 OR B0%(I-1,2)>18 AND I9>Q0-200 THEN 4830

4829 rn=INT(RND(1)*100)+1
	if rn<=20 THEN PRINT "He beats the throw!"
	if rn>20 and rn<=40 then print "He's in there!"
	if rn>40 and rn<=50 then print "Throw is off-line..."
	if rn>50 and rn<=60 then print "Throw is high..."
	if rn>60 and rn<=80 then PRINT "He slides under the tag!"
	if rn>80 and i<>4 then print u$(i+1)" with the call"
	if rn>80 and i=4 then print u$(1)" with the call"
	gosub 6095:print "safe @ "h$(i)"!"
	GOSUB 6095
	B0%(I-1,2)=0
	I5%=0
	GOSUB 9200:GOSUB 3800
	I5=B9%(D,2):I6=20:GOSUB 8008
	IF SH%<>1 THEN 4892
	SH%=0:GOTO 5051

4830 rn=int(rnd(1)*100)+1
	if rn<=20 then PRINT B$(D,B9%(D,2));" nails ";Q$(I-1)
	if rn>20 and rn<=40 then PRINT q$(i-1) " is pegged at "h$(i)
	if rn>40 and rn<=60 then print "Great throw by "B$(D,B9%(D,2))"!!"
	if rn>60 and rn<=80 then print q$(i-1) " slides into the tag"
	if rn>80 and i<>4 then print u$(i+1)" with the call"
	if rn>80 and i=4 then print u$(1)" with the call"
	gosub 6095
	A5%(I-1)=11:I5%=0
	I5=B3%(P,B0%(I-1,0)):I6=10:GOSUB 8010
	I5=P1%(D):I6=0:GOSUB 8020
	O%(0)=O%(0)+1:O%(1)=O%(1)+1:W%=2:GOSUB 3400:GOSUB 3800:GOSUB 3410:GOSUB 6095
	I5=B9%(D,2):I6=21:GOSUB 8008
	IF O%(0)>=3 THEN B1(P)=B1(P)-1:GOSUB 4845:GOTO 3900

4831 IF O%(0)>=3 OR P2=2 AND U%=3 THEN 3900
	IF SH%=1 THEN SH%=0:GOTO 5051

4832 IF AA<20 OR S4%<>5 OR B7%(P,B0%(3,0))=1 THEN 5051

4833 GOSUB 4095:IF U6=2 OR U6=1 AND P=U9 THEN I$="N":GOTO 4835
	PRINT "TRY TO STEAL HOME (YN)"

4834 I$=GetKeyPress$
4835 IF UCASE$(I$)<>"Y" THEN 5051

4836 SH%=1:GOTO 4890

4838 I1=100:Q0=Q0-200:W%=2:Q$(4)=B$(D,B9%(D,2)):GOSUB 4400:IF F%=1 THEN I5=B3%(P,B0%(I-1,0)):I6=9:GOSUB 8010:B1(P)=B1(P)-1:GOSUB 4845:GOTO 3900

4839 B0%(I-1,2)=18:GOTO 4827

4840 IF U6=0 OR U6=1 AND U9=D THEN PRINT Q$(I-1);" can't get a jump!":GOSUB 6095

4841 RETURN

4845 IF SO=7 THEN B1(P)=B1(P)+1

4846 RETURN

4850 AA=B%(P,I2,13):BB=B%(P,I2,13)+B%(P,I2,14):CC=B%(P,I2,7):' *** CHANCE TO STEAL 2B & 3B ***

4851 IF AA<=43 THEN DD=.16:GOTO 4853

4852 DD=.3

4853 TT=CC+BB-(BB*DD):' *** TOTAL CHANCES TO STEAL 3RD ***

4854 RR=(TT*.19)*1.4: ' *** "      "      "   "     "  1 OUT ***

4855 SS=(TT*.56)*1.4: ' ***                            0 OUT ***

4856 GG=RR+SS:        ' ***                            (0,1 OUT) ***

4857 IF I=3 THEN 4862

4858 FF=B%(P,I2,6)+B%(P,I2,11)-B%(P,I2,9):IF AA>=12 THEN I1=INT(((BB-BB*DD)/FF)*1500)

4859 IF AA<12 THEN I1=INT((BB/FF)*1500)

4860 IF I1<0 THEN I1=0

4861 GOTO 4868

4862 IF BB<10 AND O%(0)=0 THEN I1=0

4863 IF BB<10 AND O%(0)=1 THEN I1=10

4864 IF O%(0)=2 THEN I1=0

4865 IF BB>=10 AND O%(0)=1 THEN I1=BB*DD/TT*1000

4866 IF BB>=10 AND O%(0)=0 THEN I1=BB*DD/TT*1000

4867 IF I1<0 THEN I1=0

4868 RETURN

4870 IF I=3 AND AA>=24 THEN Q0=830:' *** SUCCESS OF 3B STEAL ***

4871 IF I=3 AND AA<24 THEN Q0=700

4872 IF I=4 THEN Q0=Q0*.5

4873 ' *** H0%=0, runner not held ***
	IF H0%=0 THEN Q0=Q0+50:GOTO 4874
	IF H0%=1 THEN Q0=Q0-((P%(D,P1%(D),18)-5)*25)-50

4874 RETURN

4880 IF S4%=5 OR I<>2 THEN 4883:REM*** PITCH OUT ***

4881 PJ=INT(RND(1)*100)+1:IF PJ>=8 THEN 4883

4882 PRINT "...PITCH OUT!!!":gosub 6095:Q0=Q0-200

4883 RETURN

4884 IF S4%<>5 THEN 4888:REM*** DOUBLE STEAL ***1st&3rd
	IF SH%=1 THEN I9=INT(RND(1)*1000)+1:GOTO 4828

4885 IF U6=2 OR U6=1 AND P<>U9 THEN I$="Y":GOTO 4887
	PRINT"Throw to 2B (YN) "

4886 I$=GetKeyPress$
4887 IF UCASE$(I$)<>"Y" THEN PJ=7:GOTO 4829

4888 W%=2:Q$(4)=B$(D,B9%(D,2)):GOSUB 4400:IF F%=1 THEN I5=B3%(P,B0%(I-1,0)):I6=9:GOSUB 8010:GOSUB 9200:B1(P)=B1(P)-1:GOTO 3900

4889 GOTO 4827

4890 I=4:A5%(1)=0:A5%(2)=0:A5%(3)=1:I2=B3%(P,B0%(I-1,0)):I1=100:Q0=700:GOTO 4826:' *** STEAL HOME ***

4892 IF S4%<>5 OR S4%=5 AND PJ=7 THEN 4831:REM*** PICK OFF IF NOT STEALING HOME ***

4893 PJ=0:I=4:IS=INT(RND(1)*20)+1:IF IS=6 THEN 4820

4894 GOTO 4831

4900 P9=D:B1(P)=B1(P)+1:U%=0:IF B1(P)=10 THEN B1(P)=1

4901 CLS:COLOR 15:PRINT "DOES ";B$(D,B3%(D,B9%(D,0)));" STAY IN THE GAME (YN)"

4902 I$=GetKeyPress$
4903 IF UCASE$(I$)="N" THEN I5=77:GOTO 5504
	I5=66
	LOCATE 5,1
	FOR I=1 TO 9:COLOR 3,0
	IF BP(P9)=I THEN COLOR 9
	LOCATE I+4,5:PRINT USING "# ";I;
	IF B7%(P9,I)=1 AND I5=66 THEN PRINT B$(P9,B3%(P9,I));
	IF B7%(P9,I)=1 AND I5<>66 THEN PRINT P$(P9,B3%(P9,I));
	IF B7%(P9,I)<> 1 THEN PRINT B$(P9,B3%(P9,I));
	PRINT " ";C$(B7%(P9,I));
	COLOR 3
	NEXT I
	I5=0
	COLOR 15:LOCATE 3,1:PRINT "WHAT LINEUP SLOT WILL THE PITCHER BAT IN (1-9)"

4905 I$=GetKeyPress$
	I2=VAL(I$)
	IF I2<1 OR I2>9 THEN 4905
	IF B7%(D,I2)=10 THEN B7%(D,I2)=1

4911 I5%=0

4907 B7%(D,B9%(D,0))=B7%(D,I2):B9%(D,B7%(D,B9%(D,0)))=B3%(D,B9%(D,0)):B7%(D,I2)=1
	I7%=D:I8%=B9%(D,0):I9%=B3%(D,B9%(D,0))
	GOSUB 7100

4908 I5=66:GOTO 5504

4950 IF J1=2 THEN J2=1:J3=0:J4=910+INT(RND(1)*10)+1
	IF J1=3 THEN J2=0:J3=3:J4=960+INT(RND(1)*10)+1
	IF J1=4 THEN J2=0:J3=3:J4=912+INT(RND(1)*10)+1
	IF J1=5 OR J1=6 THEN J2=0:J3=3:J4=902+INT(RND(1)*10)+1
	IF J1>=7 THEN J2=2:J3=2:J4=972+INT(RND(1)*10)+1
	RETURN

5000 F%=0:PQ=0:FOR I=0 TO 1:O%(I)=0:NEXT I:L0%(P)=L0%(P)+MO:MO=0:H0%=0:rp%=0

5001 FOR I=1 TO 3:FOR I1=0 TO 1:B0%(I,I1)=0:B1%(I,I1)=0:NEXT I1:B0%(I,2)=0:NEXT I

5002 S4%=0:IF P=1 THEN I%=I%+1:CZ%(1)=0:CZ%(0)=0

5003 P=1-P:D=1-P:IF I%=5 AND P=1 THEN I=FRE(Y$)
	IF I%=7 AND L%(1,13)=8 AND P=1 AND VAL(LEFT$(A$(1),2))>75 THEN GOSUB 4095:PRINT "ALL RIGHT...":GOSUB 6095:PRINT "LEMME HEAR YA...":GOSUB 6095:PRINT "A ONE, ";:GOSUB 6095:PRINT "A TWO...":GOSUB 6095
	IF I%>8 AND P=0 AND S8%(1,1)=3 THEN S8%(1,1)=2
	IF I%>8 AND P=0 AND S8%(0,1)=3 THEN S8%(0,1)=2

5005 IF I%>9 AND D=1 AND S6%(1,0)<>S6%(0,0) OR I%=28 OR I%>=9 AND P=1 AND S6%(P,0)-S6%(D,0)>0 THEN 8500

5006 IF RD=1 AND I%>=RE AND RE>=6 AND (S6%(1,0)>S6%(0,0) AND P=1 OR S6%(0,0)>S6%(1,0) AND P=0) THEN CLS:PRINT"THIS GAME HAS BEEN CALLED ON ACCOUNT OF RAIN!!!":PRINT" ALL RECORDS ARE OFFICIAL!!":GOSUB 4091:GOTO 8500

5007 IF RD=1 AND P=0 AND I%=4 THEN COLOR 0,7:CLS:PRINT"THE RAIN IS FALLING A LITTLE MORE STEADILY":PRINT"THIS GAME MAY NOT GO 9!":GOSUB 4091:CLS:GOSUB 300

5010 GOSUB 300:IF B9%(D,1)=99 AND U6=0 OR B9%(D,1)=99 AND U6=1 AND U9=1-D THEN 4900

5050 SH=0:F%=0:PQ=0:SO=0:B1(P)=B1(P)+1:I5%=0:U%=0:IF B1(P)=10 THEN B1(P)=1

5051 GOSUB 6020:GOSUB 8000:GOSUB 330:P2=0:D2=0:IF I%>=9 AND P=1 AND S6%(P,0)-S6%(D,0)>0 THEN 8500

5052 GOSUB 323:Z2%=B0%(2,0):F%=0:PQ=0:GOTO 5080

5053 GOTO 5090

5054 D2=VAL(I$):IF D2<1 OR D2>9 THEN 5052

5055 IF D2=4 THEN I5%=1:COLOR 15,2:LOCATE 23,46:PRINT "CORNERS IN":GOTO 5052

5056 IF D2=3 THEN I5%=2:COLOR 15,2:LOCATE 23,46:PRINT "INFIELD IN":GOTO 5052

5057 IF D2=5 THEN I5%=0:COLOR 15,2:LOCATE 23,46:PRINT "NORMAL    ":GOTO 5052

5058 IF D2=2 THEN GOSUB 4095:GOTO 3240

5059 IF D2=6 THEN P9=D:W5=-1:W6=-1:GOTO 5500

5060 GOTO 5085

5061 I$=GetKeyPress$
5062 IF UCASE$(I$)="Q" THEN CLS:END
	IF UCASE$(I$)="D" THEN GOSUB 700:GOTO 5580
	IF UCASE$(I$)="S" THEN GOSUB 500:GOTO 5580
	IF UCASE$(I$)="L" THEN GOSUB 840:GOTO 5580

5063 P2=VAL(I$):IF P2<1 OR P2>5 THEN 5060

5064 IF P2=5 THEN P9=P:W5=-1:W6=-1:GOTO 5500

5065 IF P2=3 THEN 4800

5066 IF P2=2 AND O%(0)=2 OR P2=2 AND S4%<>1 AND S4%<>4 AND S4%<>5 THEN 5051

5067 IF P2<>2 THEN 5100

5068 IF S4%=4 THEN I=3 ELSE I=2

5069 I3=INT(RND(1)*100)+1
	IF I3<6 THEN gosub 5200:PO=1:A5%(I-1)=10:O%(0)=O%(0)+1:O%(1)=O%(1)+1:W%=1:GOSUB 3400:GOSUB 3800:GOSUB 3410:I5=P1%(D):I6=0:GOSUB 8020:IF O%(0)>=3 THEN B1(P)=B1(P)-1:po=0:GOTO 3900

5070 IF PO=1 THEN PO=0:GOTO 5051

5071 GOSUB 4095:I3=INT(RND(1)*1000)+1:I9=INT(RND(1)*1000)+1
	IF I3>350 THEN PRINT"Runner(s) going" ELSE PRINT"Count not favorable...hit away":P2=1

5072 GOSUB 6095:IF I3<=350 THEN 3050

5073 IF I9<=(B4%(P,4,B1(P))*2*.33) THEN U%=U%+1:PRINT Q$(0);" swings and misses":GOSUB 6095:IF U%=3 THEN GOTO 3601 ELSE GOTO 4800

5074 IF I9<=B4%(P,4,B1(P))*2 THEN PRINT Q$(0);" fouls off the pitch...hit away":GOSUB 6095:GOSUB 6095:P2=1:GOTO 3050

5077 GOTO 3050

5080 IF U6=2 OR U6=1 AND P<>U9 THEN 8700
	IF S4%=1 OR S4%=5 THEN H0%=1:LOCATE 17,77:COLOR 15,2:PRINT H0$(H0%):COLOR 15,0

5081 GOSUB 4095:COLOR 14,0:PRINT A$(D)
	PRINT" 1-PITCH          D-DETAILED STATS"
	PRINT" 2-INTENT. WALK   S-BATTER/PITCHER STATS"
	PRINT" 3-INFIELD IN     H-HOLD RUNNER"
	PRINT" 4-CORNERS IN     Q-QUIT GAME"
	PRINT" 5-INFIELD NORMAL"
	PRINT" 6-PERSONNEL"
	PRINT" 7-PITCH/HIT AWAY"
	PRINT" 8-PITCH AROUND":COLOR 15,0

5082 GOTO 5090

5085 IF U6=2 OR U6=1 AND P=U9 THEN 8900

5086 GOSUB 4095:COLOR 14,0:PRINT A$(P);" SELECT:"
	PRINT" 1- HIT AWAY      D-DETAILED STATS"
	PRINT" 2- HIT AND RUN   S-BATTER/PITCHER STATS"
	PRINT" 3- STEAL         Q-QUIT GAME"
	PRINT" 4- BUNT"
	PRINT" 5- PERSONNEL"
	COLOR 15,0

5087 GOTO 5061

5090 I$=GetKeyPress$
5091 IF UCASE$(I$)="S" THEN GOSUB 500:GOTO 5580
	IF UCASE$(I$)="L" THEN GOSUB 840:GOTO 5580

5092 IF UCASE$(I$)="D" THEN GOSUB 700:GOTO 5580

5093 IF UCASE$(I$)="Q" THEN CLS:END

5094 IF UCASE$(I$)="H" THEN H0%=H0%+1:IF H0%>1 THEN H0%=0

5095 IF UCASE$(I$)="H" THEN LOCATE 17,77:COLOR 15,2:PRINT H0$(H0%):COLOR 15,0:GOTO 5081

5096 IF VAL(I$)=7 THEN P2=1:D2=1:GOTO 3050

5099 GOTO 5054

5100 IF D2<>8 THEN 3050

5101 GOSUB 275:IF P2=4 OR RN<=50 AND P2=1 THEN 3050

5105 GOTO 3240

5200 '*** picked off ***
	GOSUB 4095
	gosub 8060:gosub 275
	if rn<=33 then print "NO, "p$(d,p1%(d))" throws to "h$(i-1)
	if rn>33 and rn<=66 then print "quick move to "h$(i-1)
	if rn>66 then print "pickoff throw to "h$(i-1)
	gosub 6095:gosub 275
	if rn<=50 then PRINT Q$(I-1)" is picked off base!"
	if rn>50 then print q$(i-1)" is caught napping"
	gosub 6095:i8=int(rnd(1)*100)+1
	if i8<=25 then print "he can't believe it"
	if i8>25 and i8<=50 then print "what a teriffic move by "p$(d,p1%(d))
	gosub 6095
	return

5500 TG=0:PG=0:RG=0
	CLS
	COLOR 12:PRINT TAB(33); "PERSONNEL MENU":PRINT
	COLOR 14:PRINT "(0) ";:COLOR 15:PRINT "RESUME PLAY":PRINT
	COLOR 14:PRINT "(1) ";:COLOR 15:PRINT "RELIEF PITCHER":PRINT
	COLOR 14:PRINT "(2) ";:COLOR 15:PRINT "CHANGE BATTER/FIELDER"

5501 I$=GetKeyPress$
5502 I=VAL(I$):I5=0:IF I>2 THEN 5501

5503 ON I+1 GOTO 5580,5504,5540

5504 IF P2=5 THEN 9740

5505 GOSUB 200

5506 locate 23,1:PRINT "SELECT RELIEVER (Z = NO CHANGE) ";
	IF B9%(P9,1)<>99 THEN GOSUB 9740
	print

5507 I$=GetKeyPress$
5508 IF UCASE$(I$)="Z" THEN 5500

5509 GOSUB 165

5511 I1=VAL(I$):IF I1>21 THEN 5507

5512 IF P%(P9,I1,15)>0 THEN 5507

5513 locate 23,1:print string$(78,32):LOCATE 23,1:PRINT P$(P9,I1);" ";B1$(P%(P9,I1,0)+2)" ";

5514 PRINT"ARE YOU SURE (YN)";

5515 I$=GetKeyPress$
5516 IF UCASE$(I$)="N" THEN 5505

5517 PPH=0:PR%=0
	IF S1%(P9)<>-1 AND P6%(P9)>1 THEN I5=P1%(P9):I6=22:GOSUB 8021
	P1%(P9)=I1:P6%(P9)=P6%(P9)+1:P%(P9,I1,15)=P6%(P9):GOSUB 135:GOSUB 1800

5518 P9=1-P9:W5=P9:W6=W5:V(P9)=1:V1(P9)=9:GOSUB 220:P9=1-P9:IF D%=1 OR I5=66 OR I5=77 THEN 5538

5519 CLS:LOCATE ,14:COLOR 14:PRINT "DO YOU WISH TO MAKE A FLIP FLOP OR DOUBLE SWITCH (YN)";
	COLOR 15

5520 I$=GetKeyPress$
5521 IF UCASE$(I$)="N" THEN 5538

5522 GOSUB 140

5523 GOSUB 5541:LOCATE 12,30:PRINT"LINEUP SLOT FOR PITCHER TO BAT IN (1-9)"

5524 I$=GetKeyPress$
	IF ASC(I$)=32 AND PG=0 THEN PG=1:GOSUB 140:GOTO 5523
	IF ASC(I$)=32 AND PG=1 THEN PG=0:GOSUB 140:GOTO 5523
	IF ASC(I$)=9 AND TG=0 THEN GOSUB 5800:GOTO 5523
	IF ASC(I$)=9 AND TG=1 THEN TG=0:GOSUB 141:GOTO 5523

5525 V2=VAL(I$):IF V2<1 OR V2>9 THEN 5524

5526 LOCATE 13,30:PRINT"WHO WILL REPLACE "B$(D,B3%(D,V2))" @ "C$(B7%(D,V2));

5527 I$=GetKeyPress$
	IF ASC(I$)=32 AND PG=0 THEN PG=1:GOSUB 141:GOTO 5526
	IF ASC(I$)=32 AND PG=1 THEN PG=0:GOSUB 141:GOTO 5526
	IF ASC(I$)=9 AND TG=0 THEN GOSUB 5800:GOTO 5526
	IF ASC(I$)=9 AND TG=1 THEN TG=0:GOSUB 141:GOTO 5526

5528 GOSUB 165:V3=VAL(I$):IF V3>22 THEN 5527

5529 LOCATE 16,29:PRINT B$(D,V3)

5530 LOCATE 17,29:PRINT "ARE YOU SURE (YN)";

5531 I$=GetKeyPress$
5532 IF UCASE$(I$)="N" THEN 5518
	D0%(D)=D0%(D)+1:X0%(D,0,D0%(D))=V3:X0%(D,1,D0%(D))=B7%(D,V2)
	FOR V0=1 TO 9
	IF B7%(D,V0)=1 THEN X0%(D,2,D0%(D))=(V0*10)+(D0%(D)-9)
	NEXT V0

5533 FOR V0=1 TO 9
	IF B7%(D,V0)=1 THEN B7%(D,V0)=B7%(D,V2):B3%(D,V0)=V3:B9%(D,B7%(D,V0))=V3:P9=D:W5=D:W6=D:V(D)=V0:V1(D)=V0:GOSUB 220

5534 NEXT V0
	B%(D,V3,21)=1
	I7%=D:I8%=V2:I9%=V3
	GOSUB 7100

5535 I5=0
	FOR V0=1 TO 9
	IF V0=V2 THEN B7%(D,V2)=1:B3%(D,V2)=P1%(D):B9%(D,1)=P1%(D):V(D)=V0:V1(D)=V0:GOSUB 220

5536 NEXT V0

5537 GOTO 5500

5538 I5=0
	FOR V0=1 TO 9
	IF B7%(D,V0)=1 OR B7%(D,V0)=10 THEN B3%(D,V0)=P1%(D):B9%(D,1)=P1%(D):B7%(D,V0)=1:P9=D:W5=D:W6=D:V(D)=V0:V1(D)=V0:GOSUB 220:GOTO 5537

5539 NEXT V0

5540 GOSUB 140:GOSUB 5541:GOTO 5546

5541 LOCATE 14,1:COLOR 2:PRINT" # LINEUP       PS  FA  OB"
	COLOR 15
	FOR I=1 TO 9:LOCATE ,1
	IF B1(P9)=I THEN PRINT "*";

5542 LOCATE ,2:PRINT USING "#";I;:LOCATE ,4
	IF B7%(P9,I)=1 THEN PRINT P$(P9,P1%(P9));

5543 IF B7%(P9,I)<>1 THEN PRINT B$(P9,B3%(P9,I));
	IF B7%(P9,I)<>0 AND B7%(P9,I)<10 THEN LOCATE,20:PRINT B%(P9,B3%(P9,I),19);

5544 LOCATE ,17:PRINT C$(B7%(P9,I));
	LOCATE ,25:FOR I1=1 TO 3
	IF B0%(I1,0)=I AND P9=P THEN COLOR 3:PRINT I1;:COLOR 15

5545 NEXT I1:PRINT " ":NEXT I:RETURN

5546 LOCATE 14,29:COLOR 12:PRINT "SELECT:"
	LOCATE ,30:COLOR 14:PRINT " (1) ";:COLOR 15:PRINT "SELECT A PINCH-HITTER"
	LOCATE ,30:COLOR 14:PRINT " (2) ";:COLOR 15:PRINT "SWAP POSITIONS BETWEEN PLAYERS"
	LOCATE ,30:COLOR 14:PRINT " (3) ";:COLOR 15:PRINT "SUBSTITUTE PLAYER IN LINEUP"
	LOCATE ,30:COLOR 14:PRINT " (Z) ";:COLOR 15:PRINT "RESUME PLAY"
	LOCATE ,30:COLOR 14:PRINT "<SPACE> ";:COLOR 15:PRINT "TOGGLE ROSTER";:COLOR 14:PRINT " <TAB> ";:COLOR 15:PRINT "TOGGLE FIELDING/BATTING"

5547 I$=GetKeyPress$
	IF ASC(I$)=32 AND PG=0 THEN PG=1:GOSUB 141:GOTO 5547
	IF ASC(I$)=32 AND PG=1 THEN PG=0:GOSUB 141:GOTO 5547

5548 GOTO 5590

5550 LOCATE 20,30:PRINT"LINEUP SLOT TO REPLACE (1-9) (Z)=NO CHANGE"

5552 I$=GetKeyPress$
	IF ASC(I$)=32 AND PG=0 THEN PG=1:GOSUB 141:GOSUB 5541:GOTO 5552
	IF ASC(I$)=32 AND PG=1 THEN PG=0:GOSUB 141:GOSUB 5541:GOTO 5552
	IF ASC(I$)=9 AND TG=0 THEN GOSUB 5800:GOSUB 5541:GOTO 5552
	IF ASC(I$)=9 AND TG=1 THEN TG=0:GOSUB 141:GOSUB 5541:GOTO 5552

5553 IF UCASE$(I$)="Z" THEN 5540

5554 I2=VAL(I$):IF I2<1 OR I2>9 OR P9=D AND B7%(P9,I2)=1 THEN 5552
	LOCATE ,30
	IF B7%(P9,I2)=1 THEN PRINT P$(P9,P1%(P9));
	IF B7%(P9,I2)<>1 THEN PRINT B$(P9,B3%(P9,I2));

5555 LOCATE 22,30:PRINT"# OF SUB"

5556 I$=GetKeyPress$
	IF ASC(I$)=32 AND PG=0 THEN PG=1:GOSUB 141:GOSUB 5541:GOTO 5556
	IF ASC(I$)=32 AND PG=1 THEN PG=0:GOSUB 141:GOSUB 5541:GOTO 5556
	IF ASC(I$)=9 AND TG=0 THEN GOSUB 5800:GOSUB 5541:GOTO 5556
	IF ASC(I$)=9 AND TG=1 THEN TG=0:GOSUB 141:GOSUB 5541:GOTO 5556

5557 GOSUB 165:I1=VAL(I$):IF I1>22 THEN 5555

5558 IF B%(P9,I1,21)>0 THEN 5555

5559 LOCATE 23,30:PRINT B$(P9,I1)"... ARE YOU SURE (YN)"

5560 I$=GetKeyPress$
5561 IF UCASE$(I$)="N" THEN 5540

5562 B%(P9,I1,21)=1:B3%(P9,I2)=I1:B9%(P9,B7%(P9,I2))=I1
	IF B7%(P9,I2)=1 OR B7%(P9,I2)=10 THEN B9%(P9,1)=99:B9%(P9,0)=I2:B7%(P9,I2)=10

5563 D0%(P9)=D0%(P9)+1:X0%(P9,0,D0%(P9))=B3%(P9,I2)
	X0%(P9,1,D0%(P9))=B7%(P9,I2):X0%(P9,2,D0%(P9))=(I2*10)+(D0%(P9)-9)
	W5=P9:W6=P9:V(P9)=I2:V1(P9)=I2
	I7%=P9:I8%=I2:I9%=I1
	GOSUB 7100

5565 GOSUB 220
	GOTO 5540

5567 LOCATE 20,29:PRINT"B.O. #1 (1-9)"

5568 I$=GetKeyPress$
5569 I1=VAL(I$):IF I1<1 OR I1>9 THEN 5568

5570 IF B7%(P9,I1)=1 THEN 5568

5571 LOCATE 21,29:PRINT"B.O. #2 (1-9)"

5572 I$=GetKeyPress$
5573 I2=VAL(I$):IF I2<1 OR I2>9 THEN 5572

5574 IF B7%(P9,I2)=1 THEN 5572

5575 I3=B7%(P9,I2):B7%(P9,I2)=B7%(P9,I1):B7%(P9,I1)=I3
	B9%(P9,B7%(P9,I1))=B3%(P9,I1)
	B9%(P9,B7%(P9,I2))=B3%(P9,I2)
	I7%=P9:I8%=I2:I9%=B3%(P9,I2)
	GOSUB 7100

5576 I7%=P9:I8%=I1:I9%=B3%(P9,I1)
	GOSUB 7100

5577 GOTO 5540

5580 GOSUB 151

5581 GOSUB 300:GOTO 5051

5590 IF ASC(I$)=9 AND TG=0 THEN GOSUB 5800:GOTO 5547
	IF ASC(I$)=9 AND TG=1 THEN TG=0:GOSUB 141:GOTO 5547
	IF UCASE$(I$)="Z" THEN 5500

5592 I1=VAL(I$):IF I1=2 THEN 5567

5593 IF I1=3 THEN 5550
	IF D2=6 THEN 5550
	I2=B1(P):GOTO 5555

5700 FOR I5=20 TO 23:LOCATE I5,28

5702 PRINT"                                                    "

5704 NEXT I5

5706 RETURN

5800 'DIS FLDG
	LOCATE 1,16:PRINT "P1 FAVG A R   P2 FAVG A R   P3 FAVG A R   P4 FAVG A R      "
	FOR I=2 TO 13:LOCATE I,1:PRINT STRING$(78,32):NEXT
	LOCATE 2
	IF PG=0 THEN AA=0:BB=12
	IF PG=1 THEN AA=13:BB=22
	FOR I=AA TO BB:IF B%(P9,I,21)<>0 THEN 5802
	IF I<10 THEN PRINT USING "#";I;
	IF I>9 THEN PRINT X$(I-10);
	COLOR 15,0
	PRINT TAB(3); B$(P9,I);
	LOCATE,17:IF B%(P9,I,22)<>-1 THEN PRINT USING "# #### # #";B%(P9,I,22),B%(P9,I,26),B%(P9,I,33),B%(P9,I,34);
	LOCATE,31:IF B%(P9,I,23)<>-1 THEN PRINT USING "# #### # #";B%(P9,I,23),B%(P9,I,27),B%(P9,I,35),B%(P9,I,36);
	LOCATE,45:IF B%(P9,I,24)<>-1 THEN PRINT USING "# #### # #";B%(P9,I,24),B%(P9,I,28),B%(P9,I,37),B%(P9,I,38);
	LOCATE,59:IF B%(P9,I,25)<>-1 THEN PRINT USING "# #### # #";B%(P9,I,25),B%(P9,I,29),B%(P9,I,39),B%(P9,I,40);
	PRINT

5802 NEXT I:TG=TG+1:RETURN

6020 S4%=0:I1=B0%(1,0):I2=B0%(2,0):I3=B0%(3,0):IF I1>0 AND I2=0 AND I3=0 THEN S4%=1

6021 IF I1=0 AND I2>0 AND I3= 0 THEN S4%=2:REM*** RUNNER ON 2ND ***

6022 IF I1=0 AND I2=0 AND I3> 0 THEN S4%=3:REM*** RUNNER ON 3RD ***

6023 IF I1>0 AND I2>0 AND I3= 0 THEN S4%=4:REM*** 1ST AND 2ND ***

6024 IF I1>0 AND I2=0 AND I3> 0 THEN S4%=5:REM*** 1ST AND 3RD ***

6025 IF I1=0 AND I2>0 AND I3> 0 THEN S4%=6:REM*** 2ND AND 3RD ***

6026 IF I1>0 AND I2>0 AND I3> 0 THEN S4%=7:REM*** BASES LOADED ***

6027 RETURN

6095 IF G4=0 THEN 6097

6096 U!=TIMER:WHILE TIMER<U!+G4:WEND

6097 RETURN

6600 IF H6%<=B4%(P,5,B1(P))+25 THEN S2%=2:GOTO 3240

6601 IF P2=4 THEN GOTO 4100

6603 GOTO 3056

6800 GOSUB 6860:IF B1%(3,0)=0 OR A5%(3)=0 THEN 6805

6801 I=3

6802 IF A5%(3)=10 THEN I3=3:GOSUB 6840

6803 IF A5%(3)>=1 AND A5%(3)<= 4 THEN GOSUB 6845

6804 IF A5%(3)=11 THEN I3=4:GOSUB 6840

6805 IF B1%(2,0)=0 OR A5%(2)=0 THEN 6812

6806 I=2

6807 IF A5%(2)=10 THEN I3= 2:GOSUB 6840

6808 IF A5%(2)=11 THEN I3= 3:GOSUB 6840

6809 IF A5%(2)=12 AND B1%(3,0)=0 THEN I3=4:GOSUB 6840

6810 IF A5%(2)=1 AND B1%(3,0)=0 THEN GOSUB 6850

6811 IF A5%(2)>=2 AND A5%(2)<=4 AND B1%(3,0)=0 THEN GOSUB 6845

6812 IF B1%(1,0)=0 OR A5%(1)=0 THEN 6821

6813 I=1:IF A5%(1)=8 AND B1%(2,0)=0 THEN GOSUB 6850

6814 IF A5%(1)=10 THEN I3= 1:GOSUB 6840

6815 IF A5%(1)=11 THEN I3= 2:GOSUB 6840

6816 IF A5%(1)=12 AND B1%(3,0)=0 AND B1%(2,0)=0 THEN I3=3:GOSUB 6840

6817 IF A5%(1)=13 AND B1%(3,0)=0 AND B1%(2,0)=0 THEN I3=4:GOSUB 6840

6818 IF A5%(1)=1 AND B1%(2,0)=0 THEN GOSUB 6850

6819 IF A5%(1)=2 AND B1%(2,0)=0 AND B1%(3,0)=0 THEN GOSUB 6851

6820 IF A5%(1)>=3 AND A5%(1)<=4 AND B1%(2,0)=0 AND B1%(3,0)=0 THEN GOSUB 6845

6821 I=0:B1%(0,0)=B1(P):B1%(0,1)=P1%(D):IF A5%(0)=0 THEN 6835

6822 IF A5%(0)>0 AND A5%(0)<4 THEN I3 = A5%(0): GOSUB 6852

6825 IF A5%(0)=4 THEN I3=A5%(0):GOSUB 6845

6826 IF A5%(0)=11 THEN I3=1:GOSUB 6840

6827 IF A5%(0)=12 THEN I3=2:GOSUB 6840

6828 IF A5%(0)=13 THEN I3=3:GOSUB 6840

6829 IF A5%(0)=14 THEN I3=4:GOSUB 6840

6835 RETURN

6840 B1%(I,0)=0:B1%(I,1)=0:RETURN

6845 IF O%(1)>=3 THEN 6840

6846 I5=B1%(I,1):I6=3:GOSUB 8020:GOTO 6840

6850 FOR I4=0 TO 1:B1%(I+1,I4)=B1%(I,I4):B1%(I,I4)=0:NEXT I4:RETURN

6851 FOR I4=0 TO 1:B1%(I+2,I4)=B1%(I,I4):B1%(I,I4)=0:NEXT I4:RETURN

6852 B1%(I3,0)=B1(P):B1%(I3,1)=P1%(D):RETURN

6860 FOR I=1 TO 3:IF S2%=13 AND A5%(I)=11 AND B1%(I,0)=0 THEN A5%(0)=11

6861 NEXT I:RETURN

7000 W%=0

7001 I=INT(RND(1)*100)+1

7002 IF B7%(P,B1(P))<>1 AND B%(P,B3%(P,B1(P)),0)<0 THEN I1=-1

7003 IF B7%(P,B1(P))<>1 AND B%(P,B3%(P,B1(P)),0)>0 THEN I1=1

7004 IF B7%(P,B1(P))<>1 AND B%(P,B3%(P,B1(P)),0)=0 THEN I1=-1*P%(D,P1%(D),0)

7005 IF B7%(P,B1(P))=1 THEN I1=P%(P,P1%(P),0)

7006 RETURN

7010 GOSUB 7000:I=INT(RND(1)*100)+1

7011 IF I1=1 THEN W%=3

7012 IF I1=1 AND I<=92 THEN W%=4

7013 IF I1=1 AND I<=72 THEN W%=6

7014 IF I1=1 AND I<=32 THEN W%=5

7015 IF I1=-1 THEN W%=5

7016 IF I1=-1 AND I<=88 THEN W%=6

7017 IF I1=-1 AND I<=68 THEN W%=4

7018 IF I1=-1 AND I<=28 THEN W%=3

7019 IF I<=12 AND S4%<>20 THEN W%=1

7020 IF I<=4 OR I<=12 AND S2%=20 THEN W%=2

7021 IF S2%=21 AND W%=2 THEN W%=1

7022 IF P2=4 AND W%=6 THEN W%=1

7023 IF P2=4 AND W%=4 OR S2%=20 AND W%=1 THEN W%=2

7024 IF W%<>1 THEN Q$(4)=B$(D,B9%(D,W%))

7025 IF W%= 1 THEN Q$(4)=P$(D,P1%(D))

7026 RETURN

7030 GOSUB 7000:I=INT(RND(1)*100)+1

7031 IF I1=1 THEN W%=7

7032 IF I1=1 AND I<=50 THEN W%=9

7034 IF I1=-1 THEN W%=9

7035 IF I1=-1 AND I<=50 THEN W%=7

7036 IF I<=30 THEN W%=8

7037 Q$(4)=B$(D,B9%(D,W%)):RETURN

7040 W%=9:I=INT(RND(1)*100)+1

7041 IF I<=25 THEN W%=8

7042 IF I<=5 THEN W%=7

7043 Q$(4)=B$(D,B9%(D,W%)):RETURN

7050 GOSUB 7000:I=INT(RND(1)*100)+1

7051 IF I1=1 THEN W%=7

7053 IF I1=1 AND I<=15 THEN W%=9

7054 IF I1=-1 THEN W%=9

7055 IF I1=-1 AND I<=15 THEN W%=7

7056 IF I<=5 THEN W%=8

7057 Q$(4)=B$(D,B9%(D,W%)):RETURN

7100 ' REPLACEMENT AVERAGE CALCS, I7%,I8%,I9%
	for xx=22 to 25
	IF B7%(I7%,I8%)=B%(I7%,I9%,xx) THEN B%(I7%,I9%,15)=B%(I7%,I9%,33+(xx-22)*2):B%(I7%,I9%,17)=B%(I7%,I9%,34+(xx-22)*2):B%(I7%,I9%,19)=B%(I7%,I9%,xx+4):goto 7110
	next
	J1=B7%(I7%,I8%)
	GOSUB 4950
	B%(I7%,I9%,15)=J2
	B%(I7%,I9%,17)=J3
	B%(I7%,I9%,19)=J4

7110 RETURN

7105 BK$="## # # ## # #  # # #  #  # # ## ##"
	FOR I=0 TO 1:FOR I1=0 TO 21:IF P8%(I,I1,1)<0 THEN P8%(I,I1,1)=0
	NEXT I1:NEXT I
	FOR I=0 TO 1:FOR I1=0 TO 21
	IF P%(I,I1,15)<>1 THEN 7250
	IF P8%(I,I1,0)>=18 AND P8%(I,I1,3)<=3 THEN P8%(I,I1,21)=1

7250 NEXT I1
	NEXT I
	FOR I=0 TO 1:FOR I1=0 TO 22
	IF B8%(I,I1,11)=0 THEN 7257

7254 IF B8%(I,I1,11)=1 THEN E$(E)=B$(I,I1):GOTO 7256

7255 E$(E)=B$(I,I1)+STR$(B8%(I,I1,11))

7256 E$(E)=E$(E)+" ("+RIGHT$(STR$(B5%(I,I1,11)),(LEN(STR$(B5%(I,I1,11)))-1))+")":E=E+1

7257 IF B8%(I,I1,4)=0 THEN 7261:'        ***CHECKING DOUBLES***

7258 IF B8%(I,I1,4)=1 THEN DB$(DB)=B$(I,I1):GOTO 7260

7259 DB$(DB)=B$(I,I1)+STR$(B8%(I,I1,4))

7260 DB$(DB)=DB$(DB)+" ("+RIGHT$(STR$(B5%(I,I1,4)),(LEN(STR$(B5%(I,I1,4)))-1))+")":DB=DB+1

7261 IF B8%(I,I1,5)=0 THEN 7265:'        ***CHECKING TRIPLES***

7262 IF B8%(I,I1,5)=1 THEN TR$(TR)=B$(I,I1):GOTO 7264

7263 TR$(TR)=B$(I,I1)+STR$(B8%(I,I1,5))

7264 TR$(TR)=TR$(TR)+" ("+RIGHT$(STR$(B5%(I,I1,5)),(LEN(STR$(B5%(I,I1,5)))-1))+")":TR=TR+1

7265 IF B8%(I,I1,6)=0 THEN 7269:'        ***CHECKING HOME RUNS***

7266 IF B8%(I,I1,6)=1 THEN HR$(HR)=B$(I,I1):GOTO 7268

7267 HR$(HR)=B$(I,I1)+STR$(B8%(I,I1,6))

7268 HR$(HR)=HR$(HR)+" ("+RIGHT$(STR$(B5%(I,I1,6)),(LEN(STR$(B5%(I,I1,6)))-1))+")":HR=HR+1

7269 IF B8%(I,I1,9)=0 THEN 7272:'        ***CHECKING STOLEN BASES ***

7270 IF B8%(I,I1,9)=1 THEN SB$(SB)=B$(I,I1)+" ("+RIGHT$(STR$(B5%(I,I1,9)),(LEN(STR$(B5%(I,I1,9)))-1))+")":SB=SB+1:GOTO 7272

7271 SB$(SB)=B$(I,I1)+STR$(B8%(I,I1,9))+" ("+RIGHT$(STR$(B5%(I,I1,9)),(LEN(STR$(B5%(I,I1,9)))-1))+")":SB=SB+1

7272 NEXT I1:NEXT I:FOR I=0 TO 1:FOR I1=0 TO 21

7273 IF P8%(I,I1,34)=0 THEN 7277

7274 IF P8%(I,I1,34)=1 THEN E$(E)=P$(I,I1):GOTO 7276

7275 E$(E)=P$(I,I1)+STR$(P8%(I,I1,34))

7276 E$(E)=E$(E)+" ("+RIGHT$(STR$(P5%(I,I1,34)),(LEN(STR$(P5%(I,I1,34)))-1))+")":E=E+1

7277 IF P8%(I,I1,27)=0 THEN 7281:'        ***CHECKING DOUBLES***

7278 IF P8%(I,I1,27)=1 THEN DB$(DB)=P$(I,I1):GOTO 7280

7279 DB$(DB)=P$(I,I1)+STR$(P8%(I,I1,27))

7280 DB$(DB)=DB$(DB)+" ("+RIGHT$(STR$(P5%(I,I1,27)),(LEN(STR$(P5%(I,I1,27)))-1))+")":DB=DB+1

7281 IF P8%(I,I1,28)=0 THEN 7285:'        ***CHECKING TRIPLES***

7282 IF P8%(I,I1,28)=1 THEN TR$(TR)=P$(I,I1):GOTO 7284

7283 TR$(TR)=P$(I,I1)+STR$(P8%(I,I1,28))

7284 TR$(TR)=TR$(TR)+" ("+RIGHT$(STR$(P5%(I,I1,28)),(LEN(STR$(P5%(I,I1,28)))-1))+")":TR=TR+1

7285 IF P8%(I,I1,29)=0 THEN 7289:'        ***CHECKING HOME RUNS***

7286 IF P8%(I,I1,29)=1 THEN HR$(HR)=P$(I,I1):GOTO 7288

7287 HR$(HR)=P$(I,I1)+STR$(P8%(I,I1,29))

7288 HR$(HR)=HR$(HR)+" ("+RIGHT$(STR$(P5%(I,I1,29)),(LEN(STR$(P5%(I,I1,29)))-1))+")":HR=HR+1

7289 IF P8%(I,I1,32)=0 THEN 7292:'        ***CHECKING STOLEN BASES ***

7290 IF P8%(I,I1,32)=1 THEN SB$(SB)=P$(I,I1)+" ("+RIGHT$(STR$(P5%(I,I1,32)),(LEN(STR$(P5%(I,I1,32)))-1))+")":SB=SB+1:GOTO 7292

7291 SB$(SB)=P$(I,I1)+STR$(P8%(I,I1,32))+" ("+RIGHT$(STR$(P5%(I,I1,32)),(LEN(STR$(P5%(I,I1,32)))-1))+")":SB=SB+1

7292 NEXT I1:NEXT I

7106 GT$="2:":ATT=INT(RND(1)*100000!):IF ATT<5000 OR ATT>60000! THEN 7106

7107 MIN=INT(RND(1)*100):IF MIN>59 THEN 7107

7108 IF MIN<10 THEN MIN$="0"+RIGHT$(STR$(MIN),1):GT$=GT$+MIN$ ELSE GT$=GT$+RIGHT$(STR$(MIN),2)

7109 FOR I=0 TO 1:FOR I1=0 TO 21:IF P%(I,I1,15)>=98 THEN P%(I,I1,15)=0
	NEXT I1:NEXT I

7111 FOR I=0 TO 1:FOR I1=0 TO 22:IF B%(I,I1,21)>=98 THEN B%(I,I1,21)=0

7112 IF B%(I,I1,21)<>0 THEN B8%(I,I1,14)=1

7113 NEXT I1:NEXT I

7116 CLS:PRINT "PLEASE WAIT..."

7117 FOR I=0 TO 1

7361 M=23

7362 L=22

7363 I6=0:I7=0

7364 H1=0:H2=0:H3=0

7365 IF X0%(I,2,I6)<=X0%(I,2,I6+1) THEN 7370

7366 H1=X0%(I,2,I6):H2=X0%(I,1,I6):H3=X0%(I,0,I6)

7367 X0%(I,0,I6)=X0%(I,0,I6+1):X0%(I,1,I6)=X0%(I,1,I6+1):X0%(I,2,I6)=X0%(I,2,I6+1)

7368 X0%(I,2,I6+1)=H1:X0%(I,1,I6+1)=H2:X0%(I,0,I6+1)=H3

7369 I7=I6

7370 I6=I6+1

7371 IF I6>L THEN 7373

7372 GOTO 7364

7373 IF I7=0 THEN 7118

7374 L=I7-1:GOTO 7363

7118 NEXT I
	GOTO 7293

7119 IF X=1 THEN NM$="TEMPBOX":GOTO 7121
	IF AP=1 THEN OPEN "GAME82" FOR INPUT AS #1:INPUT #1,FF$:FOR I=0 TO 2:INPUT #1,AP%(I):NEXT:CLOSE 1:NM$="BOXSCORE."+MID$(STR$(AP%(0)+AP%(2)),2,LEN(STR$(AP%(0)+AP%(2)))):GOTO 7121

7120 CLS:PRINT "ENTER FILENAME FOR BOXSCORE ";:INPUT NM$

7121 OPEN D$(2)+NM$ FOR OUTPUT AS #2:GOSUB 7183:FOR I=0 TO 1

7122 PRINT #2, LEFT$(A$(I),12); TAB(18); "AB R H BI 2 3 HR K W SB CS E PO  A"

7123 FOR I3=0 TO 22:GOTO 7358

7124 PRINT #2, B$(I,I1)", "C$(X0%(I,1,I3)); TAB(18);

7125 PRINT #2, USING BK$;B8%(I,I1,0),B8%(I,I1,2),B8%(I,I1,1),B8%(I,I1,3),B8%(I,I1,4),B8%(I,I1,5),B8%(I,I1,6),B8%(I,I1,8),B8%(I,I1,7),B8%(I,I1,9),B8%(I,I1,10),B8%(I,I1,11),B8%(I,I1,16),B8%(I,I1,17)

7126 FOR I2=0 TO 17:T3%(I2)=T3%(I2)+B8%(I,I1,I2):NEXT I2

7127 NEXT I3
	FOR I1=0 TO 21:FOR I2=0 TO 13:T3%(I2)=T3%(I2)+P8%(I,I1,I2+23):NEXT
	FOR I2=14 TO 17:T3%(I2)=T3%(I2)+P8%(I,I1,I2+22):NEXT:NEXT

7130 FOR I1=1 TO P6%(I):FOR I2=0 TO 21:IF P%(I,I2,15)<>I1 THEN 7134

7131 PRINT #2, P$(I,I2)", p";

7132 PRINT #2, TAB(18);

7133 PRINT #2, USING BK$;P8%(I,I2,23),P8%(I,I2,25),P8%(I,I2,24),P8%(I,I2,26),P8%(I,I2,27),P8%(I,I2,28),P8%(I,I2,29),P8%(I,I2,31),P8%(I,I2,30),P8%(I,I2,32),P8%(I,I2,33),P8%(I,I2,34),P8%(I,I2,38),P8%(I,I2,39)

7134 NEXT:NEXT

7135 PRINT #2, "TOTALS" TAB(17); T3%(0); TAB(22); T3%(1); TAB(27); T3%(4);

7136 PRINT #2, TAB(32); T3%(6); TAB(36); T3%(7); TAB(42);

7137 PRINT #2, T3%(10); TAB(46); T3%(16); TAB(50); T3%(17)

7138 PRINT #2, TAB(20); T3%(2); TAB(24); T3%(3); TAB(29);

7139 PRINT #2, T3%(5); TAB(34); T3%(8); TAB(39); T3%(9);

7140 PRINT #2, TAB(44); T3%(11)

7141 PRINT #2,:FOR I1=0 TO 22:T3%(I1)=0:NEXT I1

7142 NEXT I

7143 FOR I=0 TO 1:PRINT #2,:PRINT #2, A$(I); TAB(23); "       IP   H  R  ER BB SO"

7144 FOR I1=1 TO P6%(I):FOR I2=0 TO 21

7145 IF P%(I,I2,15)<>I1 THEN 7154

7146 I4=1-I:PRINT #2, P$(I,I2);" ";

7147 IF P2%(I)=I2 AND S6%(I,0)-S6%(I4,0)>0 THEN 7179

7148 GOSUB DSKBOX:IF P2%(I)=I2 AND S6%(I,0)-S6%(I4,0)<0 THEN 7180

7149 IF P8%(I,I2,19)>0 THEN PRINT #2, "(BS,";:PRINT #2, USING "##";P5%(I,I2,19);:PRINT #2, ")";

7150 IF P8%(I,I2,22)>0 THEN PRINT #2, "(H,";:PRINT #2, USING "##";P5%(I,I2,22);:PRINT #2, ")";

7151 PRINT #2, TAB(29); STR$(INT(P8%(I,I2,0)/3));:IF P8%(I,I2,0)-(INT(P8%(I,I2,0)/3)*3)>0 THEN PRINT #2, ".";STR$(P8%(I,I2,0)-(INT(P8%(I,I2,0)/3)*3));

7152 PRINT #2, TAB(34); STR$(P8%(I,I2,1)); TAB(37); STR$(P8%(I,I2,2)); TAB(40); STR$(P8%(I,I2,3)); TAB(43); STR$(P8%(I,I2,4)); TAB(46); STR$(P8%(I,I2,5))

7153 FOR I4=0 TO 5:T3%(I4)=T3%(I4)+P8%(I,I2,I4):NEXT I4

7154 NEXT I2

7155 NEXT I1:PRINT #2, "TOTALS" TAB(29); STR$(INT(T3%(0)/3));

7156 IF T3%(0)-(INT(T3%(0)/3)*3)>0 THEN PRINT #2, ".";:PRINT #2, USING "#";T3%(0)-(INT(T3%(0)/3)*3);

7157 PRINT #2, TAB(34); STR$(T3%(1)); TAB(37); STR$(T3%(2)); TAB(40); STR$(T3%(3)); TAB(43); STR$(T3%(4)); TAB(46); STR$(T3%(5))

7158 LYN%=CINT(T3%(0)/3):FOR I1=0 TO 5:T3%(I1)=0:NEXT I1

7159 NEXT I

7160 PRINT #2,:PRINT #2, " E-";

7161 IF E=0 THEN PRINT #2,"NONE":GOTO 7163

7162 FOR X1=0 TO (E-1):PRINT #2, E$(X1);",";:NEXT:PRINT #2,

7163 PRINT #2, "2B-";

7164 IF DB=0 THEN PRINT #2,"NONE":GOTO 7166

7165 FOR X1=0 TO (DB-1):PRINT #2, DB$(X1);",";:NEXT:PRINT #2,

7166 PRINT #2, "3B-";

7167 IF TR=0 THEN PRINT #2,"NONE":GOTO 7169

7168 FOR X1=0 TO (TR-1):PRINT #2, TR$(X1);",";:NEXT:PRINT #2,

7169 PRINT #2, "HR-";

7170 IF HR=0 THEN PRINT #2,"NONE":GOTO 7172

7171 FOR X1=0 TO (HR-1):PRINT #2, HR$(X1);",";:NEXT:PRINT #2,

7172 PRINT #2, "SB-";

7173 IF SB=0 THEN PRINT #2,"NONE":GOTO 7175

7174 FOR X1=0 TO (SB-1):PRINT #2, SB$(X1);",";:NEXT:PRINT #2,

7175 PRINT #2, "LOB-";A$(0);STR$(L0%(0));","A$(1);STR$(L0%(1))

7176 PRINT #2, "DP-";A$(0);STR$(DP%(0));","A$(1);STR$(DP%(1))

7177 PRINT #2,"UMPIRES: HP-";U$(1);"; 1B-";U$(2);"; 2B-";U$(3);"; 3B-";U$(4):PRINT #2, "T: ";GT$;" A: ";ATT
	PRINT #2,
	PRINT #2,"INJURIES-";
	IF IJ%=0 THEN PRINT #2,"none":goto peject
	FOR X1=1 TO IJ%:PRINT #2,IJ$(X1);",";:NEXT:PRINT #2,
peject:
	PRINT #2,"EJECTIONS-";
	IF EJ%=0 THEN PRINT #2,"none":goto pweath
	FOR X1=1 TO EJ%:PRINT #2,EJ$(X1);",";:NEXT:PRINT #2,
pweath:
	PRINT #2,"TEMPERATURE:"TP%" WIND: ";
	IF WS%=0 THEN PRINT #2,"NONE "
	IF WS%=2 THEN PRINT #2,"LIGHT "
	IF WS%=3 THEN PRINT #2,"MODERATE "
	IF WS%>5 THEN PRINT #2,"BLOWING OUT AT "WS%" MPH"
	IF WS%<0 THEN PRINT #2,"BLOWING IN AT "WS%*-1" MPH"
	PRINT #2,"SKY: "SC$(SC%)
	PRINT #2,"PRECIPITATION: "PC$(PC%)

7178 PRINT #2, CHR$(12):CLOSE #2
	IF X=1 THEN 7580
	GOTO 7328

7179 PRINT #2, "(W,";RIGHT$(STR$(P5%(I,I2,6)+1),(LEN(STR$(P5%(I,I2,6)+1))-1));"-";RIGHT$(STR$(P5%(I,I2,7)),(LEN(STR$(P5%(I,I2,7)))-1));")";:W$=P$(I,I2):GOTO 7148

7180 PRINT #2, "(L,";RIGHT$(STR$(P5%(I,I2,6)+0),(LEN(STR$(P5%(I,I2,6)+0))-1));"-";RIGHT$(STR$(P5%(I,I2,7)+1),(LEN(STR$(P5%(I,I2,7)+1))-1));")";:L$=P$(I,I2):GOTO 7149

7181 LOCATE, 33:PRINT "-";:LOCATE, 34:PRINT USING "##";S6%(I,0);

7182 LOCATE, 37:PRINT USING "##";S6%(I,1);:LOCATE,40:PRINT USING "#";S6%(I,2):RETURN

7183 PRINT #2, "----------------------------------------"
	IF I%=10 AND P=0 OR I%=9 THEN GOSUB 7187

7184 IF I%=10 AND P=1 OR I%>10 AND I%<19 OR I%=19 AND P=0 THEN GOSUB 7194

7185 IF I%=19 AND P=1 OR I%>19 AND I%<=28 THEN GOSUB 7202

7186 RETURN

7187 FOR I=0 TO 1:PRINT #2,A$(I);:FOR I2=1 TO 9:GOSUB 7212
	IF I=0 THEN PRINT #2, USING "##";S%(I,I2);

7188 IF I=1 AND S6%(1,0)>S6%(0,0) AND I2=9 AND S%(1,9)=0 THEN PRINT #2, " X";

7189 IF I=1 AND S6%(1,0)<S6%(0,0) AND I2=9 OR I=1 AND S6%(1,0)>S6%(0,0) AND I2=9 AND S%(1,9)>0 THEN PRINT #2,USING "##";S%(I,I2);

7190 IF I=1 AND I2<9 THEN PRINT #2, USING "##";S%(I,I2);

7191 NEXT I2

7192 GOSUB 7222: NEXT I

7193 PRINT #2, "----------------------------------------":RETURN

7194 FOR I=0 TO 1:PRINT #2, A$(I);:FOR I2=1 TO 9:GOSUB 7212:PRINT #2, USING "##";S%(I,I2);

7195 NEXT I2:PRINT #2,:NEXT I

7196 PRINT #2, "----------------------------------------"
	IF P=0 AND I%<19 OR I%=19 THEN I%=I%-1

7197 FOR I=0 TO 1:FOR I2=10 TO I%:GOSUB 7212:PRINT #2,USING "##";S%(I,I2);

7198 NEXT I2

7199 GOSUB 7222

7200 NEXT I

7201 PRINT #2, "----------------------------------------":RETURN

7202 FOR I=0 TO 1:PRINT #2, A$(I);:FOR I2=1 TO 9:GOSUB 7212:PRINT #2, USING "##";S%(I,I2);

7203 NEXT I2:PRINT #2,:NEXT I

7204 PRINT #2, "----------------------------------------"
	IF I%=28 OR I%<28 AND P=0 THEN I%=I%-1

7205 FOR I=0 TO 1:FOR I2=10 TO 18:GOSUB 7212: PRINT #2, USING "##";S%(I,I2);

7206 NEXT I2:PRINT #2,:NEXT I

7207 PRINT #2, "----------------------------------------"

7208 FOR I=0 TO 1:FOR I2=19 TO I%:GOSUB 7212:PRINT #2, USING "##";S%(I,I2);:NEXT I2

7209 GOSUB 7222

7210 NEXT I

7211 PRINT #2, "----------------------------------------":RETURN

7212 IF I2<4 THEN  PRINT #2, TAB(((2*I2)+11))

7213 IF I2>3 AND I2<7 THEN  PRINT #2, TAB(((2*I2)+12))

7214 IF I2>6 AND I2<10 THEN  PRINT #2, TAB(((2*I2)+13))

7215 IF I2>9 AND I2<13 THEN  PRINT #2, TAB(((2*(I2-9))+11))

7216 IF I2>12 AND I2<16 THEN PRINT #2, TAB(((2*(I2-9))+12))

7217 IF I2>15 AND I2<19 THEN PRINT #2, TAB(((2*(I2-9))+13))

7218 IF I2>18 AND I2<22 THEN PRINT #2, TAB(((2*(I2-18))+11))

7219 IF I2>21 AND I2<25 THEN  PRINT #2, TAB(((2*(I2-18))+12))

7220 IF I2>24 AND I2<28 THEN PRINT #2, TAB(((2*(I2-18))+13))

7221 RETURN

7222 PRINT #2, TAB(33); "-";: PRINT #2, TAB(34): PRINT #2, USING "##";S6%(I,0);:PRINT #2, TAB(37): PRINT #2, USING "##";S6%(I,1);:PRINT #2, TAB(40): PRINT #2, USING "#";S6%(I,2): RETURN

7227 IF AP=1 AND U6=2 THEN RETURN

7228 LOCATE 24,1:PRINT "HIT ANY KEY TO CONTINUE";

7229 I$=GetKeyPress$
7230 RETURN

7293 I3=0:IF S6%(1,0)-S6%(0,0)>0 THEN I3=1
	IF P2%(I3)<>P1%(I3) AND S1%(I3)=-1 AND P8%(I3,P1%(I3),0)>=9 AND P8%(I3,P1%(I3),0)<=12 AND P6%(I3)>0 AND P8%(I3,P1%(I3),3)<3 THEN S1%(I3)=P1%(I3)
	IF P2%(I3)=S1%(I3) THEN S1%(I3)=-1
	CLS:PRINT TAB(32):COLOR 12:PRINT "BOXSCORE OPTIONS":PRINT
	COLOR 14:PRINT "(0) ";:COLOR 15:PRINT "SEND BOXSCORE TO THE SCREEN":PRINT
	COLOR 14:PRINT "(1) ";:COLOR 15:PRINT "SEND BOXSCORE TO THE PRINTER":PRINT
	COLOR 14:PRINT "(2) ";:COLOR 15:PRINT "SEND BOXSCORE TO A TEXT FILE"
	IF AP=1 AND U6=2 THEN X=2:GOTO 7339

7337 I$=GetKeyPress$
7338 X=VAL(I$)

7339 IF X=1 OR X=2 THEN 7119
	 X=0

7295 GOSUB 7491:GOSUB 7344:GOSUB 7342:GOSUB 7342

7296 FOR I=0 TO 1:PRINT LEFT$(A$(I),12);:LOCATE, 18: PRINT "AB R H BI 2 3 HR K W SB CS E PO  A"

7297 FOR I3=0 TO 22:GOTO 7352

7298 PRINT B$(I,I1)", "C$(X0%(I,1,I3));

7299 LOCATE, 18

7300 PRINT USING BK$;B8%(I,I1,0),B8%(I,I1,2),B8%(I,I1,1),B8%(I,I1,3),B8%(I,I1,4),B8%(I,I1,5),B8%(I,I1,6),B8%(I,I1,8),B8%(I,I1,7),B8%(I,I1,9),B8%(I,I1,10),B8%(I,I1,11),B8%(I,I1,16),B8%(I,I1,17)

7301 FOR I2=0 TO 17:T3%(I2)=T3%(I2)+B8%(I,I1,I2):NEXT I2

7302 NEXT I3

7303 FOR I1=0 TO 21:FOR I2=0 TO 13:T3%(I2)=T3%(I2)+P8%(I,I1,I2+23):NEXT
	FOR I2=14 TO 17:T3%(I2)=T3%(I2)+P8%(I,I1,I2+22):NEXT:NEXT
	FOR I1=1 TO P6%(I):FOR I2=0 TO 21:IF P%(I,I2,15)<>I1 THEN 7305

7304 GOTO 7310

7305 NEXT:NEXT:PRINT "TOTALS";: LOCATE, 17: PRINT T3%(0);: LOCATE, 22: PRINT T3%(1);: LOCATE, 27: PRINT T3%(4);

7306 LOCATE,32:PRINT T3%(6);:LOCATE,36:PRINT T3%(7);:LOCATE, 42:PRINT T3%(10);:LOCATE,46:PRINT T3%(16);:LOCATE,50:PRINT T3%(17)

7307 LOCATE,20:PRINT T3%(2);:LOCATE,24:PRINT T3%(3);:LOCATE,29:PRINT T3%(5);:LOCATE,34:PRINT T3%(8);:LOCATE,39:PRINT T3%(9);:LOCATE,44:PRINT T3%(11)

7308 PRINT:GOSUB 7344:GOSUB 7342:FOR I1=0 TO 22:T3%(I1)=0:NEXT I1

7309 NEXT I:GOTO 7313

7310 PRINT P$(I,I2)", p";:LOCATE ,18

7311 PRINT USING BK$;P8%(I,I2,23),P8%(I,I2,25),P8%(I,I2,24),P8%(I,I2,26),P8%(I,I2,27),P8%(I,I2,28),P8%(I,I2,29),P8%(I,I2,31),P8%(I,I2,30),P8%(I,I2,32),P8%(I,I2,33),P8%(I,I2,34),P8%(I,I2,38),P8%(I,I2,39)

7312 GOTO 7305

7313 FOR I=0 TO 1:GOSUB 7342:PRINT A$(I);:LOCATE,23:PRINT "       IP   H  R  ER BB SO"

7314 FOR I1=1 TO P6%(I):FOR I2=0 TO 21:IF P%(I,I2,15)<>I1 THEN 7321

7315 I4=1-I:LOCATE,1:PRINT P$(I,I2);" ";
	IF P2%(I)=I2 AND S6%(I,0)-S6%(I4,0)>0 THEN 7346

7316 GOSUB 7348:IF P2%(I)=I2 AND S6%(I,0)-S6%(I4,0)<0 THEN 7347

7317 IF P8%(I,I2,19)>0 THEN PRINT "(BS,";:PRINT USING "##";P5%(I,I2,19);:PRINT ")";

7318 IF P8%(I,I2,22)>0 THEN PRINT "(H,";:PRINT USING "##";P5%(I,I2,22);:PRINT ")";

7319 LOCATE,29:PRINT USING "##";INT(P8%(I,I2,0)/3);:IF P8%(I,I2,0)-(INT(P8%(I,I2,0)/3)*3)>0 THEN PRINT ".";:PRINT USING "#";P8%(I,I2,0)-(INT(P8%(I,I2,0)/3)*3);

7320 LOCATE,34:PRINT P8%(I,I2,1);:LOCATE,37:PRINT P8%(I,I2,2);:LOCATE,40:PRINT P8%(I,I2,3);:LOCATE,43:PRINT P8%(I,I2,4);:LOCATE,46:PRINT P8%(I,I2,5):FOR I4=0 TO 5:T3%(I4)=T3%(I4)+P8%(I,I2,I4):NEXT I4

7321 NEXT I2

7322 NEXT I1:PRINT "TOTALS";:LOCATE,29:IF (T3%(0)/3)<10 THEN PRINT INT(T3%(0)/3); ELSE LOCATE (P6%(I)+2),28:PRINT INT(T3%(0)/3);

7323 IF T3%(0)-(INT(T3%(0)/3)*3)>0 THEN PRINT ".";:PRINT USING "#";T3%(0)-(INT(T3%(0)/3)*3);

7324 LOCATE, 34:PRINT T3%(1);:LOCATE, 37:PRINT T3%(2);:LOCATE, 40:PRINT T3%(3);:LOCATE, 43:PRINT T3%(4);:LOCATE, 46:PRINT T3%(5):PRINT:GOSUB 7344:GOSUB 7342

7325 LYN%=CINT(T3%(0)/3):FOR I1=0 TO 5:T3%(I1)=0:NEXT I1

7326 NEXT I

7327 GOSUB 7340:CLS:GOSUB 7375:GOSUB 7227

7328 CLS:J=1:GOSUB 850:LOCATE 9,28:PRINT "DISPLAY STATS AGAIN (YN)";:COLOR 15,0:IF AP=1 AND U6=2 THEN 7331

7329 I$=GetKeyPress$

7330 IF UCASE$(I$)="Y" THEN 7293

7331 GOSUB 7408

7332 CLS:J=1:GOSUB 850:LOCATE 9,29:PRINT "PLAY ANOTHER GAME (YN)":COLOR 15,0

7333 I$=GetKeyPress$
7334 IF UCASE$(I$)<>"Y" THEN CLS:END

7335 CLS:J=2:GOSUB 850

7336 RUN "HELLO.EXE"

7340 IF X=1 THEN X=0

7341 RETURN

7342 IF X=0 THEN CLS

7343 RETURN

7344 IF X=0 THEN GOSUB 7227

7345 RETURN

7346 PRINT "(W,";RIGHT$(STR$(P5%(I,I2,6)+1),(LEN(STR$(P5%(I,I2,6)+1))-1));"-";RIGHT$(STR$(P5%(I,I2,7)),(LEN(STR$(P5%(I,I2,7)))-1));")";:W$=P$(I,I2):GOTO 7316

7347 PRINT "(L,";RIGHT$(STR$(P5%(I,I2,6)+0),(LEN(STR$(P5%(I,I2,6)+0))-1));"-";RIGHT$(STR$(P5%(I,I2,7)+1),(LEN(STR$(P5%(I,I2,7)+1))-1));")";:L$=P$(I,I2):GOTO 7317

7348 IF S1%(I)=I2 AND S6%(I,0)-S6%(I4,0)>0 THEN PRINT "(S,";RIGHT$(STR$(P5%(I,I2,12)+1),(LEN(STR$(P5%(I,I2,12)+1))-1));")";

7349 RETURN
DSKBOX:
	IF S1%(I)=I2 AND S6%(I,0)-S6%(I4,0)>0 THEN PRINT #2,"(S,";RIGHT$(STR$(P5%(I,I2,12)+1),(LEN(STR$(P5%(I,I2,12)+1))-1));")";
	RETURN

7352 IF X0%(I,0,I3)=-1 THEN 7302

7353 IF X0%(I,1,I3)=1 THEN 7302

7354 I1=X0%(I,0,I3):GOTO 7298

7358 IF X0%(I,0,I3)=-1 THEN 7127

7359 IF X0%(I,1,I3)=1 THEN GOTO 7127

7360 I1=X0%(I,0,I3):GOTO 7124

7375 PRINT " E-";

7376 IF E=0 THEN PRINT"none":GOTO 7378

7377 FOR X=0 TO (E-1):PRINT E$(X);",";:NEXT:PRINT

7378 PRINT "2B-";

7379 IF DB=0 THEN PRINT"none":GOTO 7381

7380 FOR X=0 TO (DB-1):PRINT DB$(X);",";:NEXT:PRINT

7381 PRINT "3B-";

7382 IF TR=0 THEN PRINT"none":GOTO 7384

7383 FOR X=0 TO (TR-1):PRINT TR$(X);",";:NEXT:PRINT

7384 PRINT "HR-";

7385 IF HR=0 THEN PRINT"none":GOTO 7387

7386 FOR X=0 TO (HR-1):PRINT HR$(X);",";:NEXT:PRINT

7387 PRINT "SB-";

7388 IF SB=0 THEN PRINT"none":GOTO 7390

7389 FOR X=0 TO (SB-1):PRINT SB$(X);",";:NEXT:PRINT

7390 PRINT "LOB-";A$(0);STR$(L0%(0));","A$(1);STR$(L0%(1))

7391 PRINT "DP-";A$(0);STR$(DP%(0));","A$(1);STR$(DP%(1))
	PRINT "INJURIES-";
	IF IJ%=0 THEN PRINT "none":goto eject
	FOR X=1 TO IJ%:PRINT IJ$(X);",";:NEXT:PRINT
eject:
	PRINT "EJECTIONS-";
	IF EJ%=0 THEN PRINT "none":goto 7392
	FOR X=1 TO EJ%:PRINT EJ$(X);",";:NEXT:PRINT

7392 PRINT"UMPIRES: HP-";U$(1);"; 1B-";U$(2);"; 2B-";U$(3);"; 3B-";U$(4):PRINT "T: ";GT$;" A: ";ATT
	PRINT
	PRINT "TEMPERATURE:"TP%" WIND: ";
	IF WS%=0 THEN PRINT "NONE "
	IF WS%=2 THEN PRINT "LIGHT "
	IF WS%=3 THEN PRINT "MODERATE "
	IF WS%>5 THEN PRINT "BLOWING OUT AT "WS%" MPH"
	IF WS%<0 THEN PRINT "BLOWING IN AT "WS%*-1" MPH"
	PRINT "SKY: "SC$(SC%)
	PRINT "PRECIPITATION: "PC$(PC%)

7393 RETURN

7408 CLS
	CLS:J=1:GOSUB 850:LOCATE 9,28:PRINT "SAVE STATS FOR EITHER TEAM (YN)";:COLOR 15,0:IF AP=1 THEN BG%=0:BN%=1:GOTO 7411

7410 I$=GetKeyPress$
	IF UCASE$(I$)<>"Y" THEN X1=3:RETURN
	CLS:COLOR 12:PRINT TAB(33); "COMPILE STATS":PRINT
	COLOR 14:PRINT "(0) ";:COLOR 15:PRINT "BOTH TEAMS":PRINT
	COLOR 14:PRINT "(1) ";:COLOR L%(0,11),L%(0,12):PRINT A$(0)" ONLY":COLOR ,0:PRINT
	COLOR 14:PRINT "(2) ";:COLOR L%(1,11),L%(1,12):PRINT A$(1)" ONLY":COLOR ,0:PRINT

7409 I$=GetKeyPress$
	X1=VAL(I$)
	IF X1<0 OR X1>2 THEN 7409

7411 SX%(0)=-1:SX%(1)=-1
	FOR PX=0 TO 1
	FOR I=0 TO 21
	IF P%(PX,I,15)=1 THEN SX%(PX)=I:GOTO 7395
	NEXT I

7395 NEXT PX:GOTO 7600

7491 CLS:PRINT "----------------------------------------":print
	IF I%=10 AND P=0 OR I%<=9 THEN GOSUB 7495

7492 IF I%=10 AND P=1 OR I%>10 AND I%<19 OR I%=19 AND P=0 THEN GOSUB 7504

7493 IF I%=19 AND P=1 OR I%>19 AND I%<= 28 THEN GOSUB 7512

7494 RETURN

7495 FOR I=0 TO 1:PRINT A$(I);:FOR I2=1 TO 9:GOSUB 7522:IF I=0 THEN PRINT USING "##";S%(I,I2);

7496 IF I=1 AND S6%(1,0)>S6%(0,0) AND I2=9 AND S%(1,9)=0 THEN PRINT " X";

7497 IF I=1 AND S6%(1,0)<S6%(0,0) AND I2=9 OR I=1 AND S6%(1,0)>S6%(0,0) AND I2=9 AND S%(1,9) > 0 THEN  PRINT USING "##";S%(I,I2);

7498 IF I=1 AND I2<9 THEN PRINT USING "##";S%(I,I2);

7499 NEXT I2

7500 GOSUB 7532:NEXT I

7501 PRINT "----------------------------------------":print

7502 REM

7503 RETURN

7504 FOR I=0 TO 1:PRINT A$(I);:FOR I2=1 TO 9:GOSUB 7522:PRINT USING "##";S%(I,I2);

7505 NEXT I2:PRINT:NEXT I

7506 PRINT "----------------------------------------":print:IF P=0 AND I%<19 OR I%=19 THEN I%=I%-1

7507 FOR I=0 TO 1:FOR I2=10 TO I%:GOSUB 7522:PRINT USING "##";S%(I,I2);

7508 NEXT I2

7509 GOSUB 7532

7510 NEXT I

7511 PRINT "----------------------------------------":print:RETURN

7512 FOR I=0 TO 1:PRINT A$(I);:FOR I2=1 TO 9:GOSUB 7522:PRINT USING "##";S%(I,I2);

7513 NEXT I2:PRINT:NEXT I

7514 PRINT "----------------------------------------":print:IF I%=28 OR I%<28 AND P=0 THEN I%=I%-1

7515 FOR I=0 TO 1:FOR I2=10 TO 18:GOSUB 7522:PRINT USING "##";S%(I,I2);

7516 NEXT I2:PRINT:NEXT I

7517 PRINT "----------------------------------------":print

7518 FOR I=0 TO 1:FOR I2=19 TO I%:GOSUB 7522:PRINT USING "##";S%(I,I2);:NEXT I2

7519 GOSUB 7532

7520 NEXT I

7521 PRINT "----------------------------------------":print:RETURN

7522 IF I2<4 THEN LOCATE,((2*I2)+11)

7523 IF I2>3 AND I2<7 THEN LOCATE,((2*I2)+12)

7524 IF I2>6 AND I2<10 THEN LOCATE,((2*I2)+13)

7525 IF I2>9 AND I2<13 THEN LOCATE,((2*(I2-9))+11)

7526 IF I2>12 AND I2<16 THEN LOCATE,((2*(I2-9))+12)

7527 IF I2>15 AND I2<19 THEN LOCATE,((2*(I2-9))+13)

7528 IF I2>18 AND I2<22 THEN LOCATE,((2*(I2-18))+11)

7529 IF I2>21 AND I2<25 THEN LOCATE,((2*(I2-18))+12)

7530 IF I2>24 AND I2<28 THEN LOCATE,((2*(I2-18))+13)

7531 RETURN

7532 LOCATE, 33:PRINT "-";:LOCATE, 34:PRINT USING "##";S6%(I,0);

7533 LOCATE, 37:PRINT USING "##";S6%(I,1);:LOCATE,40:PRINT USING "#";S6%(I,2):RETURN

7580 OPEN "TEMPBOX" FOR INPUT AS #2

7581 LINE INPUT #2,X$
	IF LEFT$(X$,1)=CHR$(12) THEN LPRINT X$:CLOSE #2:GOTO 7328
	LPRINT X$:GOTO 7581

7600 ON ERROR GOTO 7630
OPEN "GAME82" FOR INPUT AS #2
	INPUT #2,FF$
	FOR I=0 TO 2:INPUT #2,AP%(I):NEXT:CLOSE 2
OPEN FF$+".SCD" AS #1 LEN=48
	FOR X=0 TO 18
	FIELD #1,X*2 AS X$,2 AS Q$(X+1),48-2-2*X AS X$
	NEXT
	FIELD #1,38 AS X$,2 AS Q$(22),2 AS Q$(23),3 AS Q$(20),3 AS Q$(21)
	NG%(13)=1
	FOR X=0 TO 18:LSET Q$(X+1)=MKI$(NG%(X)):NEXT
	FOR X=0 TO 1:LSET Q$(20+X)=YN$(X):NEXT
	LSET Q$(22)=MKI$(S6%(0,0)):LSET Q$(23)=MKI$(S6%(1,0))
	PUT #1,AP%(0)+AP%(2)
CLOSE 1
	AP%(2)=AP%(2)+1

7615 AP=1:IF AP%(1)<AP%(0)+AP%(2) THEN KILL"GAME82":NM=1:GOTO 7665

7620 OPEN "GAME82" FOR OUTPUT AS #2
	PRINT #2,FF$
	FOR I=0 TO 2:PRINT #2,AP%(I):NEXT:CLOSE 2

7625 GOTO 7665

7630 RESUME 7665

7665 ON ERROR GOTO 0:CLS

7660 Q=0
	W2%=0:L2%=0

7662 C%=0:GOSUB 7834:IF C%=1 THEN 7770

7664 SA$(Q)=A$(Q):GOSUB 7862:F$=MID$(SA$(Q),4,QQ):R$=F$+"."+LEFT$(SA$(Q),2)

7666 ON ERROR GOTO 7684

7668 OPEN D$(2)+R$ FOR INPUT AS #1

7670 INPUT #1,A$(Q):INPUT #1,W2%:INPUT #1,L2%

7672 FOR I=0 TO 22:INPUT #1,B$(Q,I):INPUT #1,B2%(Q,I):NEXT I

7674 FOR I=0 TO 21:INPUT #1,P$(Q,I):INPUT #1,P3%(Q,I):NEXT I

7676 FOR I=0 TO 22:FOR I1=0 TO 21:INPUT #1,B6%(I,I1):NEXT I1:NEXT I

7678 FOR I=0 TO 21:FOR I1=0 TO 41:INPUT #1,P0%(I,I1):NEXT I1:NEXT I

7680 INPUT #1,D0%(Q):INPUT #1,D1%(Q): FOR I=0 TO 21:INPUT #1,T%(I):NEXT I:FOR I=0 TO 22:INPUT #1,P4%(I):NEXT I

7682 CLOSE #1:GOTO 7690

7684 CLOSE #1

7686 RESUME 7688

7688 ON ERROR GOTO 0

7690 FOR I=0 TO 22
	B2%(Q,I)=B%(Q,I,4)
	FOR I1=0 TO 11

7692 B6%(I,I1)=B6%(I,I1)+B8%(Q,I,I1):T%(I1)=T%(I1)+B8%(Q,I,I1)
	NEXT I1

7694 IF B8%(Q,I,0)>0 AND B8%(Q,I,1)=0 THEN B6%(I,12)=0:GOTO 7698

7696 IF B8%(Q,I,0)>0 AND B8%(Q,I,1)>0 THEN B6%(I,12)=B6%(I,12)+1

7698 IF B8%(Q,I,14)>0 THEN B6%(I,14)=B6%(I,14)+1

7700 FOR I1=15 TO 21
	B6%(I,I1)=B6%(I,I1)+B8%(Q,I,I1):T%(I1)=T%(I1)+B8%(Q,I,I1)
	NEXT I1

7702 IF B6%(I,12)>=B6%(I,13) THEN B6%(I,13)=B6%(I,12)

7704 NEXT I
	FOR I=0 TO 21
	FOR I1=23 TO 34
	P0%(I,I1)=P0%(I,I1)+P8%(Q,I,I1):T%(I1-23)=T%(I1-23)+P8%(Q,I,I1)
	NEXT I1
	IF P8%(Q,I,23)>0 AND P8%(Q,I,24)=0 THEN P0%(I,35)=0:GOTO 7705
	IF P8%(Q,I,23)>0 AND P8%(Q,I,24)>0 THEN P0%(I,35)=P0%(I,35)+1

7705 FOR I1=37 TO 41
	P0%(I,I1)=P0%(I,I1)+P8%(Q,I,I1):T%(I1-22)=T%(I1-22)+P8%(Q,I,I1)
	NEXT I1
	IF P0%(I,35)>=P0%(I,36) THEN P0%(I,36)=P0%(I,35)
	NEXT I
	T%(12)=T%(12)+L0%(Q)

7706 I%=0

7708 FOR I=0 TO 21
	I%=I%+P8%(Q,I,0)
	P3%(Q,I)=P%(Q,I,6)

7710 FOR I1=0 TO 5

7712 P0%(I,I1)=P0%(I,I1)+P8%(Q,I,I1):P4%(I1)=P4%(I1)+P8%(Q,I,I1)

7714 NEXT I1

7716 FOR I1=13 TO 22:P0%(I,I1)=P0%(I,I1)+P8%(Q,I,I1):P4%(I1)=P4%(I1)+P8%(Q,I,I1)

7718 NEXT I1

7722 NEXT I

7724 IF S6%(Q,0)-S6%(1-Q,0)>0 THEN P0%(P2%(Q),6)=P0%(P2%(Q),6)+1

7726 IF S6%(Q,0)-S6%(1-Q,0)<0 THEN P0%(P2%(Q),7)=P0%(P2%(Q),7)+1

7728 IF S6%(Q,0)-S6%(1-Q,0)>0 AND S1%(Q)>=0 THEN P0%(S1%(Q),12)=P0%(S1%(Q),12)+1:P4%(12)=P4%(12)+1

7730 FOR I=0 TO 21
	'IF P8%(Q,I,0)+P8%(Q,I,1)+P8%(Q,I,2)+P8%(Q,I,3)+P8%(Q,I,4)+P8%(Q,I,5)>0 THEN P0%(I,8)=P0%(I,8)+1:P4%(8)=P4%(8)+1
	IF P%(Q,I,15)<>0 THEN P0%(I,8)=P0%(I,8)+1:P4%(8)=P4%(8)+1

7732 IF I%=P8%(Q,I,0) THEN P0%(I,10)=P0%(I,10)+1:P4%(10)=P4%(10)+1

7734 IF I%=P8%(Q,I,0) AND S6%(1-Q,0)=0 THEN P0%(I,11)= P0%(I,11)+1:P4%(11)=P4%(11)+1

7736 NEXT I

7738 P0%(SX%(Q),9)=P0%(SX%(Q),9)+1:P4%(9)=P4%(9)+1

7740 D0%(Q)=D0%(Q)+D3%(Q):D1%(Q)=D1%(Q)+D3%(1-Q)

7742 IF S6%(Q,0)-S6%(1-Q,0)>0 THEN W2%=W2%+1

7744 IF S6%(Q,0)-S6%(1-Q,0)<0 THEN L2%=L2%+1

7746 PRINT ">>>>>":PRINT:PRINT:PRINT "THE STATS ARE BEING SAVED UNDER":PRINT "THE FOLLOWING NAME ::: ";R$

7748 OPEN D$(2)+R$ FOR OUTPUT AS #1

7750 PRINT #1,A$(Q):PRINT #1,W2%:PRINT #1,L2%

7752 FOR I=0 TO 22:PRINT #1,B$(Q,I):PRINT #1,B2%(Q,I):NEXT I

7754 FOR I=0 TO 21:PRINT #1,P$(Q,I):PRINT #1,P3%(Q,I):NEXT I

7756 FOR I=0 TO 22:FOR I1=0 TO 21:PRINT #1,B6%(I,I1):NEXT I1:NEXT I

7758 FOR I=0 TO 21:FOR I1=0 TO 41:PRINT #1,P0%(I,I1):NEXT I1:NEXT I

7760 PRINT #1,D0%(Q): PRINT #1,D1%(Q)
	FOR I=0 TO 21:PRINT #1,T%(I):NEXT I
	FOR I=0 TO 22:PRINT #1,P4%(I):NEXT I

7762 CLOSE #1:GOTO 7766

7764 PRINT "<<<<<"

7766 GOSUB 7780

7768 GOSUB 7850:GOSUB 7856

7770 Q=Q+1:IF Q<2 THEN 7662

7772 IF AP=1 AND NM<>1 THEN RUN "LOADER.TBC"

7774 RUN "HELLO.EXE"

7780 Y%=0:ON ERROR GOTO 7800

7782 OPEN D$(2)+R$+"S" FOR INPUT AS #1:INPUT #1,Y%

7784 FOR I=1 TO 162:INPUT #1,H%(I):NEXT I

7786 FOR I=1 TO 162:INPUT #1,HT$(I):NEXT I

7788 FOR I=1 TO 162:INPUT #1,V%(I):NEXT I

7790 FOR I=1 TO 162:INPUT #1,PL$(I):NEXT I

7792 FOR I=1 TO 162:INPUT #1,W$(I):NEXT I

7794 FOR I=1 TO 162:INPUT #1,L$(I):NEXT I

7796 FOR I=1 TO 162:INPUT #1,ING%(I):NEXT I

7798 CLOSE #1:GOTO 7806

7800 CLOSE #1:FOR I=1 TO 162:H%(I)=999:HT$(I)="XXXXXXX":V%(I)=999:PL$(I)="X":W$(I)="LYNN":L$(I)="SUMLER":ING%(I)=999:NEXT I

7802 RESUME 7804

7804 ON ERROR GOTO 0

7806 Y%=Y%+1

7808 H%(Y%)=S6%(Q,0):HT$(Y%)=A$(1-Q):V%(Y%)=S6%(1-Q,0):W$(Y%)=W$:L$(Y%)=L$:ING%(Y%)=LYN%

7810 IF Q=0 THEN PL$(Y%)="A":GOTO 7814

7812 PL$(Y%)="H"

7814 REM

7816 OPEN D$(2)+R$+"S" FOR OUTPUT AS #1:PRINT #1,Y%

7818 FOR I=1 TO 162:PRINT #1,H%(I):NEXT I

7820 FOR I=1 TO 162:PRINT #1,HT$(I):NEXT I

7822 FOR I=1 TO 162:PRINT #1,V%(I):NEXT I

7824 FOR I=1 TO 162:PRINT #1,PL$(I):NEXT I

7826 FOR I=1 TO 162:PRINT #1,W$(I):NEXT I

7828 FOR I=1 TO 162:PRINT #1,L$(I):NEXT I

7830 FOR I=1 TO 162:PRINT #1,ING%(I):NEXT I

7832 CLOSE #1:Y%=0:RETURN

7834 IF AP=1 THEN C%=0:GOTO 7840
	IF X1=0 OR X1=1 AND Q=0 OR X1=2 AND Q=1 THEN 7840
	IF X1=2 AND Q=0 OR X1=1 AND Q=1 THEN C%=1:GOTO 7840

7840 RETURN

7850 FOR I=0 TO 22:FOR I1=0 TO 21:B6%(I,I1)=0:NEXT I1:NEXT I

7852 FOR I=0 TO 21:FOR I1=0 TO 41:P0%(I,I1)=0:NEXT I1:NEXT I

7854 W2%=0:L2%=0:FOR I=0 TO 21:T%(I)=0:NEXT I:FOR I=0 TO 22:P4%(I)=0:NEXT I:RETURN

7856 FOR I=0 TO 1:FOR I1=0 TO 22:B2%(I,I1)=0:NEXT I1:NEXT I

7858 FOR I=0 TO 1:FOR I1=0 TO 21:P3%(I,I1)=0:NEXT I1:NEXT I

7860 RETURN

7862 FOR XX=4 TO 11:IF MID$(SA$(Q),XX,1)=" " OR MID$(SA$(Q),XX,1)="." THEN MID$(SA$(Q),XX,1)="_"

7864 NEXT XX

7866 QQ=8:FOR XX=11 TO 4 STEP -1:IF MID$(SA$(Q),XX,1)="_" THEN QQ=XX-4:GOTO 7870

7868 GOTO 7872

7870 NEXT XX

7872 RETURN

8000 IF B7%(P,B1(P))=1 THEN Q$(0)=P$(P,P1%(P)):GOTO 8002

8001 Q$(0)=B$(P,B3%(P,B1(P)))

8002 FOR I=1 TO 3
	IF B0%(I,0)>0 AND B7%(P,B0%(I,0))<>1 THEN Q$(I)=B$(P,B3%(P,B0%(I,0)))

8003 IF B0%(I,0)>0 AND B7%(P,B0%(I,0))=1 THEN Q$(I)=P$(P,P1%(P))

8004 NEXT I:RETURN

8006 B8%(P,I5,I6)=B8%(P,I5,I6)-1:B5%(P,I5,I6)=B5%(P,I5,I6)-1:RETURN

8007 P8%(P,I5,I6)=P8%(P,I5,I6)-1:P5%(P,I5,I6)=P5%(P,I5,I6)-1:RETURN

8008 B8%(D,I5,I6)=B8%(D,I5,I6)+1:B5%(D,I5,I6)=B5%(D,I5,I6)+1:RETURN

8009 P8%(P,I5,I6)=P8%(P,I5,I6)+1:P5%(P,I5,I6)=P5%(P,I5,I6)+1:RETURN

8010 B8%(P,I5,I6)=B8%(P,I5,I6)+1:B5%(P,I5,I6)=B5%(P,I5,I6)+1:RETURN

8011 '*** play by play for a single s2%=7, s2%=12 ***

8012 IF S2%=12 THEN PRINT q$(0)" beats out an infield single":GOTO 8013

8014 GOSUB 8060
	GOSUB 275
	if rn<=85 then y%=int(rnd(1)*13)+591:GOSUB 3990:GOSUB 9800:print y$" "c$(w%):goto 8013
	IF W%=7 THEN y%=int(rnd(1)*10)+606
	IF W%=9 THEN y%=int(rnd(1)*10)+616
	if W%=8 THEN y%=int(rnd(1)*10)+626
	GOSUB 3990:GOSUB 9800
	print y$

8013 GOSUB 6095:RETURN

8016 bv=INT(RND(1)*5)+1

8043 if bv=1 then X$=" rips"

8044 if bv=2 then X$=" lashes"

8045 if bv=3 then X$=" lines"

8046 if bv=4 then X$=" grounds"

8047 if bv=5 then X$=" drives"

8017 PRINT X$" ";:return

8020 P8%(D,I5,I6)=P8%(D,I5,I6)+1:P5%(D,I5,I6)=P5%(D,I5,I6)+1:RETURN

8021 P8%(P9,I5,I6)=P8%(P9,I5,I6)+1:P5%(P9,I5,I6)=P5%(P9,I5,I6)+1:RETURN

8030 y%=int(rnd(1)*18)+391:gosub 3990:GOSUB 9800
	PRINT Y$" "C$(W%):GOSUB 6095
	if wf<1 and rnd(1)>wf then print "The wind is bringing it back...":gosub 6095:return
	if wf>1 and rnd(1)<=1-wf then print "It's carrying well in the breeze...":gosub 6095
	y%=int(rnd(1)*17)+371:gosub 3990:GOSUB 9800
	PRINT Y$

8050 gosub 6095
	gosub 275
	if L%(1,13)=21 or l%(1,13)=27 and w%=7 then 8059
	if rn<=10 then print q$(4)" leaps"
	if rn>10 and rn<=20 then print q$(4)" jumps at the fence"
	if rn>20 and rn<=30 then print q$(4)" climbs the fence"

8059 gosub 6095:RETURN

8060 '*** setup the delivery ***
	y%=int(rnd(1)*25)+411:gosub 3990:GOSUB 9800
	PRINT Y$

8070 GOSUB 6095:RETURN

8099 '

8100 IF ZB%(D,0)=-1 THEN RETURN

8105 IF I%>=ZB%(D,5) THEN 8150

8110 FOR I=1 TO 3

8111 IF ZB%(D,I)<>-1 THEN V0=ZB%(D,I):ZB%(D,I)=-1:I=V0:GOTO 8890

8112 NEXT

8113 RETURN

8150 IF ZB%(D,4)=-1 THEN 8110

8151 A1=S6%(D,0)-S6%(P,0)

8155 ON ZB%(D,6) GOTO 8160,8165,8170,8175,8180,8185

8160 IF A1>0 AND A1<=2 THEN V0=ZB%(D,4):ZB%(D,4)=-1:I=V0:GOTO 8890

8161 RETURN

8165 IF A1>0 THEN V0=ZB%(D,4):ZB%(D,4)=-1:I=V0:GOTO 8890

8166 RETURN

8170 IF A1>=0 THEN V0=ZB%(D,4):ZB%(D,4)=-1:I=V0:GOTO 8890

8171 RETURN

8175 IF S8%(D,0)<=P8%(D,P1%(D),4)+P8%(D,P1%(D),1) AND S8%(D,1)<=P8%(D,P1%(D),2) AND A1>0 THEN V0=ZB%(D,4):ZB%(D,4)=-1:I=V0:GOTO 8890

8176 RETURN

8180 IF S8%(D,0)<=P8%(D,P1%(D),4)+P8%(D,P1%(D),1) AND S8%(D,1)<=P8%(D,P1%(D),2) AND A1>=0 THEN V0=ZB%(D,4):ZB%(D,4)=-1:I=V0:GOTO 8890

8181 RETURN

8185 IF A1<1 THEN RETURN

8186 IF S4%=0 AND A1<=2 THEN 8192

8187 IF S4%>0 AND S4%<4 AND A1<=3 THEN 8192

8188 IF S4%>3 AND S4%<7 AND A1<=4 THEN 8192

8189 IF S4%=7 AND A1<=5 THEN 8192

8190 IF S4%=0 AND O%(0)=0 AND A1<=3 THEN 8192

8191 GOTO 8195

8192 V0=ZB%(D,4):ZB%(D,4)=-1:I=V0:GOTO 8890

8195 RETURN

8500 LOCATE 22,1
	IF S6%(0,0)>S6%(1,0) THEN PRINT A$(0);" defeats ";A$(1);S6%(0,0);"TO";S6%(1,0)"!" ELSE PRINT A$(1);" defeats ";A$(0);S6%(1,0);"TO";S6%(0,0)"!"

8501 GOSUB 8600:GOTO 7105

8600 IF AP=1 AND U6=2 THEN 8607

8605 LOCATE 24,1:PRINT "PRESS ANY KEY TO CONTINUE";

8606 I$=GetKeyPress$
8607 RETURN

8700 H0%=0:GOSUB 327

8705 A1=S6%(D,0)-S6%(P,0)
	IF A1>1 OR S4%=3 OR S4%>=6 THEN 8800

8707 IF S4%<>1 AND S4%<>5 THEN I2=2:GOTO 8800

8708 I2=1

8709 J=B0%(I2,0):IF J=0 THEN 8800

8711 IF B7%(P,J)=1 THEN 8800

8712 IF B7%(P,J)<>1 AND B%(P,B3%(P,J),13)>=10 THEN H0%=1:GOSUB 327
	GOTO 8800

8720 GOSUB 275
	IF I%<9 OR ZB%(D,0)<>-1 OR CZ%(D)=-1 THEN 8725
	CZ%(D)=-1
	IF A1>3 OR A1<=0 THEN 8725
	IF P6%(D)<>1 THEN 8730
	IF S6%(P,0)=0 AND P%(D,P1%(D),5)>0 AND RN<=INT(P%(D,P1%(D),16)/P%(D,P1%(D),5)*100+.5)+10 THEN 8725
	IF P%(D,P1%(D),5)>0 AND RN<=INT(P%(D,P1%(D),16)/P%(D,P1%(D),5)*100+.5) THEN 8725
	GK%(D)=1

8725 RETURN

8730 GOSUB 275
	IF RN<=80 AND P%(D,P1%(D),3)<15 THEN GK%(D)=1
	GOTO 8725

8800 IF ZB%(D,4)<>-1 AND I%>=ZB%(D,5) THEN GOSUB 8150
	A1=S6%(D,0)-S6%(P,0)
	GOSUB 8720
	IF GK%(D)=1 THEN GK%(D)=0:GOTO 8860

8801 D2=1:IF I5%>0 THEN 8820

8802 A1=S6%(D,0)-S6%(P,0)
	IF A1>=-1 AND A1<=1 AND I%>=8 AND (S4%=3 OR S4%>=5) AND O%(0)<2 THEN 8809

8803 IF A1>0 OR O%(0)>0 OR S4%<=2 OR S4%=4 OR S4%=5 AND O%(0)=1 OR I%<=5 AND A1<-2 OR I%>5 AND A1<-4 THEN 8810

8809 I5%=2:D2=3:GOTO 5055

8810 IF S4%=0 OR S4%=2 OR S4%=3 OR S4%>= 5 OR O%(0)=2 OR B1(P)>=3 AND B1(P)<=6 OR I%<=6 AND A1>=3 OR I%<=6 AND A1<=-3 THEN 8820

8811 IF I%>7 AND A1>=1 OR I%>7 AND A1<-1 THEN 8820

8812 I5%=1:D2=4:GOTO 5055

8820 IF A1<=0 OR A1=0 AND S4%<=1 OR A1=0 AND S4%=4 OR S4%=7 OR B7%(P,B1(P))=1 THEN 8850

8821 IF I%>=9 AND P=1 AND O%(0)<2 AND A1=0 AND S4%>=2 AND S4%<=3 OR I%>=9 AND P=1 AND O%(0)=2 AND A1=0 AND S4%>=5 AND S4%<=6 THEN 8835

8828 I=B4%(P,0,B1(P)):I1=B4%(P,3,B1(P)):IF B1(P)=9 THEN I2=B4%(P,0,0):I3=B4%(P,3,0)

8829 IF B1(P)<9 THEN I2=B4%(P,0,B1(P)+1):I3=B4%(P,3,B1(P)+1)

8830 IF S4%<>2 AND S4%<>6 THEN 8850

8832 IF I-I2>=50 AND I1-I3>=50 AND A1<1 AND I%>8 OR I>=280 AND I1>=140 AND I%>8 AND I1>I3 AND A1<1 THEN 8835

8833 'IF I-I2>=50 AND I1-I3>=50 OR I>=280 AND I1>=140 AND I1>I3 THEN 8835

8834 GOTO 8850

8835 D2=2:GOTO 5058

8850 IF S8%(D,0)<=P8%(D,P1%(D),4)+P8%(D,P1%(D),1) AND S8%(D,1)<=P8%(D,P1%(D),2) THEN 8860

8851 IF B9%(D,1)=99 THEN 8860

8852 GOTO 5055

8860 GOSUB 8100
	I2=0
	A1=S6%(D,0)-S6%(P,0)
	FOR I=0 TO 21
	IF P%(D,I,15)>0 OR P$(D,I)="XXX" THEN 8874
	A2=P%(D,I,4)-P%(D,I,5)
	IF I%<=7 AND A2>0 AND P%(D,I,3)<10 THEN I2=I2+A2
	IF I%>7 AND A1<0 AND P%(D,I,3)<10 AND A2>0 THEN I2=I2+A2
	IF I%>7 AND A1>=0 THEN I2=I2+P%(D,I,3)

8874 NEXT I
	I3=INT(RND(1)*I2)+1:I2=0
	FOR I=0 TO 21
	A2=P%(D,I,4)-P%(D,I,5)
	IF P%(D,I,15)> 0 OR P$(D,I)="XXX" OR A2=0 THEN 8883
	IF I%<=7 AND A2>0 AND P%(D,I,3)<10 AND I3>I2 AND I3<=I2+A2 THEN 8890
	IF I%<=7 AND A2>0 AND P%(D,I,3)<10 THEN I2=I2+A2:GOTO 8883
	IF I%>7 AND A1<0 AND P%(D,I,3)<10 AND A2>0 AND I3>I2 AND I3<=I2+A2 THEN 8890
	IF I%>7 AND A1<0 AND P%(D,I,3)<10 AND A2>0 THEN I2=I2+A2:GOTO 8883
	IF I%>7 AND A1>=0 AND I3>I2 AND I3<=I2+P%(D,I,3) THEN 8890
	IF I%>7 AND A1>=0 THEN I2=I2+P%(D,I,3)

8883 NEXT I
	FOR I=0 TO 21
	IF P%(D,I,15)=0 AND P$(D,I)<>"XXX" THEN 8890

8885 NEXT I

8886 if ep=1 then return

8887 GOTO 5055

8890 V0=I
	IF S1%(D)<>-1 AND P6%(D)>1 THEN I5=P1%(D):I6=22:GOSUB 8020
	P1%(D)=V0:P6%(D)=P6%(D)+1:P%(D,V0,15)=P6%(D):P9=D:GOSUB 135

8891 FOR V0=1 TO 9
	IF B7%(D,V0)=1 OR B7%(D,V0)=10 THEN B3%(D,V0)=P1%(D):B7%(D,V0)=1:B9%(D,1)=P1%(D):P9=D:W5=P9:W6=P9:V(P9)=V0:V1(P9)=V0:GOSUB 220

8892 NEXT V0
	B9%(D,1)=P1%(D):GOSUB 1800

8893 W5=1-P9:W6=1-P9:V(1-P9)=1:V1(1-P9)=9
	GOSUB 4095:LOCATE 15,1
	gosub 275
	if rn<=50 then print "Hold the phone...":gosub 6095
	print M$(D);" is making a pitching change"
	GOSUB 6095:GOSUB 6095

8894 GOSUB 220:GOSUB 300:GOSUB 330:PPH=0

8895 if ep=1 then return

8896 GOTO 5055

8900 JM=B3%(P,B1(P)):IF PPH=1 THEN 8905

8901 A1=S6%(P,0)-S6%(D,0):IF B7%(P,B1(P))<> 1 THEN 8906

8902 FOR I=0 TO 21
	IF P%(P,I,15)=0 THEN 8903
	NEXT
	GOTO 8941

8903 IF I%>4 AND I%<7 AND A1<-2 AND S6%(P,0)>0 THEN 8930
	IF I%>4 AND I%<7 AND A1<-3 THEN 8930
	IF I%>6 AND A1<0 OR A1=0 AND I%>6 AND I%<9 AND S4%>1 THEN 8930
	IF A1=0 AND I%>=9 AND S4%>0 OR A1=0 AND I%>=9 AND O%(0)=0 THEN 8930
	IF P%(P,P1%(P),6)/P%(P,P1%(P),4)<=2 AND P8%(P,P1%(P),0)>=6 THEN 8930

8905 GOTO 8941

8906 IF B%(P,JM,6)/B%(P,JM,4)*1000>233 OR B%(P,JM,9)>=15 THEN 8908

8907 IF I%>6 AND A1<-2 OR A1<=0 AND I%>=8 OR A1=0 AND I%>=9 THEN 8909

8908 GOTO 8941

8909 FOR I=0 TO 22:IF B%(P,I,21)>0 OR B$(P,I)="XXX" THEN 8917

8910 IF B7%(P,B1(P))<>B%(P,I,22) AND B7%(P,B1(P))<>B%(P,I,23) AND B7%(P,B1(P))<>B%(P,I,24) AND B7%(P,B1(P))<>B%(P,I,25) THEN 8917

8911 IF P%(D,P1%(D),0)=1 AND B%(P,I,0)>0 AND B%(P,I,6)/B%(P,I,4)*1000>B%(P,JM,6)/B%(P,JM,4)*1000+35 THEN 8916

8912 IF P%(D,P1%(D),0)=-1 AND B%(P,I,0)<0 AND B%(P,I,6)/B%(P,I,4)*1000>B%(P,JM,6)/B%(P,JM,4)*1000+45 THEN 8916

8913 IF B%(P,I,0)=0 AND B%(P,I,6)/B%(P,I,4)*1000>B%(P,JM,6)/B%(P,JM,4)*1000+30 THEN 8916

8915 GOTO 8917

8916 PPH=1:D0%(P)=D0%(P)+1:X0%(P,0,D0%(P))=I:X0%(P,1,D0%(P))=B7%(P,B1(P)):X0%(P,2,D0%(P))=(B1(P)*10)+(D0%(P)-9):GOTO 8919

8917 NEXT I

8918 GOTO 8925

8919 B%(P,I,21)=1:B3%(P,B1(P))=I:B9%(P,B7%(P,B1(P)))=I
	IF B7%(P,B1(P))=1 OR B7%(P,B1(P))=10 THEN B9%(P,1)=99:B9%(P,0)=B1(P):B7%(P,B1(P))=10
	I7%=P:I8%=B1(P):I9%=B3%(P,B1(P))
	GOSUB 7100

8920 GOTO 8938

8925 GOTO 8941

8930 'GET A PINCH HITTER FOR THE PITCHER
	J%=0:GP%=0
	FOR I=0 TO 22
	IF B$(P,I)="XXX" OR B%(P,I,21)>0 OR B%(P,I,4)=0 THEN 8932
	IF B%(P,I,0)>0 AND P%(D,P1%(D),0)=1 OR B%(P,I,0)<0 AND P%(D,P1%(D),0)=-1 THEN PH%(J%,0)=INT(B%(P,I,3)/4+.5):GOTO 8931
	PH%(J%,0)=B%(P,I,3)

8931 PH%(J%,1)=I:GP%=GP%+PH%(J%,0):J%=J%+1

8932 NEXT
	IF J%=0 THEN 8941
	RN=INT(RND(1)*GP%)+1
	FOR I=0 TO J%-1
	IF RN<=PH%(I,0) THEN PH%=PH%(I,1):GOTO 8934
	PH%(I+1,0)=PH%(I,0)+PH%(I+1,0)
	NEXT:GOTO 8941

8934 I=PH%:PPH=1:D0%(P)=D0%(P)+1
	X0%(P,0,D0%(P))=I
	X0%(P,1,D0%(P))=10
	X0%(P,2,D0%(P))=81+D0%(P)

8937 B%(P,I,21)=1:B3%(P,B1(P))=I
	  IF B7%(P,B1(P))=1 OR B7%(P,B1(P))=10 THEN B9%(P,1)=99:B9%(P,0)=B1(P):B7%(P,B1(P))=10

8938 P9=P:W5=P9:W6=P9:V(P9)=B1(P):V1(P9)=B1(P):GOSUB 220:GOSUB 300:GOSUB 330

8939 Q$(0)=B$(P,B3%(P,B1(P)))
	IF B7%(P,B1(P))=1 THEN Q$(0)=P$(P,P1%(D))

8940 GOSUB 4095:PRINT M$(P);" is sending up a pinch hitter":PPH=1:GOSUB 6095:GOSUB 6095:GOTO 5052

8941 'IF PR%<>1 THEN 8990

8971 IF B7%(P,B1(P))=1 THEN BR=0
	IF B7%(P,B1(P))<>1 THEN BR=B%(P,B3%(P,B1(P)),30)
	GOSUB 275
	IF S4%=0 AND (I5%>0 AND RN<=5 OR I5%=0 AND RN<=11) AND BR>=7 THEN P2=4:GOTO 4100
	P2=1:IF S4%=1 OR S4%=5 THEN 8944

8942 IF S4%=2 OR S4%=4 THEN 8950

8943 GOTO 8960

8944 IF A1<=-1 AND B%(P,B3%(P,B0%(1,0)),13)<10 OR B0%(1,2)>1 THEN 8943:'CHANGED FROM <10

8945 IF A1>=-1 AND S4%=5 THEN 8943

8946 IF A1<-5 OR B7%(P,B0%(1,0))=1 AND S4%=1 OR B7%(P,B0%(2,0))=1 AND S4%=2 THEN 8943

8947 IF A1<0 AND I%>=7 THEN 8943

8949 P2=3:GOTO 4800

8950 IF B%(P,B3%(P,B0%(2,0)),13)<40 OR B7%(P,B1(P))=1 OR O%(0)=2 THEN 8943

8951 IF B0%(2,2)>1 THEN 8943

8952 P2=3:GOTO 4800

8960 IF S4%=0 OR O%(0)>0 OR S4%=7 OR S4%=6 OR S4%=3 OR S4%=5 OR U%=2 OR P=0 AND I%>7 THEN 8968

8962 IF B7%(P,B1(P))=1 AND S4%=1 AND O%(0)<2 OR B7%(P,B1(P))=1 AND S4%=2 AND O%(0)=0 OR B7%(P,B1(P))=1 AND S4%=4 AND O%(0)=0 THEN 8969

8964 IF B1(P)>2 AND B1(P)<6 OR B4%(P,0,B1(P))>290 AND B7%(P,B1(P))<>1 OR B7%(P,B1(P))<> 1 AND B4%(P,3,B1(P))>70 THEN 8968

8967 IF I%>7 AND A1=-1 AND O%(0)=0 AND S4%=1 THEN 8969

8968 PPH=0:PR%=0:GOTO 8980

8969 IF O%(0)>=1 AND I%>=7 THEN 8968

8970 P2=4:GOTO 4100

8980 IF O%(0)=2 OR B4%(P,4,B1(P))>100 OR S4%=0 THEN 5064

8981 IF S4%=7 OR S4%=6 OR S4%=3 OR S4%=2 OR A1<-3 THEN 5064

8984 IF INT(RND(1)*100)+1>20 THEN 5064

8985 P2=2:GOTO 5067

8990 ' *** CHECK FOR PINCH RUNNER ***
	J%=0
	IF A1>1 OR A1<-2 OR I%<8 THEN 8999
	IF S4%=1 OR S4%=5 THEN I2=1
	IF S4%=2 OR S4%=4 OR S4%=6 THEN I2=2
	J=B0%(I2,0):IF J=0 THEN 8999
	IF B7%(P,J)=1 THEN 8999
	IF B%(P,B3%(P,J),16)>3 THEN 8999
	FOR I=0 TO 22
	IF B$(P,I)="XXX" OR B%(P,I,21)>0 THEN 8992
	IF B%(P,I,16)>5 THEN PR%(J%)=I:J%=J%+1

8992 NEXT
	IF J%=0 THEN 8999
	RN=INT(RND(1)*J%)
	I1=PR%(RN)
	I2=J
	B%(P,I1,21)=1:B3%(P,I2)=I1:B9%(P,B7%(P,I2))=I1
	IF B7%(P,I2)=1 OR B7%(P,I2)=10 THEN B9%(P,1)=99:B9%(P,0)=I2:B7%(P,I2)=10
	D0%(P)=D0%(P)+1:X0%(P,0,D0%(P))=B3%(P,I2)
	X0%(P,1,D0%(P))=B7%(P,I2):X0%(P,2,D0%(P))=(I2*10)+(D0%(P)-9)
	W5=P:W6=P:V(P)=I2:V1(P)=I2
	for xx=22 to 25
	IF B7%(P,I2)=B%(P,I1,xx) THEN B%(P,I1,15)=B%(P,I1,33+(xx-22)*2):B%(P,I1,17)=B%(P,I1,34+(xx-22)*2):B%(P,I1,19)=B%(P,I1,xx+4):goto 8995
	next
	GOSUB 4095:LOCATE 15,1:PRINT M$(P);" is sending in a pinch runner":PR%=1
	GOSUB 6095:GOSUB 6095

8995 GOSUB 220:GOSUB 300:GOSUB 330:GOTO 5052

8999 GOTO 8971

9200 IF I=4 THEN V=3:GOSUB 9204:V=2:GOSUB 9204:V=1:GOSUB 9204

9201 IF I=3 THEN V=2:GOSUB 9204:V=1:GOSUB 9204

9202 IF I=2 THEN V=2:GOSUB 9204:V=1:GOSUB 9204

9203 RETURN

9204 IF B0%(V,0)>0 AND B7%(P,B0%(V,0))=1 AND F%=0 THEN I5=P1%(P):I6=32:GOSUB 8009

9205 IF B0%(V,0)>0 AND B7%(P,B0%(V,0))<>1 AND F%=0 THEN I5=B3%(P,B0%(V,0)):I6=9:GOSUB 8010

9206 RETURN

9414 OPEN "SAVE" FOR INPUT AS #1

9431 INPUT #1,S$:CLS:PRINT "PLEASE WAIT AS THE GROUNDS CREW PREPARES "S$
	FOR I=0 TO 1:INPUT #1,A$(I):NEXT I
	FOR I=0 TO 1:FOR I1=1 TO 13:INPUT #1,L%(I,I1):NEXT I1:NEXT I
	FOR I=0 TO 1:INPUT #1,T1%(I):NEXT I
	FOR I=0 TO 1:FOR I1=0 TO 22:FOR I2=0 TO 41:INPUT #1,B%(I,I1,I2):NEXT I2:NEXT I1:NEXT I
	FOR I=0 TO 1:FOR I1=0 TO 21:FOR I2=0 TO 36:INPUT #1,P%(I,I1,I2):NEXT I2:NEXT I1:NEXT I
	FOR I=0 TO 1:FOR I1=0 TO 22:INPUT #1,B$(I,I1):NEXT I1:NEXT I
	FOR I=0 TO 1:FOR I1=0 TO 21:INPUT #1,P$(I,I1):NEXT I1:NEXT I
	FOR I=0 TO 4:INPUT #1,H$(I):NEXT I
	FOR I=0 TO 1:INPUT #1,P6%(I):NEXT I
	FOR I=0 TO 1:INPUT #1,P1%(I):NEXT I
	FOR I=0 TO 1:FOR I1=0 TO 1:INPUT #1,S8%(I,I1):NEXT I1:NEXT I
	FOR I=0 TO 1:INPUT #1,P2(I):NEXT I
	FOR I=0 TO 1:INPUT #1,S1%(I):NEXT I
	FOR I=0 TO 1:FOR I1=1 TO 9:INPUT #1,B9%(I,I1):INPUT #1,B3%(I,I1):INPUT #1,B7%(I,I1):NEXT I1:NEXT I
	FOR I=0 TO 4:INPUT #1,B1$(I):NEXT I
	FOR I=0 TO 12:INPUT #1,X$(I):NEXT I
	FOR I=0 TO 10:INPUT #1,C$(I):NEXT
	FOR I=0 TO 1:INPUT #1,V(I):NEXT I
	FOR I=0 TO 1:INPUT #1,V1(I):NEXT I
	FOR I=0 TO 1:INPUT #1,P2%(I):NEXT I
	FOR I=0 TO 1:INPUT #1,K9(I):NEXT I
	INPUT #1,P:INPUT #1,U6:INPUT #1,D%
	INPUT #1,U9:INPUT #1,G4
	FOR I=0 TO 1:INPUT #1,D0%(I)
	FOR I1=0 TO 2:FOR I2=0 TO 23:INPUT #1,X0%(I,I1,I2):NEXT I2:NEXT I1:NEXT I
	FOR I=0 TO 1:INPUT #1,M$(I):INPUT #1,NN$(I):NEXT
	FOR I=1 TO 4:INPUT #1,U$(I):NEXT:INPUT #1,RE:INPUT #1,RD
	FOR I=0 TO 1:FOR I1=0 TO 6:INPUT #1,ZB%(I,I1):NEXT:NEXT
	INPUT #1,DL
	INPUT #1,TA%:INPUT #1,SC%:INPUT #1,WF:INPUT #1,PC%:INPUT #1,TP%:INPUT #1,WS%:INPUT #1,WI$
	FOR I=0 TO 2:INPUT #1,D$(I):NEXT
	'INPUT #1,CC%:
	INPUT #1,AP
	FOR I=0 TO 18:INPUT #1,NG%(I):NEXT
	FOR I=0 TO 1:INPUT #1,YN$(I):NEXT
CLOSE #1

9460 GOSUB 99

9464 GOSUB 355
	W5=0:W6=1
	FOR I=0 TO 1:FOR I1=1 TO 9:FOR I2=1 TO 4:READ UM%(I,I1,I2):NEXT:NEXT:NEXT
	FOR I=1 TO 9:READ LD%(I):NEXT
	FOR I=0 TO 1:FOR I1=1 TO 9:FOR I2=1 TO 3:READ DL%(I,I1,I2):NEXT:NEXT:NEXT
	FOR I=1 TO 9:READ OH%(I):NEXT
	FOR I=1 TO 9:FOR I1=1 TO 3:READ LL%(I,I1):NEXT:NEXT
	DATA 0,9,13,100,0,16,21,100,1,22,28,100,2,31,37,100,6,46,53,100,11,61,69,100,14,69,77,100,16,76,85,100,21,91,99,100
	DATA 0,3,5,100,0,6,9,100,1,8,11,100,2,11,16,100,6,29,36,100,11,46,55,100,12,54,65,100,16,64,75,100,21,81,94,100
	DATA 9,16,24,30,46,61,73,76,91
	DATA 2,16,100,6,20,100,7,22,100,10,26,100,29,45,100,46,63,100,52,70,100,63,82,100,81,100,0
	DATA 2,0,100,6,0,100,7,0,100,10,0,100,29,31,100,46,51,100,50,62,100,63,74,100,81,97,100
	DATA 3,6,9,11,28,46,53,64,81
	DATA 9,14,100,16,21,100,24,29,100,31,36,100,46,51,100,61,66,100,69,74,100,76,81,100,91,96,100
	FOR I=0 TO 1:FOR I1=1 TO 9:FOR I2=1 TO 9:READ BT%(I,I1,I2):NEXT:NEXT:NEXT
	DATA 6,12,37,55,66,74,86,96,100
	DATA 5,10,32,55,67,75,86,96,100
	DATA 4,8,27,54,68,76,86,96,100
	DATA 3,7,25,54,69,76,86,96,100
	DATA 3,6,22,54,69,77,86,96,100
	DATA 2,5,20,53,69,78,86,96,100
	DATA 2,4,17,53,70,78,86,96,100
	DATA 1,3,12,52,71,79,86,96,100
	DATA 1,2,7,51,72,80,86,96,100
	DATA 6,14,42,55,63,74,86,96,100
	DATA 5,12,37,55,64,75,86,96,100
	DATA 4,10,32,55,65,76,86,96,100
	DATA 3,8,30,55,66,77,86,96,100
	DATA 3,7,27,55,66,77,86,96,100
	DATA 3,6,24,55,66,77,86,96,100
	DATA 2,5,22,55,67,78,86,96,100
	DATA 1,4,17,55,68,79,86,96,100
	DATA 1,3,12,55,69,80,86,96,100
	FOR I=2 TO 9:FOR I1=0 TO 2:READ CF%(I,I1):NEXT:NEXT
	DATA 0,0,100
	data 0,15,100
	data 0,30,100
	data 0,45,100
	data 0,60,100
	data 1,75,100
	data 3,90,100
	data 4,97,100
	FOR I=2 TO 9:FOR I1=0 TO 1:READ LB%(I,I1):NEXT:NEXT
	DATA 100,0,90,100,75,100,60,100,45,100,30,100,15,100,8,100
	FOR I=0 TO 5:FOR I1=0 TO 2:READ SO%(I,I1):NEXT:NEXT
	DATA 100,0,0,90,100,0,70,95,100,50,80,100,30,65,100,10,50,100
	FOR I=11 TO 0 STEP -1:FOR I1=0 TO 2:READ TP%(I,I1):NEXT:NEXT
	DATA 0,90,100,5,87,100,10,85,100,15,80,100,20,77,100,25,75,100
	DATA 30,73,100,35,70,100,40,67,100,45,65,100,50,63,100,60,65,100
	for i=0 to 5:for i1=1 to 6:read fot%(i,i1):next:next
	data 90,94,96,98,99,100
	data 75,84,90,96,98,100
	data 50,69,82,95,98,100
	data 40,62,77,92,96,100
	data 25,50,75,90,95,100
	data 10,40,64,88,94,100
	if l%(1,8)<=85 then i=0
	if l%(1,8)>85 and l%(1,8)<=90 then i=1
	if l%(1,8)>90 and l%(1,8)<=101 then i=2
	if l%(1,8)>101 and l%(1,8)<=110 then i=3
	if l%(1,8)>110 and l%(1,8)<=120 then i=4
	if l%(1,8)>120 then i=5
	for j=1 to 6:ft%(j)=fot%(i,j):next
	GOTO 65

9500 IF S2%=20 AND W%=1 THEN W%=2:Q$(4)=B$(D,B9%(D,W%))

9600 RETURN

9740 'tired on runs and hits
	IF S8%(P9,0)<=P8%(P9,P1%(P9),4)+P8%(P9,P1%(P9),1) AND S8%(P9,1)<=P8%(P9,P1%(P9),2) THEN 9756
	if s8%(p9,1)>0 and (S8%(P9,0)<=P8%(P9,P1%(P9),4)+P8%(P9,P1%(P9),1) or S8%(P9,1)<=P8%(P9,P1%(P9),2)) then 9755
	if s8%(p9,1)>0 then 9754
	IF P8%(P9,P1%(P9),4)+P8%(P9,P1%(P9),1)<3 and P8%(P9,P1%(P9),2)=0 then 9754
	IF P8%(P9,P1%(P9),4)+P8%(P9,P1%(P9),1)>2 and P8%(P9,P1%(P9),0)>=5 then 9755

9754 PRINT P$(P9,P1%(P9))" seems to be OK!!";:goto 9757

9755 PRINT P$(P9,P1%(P9))" is tiring!!";:goto 9757

9756 gosub 275
	IF rn<=50 and S8%(P9,1)>0 AND I%<6 OR S8%(P9,1)=0 AND P8%(P9,P1%(P9),4)+P8%(P9,P1%(P9),1)>4 THEN print p$(p9,p1%(p9))" just doesn't have it today";:goto 9757
	if rn>50 and S8%(P9,1)>0 AND I%<6 OR S8%(P9,1)=0 AND P8%(P9,P1%(P9),4)+P8%(P9,P1%(P9),1)>4 THEN print p$(p9,p1%(p9))" ain't got nuthin' on his fastball folks!";:goto 9757
	PRINT P$(P9,P1%(P9))" is tired!!";

9757 IF P2=5 THEN 5501

9758 RETURN

9800 FOR BH%=1 TO LEN(Y$)
	IF ASC(MID$(Y$,BH%,1))>122 OR ASC(MID$(Y$,BH%,1))<32 THEN 3990
	NEXT
	RETURN

3990 open "fc.pbp" as #1 len=30
	field #1,30 as YY$
	GET #1,Y%:CLOSE 1
	Y$=YY$

3995 'PRINT Y$ " BEF"
	IF RIGHT$(Y$,1)=" " THEN Y$=LEFT$(Y$,LEN(Y$)-1):GOTO 3995
	if left$(y$,2)="BB" then y$=q$(0)+right$(y$,len(y$)-2)
	if left$(y$,2)="PP" then y$=P$(D,P1%(D))+right$(y$,len(y$)-2)
	if left$(y$,2)="UU" then y$=U$(1)+right$(y$,len(y$)-2)
	if left$(y$,2)="Q4" then y$=q$(4)+right$(y$,len(y$)-2)
	if RIGHT$(y$,2)="PP" then y$=LEFT$(y$,len(y$)-2)+P$(D,P1%(D))
	if RIGHT$(y$,2)="BB" then y$=LEFT$(y$,len(y$)-2)+Q$(0)
	if RIGHT$(y$,2)="UU" then y$=LEFT$(y$,len(y$)-2)+U$(1)
	if RIGHT$(y$,2)="Q4" then y$=LEFT$(y$,len(y$)-2)+q$(4)
	'PRINT Y$ " AFT"
	RETURN

'$INCLUDE: 'KeyInput.BM'
