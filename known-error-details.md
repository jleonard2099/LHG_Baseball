
Error occurred! 
Error # 9 on program file line 7354 
99 TORONTO vs 99 MINNESOTA
B3%(P9, I) -1 
B7%(P9, I)  1 
B9%(D, 1)   1 


SUMMARY

-1 --> P1%(P9) --> B3%(P9, I)
	P1%(P9) should be index # (of pitchers) for STARTER
	PitchingStarter assigns I1 (number pressed from menu)

-1 --> MG%(P9, 101 + I + LN% * 21) ---> B7%(P9, I)

I suspect something with logic from DH assignments
However it's odd that P1%(x) should never be -1

________________________________________


Error occurred! 
Error # 9 on program file line 10055 
99 ARIZONA vs 99 ATLANTA
B7%(P9, I)  1 
B9%(D, 1)   99 


- Executing code from:  14470 
	B9%(D, 1)   5
- GoTo 15000
~~ Absolutely NO WAY to modify B9%(D, 1) in between here ~~
- Executing code from:  13118 
	B9%(D, 1)   99 

________________________________________

No longer occurs???

- PH%
	- Line 17664
	- X0%(P, 0, D0%(P)) = PH%

________________________________________
