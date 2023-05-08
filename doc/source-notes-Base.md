
## Game Flow ?!?!? ##

An excerpt:
- "the pitch is in there"
- Strike three!

But it's only the first strike?
Similar behavior with balls?


## Offense Menu ##
	
*Personnel --> "Relief Pitcher"*
- is only letting you know pitcher status 
- this is b/c he's not up for relief

*"HIT AND RUN"*
only can use in certain circumstances?


## Game82 File Format ##
FILE.SCD
startGame
endGame
currentGame



## Variables ##

#### Naming ####

S$ = Stadium$ = RTrim$(player$(1840))
M$ = Manager$ = RTrim$(player$(1841))
NN$ = teamAbbrev$ = RTrim$(player$(1842))


F$ ---> pbpText$
F$() --> better name


#### Useage ####
CT = computer team (NOT Coin Toss)

DN% = DayNight
- 0 = Night
- 1 = Day

PARK$
- D = Day
- N = Night

WD$ = WindDirection

    Case 2: PST$ = "Catcher"
    Case 3: PST$ = "First Base"
    Case 4: PST$ = "Second Base"
    Case 5: PST$ = "Third Base"
    Case 6: PST$ = "Shortstop"
    Case 7: PST$ = "Left Field"
    Case 8: PST$ = "Center Field"
    Case 9: PST$ = "Right Field"


## Unsorted ## 

- Arm
- Range
- Fielding %
