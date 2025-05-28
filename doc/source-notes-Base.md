
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


## Variables ##

#### Naming ####

F$ ---> pbpText$
F$() --> better name


#### Useage ####

WD$ = WindDirection

    Case 2: PST$ = "Catcher"
    Case 3: PST$ = "First Base"
    Case 4: PST$ = "Second Base"
    Case 5: PST$ = "Third Base"
    Case 6: PST$ = "Shortstop"
    Case 7: PST$ = "Left Field"
    Case 8: PST$ = "Center Field"
    Case 9: PST$ = "Right Field"

## Programmable Manager - File Layout (2-byte [16-bit] integers)

| Offset | Meaning |
| ------ | ------- |
| 0      | manager strategy FLAG (999 = YES) |
| 1      | rotation size |
| 3-7    | pitchers in rotation |
| ------ | ------- |
| 21-36 | spot starters |
| ------ | ------- |
| 60    | middle relievers strategy FLAG (999 = YES) |
| 61-71 | middle relievers |
| ------ | ------- |
| 81-83 | setup vs left |
| 84-86 | setup vs right |
| ------ | ------- |
| 90    | setup relievers strategy FLAG (999 = YES) |
| ------ | ------- |
| 99    | starting pitching rotation FLAG (999 = YES) |
| 100-309 | lineups vs right |
| (18 times, 9 vs. right and 9 vs. left)
| 100   | % usage |
| 101   | (0 = no DH, 1 = DH) |
| 102-110 | field positions for 1-9 in lineup |
| 111-119 | player numbers on roster |
| 120   | lineup FLAG (999=SAVED) |
| ------ | ------- |
| 310-519 | lineups vs left |
| ------ | ------- |
| 520   | closing pitcher strategy FLAG (999 = YES) |
| 521-523 | closers vs left |
| 524-526 | closers vs right |
| ------ | ------- |
| 540   | pinch hitter strategy FLAG (999 = YES) |
| 541-545 | pinch hitters vs left |
| 546-550 | pinch hitters vs right |
| ------ | ------- |
| 560   | defensive players strategy FLAG (999 = YES) |
| 561-564 | defensive players being replaced |
| 565-568 | defensive substitutes |
| 569-572 | RA pct. ?? |
| 573-576 | position |
| ------ | ------- |
| 581   | closing strategy FLAG (999 = YES) |
| 590   | pinch hitting for the pitcher strategy FLAG (999 = YES) |
| ------ | ------- |
| (3 times) |
| 591   | INNING TO BEGIN PINCH HITTING FOR PITCHER |
| 592   | DOWN BY HOW MANY RUNS OR MORE |
| 593   | RUNNERS ON BASE OR MORE |
| 594   | NUMBER OF OUTS OR LESS |
| 595   | PITCHER HITS LESS THAN |
| 596   | PERCENT OF TIME |
| ------ | ------- |
| 609   | pitcher bunting strategy FLAG (999 = YES) |
| 634   | position player bunting strategy FLAG (999 = YES) |
| 671   | hit & run strategy FLAG (999 = YES) |
| 690   | infield in, 3rd base occupied strategy FLAG (999 = YES) |
| 707   | corners in vs hitter who might bunt for a hit strategy FLAG |
| 714   | corners in vs hitter in sac hit situation strategy FLAG |
| 731   | corners in vs pitcher in sac hit situation strategy FLAG |
| 740   | corners in vs hitter in squeeze situation strategy FLAG |
| 749   | guarding the lines strategy FLAG |
| 756   | pitching around hitter strategy FLAG |
| 766   | throwing to the plate strategy FLAG |
| 767-770 | throwing home options (TODO!) |
| ------ | ------- |
| 771   | pinch runners strategy FLAG |
| 772-777 | pinch runners |
| 778   | defensive subs for pinch runners (score tied or down by 1 run) strategy FLAG (999 = YES) |
| ------ | ------- |
| (6 times)
| 779   | player being substituted |
| 780   | inning |
| 781   | substitute |
| 782   | pct. |
| ------ | ------- |
| 803   | pinch running for the pitcher strategy FLAG (999 = YES) |
| 810   | base running strategy FLAG (999 = YES) |
| ------ | ------- |
| 847   | pinch hitting for the batter strategy FLAG (999 = YES) |