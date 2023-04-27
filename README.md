# LH Games Full Count Baseball #

### Game Description ###
**· 29-man rosters · Ball park effects · Stats compiler automatically keeps all player and team stats as well as past schedule results · Complete boxscore to screen and/or printer after each game · One-player vs. computer-manager, two-player, and auto-play options. · Input your own teams, draft or trade players from teams already included · YOU choose the starting lineups, batting order, relief pitchers, plus game decisions like when to hit away, bunt for a hit, sacrifice, steal, hit & run, bring in the corners or the entire infield, take an extra base, DH option and more! ·**

### About Project ###
With permission from Lance Haffner himself, this effort is being made to bring his original source code from the DOS world into the modern world. This is due in part to cross-platform compilers for BASIC. But this effort restores his original vision for a cross-platform sports sim empire, also opening the door for any manner of improvements to accuracy and portability.

Several functions from the original code were linked from Crescent Software libraries. To make a faster route to compilation of the code, most of these were re-written as copycats (where code was not available or ASM routines were used). Functions which relied on packing color into 1 byte were updated to use FG/BG separately. This is in anticipation of future work that will aim to reduce as many of these routines as possible

### About LH Games ###
Since 1983 Lance Haffner Games has been publishing the finest quality text-based computer sports games. Recognized for their statistical accuracy; national media outlets have used Haffner's games to play matchups between great teams of the century. The Ft. Worth Star-Telegram, WTVF-TV (Nashville), The Nashville Banner, and ESPN have used Lance Haffner's games to simulate real and "what if" matchups using current teams and great teams from the past.

LH Games were some of the premier simulators of the 80's. They were a staple of of the mail order days and  served as one of the earliest computer sports simulation. Available for several platforms at the time thanks to their use of BASIC, they fast became known for their impressive accuracy and surprisingly quick speed. Not only that, they allowed very detailed and thorough team, conference and season management - yet somehow maintained an entertaining feel with gameplay that was never overly detailed. 

### Visuals ###
<img src="base-manual.png" width=381 height=520>
<img src="base-screen1.png">

# The Game #
### Installation ###
No installation required. Pre-compiled binaries and required reference files are in /bin.

### Usage ###
Run Hello.exe to start with the main game menu.

Teams files will not be included at this time. However, if you're nice enough, the people at the [Haffner Files Facebook group](https://www.facebook.com/groups/183455342454939) will be glad to help you.

### Support ###
Contact me at my [profile page](https://github.com/jleonard2099/jleonard2099/tree/main) for now.
Bugs and/or feature requests can be submitted using GitHub (see the "Issues" tab at the top of page). This will be the easiest way for me to implement ideas.

# Project Status #
## Current ##
Code compiles in a modern environment and has been mostly re-factored. All known GUI bugs have been fixed, Menu navigation is improved and code was consolidated into a single EXE. This speeds up game play even more and allows compiling for Linux / Mac in QB64.

All code has been thoroughly tested with known bugs documented (including those from the original game - that I am aware of) and being worked on.

## Road Map ##
### Phase 1 ###
This phase will get the games available to the general public in a steady and reliable shape.
**Completed.**

### Phase 2 ###
This phase will start to identify these as new games, and involve these short and simple goals:
- Update standards for code
	- Make wording of menus and options consistent for all products
	- variable names, config files, etc.
	- document purpose of functions
- Improve file, path and error handling to provide helpful messages
- Iimprove code efficiency and consistency
	- Make subroutines for all repeated code
	- Global Team Selection routine 
	- Consistent operation for the same functions in each game

### Phase 3 ###
This final phase will involve the following:
- Updating the games to support modern rules
- Including support files such as team files, leagues, schedules, etc.
- Address gameplay bugs as reported by longtime users and playtesters

### The Future ###
- Updating code to work with databases instead of flat files.
- Bringing modern interface concepts to the design (including GUI)

# The Fine Print #

### License ###
Affero GNU GPL v3

### Contributing ###
Contact me directly if interested in contributing. Only considering people who share my vision for these games and are able to honor Lance's work and legacy.

### Authors / Acknowledgment ###
- [Manual images created by Cool Clips](http://search.coolclips.com/)
- [Game icons created by Joseph Gan (Icon-Icons)](https://icon-icons.com/users/XHgnBHIfJk0ZlajSy0A0x/icon-sets/)
- I am responsible for maintaining / updating most of the code and will spend equal effort cleaning up code as improving it
- Jason Lamb, for lending his expertise in BASIC to helping me learn some standards and rewriting the menu functions.
- Lance Haffner, for his hard work over 2 decades giving people endless hours of playable sports games and making dreams come true.
- [Lance Haffner Sports Sims](https://www.facebook.com/groups/115923978469802) Facebook group admin Philip Lupi, for his hard work keeping Lance's games alive and active with fans old and new.
