# LH Games Full Count Baseball #

### Game Description ###
Full Count Baseball is a speedy simulator of America's favorite summer sport. You make the lineups, you make the calls and you choose when to steal or bunt. Play teams from every era to create a match for the ages, or relive your favorite past game. Save and compile stats, input your own teams or draft or trade players. Play solo or against a friend, or watch the computer do the work. Batters and pitchers are rated using real-world stats, so the outcomes are not only possible - they feel real.

**· 29-man rosters · Ball park effects · Stats compiler automatically keeps all player and team stats as well as past schedule results · Complete boxscore to screen and/or printer after each game · One-player vs. computer-manager, two-player, and auto-play options. · Input your own teams, draft or trade players from teams already included · YOU choose the starting lineups, batting order, relief pitchers, plus game decisions like when to hit away, bunt for a hit, sacrifice, steal, hit & run, bring in the corners or the entire infield, take an extra base, DH option and more! ·**

### About Project ###
With permission from Lance Haffner himself, this effort is being made to bring his original source code from the DOS world into the modern world. This is due in part to cross-platform compilers for BASIC such as [QB64pe](https://github.com/QB64-Phoenix-Edition/QB64pe). But this effort aims to restore his original vision for a cross-platform sports sim empire, while also opening the door for improvements to accuracy, usability and portability.

Several functions from the original code were linked from Crescent Software libraries. To make a faster route to compilation of the code, most of these were re-written as copycats (where code was not available or ASM routines were used). Functions which relied on packing color into 1 byte were updated to use FG/BG separately. This helped to reduce the number of 'helper' routines as possible.

### About LH Games ###
Since 1983 Lance Haffner Games has been publishing the finest quality text-based computer sports games. Recognized for their statistical accuracy; national media outlets have used Haffner's games to play matchups between great teams of the century. The Ft. Worth Star-Telegram, WTVF-TV (Nashville), The Nashville Banner, and ESPN have used Lance Haffner's games to simulate real and "what if" matchups using current teams and great teams from the past.

Lance Haffner Games were some of the premier simulators of the 80's. They were a staple of of the mail order days and  served as one of the earliest computer sports simulation. Available for several platforms at the time thanks to their use of BASIC, they fast became known for their impressive accuracy and surprisingly quick speed. Not only that, they allowed very detailed and thorough team, conference and season management - yet somehow maintained an entertaining feel with gameplay that was never overly detailed. 

### Visuals ###
<img src="base-manual.png" width=381 height=520>
<img src="base-screen1.png">

# The Game #
### Installation ###
No installation required. Releases are available as pre-compiled binaries for each major platform.

### Usage ###
Run Hello.exe to start with the main game menu.

Teams files will not be included at this time. However, if you're nice enough, the people at the [Haffner Files Facebook group](https://www.facebook.com/groups/183455342454939) will be glad to help you.

### Support ###
Contact support@lhgames.net with inquiries for now.
Bugs and/or feature requests can be submitted using GitHub (see the "Issues" tab at the top of page). This will be the easiest way for me to implement ideas.

# Project Status #
## Current ##
Code now compiles in a modern environment and has been mostly re-factored. All known GUI bugs have been fixed, menu navigation is improved and code was consolidated into a single EXE to speed up game play. First successful compiles have been completed in Linux / Mac OS, so builds should be available for major distros with the next release.

All code features have been thoroughly tested with known bugs documented (including those from the original game - that I am aware of) and being worked on.

Code has been documented everywhere possible and cleaned up - including   global file and team selection routine. Game code is consistent in flow and naming across eacch sport. Improvements in logic and variable names have been made to code and error messages and handling are now more consistent.

A few accuracy improvements have been made and support files have been created. Some user interface improvements are under way. 

## Road Map ##
### Phase 1 ###
This phase will get the games available to the general public in a steady and reliable shape.
**Completed.**

### Phase 2 ###
This phase will start to identify these as new games, and involve these short and simple goals:
**Completed.**

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
- [Save icons created by Those Icons - Flaticon](https://www.flaticon.com/free-icons/save)
- [File explorer icons created by juicy_fish - Flaticon](https://www.flaticon.com/free-icons/file-explorer)
- I am responsible for maintaining / updating most of the code and will spend equal effort cleaning up code as improving it
- Jason Lamb, for lending his expertise in BASIC to helping me learn some standards and rewriting the menu functions.
- Lance Haffner, for his hard work over 2 decades giving people endless hours of playable sports games and making dreams come true.
- [Lance Haffner Sports Sims](https://www.facebook.com/groups/115923978469802) Facebook group admin Philip Lupi, for his hard work keeping Lance's games alive and active with fans old and new.
