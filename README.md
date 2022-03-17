#Name
## LH Games Full Count Baseball

# Game Description
29-man rosters * Ball park effects * Stats compiler automatically keeps all player and team stats as well as past schedule results * Complete boxscore to screen and/or printer after each game * One-player vs. computer-manager, two-player, and auto-play options. * Input your own teams, draft or trade players from teams already included * You choose the starting lineups, batting order, relief pitchers, plus game decisions like when to hit away, bunt for a hit, sacrifice, steal, hit & run, bring in the corners or the entire infield, take an extra base, DH option and more!
		
# About Project
With permission from Lance Haffner himself, this effort is being made to bring his original source code from the DOS world into the modern world. This is due in part to cross-platform compilers for BASIC. But this effort restores his original vision for a cross-platform sports sim empire, also opening the door for any manner of improvements to accuracy and portability.

Several functions from the original code were linked from Crescent Software libraries. To make a faster route to compilation of the code, most of these were re-written as copycats (where code was not available or ASM routines were used). Functions which relied on packing color into 1 byte were updated to use FG/BG separately. This is in anticipation of future work that will aim to reduce as many of these routines as possible

# About LH Games
Since 1983 Lance Haffner Games has been publishing the finest quality text-based computer sports games. Recognized for their statistical accuracy; national media outlets have used Haffner's games to play matchups between great teams of the century. The Ft. Worth Star-Telegram, WTVF-TV (Nashville), The Nashville Banner, and ESPN have used Lance Haffner's games to simulate real and "what if" matchups using current teams and great teams from the past.

LH Games were some of the premier simulators of the 80's. They were a staple of of the mail order days and  served as one of the earliest computer sports simulation. Available for several platforms at the time thanks to their use of BASIC, they fast became known for their impressive accuracy and surprisingly quick speed. Not only that, they allowed very detailed and thorough team, conference and season management - yet somehow maintained an entertaining feel with gameplay that was never overly detailed. 

# Visuals
![baseball box art](base-box.jpg)
![baseball in action](base-action.jpg)
	
# Installation
No installation required. Pre-compiled binaries and required reference files are in /bin.

# Usage
Run Hello.exe to start with the main game menu.

# Support
Contact me at my [profile page](https://github.com/jleonard2099/jleonard2099/tree/main) for now.
See future revisions to this for possible links to bug trackers / etc. as they are considered.

# Roadmap
## Phase 1

First work from this point will focus on further code efficiency and cleanup such as:
- Removing extraneous lines and flow control (GOSUB/GOTO)
- Use of color constants for easier visualization of code
- Make wording of menus and options consistent for all products

The following improvements to functionality are also planned:
- Improve handling of required defeault files
- Improve error handling to catch / provide helpful messages
- Update key input handling / routines (handling Esc vs. Ctrl+C, etc.)

Finally, the first efforts will be made to migrate the project to rely on fewer EXEs by converting several of the files to subroutines.

## Phase 2
This phase will involve these short and simple goals:
- Updating the games to support modern rules
- Bug fixes / etc. as reported by longtime users and playtesters

## Phase 3
This final phase will involve the following:
- Updating code to work with databases instead of flat files.
- Bringing modern interface concepts to the design (including GUI)

# Contributing
Contact me directly if interested in contributing. Only considering people who share my vision for these games and are able to honor Lance's work and legacy.

# Authors / Acknowledgment
- I am responsible for maintaining / updating most of the code and will spend equal effort to code cleanup as to code improvement.
- Jason Lamb, for lending his expertise in BASIC to helping me learn some standards and rewriting the menu functions.
- Philip Lupi of the [Lance Haffner Sports Sims](https://www.facebook.com/groups/115923978469802) Facebook group, for his hard work keeping Lance's games alive and active with fans old and new.
- Lance Haffner, for his hard work over 2 decades giving people endless hours of playable sports games and making dreams come true.

# License
...TBD...

# Project Status
Preliminary work has been done so that code should be able to compile with minimal changes. Most immediate updates will be changes made to reach successful compile.

Things are moving along as fast as personal time allows for now, which is reasonable given the scope of the project.
