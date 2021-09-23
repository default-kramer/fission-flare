I consider this game to be a prototype.
The menus might be confusing, and there will be bugs.
Still, it should be very playable once you get past the quirks.
If there is enough interest (from players and/or myself), I might create an "HD Remaster" targeting the major gaming platforms.

# Controls
* The arrows keys are used to move left and right. You can hold left shift to move farther.
* The 'd' and 'f' keys are used to rotate clockwise and counter-clockwise.
* Pressing right shift drops the current catalyst.
* Holding right shift drops the current catalyst and destroys all blank catalysts on your grid.

If you want to use a controller instead of the keyboard, try [AntiMicro](https://github.com/AntiMicro/antimicro/releases).

# Basics
Watch [this video](https://youtu.be/r3rhAv7p2Sk) to get the idea.

There are 3 types of pieces:
* Fuel - When a new game is started, all pieces on the grid are fuel.
* Catalysts - These are the pieces that you control. Catalysts usually come in pairs, but each half may be destroyed independently.
* Contaminants - These pieces only appear as a penalty in certain game modes.

The most essential goal of the game is to destroy fuel.
All 3 types of pieces can be destroyed by creating a group: a horizontal or vertical line of matching colors.
A group of size 4 or more will be destroyed (unless it contains a contaminant).

Creating combos is also an important part of this game.
A combo refers to destroying more than 1 group with a single action.
For example, the last action of [this video (at 3:48)](https://www.youtube.com/watch?v=r3rhAv7p2Sk&t=3m48s) destroys 5 groups with a single action.

## Contaminants
A "contaminated group" is a group that contains at least one contaminant.
Unlike normal groups, contaminated groups provide no reward when destroyed.
A horizontal contaminated group only requires 3 to destroy, but a vertical contaminated group requires 5 to destroy.

## Game Modes
For a basic single-player game; there are 3 main settings you can change
* Layout:
    * Standard - All the fuel will be displayed on the grid from the start.
    * Wave - The grid is divided into halves and each half holds a wave of fuel. When a wave is completely destroyed, a new wave takes its place and you are awarded a bonus.
* Level: Higher levels are more difficult
* Time Attack:
    * Disabled - Your goal is to destroy all the fuel before you run out of energy.
    * Wall Clock - Your goal is to accumulate as much energy as possible before time runs out. Time elapses normally.
    * Waiting Clock - Your goal is to accumulate as much energy as possible before time runs out. Time elapses only when you are able to take an action.

With practice, most players will be able to win on a standard layout, level 40, non-time attack.
But wave layouts at level 40 are much harder!
I only win about 10-20% of my attempts.
Here is a [video of a successful attempt](https://youtu.be/pPC5Ec0q3y4).

# Multiplayer
Multiplayer is a work-in-progress.
The number of players is limited only by CPU, network bandwith, etc.
If the host wants to play, the host needs to join just like a normal (non-host) player.

Win Condition may be set by the host. Currently this setting does nothing -- you have to manually figure out who won by looking at the scores or times.
* Speed - The player who finishes first (lowest frame count) wins. Makes no sense for a Time Attack, as all players will have identical times.
* Energy - The player who finishes with the most energy wins. Pairs nicely with Time Attack, but works fine in other games too.

Competition Type may be set by the host.
* Symmetric - All players get the same fuel layouts, catatlyst queue, and attack power
* Handicap - All players get the same fuel layouts and catatlyst queue, but can increase their attack power.
* Custom - No restrictions. Each player could play a completely different game.
