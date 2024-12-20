# Make Writeup

### Background
*Don’t Touch the Spikes* is a mobile game developed by Ketchapp and released in 2014. Known for its simplicity and addictiveness, the game features a bird that moves horizontally across the screen. Players tap the screen to make the bird jump, avoiding spikes on the walls. Its straightforward mechanics make it easy to pick up, but hard to master. These qualities inspired us to recreate and expand upon the game by adding unique features and modes.

### Goal/Objective
The objective of this project was to port our demake of *Don’t Touch the Spikes* from the NES to the TIC-80 fantasy console, while also having programming patterns in mind. While the demake focused on recreating the original game within the constraints of the NES, the TIC-80 version allowed us to approach our design as a "make" rather than a *remake*. This meant we could expand beyond replication and introduce new game mechanics and features. Specifically, we wanted to add innovative gameplay elements such as powerups and different game modes to make it more distinct from the original game.

### Implementation
To create the TIC-80 version of *Don’t Touch the Spikes*, we started by adapting the bird’s movement mechanics. We created a bird using a table that stored the horizontal and vertical positions and velocities. We also used a gravity global variable to create a realistic jumping and falling mechanic. One of the most significant challenges we faced in our demake was achieving fluid bird movement that replicated the original game. In particular, there was an issue with the jumping mechanic where the second jump would propel the bird much higher than the first jump if the button was clicked twice in rapid succession, often causing the bird to hit the ceiling and die. As a result, we identified the state programming pattern as a possible solution. To resolve this issue, we adjusted the logic of the jumping to ensure consistent behavior, whether the bird was falling, or in the middle of a previous jump. In other words, we made sure the bird was always in the same state. We did this by fine tuning the bird’s vertical velocity (bird.dy) and how it interacted with gravity. Specifically, we limited the downward velocity to prevent the bird from accelerating too fast. Additionally, we made it so that pressing the jump button resets the bird.dy to a fixed negative value, rather than adding a negative value. This made it so that regardless of how quickly the button is pressed, the jumps would be consistent. This adjustment fixed the issue that the state programming pattern could have addressed, but in a simpler way, and as a result, our bird movement was much cleaner.

The spikes in the game were implemented using tables to keep track of their positions. Each spike is represented as an entry in a table, with its x and y coordinates. When the bird collides with a wall, a function iterates over the spike table to update their positions to random spots along the wall. To ensure that no spikes overlap with each other, we wrote an algorithm that compares their y-coordinates and re randomizes any spikes location if the spacing condition isn't met. A new spike is added to each wall for every 10 points scored by the player, up to a maximum of 6, making the game more challenging as you progress. 

The candy sprite is an additional treat that players can collect for 1 bonus point each. The candy adds a strategic risk-reward element, as it spawns in random locations and often requires players to adjust their flight path to grab it. To ensure only one candy was present on the screen at a time, we used a flag variable called active that determined whether the candy was collected or not. The candy can only respawn if it has been collected, and its respawn is triggered by the bird hitting a wall, making it appear frequently.

### New Features
The spike remover powerup was one of the most significant additions to the game. This powerup allowed players to decrease the amount of spikes on each wall if picked up. To implement this, we used a table consisting of an x-coordinate, y-coordinate, and a flag that told the program whether the powerup was to be shown on the screen or not. The powerup spawns every once in a while at a random location and stays on the screen until it is picked up. If it is picked up, one spike is removed from each wall, allowing the player to progress with fewer obstacles. 

Another important addition we made to the game was the option for the player to choose between two distinct game modes. The first game mode introduced moving spikes in the middle portions of the screens, as opposed to just static spikes on the walls in the original game. This added an extra challenge for players as they would have to adjust constantly to avoid colliding with the moving spikes. 

The second game mode introduces an AI-controlled bird that chases you. In this mode, the AI bird tracks the player's positions and follows it with a slight delay. This mode adds an exciting challenge because players must constantly keep their eyes on where the AI bird is, while also ensuring they don't collide with any spikes on the walls. 

In addition to choosing the game mode, players have the option to choose between three game difficulties, easy, medium, or hard, which determines the player's speed, AI bird’s reaction time, and the number of spikes that start on each wall.

### Challenges
One of the primary challenges was designing the AI bird’s behavior. Initially, the AI bird’s movement was too precise, making it too difficult for the player to avoid. To address this, we implemented a delay meant to represent reaction time. Instead of instantly tracking the player's movements and adjusting their path, the AI bird only updates its vertical position if the difference between its position and the player’s position is greater than a certain number. This change allowed for a more realistic *game of tag*.

Randomizing the locations of the spikes, candy, and powerup also presented challenges in our game. One of the main difficulties was preventing objects from spawning in overlapping positions. To ensure this didn’t happen, we implemented algorithms to check the absolute value of the difference between two objects' coordinates, in order to prevent items from spawning too close to each other.

### Conclusion 
Developing this version of *Don’t Touch the Spikes* was both challenging and rewarding. We tried our best to stay true to the original game’s design while also introducing our own new features. From refining the bird’s movement and jump mechanics to implementing different game modes and difficulties, we focused on adding our own twist to the game, while still improving upon parts of our demake.

## Sources:
Lua style guide
(https://github.com/Olivine-Labs/lua-style-guide)

Sound effects lesson in TIC-80
(https://www.youtube.com/watch?v=q_6jmnvQwjM)

