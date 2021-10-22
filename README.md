<p>
    <a href="https://github.com/DenverCoder1/readme-typing-svg">
      <img src="https://readme-typing-svg.herokuapp.com?font=consolas&color=%2311A1F7&size=30&lines=Welcome to: Tron" />
    </a>
</p>

Our project, Tron, will be a recreation of the [Tron Light Cycle](https://en.wikipedia.org/wiki/Tron_(video_game)) game. The objective is to force enemy light cycles into walls and jet trails, while also avoiding them. The winner of the game is the last person standing.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

## Team Members

Our team is:

+ Eric Kuo
+ Duy Nguyen

## Product Pitch

<img src="https://i.pinimg.com/originals/56/e4/1b/56e41b45b51feda58b668cdd4c9a0ff2.gif" width="400"/> <img src="https://i.pinimg.com/originals/a6/78/cf/a678cfba3e2f13ff3afc4bde10bb29dd.gif" width="400">

Cool right?!? You’ve got to admit-we’ve all wanted to ride a [light cycle](https://www.google.com/url?q=https://tron.fandom.com/wiki/Light_Cycle_(1st_generation)&sa=D&source=docs&ust=1634800939945000&usg=AOvVaw2cBCh7lBM4DyGAgnFsSqL_) before, even if this is your first time hearing about it. Tron was one of our favourite childhood movies growing up and as kids, we’ve always thought this was what actually happened inside our machines; a world existing within chips and circuits. 

### Objective
The objective of the game is simple: a player and CPU will keep moving in a grid as time advances, leaving a trail (“jet trail”) behind them wherever they go. A player wins when their opponent crashes into either the boundaries or the jet trail of the CPU or their own. To win, players must avoid crashing and maneuver instead to force the opposing player to crash.

### Motivation and why this matters to us
After our lecture about the Magic Sum game, Duy and I were curious not only about how feasible Haskell is for creating complex games but were inspired to look into the following concepts:
- How one can enforce the rules of the game (e.g. where a player can move)?
- How can we efficiently represent the state of a program for CPUs to make decisions on?
- How one can handle user-controlled events and progression?
- How easy is it to navigate/update between different states of a game in Haskell?
With so many questions, we thought to ourselves, why not combine our love for Tron with Haskell!

### TODO MENTION HOW ITS COMPELLING TO LARGER AUDIENCE

## Minimal Viable Project
With most of the groundwork for our game complete, we can focus more time on our MVP to really leverage the features of Haskell and to take our project to the next level.
We plan on adding the following features:

### Multiple levels of difficulty of AI
As of right now, we only have a Beginner (Dummy) CPU which moves in the same direction the whole time. We plan on adding more complex algorithms that will allow our CPUs to become more challenging. Since we already added functionality in our proof-of-concept to support CPU’s of varying difficulties, this is only a matter of adding new algorithms.

This could involve concepts we learned in class about game trees, next moves, min-maxing, or new algorithms that we discover/create!

### Abstracting and refining our code
We plan on leveraging concepts we learned in class about Haskell to make our code more readable, performant, and to also build upon the concepts from the Magic Sum lecture such as using type classes to represent our game state. 

Furthermore, since there are no side effects in Haskell, things like passing around state and handling collisions with jet trails/walls will be much easier to test and help us be more confident that things are working correctly. This will also help us create more robust error handling whenever a player collides or goes out of bound.

Finally, we also want to be able to create objects that have a functional nature and by leveraging the strongly typed features of Haskell, we can figure out errors more easily and tell what a function is doing just by its signature.

### Haskell’s module system
For our MVP, we can leverage the power of Haskell’s module system by separating our code into different modules like a player module, CPU module, game state module, and more. This will lead to a more manageable and less coupled code that can be tested independently and easily modified in the future.

### Packges! - Matrices, listening to user inputs, UI
Lastly, like every game, we want to add visuals! This will naturally lead us to learn and apply some new package/concepts of the language we have not touched before. In addition to the `Data.Matrix` package mentioned in our proof of concept, we are curious to learn how keyboard events are handled in Haskell and how graphics are rendered. Some packages our group has been looking into are `Gloss` and `System.Console.ANSI`; we cannot wait to play around with that!

Make clear:
+ TODO how this builds meaningfully toward your product pitch above, without being nearly as much work,
+ TODO refine how builds on strengths and power of language

## Proof of Concept
Our proof-of-concept focuses on the logic and functionality of the game Tron. In more detail, it consists of the following:

### 1. A way to represent the current state of the game
At a high level, this includes where the jet trails currently are, the direction and position of players, the difficulty of the CPU, and whose turn it is. This involved creating [TODO ADD LINKE several data types and type synonyms](). We also decided to use the library [Data.Matrix](https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html) to represent the state of the game. Using this package gives us an opportunity to work with something new in Haskell and to leverage the utility functions that operate on a matrix out of the box.

The main idea is that the initial game state starts with a zero matrix, where zero indicates the cell is unoccupied, free for any CPU/player to travel. As time progresses and the players move, a 1 signifies the jet trail of the current player, and a -1 signifies the jet trail of the CPU. In our MVP, the idea is to use the numbers in this matrix as a way to color our cells graphically.

- [TODO ADD LINK TronState]()

### 2. A way to advance the current state of the game
We implemented logic to allow a player and CPU to specify where they want to move next (either left, forward, or right). By knowing the current state of the game, and where a player wants to move, we can appropriately update the matrix and each player’s position and direction.
- [TODO ADD LINK nextGameState]()

### 3. Collision and bound logic
We prototyped functionality that will allow the state of the game to detect when a player crashes into a wall, and when a player will collide into any jet trail (their own or the CPU’s). This will allow us to detect when the game is over, and who has won.
- [TODO ADD LINK Collision logic]()
- [TODO ADD LINK Out of bounds logic]()

### 4. IO

We wanted some way for users to interact with our proof of concept. This was accomplished by allowing users to specify in the terminal which direction they wanted to go, and to visually see the state of the game.
We prototyped this behaviour using recursive IO to pass the state around and to print out the player and CPU moves.
> Sneak Preview! Refer to instructions below to see how to run our code.
<img src="https://media.github.students.cs.ubc.ca/user/1272/files/e54d2280-329b-11ec-9215-78f3e097ac1b" width="300">

### TODO
Include links (likely even line-level links, which are easy to create in Github) throughout to critical pieces of
the code to make it easy for us to understand what you've accomplished and how it fulfills the requirements.

### How this illustrates a key element of our project
By laying out the groundwork for how we want to represent the state of our game and the various actions players/CPUs can execute in our game, we can be more confident in ensuring that the core logic of our game works and is feasible to implement. For prototype purposes, we prioritized our time in ensuring the logic and interaction of the game was correct, and experimented with various libraries/packages, leading to our choice of using Data.Matrix.

We were also curious about how to model direction, players, and moves, which are crucial aspects of any game that needs to be aware of their sense of direction. Hence, this led to several algebraic data types and some type-synonyms.

### How this gives us the confidence to complete our MVP
By having our core logic and thoroughly tested code, we can focus more of our time and resources on exploring different libraries for UI like Gloss, how to read keyboard inputs, and refining our code to take on more Haskell features. 

By prototyping with several other external libraries to decide how to represent the state of the game, we are more confident now with how to use and incorporate them into our code.

### How to test and run the code: Haskell

To run the code:
- `cd` into the `haskell` directory
- open VS Code on that directory (`code .`)
- run the following commands
    - `stack ghci`
    - `:l app/Main.hs`
    - `main`
    - Remarks:
        - Running `main` will execute our proof of concept for the game Tron
        - You can choose from the specified commands what to do next, each instruction will advance the game state into its next state based on the command provided
        - Each entry print two things: your move and where the CPU decided to move.
        - Some interesting cases to try
            - What happens if a player collides into their own jet trail?
            - What happens if a player travels out of bounds?
- Other interesting cases to try
    - Make sure to execute the following instructions first
    - `stack ghci`
    - `:l src/Lib.hs`
    - Try
        - `initTronState` - see how state is represented in our game!
        - `printNextGameState initTronState` - this is what `main` is calling! We used recursive IO to implement this

To run the tests:
- `cd` into the `haskell` directory
- open VS Code on that directory (`code .`)
- run `stack test`

## A road down memory-lane
As always, we can't forget our basics! We had a great time following the HtDW recipe from CPSC 110
<img src="https://media.github.students.cs.ubc.ca/user/1272/files/e46cbe80-32a4-11ec-9f52-22da8ddb2d10" width="500">

### Legacy notes from original markdown
As it is currently set up, editing works best if you first `cd` into the `haskell` subdirectory and open VS Code on that directory (`code .`). There is a `Makefile` with some helpful aliases, but you can also just use `stack` as normal.

Note: We expect to be able to test your code by running `stack test`. Included among your tests should be some that demonstrate the core functionality of your code. (We will be running `make haskell-eval` from the project root.)

We should be able to further explore your code's functionality by running `stack ghci`, and you should instruct us on some interesting cases to try.

If you include instructions different from these, be **absolutely sure** that they will work well for us in whatever environment we run your code and that they will be as easy to use as the instructions above!

