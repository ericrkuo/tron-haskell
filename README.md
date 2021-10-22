<p>
    <p>Welcome to our project:</p>
    <a href="https://github.com/DenverCoder1/readme-typing-svg">
      <img src="https://readme-typing-svg.herokuapp.com?font=consolas&color=%2311A1F7&size=30&lines=TRON" />
    </a>
</p>

Our project will be a recreation of the [Tron Light Cycle](https://en.wikipedia.org/wiki/Tron_(video_game)) game. The objective is to force enemy light cycles into walls and jet trails, while also avoiding them. The winner of the game is the last person standing.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

## Team Members

Our team is:

+ Eric Kuo
+ Duy Nguyen

## Product Pitch

<img src="https://i.pinimg.com/originals/56/e4/1b/56e41b45b51feda58b668cdd4c9a0ff2.gif" width="400"/> <img src="https://i.pinimg.com/originals/a6/78/cf/a678cfba3e2f13ff3afc4bde10bb29dd.gif" width="400">

Cool right?!? You’ve got to admit-we’ve all wanted to ride a [light cycle](https://www.google.com/url?q=https://tron.fandom.com/wiki/Light_Cycle_(1st_generation)&sa=D&source=docs&ust=1634800939945000&usg=AOvVaw2cBCh7lBM4DyGAgnFsSqL_) before, even if this is your first time hearing about it. Tron was one of our favourite childhood movies growing up and as kids, we’ve always thought this was what actually happened inside our machines; a world existing within chips and circuits. 


Replace this with a pitch for your project and the problem it solves. This is your vision for what the project
would like like as a complete product, ready for awesome action. (Yes, awesomeness seems to be a theme.)
It may be as short as a couple of paragraphs, or it may be longer. It should **definitely** take less than 4 minutes
to read carefully and thoroughly.

Be sure that this touches clearly on the [project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html#project-requirements).

Good goals to aim for are from the top two rubric items for proposal grading:

> Exciting and language-appropriate product idea tackling a problem that is clearly compelling to a significant audience.

Or:

> Solid and language-appropriate product idea with a problem that is of real interest to the submitting team.

(It's easy to focus on the product and not the problem. Remember to include both!)

## Minimal Viable Project

Replace this with a description of the minimal viable project you will actually build for CPSC 312 (if this becomes your final project).
It may be as short as a few paragraphs, or it may be longer. It should **definitely** take less than 4 minutes
to read carefully and thoroughly.

Make clear:
+ how this builds meaningfully toward your product pitch above, without being nearly as much work,
+ how it builds on the strength and power of the language, and
+ how it leads naturally to learning and applying some new element of the language (including what that element is!)

Good goals to aim for are from the top two rubric items for proposal grading:

> The minimal viable project (MVP) builds on the strengths and power of the language in exciting ways that will clearly lead to excellent learning for students.

Or:

> The MVP clearly builds significantly on the language and will lead in interesting and natural ways to learning for the students.

## Proof of Concept
Our proof-of-concept focuses on the logic and functionality of the game Tron. In more detail, it consists of the following:

### 1. A way to represent the current state of the game
At a high level, this includes where the jet trails currently are, the direction and position of players, the difficulty of the CPU, and whose turn it is. This involved creating [TODO ADD LINKE several data types and type synonyms](). We also decided to use the library [Data.Matrix](https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html) to represent the state of the game.

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

### A road down memory-lane
As always, we can't forget our basics! We had a great time following the HtDW recipe from CPSC 110
<img src="https://media.github.students.cs.ubc.ca/user/1272/files/e46cbe80-32a4-11ec-9f52-22da8ddb2d10" width="500">

### Legacy notes from original markdown
As it is currently set up, editing works best if you first `cd` into the `haskell` subdirectory and open VS Code on that directory (`code .`). There is a `Makefile` with some helpful aliases, but you can also just use `stack` as normal.

Note: We expect to be able to test your code by running `stack test`. Included among your tests should be some that demonstrate the core functionality of your code. (We will be running `make haskell-eval` from the project root.)

We should be able to further explore your code's functionality by running `stack ghci`, and you should instruct us on some interesting cases to try.

If you include instructions different from these, be **absolutely sure** that they will work well for us in whatever environment we run your code and that they will be as easy to use as the instructions above!

