<p>
    <a href="https://github.com/DenverCoder1/readme-typing-svg">
      <img src="https://readme-typing-svg.herokuapp.com?font=consolas&color=%2311A1F7&size=30&height=45&lines=Welcome to: Tron" />
    </a>
</p>

Our project, Tron, will be a recreation of the [Tron Light Cycle](https://en.wikipedia.org/wiki/Tron_(video_game)) game. The objective is to force enemy light cycles into walls and jet trails, while also avoiding them. The winner of the game is the last person standing.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

## Team hotCakes

Our team:

+ Eric Kuo - 58163288
+ Duy Nguyen - 95844189

## Link to Video
TODO insert link

## Full Proposal
Our full proposal can be found at [Proposal.md](/Proposal.md)

## MVP documentation
In our proposal, we worked on representing the state of the game and building the core logic of handling collisions and updating states. For our MVP, we wanted to challenge ourselves on several complex features that would give us great exposure to developing games in Haskell. The following section describes the features we worked on!


### GUI
We used [Gloss](https://hackage.haskell.org/package/gloss) to create a basic GUI for our game. The [playIO](https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Interface-IO-Game.html#v:playIO) function from `Gloss` was relatively easy to use; by defining functions to handle events, draw each frame, and advance each step of our game, we were quickly able to get a prototype up and running. 
TODO link to play IO

In our proposal, we created a data type, `TronState`, to represent the state of our game. For our GUI, we created a wrapper around it called `GameState`, analogous to the concept of a [“world”](https://hackage.haskell.org/package/gloss-game-0.3.3.0/docs/src/Graphics-Gloss-Game.html#:~:text=world%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20--%20%5EThe%20initial%20world%20state) in `Gloss`. Our game state contained additional information we felt was not **core** to the logic of our game, but information/metadata needed for our GUI. This included things like the mode (whether we’re in the menu, retry page, score page, etc), the current score of the game, and what move the player chose.
TODO link to GameState and Mode

Here are some of the main components of our GUI:
- Listening to key events
   - Not only do we listen to arrow keys when the game is in progress, but we also used this function to control other pages of our game such as the menu, score, and game-over page. Depending on the current page the user is in, different keys result in different actions. For example, when the game has finished, the user has the option to hit `r` to restart the game, `m` to navigate to the menu, or `ESC` to exit the game 
    - `handleKey` TODO LINK CODE
- Advancing each iteration of our game
    - The function `handleStep` advances each iteration of our game by one step. Our function simply uses the functionality we created in our proposal to advance the state of our game depending on which player's turn it is. 
- Rendering each frame
    - Rendering and drawing each frame of our game was relatively easy with `Gloss`. There were lots of helper functions to draw different shapes, text, and colours. Since our game has multiple pages to navigate to, `drawGameState` calls other functions to draw the scene depending on the mode of the game. For example, if the user is in the menu, we call [drawMenu TODO link](), if the game is in progress, we call [drawGrid TODO link](). 
    - `drawGameState` TODO Link
    - For `drawGrid`, one obstacle we encountered is that the package Data.Matrix uses 1-based indexing and `(1,1)` is the top left of the grid. We used `Data.Matrix` to represent the current state of our game. However, after researching deeper into `Gloss`, the library uses `(0,0)` to represent the center of the window. Hence, we needed several calculations to account for these discrepancies.

One big challenge we faced with `Gloss` were keys not being registered.
- When drawing frames, `Gloss`does not allow users to reuse the previous frame. Instead, we need to redraw the whole game state on every step of our game. Consequently, some of the arrow keys do not respond. We believe this is because there are timing issues when `Gloss` tries to render each frame and listen to keyboard inputs.
- We tried several iterations of drawing our grid, each time minimizing how much was drawn.
    - Our first version of `drawGrid` drew empty cells as black rectangles. Unfortunately, this caused a lot of lag since our matrix of size n x n needed to draw n x n pictures.
    - Our second version decided to only draw cells the CPU and player had their jet trails occupying. This resulted in a much smoother game compared to our first version, but we felt there was more that could be improved. https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blame/24cca26ce79963f45b0f9355d5e91649cf60536b/haskell/src/UI.hs#L65
    - Lastly, our third version of `drawGrid` reduced the size of the list we operated on and the number of times we accessed our matrix. (the matrix represents the state of the game where 0 is an empty cell, 1 represents the players jet trail, and -1 represents the CPU’s jet trail) https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/f76f3499e6b8ca1dfee147ef17acb49746462403/haskell/src/UI.hs#L132 



### CPUs varying in diffculty
Initially, we only had a dummy CPU that only moved in one direction. However, as we provided necessary infrastructure in our proof-of-concept to support the implementation of additional CPUs, we simply added new algorithms with increasing diffculties. 

In our final project, we incorporated 2 different diffculty levels:
- Easy  
    - Our first CPU plays to survive. It looks exactly one step ahead in the future (either moving forward, left, right) and picks the move that doesn’t result in a loss. If multiple moves are valid, we take priority moving forward, then left, and lastly right. 
    - TODO: insert link to easy CPU 
- Medium
    - Our medium CPU is similar to our beginner CPU, however it uses random to determine a turn.
    - TODO: insert link to medium CPU
- Hard
    - Originally we were implementing a minimax algorithm for our Hard CPU, however, as our search tree grew too large, we discovered that a pruning algorithm was required to reduce the search runtime. This required us to refactor a significant portion of our code and so we decided to leave this feature out.   

### Abstracting and refining our code
We wanted to ensure that the core logic of our game had no side effects and that all our functions were total. We accomplished this by writing extensice tests simulating the possible states of our game (TODO: link tests) and by using total functions for our external packages too. For example, we made sure all our File IO operations covered as many edge cases as possible and used the total functions [safeGet](https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html#v:safeGet) and [safeSet](https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html#v:safeSet) from the [Data.Matrix](https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html) package to safely lookup elements of our matrix. This helped us create more robust error handling whenever a player collides or goes out of bound.

### Haskell's Module System
Like other programming languages, Haskell has support for a module system that enabled us to break our one long file into 2 separate modules: Lib and UI. This greatly improved the workflow and management of our code as it separated the logic of our game from the UI. As our code was less coupled, we could make patches that can be tested independently. We also learned about using numerous modules from external packages and had fun learning about concepts such as qualified imports, import scopes, and hiding identifiers.



## What we learned
- Matrices
    - We used matrices extensively in our project to model the current state of the game. We used the `Data.Matrix` package which helped us with the following features
        - Accessing/modifying elements of a matrix (TODO link to safeSet and safetGet)
        - Initializing zero matrices (todo link to create tronState)
        - Printing matrices for our terminal-based game in our proposal
    - Using this package gave us lots of exposure to [hackage](https://hackage.haskell.org/) and how to navigate the documentation for packages. Our project also benefited from this package because it simplified how we represented our game compared to using 2D lists or vectors. It was also very easy for us to create thorough and easy-to-read test cases [TODO link].
- Comprehensive tests
    - We learned that writing tests are just as important as writing the business logic of our game. Without tests, we wouldn’t have caught a lot of bugs in our code and it would have slowed down our development. Our project benefitted from tests because it made us more confident that our code and state behaved as expected, and we could spend more time focusing on delivering the features we promised in our MVP.
        - TODO add links to tests
- Working with state
    - As mentioned earlier, we learned a lot about making careful design decisions when it comes to the state of our game. By capturing all the information we needed for our game or UI in a data type, we could leverage pattern matching and type safety to help write code more efficiently. By passing around our state as arguments to our functions, it was easier to debug and test our code. 
        - TODO link to tronState and gameState
- IO
    - We gained a lot more exposure to working with IO in our MVP. From lectures, we learned about IO Monads and gained a better understanding of why they’re descriptions or recipes of effectual computations.
    - With Gloss, we learned about `do` notation and employed it in multiple areas of our code (TODO link).
    - We also got exposure with file IO by reading and writing to a file, which helped create our robust scoring system. (TODO: maybe link)



### Running our MVP

To run the code:
- `cd` into the `haskell` directory
- open VS Code on that directory (`code .`)
- run the following commands
    - `stack build`
    - `stack ghci`
    - `:l app/Main.hs`
    - `main`
- Remarks and rules:
    - Running `main` will cause a new window to pop up and will bring you to the main menu.
    - Select the difficulty of the CPU you want to play against.
    - After the game is over, you will be prompted to try again, return to the menu, or to exit the game!
- Controls
    - Once the game starts, you (the blue player) will use the arrow keys to control the direction of your lightbike.

    > :warning: As mentioned in [TODO LINK SECTION](), the arrow keys may not always respond. This is a limitation with
    > the `gloss` package and how frames are rendered. To avoid this issue, you may need to press a key multiple
    > times to turn in the desired direction.

    - As the game progresses, your jet trail will grow and the objective is to avoid colliding into the walls or the jet trails (your own and the CPU's)!
    - Some interesting cases to try
        - What happens if a player collides into their own jet trail?
        - What happens if a player travels out of bounds?
        - Can you predict where the CPU will go next? Try to outsmart them and move onto the next level of difficulty!

To run the tests:
- `cd` into the `haskell` directory
- open VS Code on that directory (`code .`)
- run `stack test`
- Some of our main test groups testing core logic of our proof of concept
    - [jet trail collision tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L97)
    - [out of bounds tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L225)
    - [moving forward tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L260)
    - [moving right tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L351)
