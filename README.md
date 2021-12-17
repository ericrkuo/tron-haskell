<p align="center">
    <a href="https://github.com/DenverCoder1/readme-typing-svg">
      <img src="https://readme-typing-svg.herokuapp.com?font=consolas&color=%2311A1F7&size=30&height=45&width=300&lines=Welcome to: Tron" />
    </a>
    </br>
    <img src="https://user-images.githubusercontent.com/49849754/146519226-4c0972bb-b6e3-4331-983b-6ae815fbcce0.gif" width="500"/>
</p>

Our project, Tron, will be a recreation of the [Tron Light Cycle](https://en.wikipedia.org/wiki/Tron_(video_game)) game. The objective is to force enemy light cycles into walls and jet trails, while also avoiding them. The winner of the game is the last person standing.


## Running the code

To run the code:
- `cd` into the `haskell` directory
- open VS Code on that directory (`code .`)
- run the following commands
    - `stack build`
    - `stack ghci`
    - `:l src/UI.hs`
    - `start`
- Remarks and rules:
    - Running `start` will cause a new window to pop up and will bring you to the main menu.
    - Select the difficulty of the CPU you want to play against.
    - After the game is over, you will be prompted to try again, return to the menu, or exit the game!
- Controls
    - Once the game starts, you (the blue player) will use the arrow keys to control the direction of your lightbike.
    - As the game progresses, your jet trail will grow and the objective is to avoid colliding into the walls or the jet trails (your own and the CPUs)!

To run the tests:
- `cd` into the `haskell` directory
- open VS Code on that directory (`code .`)
- run `stack test`
- Some of our main test groups testing core logic of our proof of concept
    - [jet trail collision tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L97)
    - [out of bounds tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L225)
    - [moving forward tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L260)
    - [moving right tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L351)
