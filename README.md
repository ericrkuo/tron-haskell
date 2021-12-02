<p>
    <a href="https://github.com/DenverCoder1/readme-typing-svg">
      <img src="https://readme-typing-svg.herokuapp.com?font=consolas&color=%2311A1F7&size=30&height=45&lines=Welcome to: Tron" />
    </a>
</p>

Our project, Tron, will be a recreation of the [Tron Light Cycle](https://en.wikipedia.org/wiki/Tron_(video_game)) game. The objective is to force enemy light cycles into walls and jet trails, while also avoiding them. The winner of the game is the last person standing.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

## Team hotcakes

Our team:

+ Eric Kuo - 58163288
+ Duy Nguyen - 95844189

## Link to Video
TODO insert link

## Full Proposal
Our full proposal can be found at [Proposal.md](/Proposal.md)

## MVP Guide

## New Learning

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
