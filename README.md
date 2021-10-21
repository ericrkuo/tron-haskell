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

Replace this with a description of your proof-of-concept. This may be as short as a few paragraphs, or it may be longer.
It should **definitely** take less than 4 minutes to read carefully and thoroughly, though working through and running the
code may take an extra 4 minutes. (Your guidance and links should make it easy for us to work through the code.)

Tell us:

+ what key element of your project the proof-of-concept focuses on
+ what makes that such an important element
+ how completing this gives you confidence that, with sufficient work, you could complete the full (minimal viable) project

Include links (likely even line-level links, which are easy to create in Github) throughout to critical pieces of
the code to make it easy for us to understand what you've accomplished and how it fulfills the requirements.

Also include instructions for us to test and run your code. (See our guidelines below.)

A good goal to aim for is the top rubric item from proposal grading:

> Fully functional proof-of-concept is easy to use and review, and it clearly demonstrates a key element necessary for the overall project.

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

### Legacy notes from original markdown
As it is currently set up, editing works best if you first `cd` into the `haskell` subdirectory and open VS Code on that directory (`code .`). There is a `Makefile` with some helpful aliases, but you can also just use `stack` as normal.

Note: We expect to be able to test your code by running `stack test`. Included among your tests should be some that demonstrate the core functionality of your code. (We will be running `make haskell-eval` from the project root.)

We should be able to further explore your code's functionality by running `stack ghci`, and you should instruct us on some interesting cases to try.

If you include instructions different from these, be **absolutely sure** that they will work well for us in whatever environment we run your code and that they will be as easy to use as the instructions above!

