**Alpha. Likely to change dramatically.**

## Building

### Haskell

Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/), and then `stack build` should be all
that's required to build the code. `stack run` or `stack exec affable` will then run the executable.

### Front-end

There's a pre-built copy of the Javascript files for the front-end, but if you want to modify the front-end, you can
build it via the following.

Install [npm](https://www.npmjs.com/get-npm) and [rollup](https://rollupjs.org/guide/en#quick-start).

Run `npm install` to install the dependencies.

Run `rollup -c` to create the new `bundle.js`.

## Command Line Options

Use `stack run -- --help` to list the usage. Typically, `stack run start` or `stack run serve` will be a good place to start.

Once you've completed (whether by answering the top-level question, or using Ctrl-D/Ctrl-Z/Ctrl-C to terminate early),
you can export the code via `stack run -- export foo.db 1 > t.hs`. `1` should be replaced by the ID of the session you
want to export which will likely be `1` if you started with an empty database.
`t.hs` is now a self-contained Haskell program which is can be compiled and executed, e.g. via `runhaskell t.hs`.

## Interactions

These are the commands that are accepted in the interactive mode:

  - `ask <message>` - This schedules a new question to be asked.
  - `view <pointer>` - This expands a pointer revealing what it points at.
  - `reply <message>` - This answers the current question.
  - `wait` - This waits for the answers to unanswered questions.
  - `exit` - Exits the program.

Tab completion should work for these commands except for `exit`.

A `pointer` looks like `$n` where `n` is a number, e.g. `$37`.

A `message` is an arbitrary string that may contain `pointer`s except that it must have balanced square brackets, `[`, `]`. A
square bracketed part of the string becomes a sub-`message` that will be hidden behind a pointer. Sub-`messages` may be labeled and
can then be referred to elsewhere including cyclicly. For example, `foo [$1: cons [x] $1] $1`. The space after the `:` is required.

When you `ask` a question like `What is $1 minus $1?` the question that will be presented is the more general question, `What is $1 minus $2?`

Here is an animation illustrating the command-line interface in action:
![Demonstration of command-line interface](docs/lengthprim.gif)

Here is an animation illustrating the web interface in action:
![Demonstration of web interface](docs/take.gif)

See the scripts in [https://github.com/oughtinc/affable/tree/master/tests/scripts](https://github.com/oughtinc/affable/tree/master/tests/scripts)
for more example input sequences.

## Thoughts

Topics to explore:
  - Framework that allows automation based on actions or whatever is appropriate.
  - Framework that allows continuous distillation
  - Probably support reflection a la the [Taxonomy](https://ought.org/projects/factored-cognition/taxonomy#reflection)
  - Intercession on scheduling
    - Ideally some support for automation/distillation of scheduling.
  - Internal dialog via message passing
    - Persistent processes
    - Or, auto-replicating processes
      - Only access to the causally "latest" (from the current process' perspective) node
      - or acess to earlier "versions" as well
  - Or, editing versus internal dialog
    - How to handle staleness?
    - Should editing be non-local? If no, what does that mean exactly?

  - Intercession on other aspects? (Rendering, Distillation, Automation)
