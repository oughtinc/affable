**Alpha. Likely to change dramatically.**

## Building

Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/), and then `stack build` should be all
that's required to build the code. `stack run` or `stack exec affable` will then run the executable.

## Command Line Options

  - `gen-api` - Generate the client-side code for the web API into `static/command-api.js`.
  - `serve [<dbfile>]` - Start a web server optionally storing state in the Sqlite database `dbfile`. (Currently useless.)
  - `noauto [<dbfile>]` - Create an interaction with no automation optionally storing state in the Sqlite database `dbfile`.
  - `export <dbfile> <id>` - Print the automation as Haskell code for the function with ID `id` stored in the Sqlite database `dbfile`.
  - `[<dbfile>]` - Create an interaction with automation optionally storing state in the Sqlite database `dbfile`.

For example, `stack run foo.db` (or `stack exec affable foo.db`) will start a session supporting automation and storing
the results into the Sqlite database `foo.db`. This does *not* reuse the automation from prior runs.

Once you've completed (whether by answering the top-level question, or using Ctrl-D/Ctrl-Z/Ctrl-C to terminate early),
you can export the code via `stack run export foo.db 1 > t.hs`. `1` should be replaced by the ID of the top-level function you
want to export which you'd need to get by looking in the database, though it will likely be `1` if you started with an
empty database. Querying the database with `SELECT id FROM Functions WHERE isAnswer = 1` should given you appropiate IDs.
`t.hs` is now a self-contained Haskell program which is can be compiled and executed, e.g. via `runhaskell t.hs`.

## Interactions

There are three commands that are accepted in the interactive mode:

  - `ask <message>` - This asks a new question.
  - `view <pointer>` - This expands a pointer revealing what it points at.
  - `reply <message>` - This answers the current question.

Currently, you need to `ask` a question as the first thing otherwise the program will crash.

A `pointer` looks like `$n` where `n` is a number, e.g. `$3`.

A `message` is an arbitrary string that may contain `pointer`s except that it must have balanced square brackets, `[`, `]`. A
square bracketed part of the string becomes a sub-`message` that will be hidden behind a pointer.

When you `ask` a question like `What is $1 minus $1?` the question that will be presented is the more general question, `What is $1 minus $2?`

See the scripts in [https://github.com/oughtinc/affable/tree/master/tests/scripts](https://github.com/oughtinc/affable/tree/master/tests/scripts)
for example input sequences.

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
