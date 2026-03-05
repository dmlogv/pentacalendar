# pentacalendar
Pentamino calendar puzzle solver

## Usage

Run without any arguments solves the puzzle for the current date:

```bash
brew install borkdude/brew/babashka

~ bb src/app.clj            

Solving calendar puzzle for [:mar :5 :th]
"Elapsed time: 2551.449597 msecs"
██.███#
██████#
████.██
███████
███████
███████
██████.
####███
```

You can also specify a date to solve:

```
~ bb src/app.clj 2026-01-01 

Solving calendar puzzle for [:jan :1 :th]
"Elapsed time: 45.814153 msecs"
.█████#
██████#
.██████
███████
███████
███████
██████.
####███
```

Output is colored, so you will be able to see pieces :)


## Development

```bash
brew install borkdude/brew/babashka cljfmt
```
