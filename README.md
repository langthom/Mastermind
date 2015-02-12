# Mastermind #
## General ##
Mastermind is a logical game developed in 1970.

In this two player game, one player is the guesser and one the setter.

The setter sets at the beginning of the game a secret color code and the
guesser - well, what else - has to guess it in a certain amount of tries.
After each guess, the guesser gets a rating which consists of black and
white pins.

A black pin indicates that one pin has the right color and was set at the
right position, a white pin indicates a right color but a false location.

For more information, please take a look at 
[Mastermind in Wikipedia](http://en.wikipedia.org/wiki/Mastermind_(board_game) ).

## User guessing mode ##
If you start the game by using `clisp shell.lisp` (or any other Common Lisp
interpreter) or invoking directly `(main)` from the file `shell.lisp`, you
are the guesser that has to guess the color code secretly set by the AI.

To enter a move, you use the 'move' command and providing `num-slots` colors.
(Note: In the basic case this is set to 4, you can change constants by editing
the file `constants.lisp`)

Example: Entering a move with four different 'colors':

`mastermind> move 1 2 3 4`

## Machine guessing mode ##
If you want the machine to be the guesser, then you first have to switch 
guessers via

`mastermind> switch`

Then the machine will take a guess, and you can enter the rating. So the game
will run the opposite way then.

Example: Entering a rating with 2 black and 1 white pins:

`mastermind> eval 2 1`

## Commands ##
This program uses a shell user interface, for a list of available commands
just invoke the program and show the help message via

`$> clisp shell.lisp`

`mastermind> help`
