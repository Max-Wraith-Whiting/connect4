# Connect4
This was a fun little project to familiarise myself with Haskell in a (debateably) non-trivial way.
The code itself is pretty rough around the edges and has absolutely *zero* refactoring.
## Running

Using ```stack run -- connect4-exe --text``` will run the *console* version of the game.

For using the GUI, use ```stack run -- connect4-exe --gui```.

You can modify the number of rows and columns in the game like so:

```stack run -- connect4-exe [--text | --gui] --rows i --columns j```

where ```i``` and ```j``` are an appropriate number.



## GUI
When running the GUI, the number keys '```0```' through '```9```' act as column inputs. 

(*The GUI version restricts the maximum size of a board to keep within this input scheme.*)

To restart the game press '```r```'. (*This can be done at any time.*)

>If you are having difficulty with stack not running the GUI, just find the exe file in the .stack-work folder and run it with the --gui flag.
