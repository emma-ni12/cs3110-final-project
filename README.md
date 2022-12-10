# Chinese (Caml) 🐫 Checkers  
Interactive multiplayer version of the board game 
  Chinese Checkers written in OCaml for the CS 3110 Final Project. Supports turn-based gameplay for 2 to 6 players. 

## Team Members:   
Emma Ni (emma.ni2212@gmail.com / en257)   
Sunny Sun (xiangwan.sun@gmail.com / xs275)   
Ellyn Hu (iriselyn@gmail.com / eyh29)

## Terminal Gameplay Visual
![Screenshot] (game.png)

## Commands
### `make`
- Rebuilds code and launches utop 
### `make build`
- Rebuilds code
### `make test`
- Runs code through test cases
### `make play`
- Runs the game interface
### `make clean`
- Runs `ocaml build -clean` to get rid of `_build` directory
### `make docs`
- Produces documentation for the project in folders `_doc.public` and `_doc.private`
### `make zip`
- Compresses project into a zip file for submission
### `./cloc.sh`
- Counts and outputs the number of lines of code in the system
- Install cloc [here](https://github.com/AlDanial/cloc#install-via-package-manager)