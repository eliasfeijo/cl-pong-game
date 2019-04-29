# Common Lisp Pong Game

A simple pong game, made for [Lisp Game Jam 2019](https://itch.io/jam/lisp-game-jam-2019).

Binaries available at [releases](https://github.com/eliasfeijo/cl-pong-game/releases) page and at my [itch.io](https://efeijo.itch.io/pong-fight) page.

## Requirements

Should be the same requirements as [trivial-gamekit](https://github.com/borodust/trivial-gamekit) library:

* OpenGL 2.1 or 3.3+
* 64-bit (x86_64) Windows, GNU/Linux or macOS
* x86_64 SBCL or CCL

## Install and run!

Git clone into Quicklisp "local-projects" directory:

`git clone https://github.com/eliasfeijo/cl-pong-game.git $HOME/quicklisp/local-projects/cl-pong-game`

Run
```
(ql:quickload :pong)
(in-package :com.eliasfeijo.pong)
(play-game)
```

### Controls

Player 1: W and S keys to move. Space to shoot.

Player 2: Up and Down arrow keys to move. Enter to shoot.
