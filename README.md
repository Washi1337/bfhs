bfhs
====

A brainfuck interpreter written in pure Haskell.

Setup
-----
The project is built upon the Stack toolchain.

To compile the program and put a copy of `bfhs` in your local `bin` folder, run the following command:
```sh
stack install
```

Running the app in interactive mode can be done by simply running the program without any arguments.
```sh
bfhs
```

Running a brainfuck script can be done by providing a path to a file.
```sh
bfhs samples/helloworld.b
```

Why? 
----
Why not?

License
-------
MIT