bfhs
====

A brainfuck interpreter written in pure Haskell.

Setup
-----
The project is built upon the Stack toolchain. To compile the program, use
```sh
stack build
```

Alternatively, you can let stack put a copy of `bfhs` in your local `bin` folder immediately by running the following command:
```sh
stack install
```

Tutorial 
--------
Running the app in interactive mode can be done by simply running the program without any arguments.
```sh
bfhs
```

Running a brainfuck script can be done by providing a path to a file.
```sh
bfhs samples/helloworld.b
bfhs --eval samples/helloworld.b
```

Transpiling to C can be done using the `--transpile` command line argument.
```sh
bfhs --transpile samples/helloworld.b > helloworld.c
gcc helloworld.c -o helloworld
./helloworld
```

FAQ
---

### Why? 
Why not?

### Seriously why? 
Haskell is divine.

### No but really why?
In the name of filter map and reduce... Amen.


License
-------
MIT