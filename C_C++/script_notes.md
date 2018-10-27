# C/C++ Study Notes

Notes taken during study of [Rade Kutil][1]'s script for the C/C++ lecture at University of Salzburg. 

<!--TOC-->

## C

### Introduction

C was created in the 70s by Dennis Ritchie who, together with Brian Kernighan, wrote a book on C. Often abbreviated as *K&R*, the book set the original standard for C. Numerous revisions and extensions of C have been made since, named by the year they were released (the latest being C18 which mostly just fixes errors in the previous version, C11). C compilers don't neccessarily implement the standards to a T and might even differ in supported features, hence care should be taken when writing very exotic code.

#### Sample program

The following program `minimal.c` can be compiled via `cc -o minimal minimal.c` and called via `minimal <optional_arguments>`.

```c
1   #include <stdio.h>
2
3   int main(int argc, char *argv[]) {
4       int i;
5       for (i = 0; i < argc; i++)
6           printf("%d: %s\n", i, argv[i]);
7       return 0;
8   }
```

| Line | Explanation |
| ---- | ----------- |
| `1` | Basically an import statement: see section below
| `3` | argument count and an array of pointers to characters (command line arguments). First element is program name. The star indicates a pointer
| `4` | In a code block (enclosed by `{ ... }` all variables must be declared before the first "real" instruction
| `5` | Variables can however be declared here: `int i = 0` is possible since C99

#### Header files, object files, ...

*Header files* (`.h`) only hold declarations for methods. The methods are implemented in an accompanying *source file* (`.c`). Header files can be `#include`d which basically works like a Java `import`, allowing one to use the implementations of the methods in the accompanying source file.

*Object files* are "intermediate" files created during compilation, one for each header and source file. They hold assembly code. An executable can be created from object files.

[1]: https://www.cosy.sbg.ac.at/~rkutil/index.html
