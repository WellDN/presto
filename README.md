# REPEAT THIS OVER AND OVER UNTIL YOU GET GOOD AT IT
```
dune build
```

```lexer
_build/default/bin/main.exe -t linux --lex return_2.c
```

```parser
_build/default/bin/main.exe -t linux --parse return_2.c
```

```generate code
./_build/default/bin/main.exe -t linux --codegen return_2.c

```

```generate assembly
_build/default/bin/main.exe -t linux -S return_2.c
```
```create executable
./_build/default/bin/main.exe -t linux return_2.c
```

```check output
./return_2; echo $?
```
