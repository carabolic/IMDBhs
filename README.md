# IMDBhs

A parser implementation for the weekly IMDB dumps.

## Build

```
stack setup
```

```
stack build
```

In order to run the command line tool. Simply run

```
stack exec IMDBhs-exe [path to movies file]
```

NOTE: it is assumed that the movies file is already converted to UTF-8 with

```
iconv -f iso-8859-1 -t utf-8 [movies.list] > [movies.list.utf-8]
```
