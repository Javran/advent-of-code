My Advent of Code stuff.

Environment variables required:

- `PROJECT_HOME`: path to project home
- `ADVENT_OF_CODE_SESSION`: session value for downloading input files, this can be found in request header using web browsers.

To run solutions:

```sh
PROJECT_HOME=/path/to/project/home
ADVENT_OF_CODE_SESSION=<cookie session>
stack build && stack exec -- run-advent-of-code <year> <day>
```

## Data sources

Files under `data/download/` directory (intentionally ignored from VCS) are downloaded from [Advent of Code](https://adventofcode.com/).

Files with name `example.input.txt` or `example.expect.txt` under `data/testdata/` directory are manually maintained but its original author is [Advent of Code](https://adventofcode.com/).

## Navigation

[//]: # (how to comment in MD: https://stackoverflow.com/a/20885980/315302)
[//]: # (LINK_AUTOGEN_BEGIN)

- Year 2020
  + [X] [Day 1](src/Javran/AdventOfCode/Y2020/Day1.hs)
  + [ ] [Day 2](src/Javran/AdventOfCode/Y2020/Day2.hs)
- Year 2021

[//]: # (LINK_AUTOGEN_END)
