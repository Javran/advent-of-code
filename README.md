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

## Progress

[//]: # (how to comment in MD: https://stackoverflow.com/a/20885980/315302)
[//]: # (PROGRESS_AUTOGEN_BEGIN)

- Year 2021
  + [Day 1](src/Javran/AdventOfCode/Y2021/Day1.hs)
  + [Day 2](src/Javran/AdventOfCode/Y2021/Day2.hs)
  + [Day 3](src/Javran/AdventOfCode/Y2021/Day3.hs)
  + [Day 4](src/Javran/AdventOfCode/Y2021/Day4.hs)
  + [Day 5](src/Javran/AdventOfCode/Y2021/Day5.hs)
  + [Day 6](src/Javran/AdventOfCode/Y2021/Day6.hs)
- Year 2020
  + [Day 1](src/Javran/AdventOfCode/Y2020/Day1.hs)
  + [Day 2](src/Javran/AdventOfCode/Y2020/Day2.hs)
  + [Day 3](src/Javran/AdventOfCode/Y2020/Day3.hs)
  + [Day 4](src/Javran/AdventOfCode/Y2020/Day4.hs)
  + [Day 5](src/Javran/AdventOfCode/Y2020/Day5.hs)
  + [Day 6](src/Javran/AdventOfCode/Y2020/Day6.hs)
  + [Day 7](src/Javran/AdventOfCode/Y2020/Day7.hs)
  + [Day 8](src/Javran/AdventOfCode/Y2020/Day8.hs)
  + [Day 9](src/Javran/AdventOfCode/Y2020/Day9.hs)
  + [Day 10](src/Javran/AdventOfCode/Y2020/Day10.hs)
  + [Day 11](src/Javran/AdventOfCode/Y2020/Day11.hs)
  + [Day 12](src/Javran/AdventOfCode/Y2020/Day12.hs)
  + [Day 13](src/Javran/AdventOfCode/Y2020/Day13.hs)
  + [Day 14](src/Javran/AdventOfCode/Y2020/Day14.hs)
  + [Day 15](src/Javran/AdventOfCode/Y2020/Day15.hs)
  + [Day 16](src/Javran/AdventOfCode/Y2020/Day16.hs)
  + [Day 17](src/Javran/AdventOfCode/Y2020/Day17.hs)
  + [Day 18](src/Javran/AdventOfCode/Y2020/Day18.hs)
  + [Day 19](src/Javran/AdventOfCode/Y2020/Day19.hs)
  + [Day 20](src/Javran/AdventOfCode/Y2020/Day20.hs)
  + [Day 21](src/Javran/AdventOfCode/Y2020/Day21.hs)
  + [Day 22](src/Javran/AdventOfCode/Y2020/Day22.hs)
  + [Day 23](src/Javran/AdventOfCode/Y2020/Day23.hs)
  + [Day 24](src/Javran/AdventOfCode/Y2020/Day24.hs)
  + [Day 25](src/Javran/AdventOfCode/Y2020/Day25.hs)

[//]: # (PROGRESS_AUTOGEN_END)
