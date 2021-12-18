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
  + [X] [Day 1](src/Javran/AdventOfCode/Y2021/Day1.hs)
  + [X] [Day 2](src/Javran/AdventOfCode/Y2021/Day2.hs)
  + [X] [Day 3](src/Javran/AdventOfCode/Y2021/Day3.hs)
  + [X] [Day 4](src/Javran/AdventOfCode/Y2021/Day4.hs)
  + [X] [Day 5](src/Javran/AdventOfCode/Y2021/Day5.hs)
  + [X] [Day 6](src/Javran/AdventOfCode/Y2021/Day6.hs)
  + [X] [Day 7](src/Javran/AdventOfCode/Y2021/Day7.hs)
  + [X] [Day 8](src/Javran/AdventOfCode/Y2021/Day8.hs)
  + [X] [Day 9](src/Javran/AdventOfCode/Y2021/Day9.hs)
  + [X] [Day 10](src/Javran/AdventOfCode/Y2021/Day10.hs)
  + [X] [Day 11](src/Javran/AdventOfCode/Y2021/Day11.hs)
  + [X] [Day 12](src/Javran/AdventOfCode/Y2021/Day12.hs)
  + [X] [Day 13](src/Javran/AdventOfCode/Y2021/Day13.hs)
  + [X] [Day 14](src/Javran/AdventOfCode/Y2021/Day14.hs)
  + [X] [Day 15](src/Javran/AdventOfCode/Y2021/Day15.hs)
  + [X] [Day 16](src/Javran/AdventOfCode/Y2021/Day16.hs)
  + [X] [Day 17](src/Javran/AdventOfCode/Y2021/Day17.hs)
  + [X] [Day 18](src/Javran/AdventOfCode/Y2021/Day18.hs)
- Year 2020
  + [X] [Day 1](src/Javran/AdventOfCode/Y2020/Day1.hs)
  + [X] [Day 2](src/Javran/AdventOfCode/Y2020/Day2.hs)
  + [X] [Day 3](src/Javran/AdventOfCode/Y2020/Day3.hs)
  + [X] [Day 4](src/Javran/AdventOfCode/Y2020/Day4.hs)
  + [X] [Day 5](src/Javran/AdventOfCode/Y2020/Day5.hs)
  + [X] [Day 6](src/Javran/AdventOfCode/Y2020/Day6.hs)
  + [X] [Day 7](src/Javran/AdventOfCode/Y2020/Day7.hs)
  + [X] [Day 8](src/Javran/AdventOfCode/Y2020/Day8.hs)
  + [X] [Day 9](src/Javran/AdventOfCode/Y2020/Day9.hs)
  + [X] [Day 10](src/Javran/AdventOfCode/Y2020/Day10.hs)
  + [X] [Day 11](src/Javran/AdventOfCode/Y2020/Day11.hs)
  + [X] [Day 12](src/Javran/AdventOfCode/Y2020/Day12.hs)
  + [X] [Day 13](src/Javran/AdventOfCode/Y2020/Day13.hs)
  + [X] [Day 14](src/Javran/AdventOfCode/Y2020/Day14.hs)
  + [X] [Day 15](src/Javran/AdventOfCode/Y2020/Day15.hs)
  + [X] [Day 16](src/Javran/AdventOfCode/Y2020/Day16.hs)
  + [X] [Day 17](src/Javran/AdventOfCode/Y2020/Day17.hs)
  + [X] [Day 18](src/Javran/AdventOfCode/Y2020/Day18.hs)
  + [X] [Day 19](src/Javran/AdventOfCode/Y2020/Day19.hs)
  + [X] [Day 20](src/Javran/AdventOfCode/Y2020/Day20.hs)
  + [X] [Day 21](src/Javran/AdventOfCode/Y2020/Day21.hs)
  + [X] [Day 22](src/Javran/AdventOfCode/Y2020/Day22.hs)
  + [X] [Day 23](src/Javran/AdventOfCode/Y2020/Day23.hs)
  + [X] [Day 24](src/Javran/AdventOfCode/Y2020/Day24.hs)
  + [X] [Day 25](src/Javran/AdventOfCode/Y2020/Day25.hs)
- Year 2019
  + [X] [Day 1](src/Javran/AdventOfCode/Y2019/Day1.hs)
  + [X] [Day 2](src/Javran/AdventOfCode/Y2019/Day2.hs)
  + [X] [Day 3](src/Javran/AdventOfCode/Y2019/Day3.hs)
  + [X] [Day 4](src/Javran/AdventOfCode/Y2019/Day4.hs)
  + [X] [Day 5](src/Javran/AdventOfCode/Y2019/Day5.hs)
  + [X] [Day 6](src/Javran/AdventOfCode/Y2019/Day6.hs)
  + [X] [Day 7](src/Javran/AdventOfCode/Y2019/Day7.hs)
  + [X] [Day 8](src/Javran/AdventOfCode/Y2019/Day8.hs)
  + [X] [Day 9](src/Javran/AdventOfCode/Y2019/Day9.hs)
  + [X] [Day 10](src/Javran/AdventOfCode/Y2019/Day10.hs)
  + [X] [Day 11](src/Javran/AdventOfCode/Y2019/Day11.hs)
  + [X] [Day 12](src/Javran/AdventOfCode/Y2019/Day12.hs)
  + [X] [Day 13](src/Javran/AdventOfCode/Y2019/Day13.hs)
  + [X] [Day 14](src/Javran/AdventOfCode/Y2019/Day14.hs)
  + [X] [Day 15](src/Javran/AdventOfCode/Y2019/Day15.hs)
  + [X] [Day 16](src/Javran/AdventOfCode/Y2019/Day16.hs)
  + [X] [Day 17](src/Javran/AdventOfCode/Y2019/Day17.hs)
  + [X] [Day 18](src/Javran/AdventOfCode/Y2019/Day18.hs)
  + [X] [Day 19](src/Javran/AdventOfCode/Y2019/Day19.hs)
  + [ ] [Day 20](src/Javran/AdventOfCode/Y2019/Day20.hs)

[//]: # (PROGRESS_AUTOGEN_END)
