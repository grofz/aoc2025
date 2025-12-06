# aoc2025
Solving Advent of Code 2025 in Fortran

- *Day 1: Secret Entrance*

  Use brute force to simulate the rotation of safe-wheel and count how
  many times it passes through 0. Modulo approach did not work for Part 2.

- *Day 2: Gift Shop*

  Search for a repeating pattern in ranges of the numbers. Conversion between
  number and string using internal file I/O.

- *Day 3: Lobby*

  Select 2 (Part 1) or 12 (Part 2) digits from the block of digits to maximize
  the number formed from selected digits. Nice and clean solution.
  Brute-force would probably fail in Part 2.

- *Day 4: Printing Department*

  Two-dimensional array, count the number of neighbours. In Part 2,
  a "game-of-life" kind of simulation until final (no longer changing) state
  occurs.

- *Day 5: Cafeteria*

  Have a list of intervals (overlaping). To solve Part 2, intervals are be sorted
  and modified in order to avoid overlaps. Then compute the total length
  of the (now no longer overlaping) intervals. Nice problem, and relatively
  clean (and easy) solution.

- *Day 6: Trash Compactor*

  Do some math following numbers and operators from the input. 
  Part 1 reads numbers from left to right, while Part 2 reads numbers from top
  to bottom. Reading input into a 2D character array to easier parsing in Part 2.

- *Day 7:*

- *Day 8:*

- *Day 9:*

- *Day 10:*

- *Day 11:*

- *Day 12:*
