# aoc2025

Solving **Advent of Code 2025** in Fortran.

- **Day 1: Secret Entrance**

  Use brute force to simulate the rotation of the safe wheel and count how
  many times it passes through 0. A modulo-based approach did not work for
  Part 2.

- **Day 2: Gift Shop**

  Search for a repeating pattern within ranges of numbers. Conversion between
  numbers and strings is done using internal file I/O.

- **Day 3: Lobby**

  Select 2 digits (Part 1) or 12 digits (Part 2) from a block of digits to
  maximize the number formed by the selection. A nice and clean solution;
  brute force would probably fail in Part 2.

- **Day 4: Printing Department**

  Use a two-dimensional array and count the number of neighbors. In Part 2,
  perform a “Game of Life”-style simulation until a final (no longer changing)
  state is reached.

- **Day 5: Cafeteria**

  Work with a list of overlapping intervals. To solve Part 2, intervals are
  sorted and modified to remove overlaps. Then compute the total length of
  the resulting non-overlapping intervals. A nice problem with a relatively
  clean and easy solution.

- **Day 6: Trash Compactor**

  Perform arithmetic by following numbers and operators from the input.
  In Part 1, numbers are read from left to right; in Part 2, they are read
  from top to bottom. The input is read into a 2D character array to simplify
  parsing in Part 2.

- **Day 7: Laboratories**

  Split the beam as it travels downward. Part 1 is a straightforward
  simulation; Part 2 requires counting how many beams can pass through a
  given point on every line. Do not try to explicitly list all states!
  The solution becomes easy once the correct algorithm is identified.

**Day 8: Playground**

  From a list of 1000 points, find the 1000 shortest pairs. Connect those
  positions and identify the three largest clusters. In Part 2, continue
  connecting until only one cluster remains, and report the last pair that
  was connected.

  A priority queue is used to generate the list of shortest pairs, after
  which the pairs are processed to connect the boxes.

- **Day 9: Movie Theater**

  A geometrical problem: find the largest rectangle that fits inside a polygon.

  A trick is used to avoid coinciding polygon and rectangle edges: polygon
  edge coordinates are multiplied by two (making them *even*), while rectangle
  edge coordinates are multiplied by two and shifted one unit toward the
  rectangle interior (making them *odd*). This simplifies the detection of
  intersecting edges.

  A ray-lighting algorithm was also implemented to detect points inside the
  polygon, but it turned out not to be necessary for the given input.

  The final solution is clean, but it required a lot of work.

- **Day 10: Factory**

  This problem requires linear algebra optimization with integer unknowns.
  Matlab was used to solve Part 2. Part 1 uses XOR-based state changes with
  a straightforward BFS.

- **Day 11: Reactor**

  Part 1 is an easy DFS over paths, counting the total number of valid paths.
  Part 2 requires memoization and combining results from specific sub-searches.

- **Day 12: Christmas Tree Park**

  Very funny! In the end it is just parsing exercise, no algorithm needed.