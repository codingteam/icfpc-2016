icfpc-2016
==========

This is our solution for [ICFP Contest 2016][icfpc-2016].

Solution
--------

To build the solution, install [stack][] run the following script:

```console
$ cd src
$ stack build
```

There're multiple executable programs explained in the following sections.
There're also some simple scripts `apitool.py`, `gensvg.ps1`, `gensvg.sh` for
various batch tasks.

### `testSolver`

It is the simplest solver.

```console
$ stack exec testSolver -- -o zomg.svg # and type the following:
4
0,0
1,0
1,1
0,1
```

### `visualize`

It helps to visualize tasks in SVG format.

Help message doesn't say so, but `visualize` requires a filename as a parameter,
like this:

```console
$ visualize -o test.svg -w 400 ../problems/problem_1.txt
```

### `generator`

This program generates new submissions.

```console
$ stack exec generator
```

### `testSolver`

Solves one task.

```console
$ stack exec testSolver -- ../problems/problem_10.txt -o ../10.svg -h 500 -w 500 > ../10.txt
```

### `solveAllTrivial`

Solves trivial tasks (the ones where the initial square was only moved and/or
rotated without actual folding).

```console
$ solveAllTrivial done.txt problems/ solutions/
```

### `solveAllSimple`

Solves simple tasks (only convex polygons).

```console
$ solveAllTrivial done.txt problems/ solutions/
```

Team
----

Team members in randomized order:

- Friedrich von Never: [ForNeVeR](https://github.com/ForNeVeR)
- [grouzen](https://github.com/grouzen)
- Ilya Portnov: [portnov](https://github.com/portnov)
- [Minoru](https://github.com/Minoru)
- [Hagane](https://github.com/Hagane)

[icfpc-2016]: http://2016.icfpcontest.org/
[stack]: https://www.haskellstack.org/
