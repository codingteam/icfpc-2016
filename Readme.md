icfpc-2016
==========

This is our solution for [ICFP Contest 2016][icfpc-2016].

We [are holding](https://icfpc2016.blogspot.ru/2016/09/contest-results-are-up.html) 50th place.

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

### `unfoldingSolver`

An unfinished prototype where we wanted to find crease patterns by unfolding the
skeleton back into a square.

```console
$ stack exec -- unfoldingSolver ../problems/problem_10.txt
```

### `deconstruct`

A tool we wrote in an attempt to solve problem No.101 manually. It generates
a bunch of SVG files where each vertex in skeleton is described separately.

The idea is to mark these points on a paper crane, then unfold it and write
a solution entry by hand.

```console
$ stack exec -- deconstruct ../problems/problem_101.txt
```

Team
----

Team members in randomized order:

- Friedrich von Never ([ForNeVeR][fornever]): [report][report-fornever]
- [grouzen](https://github.com/grouzen)
- Ilya Portnov ([portnov][]): [report][report-portnov]
- Alexander Batischev ([Minoru][minoru]): [report][report-minoru]
- [Hagane][hagane]: [report][report-hagane]

[fornever]: https://github.com/ForNeVeR
[hagane]: https://github.com/Hagane
[icfpc-2016]: http://2016.icfpcontest.org/
[minoru]: https://github.com/Minoru
[portnov]: https://github.com/portnov
[report-fornever]: https://fornever.me/posts/2016-08-08-icfpc-2016-report_ru.html
[report-hagane]: http://vk.com/wall226611640_1001
[report-minoru]: https://blog.debiania.in.ua/posts/2016-08-08-icfpc-2016.html
[report-portnov]: http://iportnov.blogspot.ru/2016/08/icfpc-2016.html
[stack]: https://www.haskellstack.org/
