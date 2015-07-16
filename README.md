# Brief introduction to Opaleye [![Hackage version](https://img.shields.io/hackage/v/opaleye.svg?style=flat)](https://hackage.haskell.org/package/opaleye) [![Build Status](https://img.shields.io/travis/tomjaguarpaw/haskell-opaleye.svg?style=flat)](https://travis-ci.org/tomjaguarpaw/haskell-opaleye)

Opaleye is a Haskell library which provides an SQL-generating embedded
domain specific language for targeting Postgres.  You need Opaleye if
you want to use Haskell to write typesafe and composable code to query
a Postgres database.

> "Opaleye really is great. You've managed to bring what is so
wonderful about relational databases and give it type safety and
composition (i.e. what is wonderful about Haskell)" &ndash; Daniel
Patterson, [Position Development](http://positiondev.com/)

Opaleye allows you to define your database tables and write queries
against them in Haskell code, and aims to be typesafe in the sense
that if your code compiles then the generated SQL query will not fail
at runtime.  A wide range of SQL functionality is supported including
inner and outer joins, restriction, aggregation, distinct, sorting and
limiting, unions and differences.  Facilities to insert to, update and
delete from tables are also provided.  Code written using Opaleye is
composable at a very fine level of granularity, promoting code reuse
and high levels of abstraction.

## Tutorials

Please get started with Opaleye by referring to these two tutorials

* [Basic tutorial](Doc/Tutorial/TutorialBasic.lhs)
* [Manipulation tutorial](Doc/Tutorial/TutorialManipulation.lhs)

# Contact

## Contact the author

The main author of Opaleye is Tom Ellis.  He can be [contacted via
email](http://web.jaguarpaw.co.uk/~tom/contact/).

## File bugs

Please file bugs on the [Opaleye GitHub issue tracking
page](https://github.com/tomjaguarpaw/haskell-opaleye/issues/).

## Mailing list

Please join the [opaleye-users mailing
list](https://lists.sourceforge.net/lists/listinfo/opaleye-users).

# `Internal` modules

Opaleye exports a number of modules named `Opaleye.Internal....`.
They are provided in case of urgent need for access to the internals,
but they are not intended to be used by API consumers and if you find
yourself repeatedly accessing them this is a sign that either you or
Opaleye are doing something wrong.  In such a case please file a bug.

The interface of `Internal` modules does not follow the PVP and may
break between minor releases, so be careful.

# Commercial support

Commercial support for Opaleye is provided by [Purely
Agile](http://www.purelyagile.com/).

# Backup maintainers

In the event of the main developer becoming unreachable, please
contact the following who are authorised to make bugfixes and
dependency version bumps:

* Adam Bergmark
* Erik Hesselink
* Oliver Charles

# Contributors

The Opaleye Project was founded by Tom Ellis, inspired by theoretical
work on databases by David Spivak.  Much of the implementation was
based on ideas and code from the HaskellDB project by Daan Leijen,
Conny Andersson, Martin Andersson, Mary Bergman, Victor Blomqvist,
Bjorn Bringert, Anders Hockersten, Torbjorn Martin, Jeremy Shaw and
Justin Bailey.

The following individuals and organisations have made helpful
contributions:

* Silk (Erik Hesselink, Adam Bergmark)
* Karamaan (Christopher Lewis)
* Fynder (Renzo Carbonara, Oliver Charles)
* Daniel Patterson
* Jakub Ryška
* Travis Staton

Joseph Abrahamson, Alfredo Di Napoli and Mietek Bak performed useful
reviews of early versions which helped improve the codebase.
