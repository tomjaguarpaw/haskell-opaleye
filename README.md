# Brief introduction to Opaleye [![Hackage version](https://img.shields.io/hackage/v/opaleye.svg?label=Hackage)](https://hackage.haskell.org/package/opaleye) [![Linux build status](https://img.shields.io/travis/tomjaguarpaw/haskell-opaleye/master.svg?label=Linux%20build)](https://travis-ci.org/tomjaguarpaw/haskell-opaleye)

Opaleye is a Haskell library that provides an SQL-generating embedded
domain specific language for targeting Postgres.  You need Opaleye if
you want to use Haskell to write typesafe and composable code to query
a Postgres database.

> "Opaleye really is great. You've managed to bring what is so
wonderful about relational databases and give it type safety and
composition (i.e. what is wonderful about Haskell)" &ndash; Daniel
Patterson, [Position Development](http://positiondev.com/)

> "We use it for most of our DB code. It's very flexible and almost
  always as performant as manually written queries" &ndash; [Adam
  Bergmark](http://ircbrowse.net/browse/haskell?id=22634197&timestamp=1460980502#t1460980502),
  [Silk.co](http://www.silk.co/)

> "Opaleye is absolutely fantastic. It has been solid in production
  for years!" &ndash; [Matt Wraith](https://github.com/wraithm)

> "Opaleye just works, and it’s my personal recommendation ... I like
  it a lot" &ndash; [William
  Yao](https://www.williamyaoh.com/posts/2019-12-14-typesafe-db-libraries.html)

Opaleye allows you to define your database tables and write queries
against them in Haskell code, and aims to be typesafe in the sense
that if your code compiles then the generated SQL query will not fail
at runtime.  A wide range of SQL functionality is supported including
inner and outer joins, restriction, aggregation, distinct, sorting and
limiting, unions and differences.  Facilities to insert to, update and
delete from tables are also provided.  Code written using Opaleye is
composable at a very fine level of granularity, promoting code reuse
and high levels of abstraction.

## Getting Opaleye

* Github: https://github.com/tomjaguarpaw/haskell-opaleye
* Hackage: https://hackage.haskell.org/package/opaleye

## Tutorials

Please get started with Opaleye by referring to these two tutorials

* [Basic tutorial](https://github.com/tomjaguarpaw/haskell-opaleye/blob/master/Doc/Tutorial/TutorialBasic.lhs)
* [Manipulation tutorial](https://github.com/tomjaguarpaw/haskell-opaleye/blob/master/Doc/Tutorial/TutorialManipulation.lhs)

### Advanced

* [Abstracting out common columns in Opaleye](https://www.williamyaoh.com/posts/2019-12-28-abstracting-out-common-columns-opaleye.html)

# Contact

## Contact the author

The main author of Opaleye is Tom Ellis.  He can be [contacted via
email](http://web.jaguarpaw.co.uk/~tom/contact/).

## File bugs

Please file bugs on the [Opaleye GitHub issue tracking
page](https://github.com/tomjaguarpaw/haskell-opaleye/issues/).

## Discuss and ask questions about Opaleye

You are welcome to use the [Opaleye GitHub issue tracking
page](https://github.com/tomjaguarpaw/haskell-opaleye/issues/) for
discussion of or questions about Opaleye even if they don't relate to
a bug or issue.

## PRs

You are welcome to make PRs to Opaleye.  If you would like to discuss
the design of your PR before you start work on it feel free to do so
by [filing a new
issue](https://github.com/tomjaguarpaw/haskell-opaleye/issues/new).

# `Internal` modules

Opaleye exports a number of modules named `Opaleye.Internal....`.
They are provided in case of urgent need for access to the internals,
but they are not intended to be used by API consumers and if you find
yourself repeatedly accessing them this is a sign that either you or
Opaleye are doing something wrong.  In such a case please file a bug.

The interface of `Internal` modules does not follow the PVP and may
break between minor releases, so be careful.

# Running tests

You must have running PostgreSQL server to run tests. Specify the database
by setting the `POSTGRES_CONNSTRING` environment variable:

```
POSTGRES_CONNSTRING="user=tom dbname=opaleye_test" stack test
```

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

The following individuals and organisations made helpful contributions
which were important in helping to get the project off the ground.

* Silk (Erik Hesselink, Adam Bergmark)
* Karamaan (Christopher Lewis)
* Fynder (Renzo Carbonara, Oliver Charles)
* Daniel Patterson
* Jakub Ryška
* Travis Staton

Joseph Abrahamson, Alfredo Di Napoli and Mietek Bak performed useful
reviews of early versions which helped improve the codebase.  Since
then there have been helpful contributions from many others.  Thanks
to them all for their help.
