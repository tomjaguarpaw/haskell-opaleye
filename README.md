# Brief introduction to Opaleye

Opaleye is a Haskell library which provides an SQL-generating embedded
domain specific language for targeting Postgres.  You need Opaleye if
you want to use Haskell to write typesafe and composable code to query
your Postgres database.

## Tutorials

Please get started with Opaleye by referring to these two tutorials

* [Basic tutorial](Doc/Tutorial/TutorialBasic.md)
* [Manipulation tutorial](Doc/Tutorial/TutorialManipulation.md)

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

# Contributors

The Opaleye Project was founded by Tom Ellis, inspired by theoretical
work on databases by David Spivak.  Much of the implementation was
based on ideas and code from the HaskellDB project by Daan Leijen,
Conny Andersson, Martin Andersson, Mary Bergman, Victor Blomqvist,
Bjorn Bringert, Anders Hockersten, Torbjorn Martin, Jeremy Shaw and
Justin Bailey.

Silk (Erik Hesselink, Adam Bergmark), Karamaan (Christopher Lewis),
Fynder (Renzo Carbonara, Oliver Charles) and Daniel Patterson
contributed code to the project.

Joseph Abrahamson, Alfredo Di Napoli and Mietek Bak performed useful
reviews of early versions which helped improve the codebase.
