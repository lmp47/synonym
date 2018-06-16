Overview
===========

Synonym contains three automated tools for proving hyperproperties:

1. [descartes](https://github.com/marcelosousa/descartes)
2. syn - exploits synchrony
3. synonym - exploits synchrony and symmetry

Installation
===========

Synonym is implemented as a modification of Descartes, and
installation proceeds similarly:

First install language-java and z3 by running

* `cabal install` in *dependencies/language-java-0.2.7* and
* `cabal install` in *dependencies/z3-4.0.0*.

To install the main package, run `cabal install` in the 
*synonym* directory.

Running Synonym
=================

Running `synonym --h` will print the help information.

To run Synonym, you must provide options `-p` and `-m`
and a Java file.

* The `-p` option gives the number of the hyperproperty
to verify, and
* the `-m` option gives the name of the
tool to use to verify it (which must be one of
descartes, syn, or synonym).

E.g. the following will have syn try to prove property 1
on FILE.java.

    synonym -p=1 -m=syn FILE.java

The directory *benchmarks/cav-18* contains examples on which
the tools can be run.

To add new properties, the files *src/Analysis/Properties.hs*
and *src/Main.hs* need to be modified.
Information about the properties can be found in *src/Main.hs*.
