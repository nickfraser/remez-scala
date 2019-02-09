# Remez-Scala

This repository provides routines to calculate the Mini-max polynomial using the Remez algorithm.

It is heavily based on the MATLAB implementation by [Sherif Tawfik](http://au.mathworks.com/matlabcentral/fileexchange/8094-remez-algorithm) (Cairo University).

## Installation

There is no hosted version of this repository.
The easiest way to install is using `sbt`'s publishing capability.

This can be done as follows:
1. Clone this repository
1. Run `sbt test`, to confirm all of the tests pass
1. Run `sbt publishLocal+` to install

Using these library components in your project can then be done by adding an `.sbt` similar to [this one](https://bitbucket.org/nick_fraser/knlms_core_gen/src/master/remez-dependent.sbt).
