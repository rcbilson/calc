# calc

Richard's quirky RPN calculator.

## What is it?

A calculator that can be run from a shell. It's intended to be what _I_ want
from a desk calculator, which is probably different from what _you_ want as a
desk calculator.

It has six different arithmetic modes, which you can switch among by typing a
code:

    \i  -> unbounded integer (startup mode)
    \f  -> floating point
    \8  -> 8-bit fixed-width integer
    \16 -> 16-bit fixed-width integer
    \32 -> 32-bit fixed-width integer
    \64 -> 64-bit fixed-width integer

Generic operations: `+ - * neg dup swap drop`

Integer operations: `bin dec hex chunk ^ / % & | ~ ! shift < > sb cb`

Floating-point operations: `/ ^ setp clearp setw clearw`

## Building and running

Install [GHCup](https://www.haskell.org/ghcup/install/) to get a working
version of GHC and Cabal. Then `cabal run` is all you need.

## Obtaining a compiled binary

There is a GitHub workflow that builds a couple of variants and stashes the
executables in s3 so that they can be used without having to install GHC.

* [Linux-x86_64](https://s3.amazonaws.com/rcbilson-dist/Linux/x86_64/calc)
* [Linux-aarch64](https://s3.amazonaws.com/rcbilson-dist/Linux/aarch64/calc)

These are built with GitHub's standard ubuntu runners so they will work on many
but by no means all Linuxes.

## Tmux

I actually use this most of the time directly from tmux. In particular my
configuration is

    bind-key -n M-c split-window -v -l 6 ~/dotfiles/bin/calc

When I type 'Alt-C' this opens up a small pane at the bottom of my current pane
with the calculator running.
