# Advent of Code
My solutions to [Advent of Code](adventofcode.com). Each subdir is for one year.

## 2019
Even days make you build an [Intcode](https://adventofcode.com/2019/day/2)
interpreter. I think this could be an interesting exercise to do in Haskell.

## 2020
My first partecipation. Half Haskell, half C++ solutions. Some solutions have
problems, but they all got me a (two) star(s) in the end. And
[a star is a star](https://github.com/flavio-a/adventofcode/commit/27ebee1ebcdbb9d8b562d28f34640e96c2ab77bb).

## 2021
Second time, started in Rust. I don't know if I'll have the strength to finish
this without going back to Haskell and/or C++, because Rust is really heavy
until you get used to it (eg. I spent 13 minutes to memoize a function, and I
even did it "wrong" using an `unsafe`).

Ok, I got it. When you have to manipulate strings more than "split on chars and
parse to integers" don't use Rust. Just, don't. Substrings and concatenation
are much harder than you would expect, and once you have to use string to index
an hashtable, that's it. Borrowing wasn't made for strings. Or maybe (probably)
it's just me that don't understand it, but alas.

So Lua ftw.

## 20something
I'd like to learn Rust, so I thought to use an old AoC for this purpose. It may
remain a dream for long, but who knows what boredom can do.
