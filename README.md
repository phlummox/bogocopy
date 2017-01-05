# bogocopy [![Hackage version](https://img.shields.io/hackage/v/bogocopy.svg?label=Hackage)](https://hackage.haskell.org/package/bogocopy) [![Linux Build Status](https://img.shields.io/travis/phlummox/bogocopy.svg?label=Linux%20build)](https://travis-ci.org/phlummox/bogocopy)

Copies a directory tree, preserving permissions and modification times, but
making zero-size sparse copies of big files.

## Installing and running

Install in the standard Haskell way: `cabal install bogocopy`, or `stack
install bogocopy` if using Stack.

Usage: 

        bogocopy [-v|--verbose] (-s|--size SIZE_MB) SRCDIR DSTDIR

> copy a directory tree, preserving permissions and modification times, but
> making zero-size sparse copies of big files
>
> `DSTDIR` will be created.

Available options:

`-h,--help`         

>  Show this help text

`--v,--verbose`     

>  Verbose (debugging) output

`--s,--size SIZE_MB`

>  Size limit, files leq to this size (in MB) are real-copied, those above are not.

### Bugs and limitations

- Limited to unix-like systems with `rsync` and `cp` commands available.
- Won't preserve the "ctime" (inode change time) of a node
- Tested in only a desultory fashion, use at your own risk

