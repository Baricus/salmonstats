# salmonstats
`salmonstats` is a program for parsing json dumps created via [nxapi](https://gitlab.fancy.org.uk/samuel/nxapi)'s `splatnet3 dump-results` and `splatnet3 monitor` commands.
`nxapi` downloades detailed post-match results in json format.
This is a relatively simple program to analyze and query those results.

This project is still very early in development and is largely incomplete.
If you know haskell, it is relatively easy to write your own queries on the parsed data in the repl.
If not, the analysis you are looking for is likely not implemented yet.
Feel free to open an issue suggesting the kind of analysis you'd be interested in;
I eventually hope for `salmonstats` to cover most analysis needs.

## Running salmonstats
`salmonstats` is a cabal project and can be run via
```bash
cabal run salmonstats -- -h
```
Note that when running the program via cabal, `--` signifies the end of cabal's command line arguments and the start of salmonstats.
If you wish to install `salmonstats` (not currently recommended) then
```bash
cabal install
```
will build the binary and add it to your system path.
At that point, running
```bash
salmonstats -h
```
will do the same as the earlier `cabal run` command.

## Usage
As `salmonstats`'s command line interface is still being developed,
please see the help text for up-to-date usage instructions.
Be aware that some options may not be implemented yet, even if present on the command line.
