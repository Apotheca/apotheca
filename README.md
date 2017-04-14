# Apotheca DHT

A simple, encrypted, (soon-to-be) distributed data store.

### Please note:
This is a work-in-progress, and capabilities are still limited to local, non-distributed repositories. Nor is ready for use in real cryptographic scenarios. Cryptography is currently disabled for debugging purposes, but will be re-enabled in an impending update. 

## Installation

First, if you have not done so already, install [Stack]. Stack is a wonderful tool for building Haskell projects, and will make this much easier.

Once `stack` is installed, clone the repository:

```sh
$ git clone https://github.com/Apotheca/apotheca.git
```

Then, build and install from source:

```sh
$ stack build
$ stack install
```

## Usage

All apotheca commands may be listed by simply calling `apo` on the command-line

```sh
Apotheca DHT - distributed data storage

Usage: apo [-s|--store-dir STOREDIR] [-e|--ext-dir EXTDIR] [-i|--int-dir INTDIR]
           [--no-magic-slash] ([--silent] | [--fatal] | [--warn] | [--verbose] |
           [--debug]) [COMMAND]
  Manage Apotheca storage.

Available options:
  -h,--help                Show this help text
  -s,--store-dir STORE-DIR Apotheca store directory
  -e,--ext-dir EXT-DIR     External working directory
  -i,--int-dir INT-DIR     Internal working directory
  --no-magic-slash         Disable magic slash
  --silent                 Run without any print output.
  --fatal                  Run with only fatal print output.
  --warn                   Run with >= warning print output.
  --verbose                Run with >= verbose print output.
  --debug                  Run with all print output, including debug.

Available commands:
  new                      Initialize a store.
  nuke                     Nuke a store.
  where                    Find and print store directory, if it exists.
  info                     Print store info.
  list                     List files in a store.
  get                      Get a file from a store.
  put                      Put a file into a store.
  del                      Delete a file from a store.
  push                     Pushes a path into a store.
  pull                     Pulls a path from a store.
  watch                    Adds a directory to the watchlist.
  unwatch                  Removes a directory from the watchlist.
  run                      Run a distributed node.
```

Individual command help may be displayed using `-h` or `--help` flags

```sh
$ apo put -h
Apotheca DHT - distributed data storage

Usage: apo put [-o|--overwrite] [-p|--replace] [-r|--recurse] EXT-PATH INT-PATH
  Put a file into a store.

Available options:
  -h,--help                Show this help text
  -o,--overwrite           Overwrite existing files.
  -p,--replace             Replace directories instead of merging.
  -r,--recurse             Recurse over directory contents.
  EXT-PATH                 An external path; if it is a relative path, is
                           relative to EXT-DIR.
  INT-PATH                 An internal path; if it is a relative path, is
                           relative to INT-DIR.
```

## Development

In prototyping stage

## Copyright and references

Copyright (c) 2016 apotheca.io

[//]: # (Markdown help - https://help.github.com/articles/basic-writing-and-formatting-syntax/)

[//]: # (Reference links)
[stack]: <https://docs.haskellstack.org/en/stable/README/>
