# Apotheca DHT

A simple, encrypted, distributed data store.

## Installation

First, if you have not done so already, install [Stack]. Stack is a wonderful tool for building Haskell projects, and will make this much easier.

Once `stack` is installed, clone the repository:

```sh
$ git clone https://caligo.xyz/[repo-name].git
```

Then, build and install from source:

```sh
$ stack build
$ stack install
```

## Usage

All caligo commands may be listed by simply calling caligo

```sh
$ caligo
Apotheca DHT - distributed data storage

Usage: caligo [-s|--store STORE] [COMMAND]
  Manage Apotheca storage.

Available options:
  -h,--help                Show this help text
  -s,--store STORE         Apotheca store directory

Available commands:
  new                      Initialize a store.
  nuke                     Nuke a store.
  info                     Print store environment info.
  get                      Get a file from storage.
  put                      Put a file into storage.
  del                      Delete a file from storage.
  list                     List files in storage.
```

Individual command help may be displayed using `-h` or `--help` flags

```sh
$ caligo put --help
Apotheca DHT - distributed data storage

Usage: caligo put KEY SRC-FILE
  Put a file into storage.

Available options:
  -h,--help                Show this help text
```

## Development

In prototyping stage

## Copyright and references

Copyright (c) 2016 Apotheca.xyz | Readme constructed via [StackEdit][mdeditor]

[//]: # (Markdown help - https://help.github.com/articles/basic-writing-and-formatting-syntax/)

[//]: # (Reference links)
[stack]: <https://docs.haskellstack.org/en/stable/README/>
[mdeditor]: <https://stackedit.io/editor>
