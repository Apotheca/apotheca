# Apotheca DHT

A simple, encrypted, (soon-to-be) distributed data store.

## Development

### Please note:
This is a work-in-progress and proof of concept, currently available for demonstration purposes only. Capabilities are still limited to local, non-distributed repositories, and the security implementation is incomplete.

Known insecurities:

1. Only CTR mode is used in encryption. This has implications if a cipher nonce is reused.
2. Master password is leaked to the command line history. Later, command-line improvements will fix this.
3. Auth currently exposes the master password to the filesystem. Later, shell hooks will allow for storing this password securely.
4. Block splitting occurs only after encryption, so encryption can not yet be applied per-block.
5. Per-file keys are not an option yet - all encryption currently uses the master password.

Because of this, please do not use this tool for secure storage at this time - it is currently available for demonstration purposes only.

## Installation

First, if you have not done so already, install [Stack]. Stack is a wonderful tool for building Haskell projects, and will make this much easier.

Once `stack` is installed, clone the repository:

```sh
$ git clone https://github.com/Apotheca/apotheca.git
```

Then, build and install from source:

```sh
$ stack init
$ stack build
$ stack install
```

## Usage

> NOTE: <...> means that text was removed there for brevity. Calling the commands will result in the full text.

All apotheca commands may be listed by simply calling `apo` on the command-line without arguments. It is equivalent to calling `apo` using the `-h` or `--help` flags:

```sh
$ apo --help
Apotheca DHT - distributed encrypted data storage

Usage: apo <...>
  Manage Apotheca storage.

Available options:
  <...>

Available commands:
  <...>
```

Help may also be displayed for individual commands using `-h` or `--help` flags:

```sh
$ apo put --help
<...>
```

### Creating a repo

A blank repository may be created without specifying any settings. In this case, the repository will default to performing no transformations on stored data.

```sh
$ apo new
```

The repository can also be configured when you create it, by using flags. Later, templates will be available to make this even easier. A sample configuration is below:

```sh
$ apo new --hash SHA3 --adapt-split -M 4096 -N 65536 --large 65536 --cipher AES256 --ct-hash SHA3 --gzip-best
```

Here, we create a repository that, by default:
1. Stores the hash of the plaintext using `SHA3`
2. Compresses the data using `gzip-best`
3. Encrypts the data using `AES256`
4. Stores the hash of the ciphertext using `SHA3`
5. Splits the ciphertext into blocks of between `4kb` and `64kb` using `adaptive split`, or into blocks of `64kb`
  * Or if the data is larger the `large` data limit, splits it into blocks of `64kb`.

This allows for default behavior to be specified once, instead of in subsequent commands. These flags will be explained more further on.

The location of a repo can be printed with `where`:

```sh
$ apo where
my/repo/path
```

#### Bare repos

Similar to the tool `git`, `apo` has the concept of `bare` and `hidden` repositories.

By default, a repo is created as a `hidden` repo. A hidden repo is actually stored in a hidden dot-folder name located at `STORE-DIR/.apo`, and allows for files and directories to exist 'underneath' the repo. This has special implications covered further down.

A bare repo does not hide the repo directory, instead storing it directly in `STORE-DIR`. A bare repo can be specified with the `--bare` flag upon creation:

```sh
$ apo new --bare
```

### Auth

> NOTE: Auth currently exposes the master password to the filesystem.

When creating a repository, `apo` will ask if you want to create a master password. If you choose to do so, this password will be required for interacting with the repository.

To avoid having to supply the password manually every command, you may opt to `auth` the repo. This will cache the repository master password.

```sh
$ apo auth
Enter password:
<password>
```

To once again require that the password be supplied manually, use `unauth`. This will clear the cached repository master password.

```sh
$ apo unauth
```

### Storing data

Files and directories can be stored in the repository by using `put`:

```sh
$ apo put myfiles /
Putting: somedir/myfiles
Putting: somedir/myfiles/bar.txt
Putting: somedir/myfiles/foo.txt
```

When the source is a directory, it normally does not recurse over child directories. We can opt to do this using `-r`:

```sh
$ apo put -r myfiles /
Putting: somedir/myfiles
Putting: somedir/myfiles/bar.txt
Putting: somedir/myfiles/foo.txt
Putting: somedir/myfiles/stuff
Putting: somedir/myfiles/stuff/otherthings.txt
Putting: somedir/myfiles/stuff/things.txt
```

#### Write Mode

A `write mode` may be specified to better control behavior, and to help eliminate unnecessary reads and writes. These modes are similar to the modes in the tool `zip`, but are slightly different. The write modes are:

```
-a,--add                 Add if non-existent, ignore if existent. Default.
-o,--overwrite           Add if non-existent, overwrite if existent and different.
-u,--update              Add if nonexistent, overwrite if more recent.
-e,--freshen             Ignore if non-existent, overwrite if more recent.
```

For example, `apo put --freshen foo .` will update existing files from the source only if they have more recent timestamps and differing hashes (if available), whereas `apo put --update foo .` will do the same, as well as adding new files.

By default, the `write mode` is set to `--add`.

#### Behavior flags

By default, `put` uses the behavior specified by the config, but by using flags, we can override the configuration behavior.

We can use `--hash` to specify that a plaintext checksum is to be calculated using `Tiger` and the salt `foo`. `--no-hash` can be also be specified, in order to force non-calculation of the checksum. This checksum is used when a file is being overwritten - the incoming data may be hashed and if it matches, the write is aborted because it is unnecessary.

> NOTE: Because of this, new transformations will not be applied to existing data. Overwrite should abort only if the hashes match and the transformation flags are the same, but this is not yet the case.

```sh
$ apo put --hash Tiger --salt foo myfiles /
```

We can specify compression with one of the `gzip` flags. Options are `--gzip`, `--gzip-best`, `--gzip-fast` and `--no-gzip`.

```sh
$ apo put --gzip somefile.txt /
```

We can use `--cipher` to specify encryption, and similarly, `--no-cipher` to specify no encryption. We can additionally compute a ciphertext checksum using `--ct-hash`.

```sh
$ apo put --cipher Blowfish448 --ct-hash Whirlpool myfiles /
```

As a final step, data is split into blocks. `--adapt-split` splits data into blocks of varying size between M and N, and attempts to limit the number of blocks created, whereas `--const-split` splits data into blocks of equal size.

This creates blocks between 1kb and 64kb in size, preferring as large a block as possible within the size range.

```sh
$ apo put --adapt-split -M 1024 -N 65536 myfiles /
```

This creates blocks of 64kb in size.

```sh
$ apo put --const-split 65536 myfiles /
```

> NOTE: The last block may be smaller than the rest of the blocks, as there are no options to pad it.

Currently, block splitting is only applied after transformation (post-compression, post-encryption). In the future, pre-transformation splitting will be an option.

If the repository has a `--large` limit specified, and the incoming file is larger, it will be `const-split` into blocks the size of the limit.

#### Hashes and ciphers

Available hashes can be listed with the `hashes` command:

```sh
$ apo hashes
Available hashes:
  Blake2
  MD5
  RIPEMD
  SHA1
  SHA2
  SHA3
  Skein
  Tiger
  Whirlpool
```

Available ciphers can be listed with the `ciphers` command:

```sh
$ apo ciphers
Available ciphers:
  AES256
  Blowfish448
  ChaCha256
  Salsa256
```

### Listing data

The contents of a repository can be printed using `list`, and can be made recursive with `-r`:

```sh
$ apo list -r myfiles
/myfiles/bar.txt
/myfiles/foo.txt
/myfiles/stuff
/myfiles/stuff/otherthings.txt
/myfiles/stuff/things.txt
```

### Fetching data

Files and directories can be fetched from the repository by using `get`.:

```sh
$ apo get myfiles .
Getting: /myfiles
Getting: /myfiles/bar.txt
Getting: /myfiles/foo.txt
```

Like `put`, a `write mode` may be specified, and again we may opt to recurse with `-r`.

```sh
$ apo get --update -r myfiles .
Getting: /myfiles
Getting: /myfiles/bar.txt
Getting: /myfiles/foo.txt
Getting: /myfiles/stuff
Getting: /myfiles/stuff/otherthings.txt
Getting: /myfiles/stuff/things.txt
```

### Deleting data

The data can be removed from the repository using `del`. A directory will not be deleted if it is not empty, unless `-f` is used to force deletion.

```sh
apo del -r myfiles
```

Calling `del` on root `/` will delete its children, but not the root directory itself.

```sh
$ apo del -f /
```

### STDIN and STDOUT

Sometimes, we do not want to read from or write to a file. At times, it is useful to read data from `stdin` and write it to `stdout`. Following the convention of many existing tools, if the source or destination is specified as `-`, we treat it as coming from `stdin` or `stdout` respectively. This allows `apo` to be chained with other command-line tools.

If the source is specified as `-`, we use `stdin` as the data input instead of reading from a file. The following writes "foo bar baz" from `stdin` to `foo.txt`:

> NOTE: The destination must include a filename, because a filename cannot be inferred from a handle.

```sh
$ apo put - foo.txt
> foo bar baz
```

If the destination is specified as `-`, we print the contents to `stdout` instead of writing to a file. The following reads "foo bar baz" from `foo.txt` and prints it to `stdout`:

> NOTE: The source must be a file.

```sh
$ apo get foo.txt -
foo bar baz
```

### Environment

`apo` uses 3 special directories to function.

1. The `store directory`.
2. The `external working directory`.
3. The `internal working directory`.

### STORE-DIR

The `store directory` is where the repository is actually located. Manifest, config, blocks are stored on-disk relative to this path. It can be specified with `-s` or `--store-dir`. This is convenient if you are not working under the repo, or the repo is in another directory somewhere else.

```sh
$ apo -s ~/myrepo list /
<...>
```

By default, `apo` will recurse upwards from the current working directory, looking for a repo above it in the hierarchy.

### EXT-DIR

The `external working directory` is where external files will be written to and read from. Any relative paths supplied to an `EXT-PATH` argument will be made relative to `EXT-DIR`. The following two commands are equivalent:

```sh
$ apo put ~/myfiles/foo /
$ apo -e ~/myfiles put foo /
```

By default, `EXT-DIR` will be set to the current working directory.

### INT-DIR

The `internal working directory` is where data will be written to and read from internally. Any relative paths supplied to an `INT-PATH` argument will be made relative to `INT-DIR`. The following two commands are equivalent:

```sh
$ apo put foo bar
$ apo -i bar put foo .
```

By default, `INT-DIR` will be set to the top-level root `/`, with one exception. If you are working underneath (that is, `EXT-DIR` is a subdirectory of `STORE-DIR`) a `hidden` repo, `INT-DIR` will automagically be inferred as the relative path between `EXT-DIR` and `STORE-DIR`. The following two command sets are equivalent:

```sh
$ apo -i foo/bar put baz .
```
```sh
$ cd foo/bar
$ apo put baz .
```

This allow for a repository to easily mirror an external source's structure.

#### Magic slash

Similar to the tool `rsync`, `apo` allows something called a `magic slash` can be used to change the behavior of `get` and `put`, depending on if there is a trailing slash. When a directory path has a `magic slash`, instead of applying the command against the directory, the command is applied against the children of the directory. In other words, instead of storing the directory under the target, it stores its children under the target, ignoring itself. Compare the same command without and with a `magic slash`:

```sh
$ apo put myfiles /
<...>
$ apo list -r /
/myfiles
/myfiles/bar.txt
/myfiles/foo.txt
```
```sh
$ apo put myfiles/ /
<...>
$ apo list -r /
/bar.txt
/foo.txt
/stuff
/stuff/otherthings.txt
/stuff/things.txt
```

You will note that the first only stored files directly in `myfiles`, because it was not recursive. In the second, we still did not use recursion, but because we used a `magic slash`, it was equivalent to calling `apo put <PATH> /` on each direct child of `myfiles`.

By default, `magic slash` is turned on, so be careful with trailing slashes.

#### Verbosity

By default, verbosity is set to `--terse`. This minimizes console output while still providing basic essential information.

```sh
$ apo put myfiles /
Putting: somedir/myfiles
Putting: somedir/myfiles/bar.txt
Putting: somedir/myfiles/foo.txt
```

By using `--verbose` or `--debug` instead, more information can be logged. This is the same command, but with `--debug` turned on.

```sh
$ apo --debug put myfiles /
Building environment...
Finding repo...
Found repo: "somedir"
Applied INTDIR path magic: /
Putting: somedir/myfiles
Creating dir: /myfiles
Putting: somedir/myfiles/bar.txt
Reading extfile: somedir/myfiles/bar.txt
Writing datum: /myfiles/bar.txt
Compressing with: BestCompression
Encrypting with: AES256
Storing blocks...
Storing block: local/lPm_FmcM0iE5nxRMPFqEJwz7ZM2oEGMdxbqXBXgThtY=
Putting: somedir/myfiles/foo.txt
Reading extfile: somedir/myfiles/foo.txt
Writing datum: /myfiles/foo.txt
Compressing with: BestCompression
Encrypting with: AES256
Storing blocks...
Storing block: local/J2WCIiMszhcEUcFWuGpY4ZZzQ-fe1IrgwEMT8b5FNio=
Persisting repo...
```

## Copyright and references

Copyright (c) 2017 apotheca.io

[//]: # (Markdown help - https://help.github.com/articles/basic-writing-and-formatting-syntax/)

[//]: # (Reference links)
[stack]: <https://docs.haskellstack.org/en/stable/README/>
