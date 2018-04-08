# unrpa

Extractor for RPA archives used by [RenPy](https://www.renpy.org) visual novel engine. Currently only version 3.0 of the archive format is supported.

## Installation

`unrpa` can be built using the [Nix](https://nixos.org/nix/) package manager:

```
$ nix-build
```

Use `nix-env` to install it into your user profile after executing the previous command:

```
$ nix-env -i ./result
```

## Usage

```
$ unrpa files...
```

Multiple files can be specified at once. The contents will be extracted into the current directory.

