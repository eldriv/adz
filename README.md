# adz

**adz** is a mini-wrapper CLI tool to run my Emacs development environment and others like graphic, web, and IDE tools, and to `rsync` into my NixOS machine.

## Dependencies

- [Marie](https://github.com/krei-systems/marie)

## Build

If there's no `marie` yet:

```sh
make clone
```

Then build it using:

```makefile
make build
```

To remove the executable:

```sh
make clean
```

## Usage

After building, an executable named `adz` will be created. You can run it from within the project directory using:

```sh
./adz --help
```

To make it accessible globally, add this to your shell configuration file (`.bashrc`, `.zshenv`, etc.):

```sh
export PATH="$HOME<your/project/directory/>:$PATH"
```

To test if it's working properly:

```sh
adz h
```

## Lisp Flakes

For NixOS users with flakes in their machine for development environments, modify the special variable named [`*path*`](https://github.com/eldriv/adz/blob/main/src/core.lisp) to fit your own pathname.

Then run:

```lisp
adz lf e    # To run Emacs development environment
adz lf sv   # To check SBCL version
```

## GIF

![adz usage demo](https://media3.giphy.com/media/v1.Y2lkPTc5MGI3NjExemZmM2FqOWI5bmR3MGtjODZ4cHdlOGlnb21tNHVyeTF6bDF4eGJnaiZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/ujLrQBYlHF6X2YYUmy/giphy.gif)

> **NOTE:** You can extend and change any values depending on your own needs—just see the source code to do that. If you have NixOS installed and want to use flakes, then this CLI codebase structure will be beneficial for you.
