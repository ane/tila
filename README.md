![tila](./doc/tila_small.png)

##### A system monitoring program for status bars like i3bar, dzen and xmobar.

**tila** is configured using a high-level programming language called **Scheme**,
instead of relying on ad-hoc configuration file language DSLs that are tricky to extend. This lets you write powerful status display modules without running into the limitations of shell scripts.

tila currently supports only **i3** and is very, very early stage.

A simple configuration looks like this:

```scheme
(import tila-core)

(tila 'i3
    (element hostname)
    (element date-and-time #:color "red"))
```

This will print the system hostname and the current date.

## Supported elements

* `hostname` - get the system hostname
* `date-and-time` - current date & time
* `say-hello` - says hello

Writing your own is easy. All you need is a function that returns a string. So
even `(element (lambda () "blah"))` will work.

The config syntax is being worked on, though. Currently they need to be
zero-parameter functions.

## Installation

Tila is written in the [Chicken](http://www.call-cc.org) Scheme
implementation. You need it installed to run tila. Chicken should be available in most Linux package managers.

To build tila, clone this repo and change to its directory and run
`chicken-install -s`. This will put the tila executable in your path. Create the
file `~/.tila` using the example above.

### For i3

Add the line `status_command tila` to your `bar` section in your i3 config.

### Others

Not implemented yet.


######

## License

Tila is free software, released under the GNU General Public License
version 3. See the [LICENSE](./LICENSE.md).
