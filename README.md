![tila](./doc/tila_small.png)
https://api.travis-ci.org/ane/tila.svg

##### A system monitoring program to be used with X status bar programs like i3bar, dzen2 and xmobar.

**tila** is performant an extensible. **tila** is configured using a high-level
programming language called **Scheme**, instead of relying on a custom
configuration language. This lets you wield the power of a full programming
language when creating your displays.

tila currently supports only **i3** and is very, very early stage.

A simple configuration looks like this:

```scheme
(use tila-core)

(tila '((output . i3))
    (element hostname)
    (element date-and-time #:color "red"))
```

This will print the system hostname and the current date.

### Configuration structure

The configuration is created using the `tila` procedure.

#### `tila config elements...`

The config is an association list of the form `'((key . value) ...)`. The
following options are available:

* `output`
    * `i3` (default)
* `color`
    * the default color for all elements either a color name or a hex RGB
    (`#00BBCC`), defaults to `white`

#### `element procedure|string [#:color <color>]`
  
Specifies an **element**. The procedure is either a zero-parameter function (a
thunk) or a string. The optional color keyword argument specifies the color used.

### Supported elements

#### `hostname`
get the system hostname

#### `date-and-time [FMT]`
print current date & time. `FMT` is an optional string in the [SRFI-19](http://srfi.schemers.org/srfi-19/srfi-19.html) format.
  
#### `say-hello`
says hello

#### `load-average [COUNT]`
print load average, where `COUNT` is a number from 1 to 3, specifying how many to print of the 1m, 5m and 15m load averages

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
