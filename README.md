# ![tila](./doc/tila_small.png)
![travis build status](https://api.travis-ci.org/ane/tila.svg)

##### An extensible system monitoring program for X status bar displays like i3bar, dzen2 and xmobar.

**tila** offers fast performance and is configured using **Scheme**, a high-level programming language, providing powerful extensibility, while at the same time offering a simple configuration syntax.

**NOTE:** tila currently supports *only i3* and is very, very early stage.

A simple configuration looks like this:

```scheme
(use tila-core)

(tila '((output . i3))       ; configuration
      (element hostname)       ; elements...
      (element date-and-time #:color "red"))
```

This will print the system hostname and the current date.

--

* [Supported elements](#supported-elements)
* [Configuration](#configuration)

--

# Supported elements

##### `hostname`
get the system hostname

##### `date-and-time [FMT]`
print current date & time. `FMT` is an optional string in the [SRFI-19](http://srfi.schemers.org/srfi-19/srfi-19.html) format.
  
##### `say-hello`
says hello

##### `load-average [COUNT]`
print load average, where `COUNT` is a number from 1 to 3, specifying how many to print of the 1m, 5m and 15m load averages

# Configuration

The top-level tila configuration is created using the `tila` procedure. tila works by re-printing its information every `n` seconds. The interval can be defined in the configuration with the `interval` parameter, it defaults to 5 seconds. Additionally, in the future, elements will support their own custom intervals.

##### `(tila <config> <elements...>)`

The config is an association list of the form `'((key . value) ...)`. The
following options are available:

* `output`
    * `i3` (default)
* `color`
    * the default color for all elements either a color name or a hex RGB
    (`#00BBCC`), defaults to `white`

The configuration is followed by any number of elements, which are created as follows.

##### `(element <procedure|string> [#:color <color>])`
  
This specifies an **element**. The procedure is either a zero-parameter function (a
thunk) or a string. The procedure is called to generate the output at every iteration.
The optional color keyword argument specifies the color used.

# Installation

Tila is written in the [Chicken](http://www.call-cc.org) Scheme
implementation. You need it installed to run tila. Chicken should be available in most Linux package managers.

To build tila, clone this repo and change to its directory and run
`chicken-install -s`. This will put the tila executable in your path. Create the
file `~/.tila` using the example above.

### For i3

Add the line `status_command tila` to your `bar` section in your i3 config.

### Others

Not implemented yet.

# License

tila is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with tila; see the file [LICENSE.md](./LICENSE.md). If not, see <http://www.gnu.org/licenses/>.
