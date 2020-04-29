# Preparing your package for FPM

This document describes how you need to organize your application or library
for it to successfully build with the Fortran Package Manager (FPM). 

* [What kind of package can FPM build?](#what-kind-of-package-can-fpm-build)
* [Example package layouts](#example-package-layouts)
  - [Single program](#single-program)
  - [Single-module library](#single-module-library)
  - [Multi-module library](#multi-module-library)
  - [Application and library](#application-and-library)
  - [Multi-level library](#multi-level-library)

## What kind of package can FPM build?

You can use FPM to build:

* Applications (program only)
* Libraries (modules only)
* Combination of the two (programs and modules combined)

Let's look at some examples of different kinds of package layouts that you can
use with FPM.

## Example package layouts

This section describes some example package layouts that you can build with FPM.
You can use them to model the layout of your own package.

### Single program

Let's start with the simplest package imaginable--a single program without 
dependencies or modules. 
Here's what the layout of the top-level directory looks like:

```
.
├── app
│   └── main.f90
└── fpm.toml
```

We have one source file (`main.f90`) in one directory (`app`).
Its contents are:

```fortran
program hello
  print *, 'Hello, World!'
end program hello
```

This program prints the usual greeting to the standard output, and nothing more.


There's another important file in the top-level directory, `fpm.toml`.
This is FPM's configuration file specific to your package.
It includes all the data that FPM needs to build your app.
In our simple case, it looks like this:

```toml
name = "hello"
version = "0.1.0"
license = "MIT"
author = "Jane Programmer"
maintainer = "jane@example.com"
copyright = "2020 Jane Programmer"
dependencies = []
compiler = "gfortran"
```

The preamble includes some metadata, such as `license`, `author`, and similar, 
that you may have seen in other package manager configuration files.
The one option that matters here right now is:

```toml
name = "hello"
``` 

This line specifies the name of your package, which determines the name of 
the executable file of your program.
In this example, our program executable, once built, will be called `hello`. 

Let's now build this program using FPM:

```
$ fpm build
# gfortran (for build/debug/app/main.o)
# gfortran (for build/debug/app/hello)
```

On the first line, we ran `fpm build` to compile and link the application.
The latter two lines are emitted by FPM, and indicate which command was 
executed at each build step (`gfortran`), and which files have been output 
by it: object file `main.o`, and executable `hello`.

We can now run the app with `fpm run`:

```
$ fpm run
 Hello, World!
```

If your application needs to use a module internally, but you don't intent 
to build it as a library to be used in other projects, you can include the 
module in your program source file as well.
For example:

```fortran
$ cat app/main.f90
module math_constants
  real, parameter :: pi = 4 * atan(1.)
end module math_constants


program hello
  use math_constants, only: pi
  print *, 'Hello, World!'
  print *, 'pi = ', pi
end program hello
```

Now run this using `fpm run`:

```
$ fpm run
# gfortran (for build/debug/app/main.o)
# gfortran (for build/debug/app/hello)
 Hello, World!
 pi =    3.14159274  
```

Notice that you can run `fpm run`, and if the package hasn't been built yet,
`fpm build` will run automatically for you. 
This is true if the source files have been updated since the last build.
Thus, if you want to run your application, you can skip the `fpm build` step,
and go straight to `fpm run`.

Although we have named our program `hello`, which is the same name as the 
package name in `fpm.toml`, you can name it anything you want as long as it's
permitted by the language.

In this last example, our source file defined a `math_constants` module
inside the same source file as the main program. 
Let's see how we can define an FPM package that makes this module available
as a library.

### Single-module library

The package layout for this example looks like this:

```
.
├── fpm.toml
└── src
    └── math_constants.f90
```

In this example we'll build a simple math constants library that exports 
the number pi as a parameter:

```fortran
$ cat src/math_constants.f90 
module math_constants
  real, parameter :: pi = 4 * atan(1.)
end module math_constants
```

and our `fpm.toml` is the same as before.

Now use `fpm build` to build the package:

```
$ fpm build
# gfortran (for build/debug/library/math_constants.o build/debug/library/math_constants.mod)
# ar (for build/debug/library/math_constants.a)
ar: creating build/debug/library/math_constants.a
```

Based on the output of `fpm build`, FPM first ran `gfortran` to emit the 
binary object (`math_constants.o`) and module (`math_constants.mod`) files.
Then it ran `ar` to create a static library archive `math_constants.a`. 
`build/debug/library` is thus both your include and library path, should you
want to compile and link an exteranl program with this library.

For modules in the top-level (`src`) directory, FPM requires that:

* The module has the same name as the source file.
* There is only one module per file.

These two requirements simplify the build process for FPM. 
As Fortran compilers emit module files (`.mod`) with the same name as the 
module itself (but not the source file, `.f90`), naming the module the same as 
the source file allows FPM to:

* Uniquely and exactly map a source file (`.f90`) to its object (`.o`) and 
module (`.mod`) files.
* Avoid conflicts with modules of the same name that could appear in dependency
packages (more on this in a bit).

Since this is a library without executable programs, `fpm run` here does 
nothing.

In this example, our library is made of only one module.
However, most real-world libraries are likely to use multiple modules.
Let's see how you can package your multi-module library. 

### Multi-module library

In this example, we'll use another module to define a 64-bit real kind
parameter and make it available in `math_constants` to define `pi` with 
higher precision.
To make this exercise worthwhile, we'll define another math constant, 
Euler's number.

Our package layout looks like this:

```
.
├── fpm.toml
└── src
    ├── math_constants.f90
    └── type_kinds.f90
```

and our source file contents are:

```fortran
$ cat src/math_constants.f90 
module math_constants
  use type_kinds, only: rk
  real(rk), parameter :: pi = 4 * atan(1._rk)
  real(rk), parameter :: e = exp(1._rk)
end module math_constants

$ cat src/type_kinds.f90 
module type_kinds
  use iso_fortran_env, only: real64
  integer, parameter :: rk = real64
end module type_kinds
```

and there are no changes to our `fpm.toml` relative to previous examples.

Like before, notice that the module `type_kinds` is name exactly as the 
source file that contains it. 
This is important.

By now you know how to build the package:

```
$ fpm build
# gfortran (for build/debug/library/type_kinds.o build/debug/library/type_kinds.mod)
# gfortran (for build/debug/library/math_constants.o build/debug/library/math_constants.mod)
# ar (for build/debug/library/math_constants.a)
ar: creating build/debug/library/math_constants.a
```

Our build path now contains:

```
$ ls build/debug/library/
math_constants.a  math_constants.mod  math_constants.o  type_kinds.mod  type_kinds.o
```

and the static library includes all the object files:

```
$ nm build/debug/library/math_constants.a 

math_constants.o:

type_kinds.o:
```

The takeaways from this example are that:

* FPM automatically scanned the `src` directory for any source files.
* It also resolved the dependency order between different modules.

### Application and library

Let's now combine the two previous examples into one: 
We'll build the math constants library _and_ an executable program that uses
it.
We'll use this program as a demo, and to verify that defining higher-precision
constants from the previous example actually worked.

Here's the package layout for your application + library package:

```
.
├── app
│   └── main.f90
├── fpm.toml
└── src
    ├── math_constants.f90
    └── type_kinds.f90
```

Our `fpm.toml` remains unchanged and our executable program source file is:

```fortran
$ cat app/main.f90 
program demo
  use math_constants, only: e, pi
  print *, 'math_constants library demo'
  print *, 'pi = ', pi
  print *, 'e = ', e
end program demo
```

Let's go straight to running the demo program:

```
$ fpm run
# gfortran (for build/debug/library/type_kinds.o build/debug/library/type_kinds.mod)
# gfortran (for build/debug/library/math_constants.o build/debug/library/math_constants.mod)
# ar (for build/debug/library/math_constants.a)
ar: creating build/debug/library/math_constants.a
# gfortran (for build/debug/app/main.o)
# gfortran (for build/debug/app/math_constants)
 math_constants library demo
 pi =    3.1415926535897931     
 e =    2.7182818284590451  
```

The FPM build + run process works as expected, and our program correctly outputs
higher-precision constants.

So far we covered how FPM builds:

* A single program
* A single-module library
* A multi-module library
* A program and a library

However, all our modules so far have been organized in the top level source 
directory. 
More complex libraries may organize their modules in subdirectories.
Let's see how we can build this with FPM.

### Multi-level library

In this example, we'll define our library as a collection of modules,
two of which are defined in a subdirectory:

```
.
├── app
│   └── main.f90
├── fpm.toml
└── src
    ├── math_constants
    │   ├── derived.f90
    │   └── fundamental.f90
    ├── math_constants.f90
    └── type_kinds.f90
```

First, `fpm.toml` and `src/type_kinds.f90` remain unchanged relative to the 
previous example.

The rest of the source files are:

```fortran
$ cat src/math_constants.f90 
module math_constants
  use math_constants_fundamental, only: e, pi
  use math_constants_derived, only: half_pi, two_pi
end module math_constants

$ cat src/math_constants/fundamental.f90 
module math_constants_fundamental
  use type_kinds, only: rk
  real(rk), parameter :: pi = 4 * atan(1._rk)
  real(rk), parameter :: e = exp(1._rk)
end module math_constants_fundamental

$ cat src/math_constants/derived.f90 
module math_constants_derived
  use math_constants_fundamental, only: pi
  use type_kinds, only: rk
  real(rk), parameter :: two_pi = 2 * pi
  real(rk), parameter :: half_pi = pi / 2
end module math_constants_derived

$ cat app/main.f90 
program demo
  use math_constants, only: e, pi, half_pi, two_pi
  print *, 'math_constants library demo'
  print *, 'pi = ', pi
  print *, '2*pi = ', two_pi
  print *, 'pi/2 = ', half_pi
  print *, 'e = ', e
end program demo
```

Our top-level `math_constants` module now doesn't define the constants,
but imports them from the two modules in the subdirectory.
Constants `e` and `pi` we define in the `math_constants_fundamental` module,
and `two_pi` and `half_pi` in the `math_constants_derived` module.
From the main program, we access all the constants from the top-level
module `math_constants`.

Let's build and run this package:

```
$ fpm run
# gfortran (for build/debug/library/type_kinds.o build/debug/library/type_kinds.mod)
# gfortran (for build/debug/library/math_constants_fundamental.o build/debug/library/math_constants_fundamental.mod)
# gfortran (for build/debug/library/math_constants_derived.o build/debug/library/math_constants_derived.mod)
# gfortran (for build/debug/library/math_constants.o build/debug/library/math_constants.mod)
# ar (for build/debug/library/math_constants.a)
ar: creating build/debug/library/math_constants.a
# gfortran (for build/debug/app/main.o)
# gfortran (for build/debug/app/math_constants)
 math_constants library demo
 pi =    3.1415926535897931     
 2*pi =    6.2831853071795862     
 pi/2 =    1.5707963267948966     
 e =    2.7182818284590451
```

Again, FPM built and run the package as expected.

Recall from an earlier example that FPM required the modules in the top-level
`src` directory to be named the same as their source file.
This is why `src/math_constants.f90` defines `module math_constants`.

For modules defined in subdirectories, there's an additional requirement:
module name must contain the path components of the directory that its 
source file is in. 
In our case, `src/math_constants/fundamental.f90` defines 
the `math_constants_fundamental` module.
Likewise, `src/math_constants/derived.f90` defines 
the `math_constants_derived` module.

This rule applies generally to any number of nested directories and modules.
For example, `src/a/b/c/d.f90` must define a module called `a_b_c_d`.

Takeaways from this example are that:

* You can place your module source files in any levels of subdirectories inside `src`.
* The module name must include the path components and the source file name--for example, 
`src/a/b/c/d.f90` must define a module called `a_b_c_d`. 
