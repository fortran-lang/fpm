# Preprocess_project_root
If you have activated preprocess option in `fpm.toml` via
```toml
[preprocess]
[preprocess.cpp]
```
some predefined variables will be added to macro definitions. In this example, the macro `PROJECT_ROOT=\path\to\project` is passed to the Fortran source code.

In practice, the value of `PROJECT_ROOT`, which is literally `\path\to\project`, is simply pasted without quoted as a valid string literal. So the code using `PROJECT_ROOT` macro may not compile. You can refer to [Stringify macro with GNU gfortran](https://stackoverflow.com/a/46342008) to solve this problem.


