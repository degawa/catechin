# catechin
A wrapper for [Fortran-stdlib](https://github.com/fortran-lang/stdlib) [logger](https://stdlib.fortran-lang.org/page/specs/stdlib_logger.html), categorizing log messages and extending the feature by the operator chaining.

## Motivation
I need log-message categorization, not detaied log leveling.

## Getting started
### Requirements
- Modern Fortran compiler
    - The compilers and versions listed below have been used to develop catechin.
    - gfortran 11.2 bundled with [quickstart Fortran on Windows](https://github.com/LKedward/quickstart-fortran)
    - Intel Fortran Classic 2021.5.0 Build 20211109_000000
    - NAG Fortran 7.1 Build 7117
- [Fortran-stdlib](https://github.com/fortran-lang/stdlib)
    - catechin is a wrapper for logger provided by the Fortran-stdlib.
- [Fortran Package Manager](https://github.com/fortran-lang/fpm) (fpm) 0.7.0 alpha
    - catechin is created as an fpm project.
- [test-drive](https://github.com/fortran-lang/test-drive) 0.4.0
- [FORD](https://github.com/Fortran-FOSS-Programmers/ford) (optional)

### Get the code
To get the code, execute the following commnad:

```console
git clone https://github.com/degawa/catechin.git
cd catechin
```

### Build with fpm
To build the library using fpm, execute the following command:

```console
fpm build
```

Then, install the library using:

```console
fpm install --prefix path/to/your/libdir
```

### Reference from your project
Add the following `use` statement to modules or procedures calling par-funnel.

```Fortran
use :: catechin
```

### Reference as a fpm project's dependency
To use par-funnel in your fpm project, add the following to the fpm.toml.

```TOML
[dependencies]
catechin = {git = "https://github.com/degawa/catechin.git"}
```

## usage
catechin provides 2 logging subroutines.
- `logging(level, message, module, procedure, purpose, category)`
- `logging(logger(purpose, level, category))`
    - `logger()` can be extended using operators provided by catechin, like `logging(logger(purpose, level, category) .in. module("module name") .in. procedure("procedure name") .message. "log message")`

catechin uses several loggers for different purposes. The loggers can be configured via `configure` subroutine.
- `configure(purpose, level, timestamp, filename, stdout, log_unit, stat)`
    - `purpose`: logger
    - `level`: log level
    - `timestamp`:  flag to prepend the time stamp
    - `filename`: filename for logging of specific purpose
    - `stdout`: flag to write a log message to stdout when log messages are output to a file
    - `log_unit`: unit number opened to output log messages to a file
    - `stat`: status of configuration

### examples
```Fortran
call logging(Lv_ERROR, "error", purpose=Purpose_Trace, category="test.configure.level.warn")
! 2023-04-09 21:56:33.411: ERROR: [trace]: [test.configure.level.warn]: error
```

```Fortran
call logging(logger(Purpose_Report, Lv_INFO, "category") .in.procedure("procedure") &
             .message."log message")
! 2023-04-09 21:58:28.859: procedure: INFO: [report]: [category]: log message
```

## Todo
- To add procedure/operator for outputting variable in key-value format.