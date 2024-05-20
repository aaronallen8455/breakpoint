# Revision history for breakpoint

## 0.1.4.0 -- 2024-02-25
* Support GHC 9.10.x
* Drop support for 9.2

## 0.1.3.1 -- 2024-02-25
* Bump `ansi-terminal` bounds

## 0.1.3.0 -- 2023-10-28
* Support GHC 9.8.x
* Drop support for GHC 8.10.x and 9.0.x

## 0.1.2.2 -- 2023-09-02
* Improvement to instance resolution for showing arbitrary values
* Strictly evaluate variable output before modifying timeouts

## 0.1.2.1 -- 2023-03-12
* Support GHC 9.6.x

## 0.1.2.0 -- 2022-11-18
* `breakpoint` and `queryVars` include a `*result` binding in their output
* Fix a bug breaking Windows compatibility
* Fix a bug with overlapping breakpoints and timeouts

## 0.1.1.1 -- 2022-11-02
* Support `IsString` version of string literals in `excludeVars`

## 0.1.1.0 -- 2022-10-30

* Support for GHC 9.4.*
* Values are pretty printed using `pretty-simple`
* Timeouts are suspended during breakpoints for GHC >= 9.2 and non-windows
* Fix a bug with monadic binds in do blocks
* Variable names are no longer visible in their definition body
* Adds `excludeVars` to ingore a list of vars, especially those that don't compile

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
