# Breakpoint

The ability to set breakpoints in a program can be an extremely valuable
debugging tool. While GHCi has built-in support for setting breakpoints, it
suffers from several critical limitations:
- It's prohibitively buggy when used with concurrent programs, such as web servers.
- Breakpoints can only be set in interpreted code.

The `breakpoint` library solves these problems by implementing breakpoints as
a GHC plugin.

### Quick start guide

Add `breakpoint` as a dependency to your project then enable breakpoints in a
given module by adding `{-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}` to the
top of the file. Then import the `Debug.Breakpoint` module and use the
`breakpoint`, `breakpointIO`, or `breakpointM` functions as appropriate to set
a breakpoint.

- `breakpoint :: a -> a` is for use in pure code. Apart from the side-effect of
  setting a breakpoint, it is the identity function.
- `breakpointIO :: MonadIO m => m ()` is for monadic code that can perform IO.
- `breakpointM :: Applicative f => f ()` is for arbitrary `Applicative`
  contexts.

`breakpoint` and `breakpointM` both use `unsafePerformIO` which means they are
at the mercy of the simplifier and all the other pitfalls of lazy IO. For this
reason, it's generally preferable to use `breakpointIO` in contexts that
support it.

Here's an example module:
```haskell
{-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}

import Debug.Breakpoint

main :: IO ()
main = do
  x <- getLine
  let y = 2 :: Int
      z = id :: Bool -> Bool
  breakpointIO
  pure ()
```

When the breakpoint expression gets evaluated, you will see terminal output such
as
```
### Breakpoint Hit ###
(app/Main.hs:24:3-6)
x = "input"
y = 2
z = <Bool -> Bool>

Press enter to continue
```
showing the location of the breakpoint and the free variables that are visible
from the callsite, this includes function arguments, let bindings, where binds,
monadic binds, pattern binds, etc.

If the type of a value has a `Show` instance then that will be used to generate
the printed value, otherwise the output will contain the type of the value
within angle brackets.

Execution of the program effectively halts on waiting for user input. In
concurrent programs, all threads will be stopped, not just the one executing
the breakpoint.

### Caveats
- Currently only supports GHC version 8.10.x, 9.0.x, and 9.2.x
- Printing values may cause thunks to be evaluated earlier than they otherwise
  would which could be problematic for programs that rely heavily on laziness.
- Calls to `threadDelay` are not paused by breakpoints in the sense that time
  continues to elapse, however they won't unblock until the breakpoint
  finishes.
- Implicit params are not currently supported
- `RecursiveDo` binds aren't visible before they are bound, despite being in scope.
- If there is anything buffered in `stdin` then that will interfere with the
  blocking mechanism.
