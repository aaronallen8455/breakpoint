# Breakpoint

Set breakpoints in Haskell programs using a GHC plugin. After encountering some
serious bugs with the builtin breakpoint system in GHCi, I decided to try
implementing that functionality using a plugin.

### How to use it

Add `breakpoint` as a dependency to your project then enable breakpoints in a
given module by added `{-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}` to the top
of the file. Then import the `Debug.Breakpoint` module and use the `bp`, `bpIO`,
or `bpM` functions as appropriate to set a breakpoint.

For example:
```haskell
main :: IO ()
main = do
  let w = 5# :: Int#
      x = "one"
      y = 2 :: Int
      z = id :: Bool -> Bool
  bpIO
  pure ()
```

When the breakpoint expression gets evaluated, you will see terminal output such
as
```
### Breakpoint Hit ###
(app/Main.hs:24:3-6)
w = 5
x = "one"
y = 2
z = <Bool -> Bool>

Press enter to continue
```
showing the location of the breakpoint and the free variables that are visible
from the callsite.

Note that there is not a limitation on values having a `Show` instance: if it
does have a `Show` instance then that will be used, otherwise the output will
contain the type of the value within angle brackets.
