# Velma

[![CI](https://github.com/tfausak/velma/actions/workflows/ci.yaml/badge.svg)](https://github.com/tfausak/velma/actions/workflows/ci.yaml)
[![Hackage](https://img.shields.io/hackage/v/velma)](https://hackage.haskell.org/package/velma)

:eyeglasses: Automatically add files to `exposed-modules` and `other-modules`.

<https://hackage.haskell.org/package/velma>

---

Although this package works, I wouldn't recommend using it.
There are critical usability problems with both Cabal and Stack.
(These problems also apply to [autopack](https://github.com/kowainik/autopack) and [cabal-auto-expose](https://github.com/deech/cabal-auto-expose).)

The problem with Stack is straightforward:
It shows a warning for modules that are automatically discovered by Velma.
Everything continues to function correctly, but the warnings are annoying and can't be disabled.
Here's the relevant issues: [#1881](https://github.com/commercialhaskell/stack/issues/1881) and [#1705](https://github.com/commercialhaskell/stack/issues/1705).

The problem with Cabal is more complicated.
Of course the core problem is that the `*.cabal` file format doesn't support globbing for module names ([#5343](https://github.com/haskell/cabal/issues/5343)).
Velma attempts to work around that by discovering modules as part of the custom setup.
Unfortunately `sdist` doesn't run the custom setup ([#3424](https://github.com/haskell/cabal/issues/3424)).
That alone wouldn't be the end of the world since Velma is meant more for applications than libraries.
But the show stopper is that Cabal won't rebuild when you change a module that was discovered by Velma.
This means that you need to reconfigure before rebuilding.

Taking all of the above into account, my suggestion is:
Use something like [hpack](https://github.com/sol/hpack) or [cabal-fmt](https://github.com/phadej/cabal-fmt).
If you're going to have to modify your build to run extra commands,
you might as well explicitly generate the list of exposed modules.
