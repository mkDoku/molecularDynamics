# Molecular Dynamics in Haskell

This is a Haskell implementation of the Verlet-Velocity algorithm in a molecular dynamics
framework for my [blog
post](https://mkdoku.github.io/posts/2021-06-19-molecular-dynamics.html).

## Install

```bash
stack install
```

## Run examples

Change the `main` function in `src/Main.hs` according to the following section names
and run the example via:

```bash
stack run
```

### `mainNewton`

```haskell
main :: IO ()
main = mainNewton
```

![A particle moving to the right](./gifs/newton.gif)

### `mainNewtonBounce`

```haskell
main :: IO ()
main = mainNewtonBounce
```

![A particle moving to the right and bouncing of a wall](./gifs/newton_bounce.gif)

### `mainVerlet`

```haskell
main :: IO ()
main = mainVerlet
```

![Two particles attracting and repulsing each other](./gifs/verlet.gif)

### `mainVerletSquare`

```haskell
main :: IO ()
main = mainVerletSquare
```

![Molecular Dynamics simulation of 64 particles](./gifs/verletSquare.gif)

### `mainVerletRandom`

```haskell
main :: IO ()
main = mainVerletRandom
```

![Molecular Dynamics simulation of 24 randomly generated particles](./gifs/verletRandom.gif)

## Documentation

Have a look at [the docs](./docs/Main.html) for the API documentation.

### Generate the docs

Generate the Haddock documentation with:

```bash
stack exec -- haddock --html src/Main.hs --hyperlinked-source --odir=docs
```

