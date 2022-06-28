<h1>
	<p align="center">
	SPH Simulation
	</p>
</h1>

Smothered-particle hydrodynamics simulation of liquids written in
Haskell.

## Quick start

To build and run the project using Stack use:

```sh
stack run
```
Or using Cabal:

```sh
cabal run SPHSimulation-exe
```

### Known issues

In case of having problems with OpenGL install
[freeglut](http://freeglut.sourceforge.net/)

macOS:
```sh
brew install freeglut
```

Arch Linux:
```sh
sudo pacman -Sy --noconfirm freeglut
```

## Development

### Adding new file

1. Create `NewFileName.hs` in `./src` folder

**Important**: Filename should start with capital letter

2. Head of the file should contain module declaration

```hs
 module NewFileName where
```

3. Import this module to `./app/Main.hs`

```hs
 import NewFileName
```

1. Update `./SPHSimulation.cabal` correspondingly
```cabal
 library
  exposed-modules:
      Lib
      Objects
      NewFileName
```

## Profiling

In some case you may want to profile program.

You first need to reinstall packages with profiling capabilities
```
cabal install --reinstall --enable-profiling
```

Then edit cabal configuration (`SPHSimulation.cabal`) with such ghc options
```
ghc-options: -threaded -rtsopts "-with-rtsopts=-N -p" -prof -O2
```

And finally run with profiling enabled
```
cabal run SPHSimulation-exe --enable-profiling
```

You will find `*.prof` file at root dir of project

