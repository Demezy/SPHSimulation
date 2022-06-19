# liquid-simulation
Physical based liquid simulation tool written in haskell
# Quick start
For a quick start use:

`stack run`

or

`cabal run SPHSimulation-exe`

In case of problems try to install `freeglut` package

`sudo pacman -Sy freeglut`

# Add new file
1. Add \<newFile\>.hs to the directory - "~/SPHSimulation/src/" (Name the file with a capital first letter)
2. Add to the head of new file:
```hs
 module NewFileName where
```

3. Import new file to the "~/SPHSimulation/app/Main.hs"
```hs
 import NewFileName
```

4. Add new file to the "~/SPHSimulation/SPHSimulation.cabal"
```cabal
 library
  exposed-modules:
      Lib
      Objects
      NewFile
```
