Back: [Haskell MOC](Haskell%20MOC.md)


# Data types MOC

## Declaration

You can define automatic getters for data types

```haskell
data Universe = Universe
  {world :: [Object],
  player :: Player}
```


## Extending data type without breaking anything

If you want data types to be modifiable without changing whole program consider changing only fields that your function changes

For example:
```haskell
data Universe = Universe
  {world :: [Object],
  player :: Player}
  
data Object = Object
  { offsetX :: Double,
    offsetY :: Double,
    lowerHeight :: Double,
    upperHeight :: Double
  }

handleUniverse :: Event -> Universe -> Universe
handleUniverse (TimePassing dt) universe = universe {world = map addOffset (world universe)}
  where
    offset = - dt * 2
    addOffset object =
      object{offsetX = offsetX object + offset}
handleUniverse _ universe = universe
```

In `addOffset` function only  `offsetX` is changed and all other field are the same as in initial object