# named-binary-tag

A replacement for the old [nbt](https://github.com/acfoltzer/nbt) package, because that one has
been defunct for 2 years due to missing a single-line fix.

There are also a few improvements(?) made while I was at it:
- You can choose between a version with or without label maps using `MapNbt` or `Nbt'` respectively.
- Uses konsumlamm's [rrb-vector](https://github.com/konsumlamm/rrb-vector) package instead of a mix of lists and unboxed arrays for more well-rounded asymptotics.
- `StrictData` is enabled.
- Names of types and constructors are shortened a bit, with the reasoning that you should really be using qualified access for most of them.
- The code is shorter and better documented now (not that it really needed any documentation...)
- Less boilerplate is required to use it, at the expense of having `zlib` as a dependency. See below.

## Usage

```Haskell
import Data.Nbt qualified as Nbt
import Data.Serialize

main :: IO ()
main = do
  shouldBeNbt <- Nbt.readCompressed "level.dat" :: IO (Either String Nbt') -- see also "Nbt.readUncompressed"
  case shouldBeNbt of
    Left err -> putStrLn err
    Right nbt -> do
      print nbt
      Nbt.writeCompressed "anotherlevel.dat" nbt -- see also "Nbt.writeUncompressed"
```

## Possible Future Work

- Drop the `cereal` dependency and do bytestring parsing directly.
- Conversion to/from SNBT.
- Conversion to/from JSON.
