# purescript-webstorage-free

A free monad interface to the canvas:

```haskell
import Browser.WebStorage (WebStorage())
import Browser.WebStorage.Free (runLocalStorage, setItem, getItem)

incrementCounter :: ∀ eff. Eff (webStorage :: WebStorage | eff) Unit
incrementCounter = runLocalStorage do
  value <- getItem "counter"
  setItem "counter" (value + 1)
```

[`examples`](examples/) directory contains some examples:

- [`basic`](examples/basic/)
    -- Basic usage: read a value, print it to console, write a value.
- [`halogen-storage`](examples/halogen-storage)
    -- Integrating with purescript-halogen, no other side effects allowed.
- [`halogen-storaget-aff`](examples/halogen-storaget-aff)
    -- Integrating with purescript-halogen using transformer `StorageT Aff`
       with ability to use `liftEff'`, `liftAff'` alongside.
