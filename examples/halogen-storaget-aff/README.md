# Halogen transformer example

This is an example of a counter (increment/decrement) which persists its state to
the local storage and prints to the console.

`StorageT (Aff _)` is used as a base functor for Halogen's `Component`.
In order to run the application `StorageT (Aff _)` is interpreted as `Aff`.
