# Halogen example

This is an example of a counter (increment/decrement) which persists its state to
the local storage.

`Storage` is used as a base functor for Halogen's `Component`. In order to run the
application `Storage` is interpreted as `Aff`.
