# Recs

Recs is an archetypal ECS for Haskell.

## Goals

1. Speed
    * Cache locality is key: `recs` is specifically designed with cache locality in mind
        (to the extent that Haskell allows). Components are stored unboxed by default.
    * Iteration times: `recs` is an archetypal ECS, which often have faster iteration times
        compared to other designs (like sparse sets)
2. Simplicity
    * `recs` tries to use as few ECS primitives as possible
    * Making illegal states unrepresentable: foot-gunning is very unlikely by design
3. Flexibility

## Design

Recs is based on Bevy ECS, written in Rust. Bevy's design is straightforward for users
and helpful for designers. In particular, it:

  * Provides scoped ECS access rules via system parameters
    * This gives the scheduler proof that all instances of component access are tracked, which
      allows it to AOT optimally schedule systems for parallel execution. Other ECS designs use
      suboptimal (scheduling-wise) job systems.

```haskell
mySystem ::
     Global (Nab MyGlobal)                   -- Access a global resource as read-only
  -> Query  (Nab Position |&| Stow Velocity) -- Get a query that filters for Position and Velocity
  -> System ()                               -- Our system returns nothing
mySystem g q =
  forQ q $ \qh -> do                         -- Iterate over the query results
    (MkPosition p) <- nab qh                 -- Pull a position from this query
    liftIO . putStrLn $ "At: " <> show p
    MkMyGlobal <- nab g                      -- Getting globals with the same syntax
    stow qh (MkVelocity 5.0)                 -- Modify this entity's 'Velocity' component
```
