## v0.2.1

  * Semigroup fix for GHC 8.4 (requires `non-negative` 0.1.2 with same fix)

## v0.2

  * added `TimeSig` type and associated functions, so time signatures
    in a `MeasureMap` remember their denominators
    instead of e.g. converting 6/8 to 3/4

  * `unmakeTempoMap`, `unmakeMeasureMap`

  * relicensed as BSD3

  * note: the only change that necessitated a major version bump
    was the removal of the `Ord` instance for `TempoMap` and `MeasureMap`

## v0.1.1.1

  * fixed a bug in "extractFirst" (the time value returned was incorrect)

## v0.1.1

  * added `Eq`/`Ord`/`Show` instances to `TempoMap` and `MeasureMap`

  * `TempoMap` can go back to an event list of tempos,
    and `MeasureMap` can go back to an event list of time signatures

## v0.1

  * initial release
