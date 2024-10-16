# Revision history for data-effects

## 0.1.1.0 -- 2024-09-01
* Added 'Timer' effects.
* Added convenience functions for error handling with the Except effect.

## 0.2.0.0 -- 2024-10-10
* Support for the core version upgrade to 0.2.
    * Support for GHC 9.8.2.
* Changed Shift/Reset and Provider effects to unlift form, making all effects HFunctor.
* Renamed the 'Coroutine' constructor (incorrect) of the 'Status' type in coroutines to 'Continue'.

## 0.2.0.0 -- 2024-10-16
* Added parallelism effects.
* Added an effect for the `co-log` logging.
