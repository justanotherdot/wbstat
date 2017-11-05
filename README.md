wbstat
======

## Installation

###### **Note If you do not have the haskell build tool `Stack` installed, please follow the instructions listed at [their docs](https://docs.haskellstack.org/en/stable/README/#how-to-install).**
```
git clone https://github.com/justanotherdot/wbstat
cd wbstat
stack build
```

## Todo

* Switch over to parser combinators.
* Remove all the exception logic and instead encore failures with types. 
* Replace all unit testing (hspec) with QuickCheck testing (emphasis on _replace_).
* Increase legibility in the main monadic operations by segreating out much of the logic. 
* Try to 'purify' as much of the logic in all modules as possible (i.e. extract as much logic that doesn't need to live in IO into it's own functions).
* Consider using a monad transformer for running the IO logic inside of the main cases; this would actually absolve the exception handling s.t. we could trap everything inside of a Maybe and handle cases of parsing / test generation / analysis failure much more easily.

## Usage

* Formats weather balloon log data into a normalized format. Log data is of the form:

  ```
  <timestamp>|<location>|<temperature>|<observatory>
  ```

  Where the `timestamp` is `yyyy-MM-ddThh:mm` in UTC/ISO 8601
  date format, the `location` is a co-ordinate `x,y` and x,
  and y are natural numbers in observatory specific units,
  the `temperature` is an integer representing temperature
  in observatory specific units, and the `observatory` is a
  code indicating where the measurements were relayed from
  relating to the following table.

  | Observatory | Temperature | Distance |
  | ----------- | ----------- | -------- |
  | AU          | celsius     | km       |
  | US          | fahrenheit  | miles    |
  | FR          | kelvin      | m        |
  | All Others  | kelvin      | km       |


## Caveats and assumptions

* For simplifications sake (but not lacking in terms of flexibility) an assumption has been made
  that the total distance is not the total distance traversed (which would mean calculating
  the euclidean distance between each point in a sorted version of this input). Instead I've made it the distance between the start and end points of the flight based upon the earliest and latest times found. I would probably do something like a mapreduce after chucking everything into a large data store to better do this.

* I made some assumptions about bounds here and there based on quick google
  searches for that wound up as magic numbers in the program e.g. I couldn't
  find a weather balloon flight prior to 1896 so I made that the lower bound for
  dates generated.

* Test generation works by generating a random seed log line, and then
  subsequently generating deltas within the acceptable bounds prescribed and
  applying them; this way the data doesn't just seem chaotic. Also, we stick to
  a normalized format of kilometers and kelvin (picked arbitrarily).

* LeapSeconds aren't handled.

* I originally implemented a way to mangle data occasionally but took it out; I
  had the exception handling working swell for the normalization mode but it was
  acting a bit wonky in the analysis mode. Exception handling wasn't
  exceptional, though; we would ignore the dirty input, but it's possible to
  easily add a flag to stop on the discovery of dirty data or some other ideal
  predetermined behavior.
