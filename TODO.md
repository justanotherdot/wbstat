* Convert all exceptions and exception handling to using Either
* Use Data.Text instead of strings.
* Make the frequency of mangled data an optional flag (the odds thereof).
* Could probably go back and refactor some of the types that are longer product types into records.
* Integrate lenses
* Allow a format string to be passed in for analysis results.
* Put in checks for bounds for temperatures and distances; at the moment the test generator gives some pretty ridiculous values (usually when run for longer).
