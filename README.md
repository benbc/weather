# Weather aggregator

Aggregates various sources of weather information into one place
to make it easy to see the latest forecasts when sailing.

Deployed at https://benbc.github.io/weather/.

## Implementation

This was originally an experiment in "vibe-coding",
to see if it was possible to produce something useful without caring about what the code looked like.
It was built entirely by telling Claude Code (Sonnet 4) what the results should look like.

The only thing I did to try to influence code quality was to tell it to write unit tests
and enforce full code coverage.
The tests themselves are very low quality, of course,
but my impression is that the extra feedback loops helped.
I also got it to add an end-to-end test and automate deployment to reduce the checking burden on me.

The original version was written in Python and now lives in the `python` directory.
There is a new hand-coded version under development, written in Haskell, in the `haskell` directory.
