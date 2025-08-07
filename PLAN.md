# Plan for development

## Tasks

- [x] Describe initial plan
- [x] Write simple code to scrape one piece of information
- [x] Render one piece of information in a simple web page
- [x] Locate forecast data using HTML structure rather than a regex
- [x] Deploy the simple application
- [x] Fix the URL
- [x] Work out why Claude config isn't being picked up (commit authoring)
- [x] Add desktop notifictions to Claude config
- [x] Add tests
- [x] "100%" code coverage
- [x] Add code formating with ruff
- [x] Scrape the latest inshore waters forecast for "Lyme Regis to Lands End" sea area and display it on the page
- [x] Fix problem with git conflicts in docs/index.html when we change locally and the workflow runs remotely
- [ ] Link to ECMWF pressure chart

## Current task

### Goal

Chart is here: : https://charts.ecmwf.int/products/medium-wind-10m?projection=opencharts_north_west_europe

That page is configurable has parameters for "base time" (when the forecast was released)
and "valid time" (the time for which the forecast is made).
The values for both can be set in the URL (?base_time=202508070000&projection=opencharts_north_west_europe&valid_time=202508170000)
or in dropdowns on the page.

We are always interested in the most recent forecast (base time).
The scrapper should check what that time is and annotate the link with that time so we know if it's worth checking the charts.
