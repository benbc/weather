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
- [x] Link to ECMWF pressure chart
- [x] Change all dates/times displayed on the page so they say today/yesterday/tomorrow and use daylight saving times (but don't bother to mention the timezone)
- [x] Change "24 hour forecast" and "following 24 hours" in the inshore waters forecast to use the same format as the rest of the page
- [x] Remove all fallback logic
- [x] Replace ECMWF page fetching with headless browser
- [x] Workflow is failing on GitHub
- [x] Run the workflow every ten minutes
- [x] Add links to locations in Google Maps
- [x] Open all links in the same browser tab, not a new one
- [x] Correct for limited location resolution in ECMWF forecasts
- [x] Add a Just command for deploying: pull, push, run the workflow, check it's working
- [x] Specify locations based on human-friendly description
- [x] Change the positions
- [x] Visual redesign
- [x] I've manually updated the template. Remove CSS that is no longer needed.
- [x] Fix date-based tests to be deterministic and future-proof by mocking datetime.now() instead of using dynamic date calculations
- [x] Modify deploy task so that it waits for the workflow to complete and fails if the workflow fails
- [ ] don't add base time to ECMWF URLs so if there happens to be an even more recent forecast by the time we view the page then we'll see it
- [ ] Determine per-location last forecast dates for meteograms instead of using wind chart date; display for each location (just brief time and date at the end of the line e.g. "(released at 1am today)")
- [ ] Add shipping forecast for sea areas Portland and Plymouth (see https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/shipping-forecast)
- [ ] Modify design: two columns, with forecast detail on left and links to charts etc on right, adaptive so it remains one column on mobile

## Current task

### Goal
