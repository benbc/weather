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
- [ ] Specify locations based on human-friendly description

## Current task

### Goal
Specify locations based on human-friendly description

The descriptions currently don't match the resulting positions.
We should take a description as input, find a useable nearby-position,
_and then update the description_.
So if we are given "1M off Dartmouth", but find that the only usable position is 3M off
then the final description should be "3M off Dartmouth".

NB that we are only interested in points that are actually the sea.
The current Dartmouth point is actually on the land.
The forecasts helpfully indicate if it's a land or sea point.

### Approach
* Check current locations to identify land vs sea points from meteogram images
* Create a location search tool that takes human descriptions (e.g., "2M off Plymouth")
* Search for nearby ECMWF sea points within reasonable distance
* Calculate actual distances from landmarks and generate accurate descriptions
* Round descriptions to nearest 1M for human readability
* Build Just commands for easy location management
* Update current locations to use proper sea points with accurate descriptions
