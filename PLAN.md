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
- [x] don't add base time to ECMWF URLs so if there happens to be an even more recent forecast by the time we view the page then we'll see it
- [x] Determine per-location last forecast dates for meteograms instead of using wind chart date; display for each location (just brief time and date at the end of the line e.g. "(released at 1am today)")
- [x] Add shipping forecast for sea areas Portland and Plymouth (see https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/shipping-forecast)
- [x] Modify design: two columns, with forecast detail on left and links to charts etc on right, adaptive so it remains one column on mobile
- [x] Change styling for all forecast dates so they are not bold or larger letters and don't have colons at the end
- [x] Remove the bullet points in the weather/visibility lines
- [x] Investigate workflow failures and report on cause and possible fixes
- [x] Implement retry logic for ECMWF meteogram base_time parameter failures

## Current task

### Goal

### Approach

### Checklist

## Investigation Report: Workflow Failures

**Failure Pattern:**
- 2 failed workflow runs found in recent history:
  - Run 17090835255 (2025-08-20T06:49:38Z)
  - Run 17087074109 (2025-08-20T02:44:03Z)
- Both failures occur during the "Generate weather forecast HTML" step
- All other recent runs (10+) are successful

**Root Cause:**
The failure occurs in `get_meteogram_forecast_time()` function in `src/weather/scraper.py:259`

**Error Details:**
```
Exception: Meteogram redirect URL does not contain base_time parameter: https://charts.ecmwf.int/products/opencharts_meteogram?epsgram=classical_15d&lat=49.92&lon=-5.2
```

**Technical Analysis:**
1. The function expects ECMWF meteogram URLs to redirect to URLs containing a `base_time` parameter
2. When the ECMWF site doesn't include this parameter (likely due to service issues or changes), the function fails
3. This is a brittle dependency on external service behavior
4. The workflow runs every 10 minutes, so temporary ECMWF issues cause failures

**Proposed Fixes:**

1. **Immediate Fix (Graceful Degradation):**
   - Add error handling to catch this specific case
   - Fall back to a default time or skip meteogram times when ECMWF fails
   - Log the issue but don't fail the entire workflow

2. **Robust Fix (Retry Logic):**
   - Implement retry logic with backoff for ECMWF requests
   - Add timeout and error handling for network issues
   - Consider caching previous successful results as fallback

3. **Monitoring Fix:**
   - Add logging to track ECMWF service reliability
   - Consider alerting on repeated failures vs single failures
