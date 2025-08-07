# Plan for development

## Tasks

- [x] Describe initial plan
- [x] Write simple code to scrape one piece of information:
      latest forecast date from https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/inshore-waters-forecast
- [x] Render one piece of information in a simple web page
- [x] Locate forecast data using HTML structure rather than a regex
- [ ] Deploy the simple application

## Current task

### Goal
Locate forecast data using HTML structure rather than a regex

### Approach
1. Analyze the HTML structure of the Met Office page to identify specific elements containing forecast data
2. Update the scraper to use Beautiful Soup's HTML parsing to target specific elements
3. Make the scraper more robust by using CSS selectors or element attributes
4. Test the improved scraper

### Checklist
- [x] Analyze the HTML structure of the Met Office page
- [x] Update scraper to use HTML elements instead of regex
- [x] Test the improved scraper
