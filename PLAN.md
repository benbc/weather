# Plan for development

## Tasks

- [x] Rationalise entrypoints
- [x] Add Python entrypoints rather than having inline Python in justfile
- [x] Replace all the existing test/lint/format just targets with two: `just verify` and `just fix`; the first makes no changes but fails if anything is wrong, the second fixes and reformats what it can; `just verify` should be called by the pre-commit hook
- [x] Replace the three deployment-related just targets with a single one which deploys, checks and prints logs if there is a problem
- [x] Investigate the two location-related just targets; explain what they're for; consider whether both are necessary
- [ ] Add a new target for adding locations
- [ ] Remove unused code

## Current task

### Goal

Create a user-friendly interface for adding new locations. This needs to make a best-effort attempt to find a location
that is near where the user has requested but which can be provided as a "sea point" meteogram by the ECMWF. The
location should be specified in a user-friendly way (e.g. "2M off Salcombe"). After finding the closest meteogram

### Approach

#### Constraints

We are constrained to locations where meteograms are available from the ECMWF

### Checklist
