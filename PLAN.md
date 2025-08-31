# Plan for development

## Tasks

- [x] Rationalise entrypoints
- [x] Add Python entrypoints rather than having inline Python in justfile
- [x] Replace all the existing test/lint/format just targets with two: `just verify` and `just fix`; the first makes no changes but fails if anything is wrong, the second fixes and reformats what it can; `just verify` should be called by the pre-commit hook
- [ ] Replace the three deployment-related just targets with a single one which deploys, checks and prints logs if there is a problem
- [ ] Investigate the two location-related just targets; explain what they're for; consider whether both are necessary
- [ ] Remove unused code

## Current task

### Goal

### Approach

### Checklist
