# Plan for development

## Tasks

- [x] Add new (Haskell) version to workflow
- [x] Check that the workflow runs successfully
- [x] Local change to `/docs/` show up as a diff even though it's ignored -- do we need to rethink?
- [x] Change from `/docs/` to `/out/` for output directory
- [x] New version of `just deploy` that works at the top level and does a better job of checking success
- [ ] Make just commands uniform for the two versions (including remove dev-render)
- [ ] Add `just verify` at top level that tests both versions
- [ ] Add last update time to Haskell page (and check in `just deploy`)
- [ ] separate python/haskell updates into two different jobs (can they run in parallel?)

## Current task
