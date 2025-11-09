# Plan for development

## Tasks

- [x] Add new (Haskell) version to workflow
- [x] Check that the workflow runs successfully
- [ ] Local change to `/docs/` show up as a diff even though it's ignored -- do we need to rethink?
- [ ] Change from `/docs/` to `/out/` for output directory
- [ ] New version of `just deploy` that works at the top level and does a better job of checking success

## Current task

### Goal

Switch to GitHub Pages deployment from workflow artifacts to eliminate git commits of generated files and resolve .gitignore confusion.

### Approach

1. Rewrite workflow to use artifact-based deployment (two jobs: build and deploy)
2. Remove git commit/push logic from workflow
3. Update permissions and add concurrency control
4. Configure GitHub Pages to use Actions deployment (manual step via web UI)
5. Test the new workflow

#### Constraints

- Must maintain the same 10-minute update schedule
- Both Python and Haskell outputs must still be deployed
- Requires manual GitHub Pages settings change via web interface

### Checklist

- [x] Modify workflow file to use actions/upload-pages-artifact@v4 and actions/deploy-pages@v4
- [x] Remove git commit/push steps from workflow
- [x] Add proper permissions (pages: write, id-token: write)
- [x] Verify docs files are not tracked in git
- [ ] Commit workflow changes
- [ ] Change GitHub Pages settings to "GitHub Actions" source
- [ ] Test workflow manually
- [ ] Verify deployment works correctly
