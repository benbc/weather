# Plan for development

## Tasks

- [x] Rationalise entrypoints
- [x] Add Python entrypoints rather than having inline Python in justfile
- [x] Replace all the existing test/lint/format just targets with two: `just verify` and `just fix`; the first makes no changes but fails if anything is wrong, the second fixes and reformats what it can; `just verify` should be called by the pre-commit hook
- [x] Replace the three deployment-related just targets with a single one which deploys, checks and prints logs if there is a problem
- [ ] Investigate the two location-related just targets; explain what they're for; consider whether both are necessary
- [ ] Remove unused code

## Current task

### Goal
Consolidate three separate deployment-related commands (`deploy`, `check-deployment`, `deployment-logs`) into a single intelligent `deploy` command that handles the full deployment lifecycle and automatically shows logs if there are problems.

### Approach
1. Analyze current deployment commands:
   - `deploy`: Does full deployment (pull, push, trigger, watch)
   - `check-deployment`: Shows status of latest deployment  
   - `deployment-logs`: Shows logs from latest deployment
2. Design new unified `deploy` command that:
   - Performs the deployment (existing `deploy` functionality)
   - Automatically checks status after completion
   - Shows logs if deployment fails or has issues
   - Provides clear success/failure messaging
3. Remove the old separate `check-deployment` and `deployment-logs` commands
4. Test the new command handles both success and failure scenarios

### Checklist
- [ ] Analyze current deployment commands
- [ ] Design unified deploy command logic
- [ ] Update deploy command to include status checking and error handling
- [ ] Remove old check-deployment and deployment-logs commands
- [ ] Test deploy command works correctly
- [ ] Test error handling shows logs appropriately  
- [ ] Commit changes
