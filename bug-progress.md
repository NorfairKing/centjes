# Bug: generateEmptyBlocks doesn't increment blockNum

## Bug Report

Bug in `centjes/src/Centjes/Report/Register.hs`: the `generateEmptyBlocks` function (line 984-988) and the trailing empty block generation (line 931) pass the same `blockNum` to every empty block without incrementing it. This means consecutive empty months all compute the same running average instead of each dividing by an increasing block count.

The fix: `generateEmptyBlocks` should increment `blockNum` for each empty block it generates. Similarly, the trailing empty blocks at line 931 should increment `blockNum`.

Regression test already exists: `centjes-gen/test_resources/register/valid/quarterly-subscription-monthly-average-with-begin.cent` (with its `.config` and `.txt`). The golden output currently captures the buggy behavior and will need to be reset after the fix.

Feedback loops:
1. `stack build centjes --pedantic`
2. `stack test centjes-gen --pedantic --test-arguments "quarterly-subscription-monthly-average"`
3. `nix flake check`

## Workflow Steps

### Step 0: Prepare git
Before implementing, prepare your git environment.

### Step 1: Understand the bug
Research the codebase to understand the area where the bug occurs.

### Step 2: Write a regression test
Before making any fix, write a test that reproduces the bug.

### Step 3: Falsify
Run the test and confirm that it fails.

### Step 4: Ask questions
If you have any questions about the bug, ask them now.

### Step 5: Fix the bug
Implement the simplest fix that addresses the root cause.

### Step 6: Confirm the fix
Run the regression test again and confirm that it passes.

### Step 7: Get CI to pass
Use the /ci skill to get CI to pass.

### Step 8: Commit, Push, and PR
Remove bug-progress.md, commit, push, create PR.

## Notes

- The bug is in the multi-currency path (`groupMultiIntoBlocks`). Need to check if the single-currency path (`groupSingleIntoBlocks`) has the same issue.
- Regression test files already created in the earlier conversation.

## Compaction Note

After any context compaction, re-read both `bug-progress.md` AND the `/fix` skill file to reload the full workflow instructions.
