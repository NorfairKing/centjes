# Bug: Price changes counted as transactions in average calculation

## Bug Report

In convert-year-filter.txt, a price change is counted as a transaction but it shouldn't be. This affects the running average calculation in the single-currency register path.

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

- The bug is in the single-currency path (groupSingleIntoBlocks) where `hasTransactions` determines whether blockNum is incremented.
- Price change entries (revaluations) should not be counted as transactions for the average.

## Compaction Note

After any context compaction, re-read both `bug-progress.md` AND the `/fix` skill file to reload the full workflow instructions.
