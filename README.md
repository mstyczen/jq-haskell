# Project for FP course at TU Delft

A clone of `jq` in Haskell.

Read `ASSIGNMENT.md` for task description and detailed instructions

## Build
```
> stack build
```

## Use

```
> echo '{"this" : "that"}' | jq-clone '.this'
```

## Test `jq` online

[jqplay.org](https://jqplay.org/)

## Docs

[stedolan.github.io/jq/manual](https://stedolan.github.io/jq/manual)
