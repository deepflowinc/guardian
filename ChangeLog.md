# Changelog for guardian

## Unreleased changes

- Implements `wildcards` section: if `wildcards: true` is specified, you can use wildcard `*` in your domain entity definition.
  + You cannot use wildcards in exceptional rule targets; this is intentional.
- Now guaridan can talk with external program to get a dependency graph!
  + Currently, it supports Dot language as the only input format.
  + This enables you to use guardian to maintain:
    - non-Haskell projects
    - the dependency between arbitrary entities not limited to packages, e.g. modules.
  + See [`./dependency-domains-graphmod.yaml`](./dependency-domains-graphmod.yaml) for the example usage in conjunction with `graphmod` to maintain _module_ dependecy boundaries.
- Rework on GitHub Actions.

## 0.4.0.0

- This is the first official OSS release! :tada:
