# Developer notes

## Contributing

Some things to keep in mind when making changes:

* Add a comment to `CHANGELOG.md` under "Unreleased" when adding features, fixing bugs, or otherwise changing behaviors that users can see.

## Release a new version

To release a new version, do the following workflow:

1. Create a new branch

    1. Bump version in `package.yaml`
        * All version bumps should follow [PvP](https://pvp.haskell.org/)

    1. Curate `CHANGELOG.md`, adding a `vX.Y.Z` section (but keep the `Unreleased` section)

    1. Run `stack haddock` or `cabal haddock` and skim through documentation

1. Create PR as usual and merge into `main`
    1. In the `check_sdist` CI job, check the output of the `stack sdist` step for any warnings.
    1. Inspect the test jobs, make sure tests are running (and the plugin didn't break and finds zero tests)

1. Ensure your Hackage token is set in Settings > Secrets > Actions as `HACKAGE_TOKEN_<github_username>` (replace any non alphanumeric characters in username with `_`).
    * Generate a token from `https://hackage.haskell.org/user/<hackage_username>/manage`

1. Go to the GitHub Actions page, click on the "Release" workflow, and click "Run workflow" on the `main` branch

1. Publish the candidate: https://hackage.haskell.org/package/string-syntax/candidates

1. Publish the GitHub release: https://github.com/brandonchinn178/string-syntax/releases
