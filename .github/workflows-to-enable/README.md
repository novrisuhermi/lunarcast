# GitHub Actions workflows to enable

These two standard workflows from [r-lib/actions](https://github.com/r-lib/actions)
could not be pushed by the access token used to create this repository
(GitHub requires the *Workflows* permission for files in `.github/workflows/`).

To enable them, move both files into `.github/workflows/`:

* `R-CMD-check.yaml` runs `R CMD check` on macOS, Windows and Linux at every
  push and pull request (this feeds the badge in the README).
* `pkgdown.yaml` rebuilds the documentation website and deploys it to the
  `gh-pages` branch.

From a local clone:

```sh
git mv .github/workflows-to-enable/*.yaml .github/workflows/
git commit -m "Enable GitHub Actions" && git push
```

Alternatively, with usethis: `usethis::use_github_action("check-standard")`
and `usethis::use_github_action("pkgdown")`, then delete this folder.
