# Contributing to lunarcast

Thank you for considering a contribution. This outlines how to propose a
change.

## Reporting a problem

Open an issue at <https://github.com/novrisuhermi/lunarcast/issues> with a
minimal reproducible example (the [reprex](https://reprex.tidyverse.org)
package helps). For holiday dates, please include an official source.

## Fixing typos

Small fixes to the documentation can be proposed directly on GitHub. Please
edit the roxygen comments in the `.R` files under `R/`, not the generated
`.Rd` files under `man/`.

## Bigger changes

1. Open an issue first to discuss the change.
2. Fork the repository and create a branch.
3. Make the change, add tests in `tests/testthat/` and a bullet to `NEWS.md`.
4. Run `devtools::document()`, `devtools::test()` and `devtools::check()`;
   the check should report no errors, warnings or notes.
5. Open a pull request.

The code follows the [tidyverse style guide](https://style.tidyverse.org).

## Code of Conduct

Please note that the lunarcast project is released with a
[Contributor Code of Conduct](https://novrisuhermi.github.io/lunarcast/CODE_OF_CONDUCT.html).
By contributing to this project you agree to abide by its terms.
