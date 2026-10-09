# Contributing to CSUBstats

Report bugs and suggestions at <https://github.com/emontoya2/csubstats/issues>.
For a bug, include a small reproducible example, the output of `sessionInfo()`,
and the result you expected. Avoid including confidential research data.

## Development setup

```r
install.packages(c("remotes", "roxygen2", "testthat", "rcmdcheck"))
remotes::install_deps(dependencies = TRUE)
```

Keep the existing exported function and dataset names so that code in the
training modules continues to work. Use plain language in help pages and examples.

## Documentation

Edit the roxygen comments in `R/`, then regenerate the help pages and namespace:

```r
roxygen2::roxygenise()
```

Commit the source comments, generated `man/` files, and `NAMESPACE` together.
Dataset help pages should describe the actual bundled data, including dimensions,
variable names and types, and sources.

## Checks

Run these commands from the package directory:

```r
testthat::test_local()
rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "warning")
```

The GitHub Actions workflow also checks the package on Linux, macOS, and Windows.
For changes to a statistical calculation, add a test against an appropriate
reference result. Update `NEWS.md` for changes that affect users.
