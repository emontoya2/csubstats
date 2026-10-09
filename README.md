# CSUBstats

CSUBstats provides R functions and datasets for teaching and learning statistics.
It supports [Training modules on selected statistical methods](https://emontoya2.github.io/tmsm/)
and other educational resources, with functions for comparing groups, regression,
and exploratory data analysis.

Several functions build on existing functions in R and other packages, bringing
together statistical tests, summaries, and plots in a format designed for teaching
and learning.

The GitHub repository is named `csubstats`; the R package is named `CSUBstats`.
Use the package name, including its capitalization, when loading it in R.

## Installation

Install the development version from GitHub:

```r
install.packages("remotes")

remotes::install_github("emontoya2/csubstats")

library(CSUBstats)
```

## Getting started

Load a case-study dataset and examine its variables:

```r
data("motidf", package = "CSUBstats")
head(motidf)
help("motidf", package = "CSUBstats")
```

Compare mean creativity scores using Welch's two-sample t-test:

```r
two.mean.test(Score ~ Treatment, data = motidf,
              first.level = "Intrinsic", direction = "two.sided")
normqqplot(Score ~ Treatment, data = motidf)
```

These examples demonstrate how to use the functions. Choose a method based on
the research question, study design, and assumptions discussed in the training modules.

## Functions

| Function | Purpose |
|---|---|
| `two.mean.test()` | Two-sample t-test with optional randomization |
| `two.wilcox.test()` | Wilcoxon rank-sum test with optional randomization |
| `sfaov()` | One-way ANOVA, Welch's ANOVA, and pairwise comparisons |
| `sfkw()` | Kruskal-Wallis test and optional Dunn comparisons |
| `games.howell()` | Games-Howell pairwise comparisons |
| `slr.randtest()` | Randomization test for a simple regression slope |
| `normqqplot()` | Normal Q-Q plots, including plots by group |
| `fviz_eig.psych()` | Scree plots for results from the psych package |

For arguments and examples, use `help("function_name", package = "CSUBstats")`.
Several teaching functions print results to the console; their help pages describe
any returned values or optional plots.

## Datasets

| Dataset | Contents |
|---|---|
| `BMIcsdata` | Mean BMI by country, sex, region, and year |
| `BMIcsdataNT` | The BMI case study in wide format |
| `dailyPM10.2021` | Air quality and weather measurements from 2021 |
| `dssurv` | Delta smelt survival under light and turbidity conditions |
| `motidf` | Motivation questionnaires and creativity scores |
| `ssdsample` | A sample from the K-12 School Shooting Database |
| `xylemDF` | Xylem measurements for shrub species |

Each dataset has a help page describing its variables and source.
Use `help(package = "CSUBstats")` to browse the package documentation.

## Citation and feedback

Use `citation("CSUBstats")` for the package citation.
Report problems or suggestions in the [issue tracker](https://github.com/emontoya2/csubstats/issues).
See [CONTRIBUTING.md](CONTRIBUTING.md) for development instructions.
