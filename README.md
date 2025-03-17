# input data generation for the REMIND industry module

R package **mrindustry**, version **0.19.1**

[![CRAN status](https://www.r-pkg.org/badges/version/mrindustry)](https://cran.r-project.org/package=mrindustry) [![R build status](https://github.com/pik-piam/mrindustry/workflows/check/badge.svg)](https://github.com/pik-piam/mrindustry/actions) [![codecov](https://codecov.io/gh/pik-piam/mrindustry/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrindustry) [![r-universe](https://pik-piam.r-universe.dev/badges/mrindustry)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

The mrindustry packages contains data preprocessing for the
    REMIND model.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrindustry")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Falk Benke <benke@pik-potsdam.de>.

## Citation

To cite package **mrindustry** in publications use:

Benke F, Dürrwächter J, Rodrigues R, Moreno-Leiva S, Baumstark L, Pehl M, Weiss B (2025). "mrindustry: input data generation for the REMIND industry module." Version: 0.19.1, <https://github.com/pik-piam/mrindustry>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {mrindustry: input data generation for the REMIND industry module},
  author = {Falk Benke and Jakob Dürrwächter and Renato Rodrigues and Simón Moreno-Leiva and Lavinia Baumstark and Michaja Pehl and Bennet Weiss},
  date = {2025-03-17},
  year = {2025},
  url = {https://github.com/pik-piam/mrindustry},
  note = {Version: 0.19.1},
}
```
