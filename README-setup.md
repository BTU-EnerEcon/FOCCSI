## Setup R environment

- install the development packages.
```R
install install.packages("devtools")
install.packages("roxygen2")
install.packages("shiny.i18n")
install.packages("glmnet")
install.packages("tseries")
install.packages("forecast")
install.packages("imputeTS")
install.packages("data.table")
install.packages("dplyr")
install.packages("zoo")
install.packages("plotly")
install.packages("orca")
install.packages("xts")
install.packages("lubridate")
install.packages("stringr")
```

## Install Orca binaries on your Windows machine
- install windows binaries from https://github.com/plotly/orca/releases (e.g. https://github.com/plotly/orca/releases/download/v1.3.1/windows-release.zip)
- unpack the zip in a temporary folder and run the setup
- add orca installation folder to environment system `PATH`variable (e.g. `C:\Users\<username>\AppData\Local\Programs\orca` )

## To build help files
```R
library(roxygen2)
roxygenise()
```

## Run test
- Run  `./tests/test.R`

## Build Package

```R
build()
```