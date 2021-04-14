############################################################################################
### Prognosekombination aller Anbieter aus der Datei Eingabewerte_2010-2019_komplett.csv ###
### mittels dynamischer "Elastic Net"-Regression der Prognosen auf die Istwerte (HR)     ###
############################################################################################

# ==== Note on Naming Conventions ====
# English language:
#  Plotting csv data is saved into Chart1.csv, Chart2.csv,...
#   Plot images are saved as:
#    Images of plot based on Chart1.csv data: Chart1_1600x1200.png, Chart1_1920x1080.pdf, ... according with resolution and format
#    Images of plot based on Chart2.csv data: Chart2_1600x1200.png, ...
# German language:
#  Plotting csv data is saved into Grafik1.csv, Grafik2.csv,...
#   Plot images are saved as:
#    Images of plot based on Grafik1.csv data: Grafik1_1600x1200.png, Grafik1_1920x1080.pdf, ... according with resolution and format
#    Images of plot based on Grafik2.csv data: Grafik2_1600x1200.png, ...


# ==== initialization  ====
packages = c(
  "shiny.i18n", "jsonlite", "stringr",
  "glmnet", "tseries", "forecast", "imputeTS",
  "data.table", "dplyr", "zoo", "plotly", "orca", "xts", "lubridate", "roxygen2"
)
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("roxygen2")
library("shiny.i18n")
library("glmnet")
library("tseries")
library("forecast")
library("imputeTS")
library("data.table")
library("dplyr")
library("zoo")
library("plotly")
library("orca")
library("xts")
library("lubridate")
library("stringr")

# ==== main execution ====

# set locale, translator and translator language
locale = "en" # "de", "en" are supported

i18n <- Translator$new(translation_json_path = file.path("translations/translation.json"))
i18n$set_translation_language(locale)


# create output folder in case does not exists
if (!dir.exists(file.path("tests/output"))) {
  dir.create(file.path("tests/output"))
}

# load analysis raw data
raw_data <- load_input_data(filename = file.path("tests/input/sample_1y_german_date.csv"))

# set parameters
# a data source such as filename = file.path("tests/input/params.csv")
# can be given, otherwise the default values (default values from the original code) are returned
parameters <- parameters(filename = file.path("tests/input/params.csv"))

# run analysis
results <- run_forecast_BTU(raw_data, parameters,  datapath = file.path("tests/output"), i18n = i18n)

# on analysis error stop.
if (results != "DONE") {
  warning("Analysis may be incomplete")
  print(results)
}

# ==== visualization and charts saving. Notice is independent of analysis if data is already created ====

# get all chart related created resources i.e. files Chart1.csv,... or Grakik1.csv, ...
# depending on the current language

nameFilter1 <- i18n$t("Forecasting")
# get names of EXISTING resources without ".csv" extension
chartnames1 <- lapply(
  list.files(path = file.path("tests/output"), pattern = paste("^([",nameFilter1, "].*)\\.csv$", sep="")),
  FUN = function(filename) {tools::file_path_sans_ext(filename)}
)

nameFilter2 <- i18n$t("VS")
# get names of EXISTING resources without ".csv" extension
chartnames2 <- lapply(
  list.files(path = file.path("tests/output"), pattern = paste("*.",nameFilter2, ".*\\.csv$", sep="")),
  FUN = function(filename) {tools::file_path_sans_ext(filename)}
)
chartnames <- c(chartnames1, chartnames2)

errornameFilter <- i18n$t("Error")
errornames <- lapply(
  list.files(path = file.path("tests/output"), pattern = paste("^([",errornameFilter, "].*)\\.csv$", sep="")),
  FUN = function(filename) {tools::file_path_sans_ext(filename)}
)
# generate plots of all available data resources
plots <- plot_charts(names = chartnames, datapath = file.path("tests/output"), i18n = i18n)

# save plots using many formats and many resolutions
# supported formats: "png", "jpeg", "pdf", " eps"
# supported resolutions:
# 4:3 aspect ratio resolutions: 1024x768, 1280x960, 1400x1050, 1440x1080 , 1600x1200, 1856x1392, 1920x1440, and 2048x1536.
# 16:10 aspect ratio resolutions: 1280x800, 1440x900, 1680x1050, 1920x1200 and 2560x1600.
# 16:9 aspect ratio resolutions: 1024x576, 1152x648, 1280x720, 1366x768, 1600x900, 1920x1080, 2560x1440 and 3840x2160.
# save_plots(plotdata = plots, formats = c("png", "pdf"),  resolutions = c("1920x1440","1920x1200", "1920x1080"))

# save error charts
save_plots(plotdata = plot_errors, formats = c("png"),  resolutions = c("1920x1200"))
