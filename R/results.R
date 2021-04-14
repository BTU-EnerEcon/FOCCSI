# ==== Draw Charts ====
# BTU color #2c3e50

#' Draw specified charts  
#'
#' @description
#'  Draw specified charts  
#' 
#' @param names the names of the generated data resources, to be used for plotting  
#' @param datapath - optional, the path on disk where all generated data will be saved
#' @param i18n - required, the translator object used to personalize output according with user set language
#' 
#' @return a list of plotly objects 
#' 
#' @examples 
#' plots <- plot_charts(names = chartnames, datapath = file.path("tests/output"), i18n = i18n)
#' 
#' @export
#' @import zoo
#' @import orca
#' @import xts
#' @import lubridate
#' @import stringr
#' @import plotly
plot_charts <- function(names = names, datapath = datapath, i18n = i18n){
  plots <- NULL  #character(0)
  for (name in names) {
    
    if (name == i18n$t("Chart1") ) {
      
      plots[[name]] <- plot_Grafik1(name, datapath, i18n = i18n)
    }
    if (name == i18n$t("Chart2") ) {
      plots[[name]] <- plot_Grafik2(name, datapath, i18n = i18n)
    }
    if (name == i18n$t("Chart3") ) {
      plots[[name]] <- plot_Grafik3(name, datapath, i18n = i18n)
    }
    if (name == i18n$t("Chart4") ) {
      plots[[name]] <- plot_Grafik4(name, datapath, i18n = i18n)
    }
    if (name == i18n$t("Chart5") ) {
      plots[[name]] <- plot_Grafik5(name, datapath, i18n = i18n)
    }
    if (name == i18n$t("Chart6") ) {
      plots[[name]] <- plot_Grafik6(name, datapath, i18n = i18n)
    }
    
    if (name == i18n$t("Forecast_Error_Provider") ) {
      
      plots[[name]] <- plot_Prognosefehler_Anbieter(name, datapath, i18n = i18n)
    }
    if (name == i18n$t("Forecast_Error_Total") ) {
      
      plots[[name]] <- plot_Prognosefehler_Gesamt(name,datapath, i18n = i18n)
    }
    
    # if (name == i18n$t("Error_Measures_Yearly") ) {
    #   plots[[name]] <- plot_Fehlermasse_J(name, datapath, i18n = i18n)
    # }
    # if (name == i18n$t("Error_Measures_Monthly") ) {
    #   plots[[name]] <- plot_Fehlermasse_M(name, datapath, i18n = i18n)
    # }
  }
  return(plots)
}

#' Draw Grafik1
#'
#' @description
#' Draw Grafik1 in the initial code  
#' Requires the following data columns:
#' "Jahr",
#' "mse_A1_J","mse_A2_J","mse_A3_J","mse_A4_J","mse_A5_J","mse_A6_J","mse_A7_J",
#' "MW_J","JMW"
#' @param name the name of the file on disk to be used as data source  
#' @param datapath - optional, the path on disk where data is stored
#' @param i18n - required, the translator object used to personalize output according with user set language
#' 
#' @return a plotly object 
#' 
#' @examples 
#' p1 <- plot_Grafik1(name = i18n$t("Chart1"), datapath = file.path("tests/output"), i18n = i18n)
#' 
#' @export
plot_Grafik1 <-  function(name, datapath = datapath, i18n = i18n) {
  
  # read and normalize data
  df <- fread(paste(datapath, "/", name, ".csv",sep=""), header = TRUE)
  
  
  # columns to be plotted
  columns <- list("mse_A1_J","mse_A2_J","mse_A3_J","mse_A4_J","mse_A5_J","mse_A6_J","mse_A7_J"
                  # ,"MW_J","JMW"
  )
  
  # whatever cleaning is necessary before plot.
  # each chart may have its own issues
  df[ df == "#NV" ] <- NA
  for (i in nrow(df)) {
    for (j in 2:10) {
      df[[i,j]] <- as.double(df[[i,j]])
    }
  }
  
  # the plot
  p <- plot_ly(type = "scatter", mode = "line-marker") %>%
    plotly::layout(
      # see https://plotly.com/r/reference/layout/
      title = i18n$t("CHART1_TITLE"),
      xaxis = list(
        type="category",
        title = i18n$t("Jahr"),
        automargin = T
      ),
      yaxis = list (
        title = i18n$t("CHART1_Y_TITLE")
      ),
      legend = list(
        title = i18n$t("LEGEND1_TITLE"),
        orientation="h",
        x = 0,
        y = - 0.25,
        automargin = T
      ),
      margin = list(l=100, r = 100, t=100, b=100)
    )
  # add each column as plot trace
  for(col in columns){
    p <- p %>% add_trace(x = df[[1]], y = df[[col]], name = i18n$t(col), color = col, colors = "Set3")
  }
  
  # return the plotly object
  p
}

#' Draw Grafik2
#'
#' @description
#' Draw Grafik2 described in the initial code  
#' Requires the following data columns:
#' "Jahr",
#' "mse_A1_J/JMW","mse_A2_J/JMW","mse_A3_J/JMW","mse_A4_J/JMW","mse_A5_J/JMW","mse_A6_J/JMW","mse_A7_J/JMW"
#' 
#' @param name the name of the file on disk to be used as data source  
#' @param datapath - optional, the path on disk where data is stored
#' @param i18n - required, the translator object used to personalize output according with user set language
#' 
#' @return a plotly object 
#' 
#' @examples 
#' p1 <- plot_Grafik2(name = i18n$t("Chart2"), datapath = file.path("tests/output"), i18n = i18n)
#' 
#' @export
plot_Grafik2 <- function(name, datapath = datapath, i18n = i18n) {
  # read and normalize data
  df <- fread(paste(datapath, "/", name, ".csv",sep=""), header = TRUE)
  
  columns <- list("mse_A1_J/JMW","mse_A2_J/JMW","mse_A3_J/JMW","mse_A4_J/JMW","mse_A5_J/JMW","mse_A6_J/JMW","mse_A7_J/JMW")
  
  # TODO: whatever cleaning is necessary before plot.
  # each chart may have its own issues
  df[ df == "#NV" ] <- NA
  for (i in nrow(df)) {
    for (j in 2:8) {
      df[[i,j]] <- as.double(df[[i,j]])
    }
  }
  
  # the plot
  p <- plot_ly(type = "scatter", mode = "line-marker") %>%
    plotly::layout(
      title = i18n$t("CHART2_TITLE"),
      xaxis = list(
        type="category",
        title = i18n$t("Jahr"),
        automargin = T
      ),
      yaxis = list (
        title = i18n$t("CHART2_Y_TITLE")
      ),
      legend = list(
        title = i18n$t("LEGEND2_TITLE"),
        orientation="h",
        x = 0,
        y = - 0.25,
        automargin = T
      ),
      margin = list(l=100, r = 100, t=100, b=100)
    )
  # add each column as plot trace
  for(col in columns){
    p <- p %>% add_trace(x = df[[1]], y = df[[col]], name = i18n$t(col), color = col, colors = "Set3")
  }
  
  # return the plotly object
  p
  
  
}

#' Draw Grafik3
#'
#' @description
#' Draw Grafik3 described in the initial code  
#' Requires the following data columns:
#' "Jahr", "Monat",
#' "mse_AM","mse_K", "mse_BTU", "Anbieteranzahl"
#' 
#' @param name the name of the file on disk to be used as data source  
#' @param datapath - optional, the path on disk where data is stored
#' @param i18n - required, the translator object used to personalize output according with user set language
#' 
#' @return a plotly object 
#' 
#' @examples 
#' p1 <- plot_Grafik3(name = i18n$t("Chart3"), datapath = file.path("tests/output"), i18n = i18n)
#' 
#' @export
plot_Grafik3 <-  function(name, datapath = datapath, i18n = i18n) {
  # read and normalize data
  df <- fread(paste(datapath, "/", name, ".csv",sep=""), header = TRUE)
  
  columns <- c("mse_AM","mse_K", "mse_BTU") # , "Anbieteranzahl"
  
  # TODO: whatever cleaning is necessary before plot.
  # each chart may have its own issues
  df[df == "#NV"] <- NA
  
  for (i in nrow(df)) {
    for (j in 3:6) {
      df[[i,j]] <- as.double(df[[i,j]])
    }
  }
  # Join Jahr and Monat to create real date
  df$date <-zoo::as.Date(paste(df$Jahr, df$Monat, "01", sep="-"), "%Y-%m-%d")
  
  # the plot
  p1 <- plot_ly(type = "scatter", mode = "line-marker", height=1024) %>%
    plotly::layout(
      title = i18n$t("CHART3_TITLE"),
      xaxis = list(
        title = paste(i18n$t("Jahr"), i18n$t("Monat")),
        type="date",
        tickformat = "%b <br>%Y",
        automargin = T
      ),
      yaxis = list (
        title = i18n$t("CHART3_Y_TITLE")
      ),
      legend = list(
        title = i18n$t("LEGEND3_TITLE"),
        orientation="h",
        x = 0,
        y = - 0.25,
        automargin = T
      ),
      margin = list(l=100, r = 100, t=100, b=100)
    )
  # add each column as plot trace
  for(col in columns){
    if (col != "mse_BTU") {
      
      p1 <- p1 %>% add_trace(x = df$date, y = df[[col]], name = i18n$t(col), color = col, colors = "Set3")
      
    } else {
      
      p1 <- p1 %>% add_trace(x = df$date, y = df[[col]], name = i18n$t(col), line = list(color = 'rgba(44, 62, 80, 1)', width = 2))
      
    }
    
  }
  
  p2 <- plot_ly(type = "bar") %>%
    plotly::layout(
      yaxis = list (
        title = i18n$t("Anbieteranzahl")
      ),
      margin = list(l=100, r = 100, t=100, b=100)
    )
  p2 <- p2 %>% add_trace(
    x = df$date, y = df[["Anbieteranzahl"]],
    name = i18n$t("Anbieteranzahl"), color = col, colors = "Set3"
  )
  p <- subplot(list(p1, p2),nrows = 2,titleY = TRUE, shareX = TRUE)
  
  return(p)
}

#' Draw Grafik4
#'
#' @description
#' Draw Grafik4 described in the initial code  
#' Requires the following data columns:
#' "Monat",
#' "Obere", "Untere", "me_BTU_M", "Anbieteranzahl"
#' 
#' @param name the name of the file on disk to be used as data source  
#' @param datapath - optional, the path on disk where data is stored
#' @param i18n - required, the translator object used to personalize output according with user set language
#' 
#' @return a plotly object 
#' 
#' @examples 
#' p1 <- plot_Grafik4(name = i18n$t("Chart4"), datapath = file.path("tests/output"), i18n = i18n)
#' 
#' @export
plot_Grafik4 <-  function(name, datapath = datapath, i18n = i18n) {
  # read and normalize data
  df <- fread(paste(datapath, "/", name, ".csv",sep=""), header = TRUE)
  columns <- list("Obere", "Untere", "me_BTU_M") #  "Anbieteranzahl"
  
  # TODO: whatever cleaning is necessary before plot.
  # each chart may have its own issues
  df[df == "#NV" || df == "-Inf" || df == "Inf"] <- NA
  
  for (i in nrow(df)) {
    for (j in 2:4) {
      df[[i,j]] <- as.double(df[[i,j]])
    }
  }
  
  # Join Jahr and Monat to create real date
  df$date <-zoo::as.Date(paste(df$Jahr, df$Monat, "01", sep="-"), "%Y-%m-%d")
  
  # the plot
  p1 <- plot_ly(type = "scatter", mode = "lines", height=1024) %>%
    plotly::layout(
      title = i18n$t("CHART4_TITLE"),
      xaxis = list(
        title = paste(i18n$t("Jahr"), i18n$t("Monat")),
        type="date",
        tickformat = "%b <br>%Y",
        automargin = T
      ),
      yaxis = list (
        title = i18n$t("CHART4_Y_TITLE")
      ),
      legend = list(
        title = i18n$t("LEGEND4_TITLE"),
        orientation="h",
        x = 0,
        y = - 0.25,
        automargin = T
      ),
      margin = list(l=100, r = 100, t=100, b=100)
    )
  # add each column as plot trace
  p1 <- p1 %>%
    add_trace(x = df$date, y = df$Obere, name = i18n$t("Obere"), fill = 'tozeroy', fillcolor='rgba(0,100,80,0.2)', line = list(color="rgba(0,100,80,0.5)",  width=1)) %>%
    add_trace(x = df$date, y = df$me_BTU_M, name = i18n$t("me_BTU_M"), line = list(color = 'rgb(44, 62, 80)') ) %>%
    add_trace(x = df$date, y = df$Untere, name = i18n$t("Untere"), fill = 'tozeroy', fillcolor='rgba(0,100,80,0.2)', line = list(color="rgba(0,100,80,0.5)", width=1))
  
  p2 <- plot_ly(type = "bar") %>%
    plotly::layout(
      yaxis = list (
        title = i18n$t("Anbieteranzahl")
      ),
      margin = list(l=100, r = 100, t=100, b=100)
    )
  p2 <- p2 %>% add_trace(
    x = df$date, y = df$Anbieteranzahl,
    name = i18n$t("Anbieteranzahl"), color = "Anbieteranzahl", colors = "Set3"
  )
  p <- subplot(list(p1, p2),nrows = 2,titleY = TRUE, shareX = TRUE)
  
  return(p)
}

#' Draw Grafik5 - UNDER CONSTRUCTION
#'
#' @description
#' Draw Grafik5 described in the initial code  
#' Requires the following data columns:
#' "Jahr", "Monat",
#' "Vergleich", "Durchschnitt", "Verbesserung", "Anzahl_Verb", "Verschlechterung", "Anzahl_Vers"
#' 
#' @param name the name of the file on disk to be used as data source  
#' @param datapath - optional, the path on disk where data is stored
#' @param i18n - required, the translator object used to personalize output according with user set language
#' 
#' @return a plotly object 
#' 
#' @examples 
#' p1 <- plot_Grafik5(name = i18n$t("Chart5"), datapath = file.path("tests/output"), i18n = i18n)
#' 
#' @export
plot_Grafik5 <-  function(name, datapath = datapath, i18n = i18n) {
  # read and normalize data
  df <- fread(paste(datapath, "/", name, ".csv",sep=""), header = TRUE)
  columns <- list("Vergleich", "Durchschnitt", "Verbesserung", "Anzahl_Verb", "Verschlechterung", "Anzahl_Vers")
  
  # TODO: whatever cleaning is necessary before plot.
  # each chart may have its own issues
  df$Vergleich <- as.double(df$Vergleich) 
  # Join Jahr and Monat to create real date
  df$date <-zoo::as.Date(paste(df$Jahr, df$Monat, "01", sep="-"), "%Y-%m-%d")
  
  
  p <- plot_ly(
    x = as.Date(df$date, format= "%Y-%m-%d"), y = df$Vergleich,
    name = i18n$t("Vergleich"),
    type = "bar",
    height=1024
  ) %>%
    plotly::layout(
      title = i18n$t("CHART5_TITLE"),
      xaxis = list(
        title = i18n$t("Monat"),
        type="date",
        tickformat = "%b <br>%Y",
        automargin = T
      ),
      yaxis = list (
        title = i18n$t("CHART5_Y_TITLE")
      ),
      margin = list(l=100, r = 100, t=100, b=100)
    )
  return(p)
}

#' Draw Grafik6 - UNDER CONSTRUCTION
#'
#' @description
#' Draw Grafik6 
#' Requires the following data columns:
#' "Jahr", 
#' "Vergleich", "Durchschnitt", "Verbesserung", "Anzahl", "Verschlechterung", "Anzahl"
#' 
#' @param name the name of the file on disk to be used as data source  
#' @param datapath - optional, the path on disk where data is stored
#' @param i18n - required, the translator object used to personalize output according with user set language
#' 
#' @return a plotly object 
#' 
#' @examples 
#' p1 <- plot_Grafik6(name = i18n$t("Chart6"), datapath = file.path("tests/output"), i18n = i18n)
#' 
#' @export
plot_Grafik6 <-  function(name, datapath = datapath, i18n = i18n) {
  
  # read and normalize data
  df <- fread(paste(datapath, "/", name, ".csv",sep=""), header = TRUE)
  columns <- list("Vergleich", "Durchschnitt", "Verbesserung", "Anzahl", "Verschlechterung", "Anzahl")
  
  # TODO: whatever cleaning is necessary before plot.
  # each chart may have its own issues
  df$Vergleich <- as.double(df$Vergleich) 
  # Join Jahr and Monat to create real date
  df$date <-zoo::as.Date(paste(df$Jahr, "01", "01", sep="-"), "%Y-%m-%d")
  
  # use i18n$t("LEGEND6_TITLE") for legend title
  p <- plot_ly(
    type = "bar", x = df$date, y = df$Vergleich, height=1024) %>%
    plotly::layout(
      title = i18n$t("CHART6_TITLE"),
      xaxis = list(
        title = i18n$t("Jahr"),
        type="date",
        tickformat = "%Y",
        automargin = T
      ),
      yaxis = list (
        title = i18n$t("CHART6_Y_TITLE")
      ),
      margin = list(l=100, r = 100, t=100, b=100)
    )
  return(p)}

#' Prognosefehler_Anbieter
#'
#' @description
#' Draw Prognosefehler_Anbieter 
#' Requires the following data columns:
#' "Z", 
#' "PF_A1", "PF_A2", "PF_A3", "PF_A4", "PF_A5", "PF_A6", "PF_A7"
#' 
#' @param name the name of the file on disk to be used as data source  
#' @param datapath - optional, the path on disk where data is stored
#' @param i18n - required, the translator object used to personalize output according with user set language
#' 
#' @return a plotly object 
#' 
#' @examples 
#' p1 <- plot_Prognosefehler_Anbieter(name = i18n$t("Forecast_Error_Provider"), datapath = file.path("tests/output"), i18n = i18n)
#' 
#' @export
plot_Prognosefehler_Anbieter <-  function(name, datapath = datapath, i18n = i18n) {
  
  # read and normalize data
  
  df <- fread(paste(datapath, "/", name, ".csv",sep=""), header = TRUE)
  
  columns <- list("PF_A1", "PF_A2", "PF_A3", "PF_A4", "PF_A5", "PF_A6", "PF_A7")
  
  # cleaning is necessary before plot
  
  if  (all(grepl("[0-9]{2}/[0-9]{2}/[0-9]{4} [0-9]{2}:[0-9]{2}",df$Z)))  {
    # date like "19/04/2010 11:45:00"
    
    df$Z  <- parse_date_time(df$Z, "%d/%m/%Y %H:%M") 
    time_period <-base::unique(lubridate::year(df$Z))
    df <- xts(df, order.by=df$Z)
  } else if  (all(grepl("[0-9]{2}.[0-9]{2}.[0-9]{4} [0-9]{2}:[0-9]{2}",df$Z)))  {
    # date like "19.04.2010 11:45:00"
    
    df$Z  <- parse_date_time(df$Z, "%d\\.%m\\.%Y %H:%M") 
    time_period <-base::unique(lubridate::year(df$Z))
    df <- xts(df, order.by=df$Z)
  } else if (all(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}\\s[0-9]{2}:[0-9]{2}:[0-9]{2}", df$Z))) {
    # iso like "2018-09-26 09:15:00" this is how is loaded by read.csv (is NOT loaded as "2018-09-26T09:15:00Z")
    class(df$Z) = c('POSIXt', 'POSIXct')
    time_period <-unique(format(df$Z, format = "%Y"))
  } else {
    stop("Plotly error: An invalid date format was found")
  }
  
  
  # summarize time series
  df <- get_plot_period_data(df, length(time_period) )
  df <- data.frame(Z = index(df), coredata(df))
  
  # the plot
  p <- plot_ly(type = "scatter", mode = "line-marker")   %>%
    plotly::layout(
      title = i18n$t("Forecast_Error_Provider_TITLE"),
      xaxis = list(
        title = i18n$t("Z"),
        type="date",
        tickformat = "%b <br>%Y",
        automargin = T
      ),
      yaxis = list (
        title = i18n$t("Forecast_Error_Provider_Y_TITLE")
      ),
      legend = list(
        title = i18n$t("Forecast_Error_Provider_LEGEND_TITLE"),
        orientation="h",
        x = 0,
        y = - 0.25,
        automargin = T
      ),
      margin = list(l=100, r = 100, t=100, b=100)
    )
  # add each column as plot trace
  for(col in columns){
    p <- p %>% add_trace(x = df$Z, y = df[[col]], name = i18n$t(col), color = col, colors = "Set3")
  }
  
  # return the plotly object
  p
}

#' Prognosefehler_Gesamt
#'
#' @description
#' Draw Prognosefehler_Gesamt 
#' Requires the following data columns:
#' "Z", 
#' "Anbieteranzahl",
#' "AM", "K", "BTU", "PF_AM", "PF_K", "PF_BTU"
#' 
#' @param name the name of the file on disk to be used as data source  
#' @param datapath - optional, the path on disk where data is stored
#' @param i18n - required, the translator object used to personalize output according with user set language
#' 
#' @return a plotly object 
#' 
#' @examples 
#' p1 <- plot_Prognosefehler_Gesamt(name = i18n$t("Forecast_Error_Total"), datapath = file.path("tests/output"), i18n = i18n)
#' 
#' @export
plot_Prognosefehler_Gesamt <-  function(name, datapath = datapath, i18n = i18n) {
  
  # read and normalize data
  df <- fread(paste(datapath, "/", name, ".csv",sep=""), header = TRUE)
  
  # columns to be plotted
  columns <- list("AM", "K", "BTU", "PF_AM", "PF_K", "PF_BTU")
  
  # cleaning is necessary before plot
  
  df$Anbieteranzahl <- as.double(df$Anbieteranzahl)
  for (colname in columns) {
    df[[colname]] <- as.double(df[[colname]])
  }
  
  if  (all(grepl("[0-9]{2}/[0-9]{2}/[0-9]{4} [0-9]{2}:[0-9]{2}",df$Z)))  {
    # date like "19/04/2010 11:45:00"
    
    df$Z  <- parse_date_time(df$Z, "%d/%m/%Y %H:%M") 
    time_period <-base::unique(lubridate::year(df$Z))
    df <- xts(df, order.by=df$Z)
  } else if  (all(grepl("[0-9]{2}.[0-9]{2}.[0-9]{4} [0-9]{2}:[0-9]{2}",df$Z)))  {
    # date like "19.04.2010 11:45:00"
    
    df$Z  <- parse_date_time(df$Z, "%d\\.%m\\.%Y %H:%M") 
    time_period <-base::unique(lubridate::year(df$Z))
    df <- xts(df, order.by=df$Z)
  } else if (all(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}\\s[0-9]{2}:[0-9]{2}:[0-9]{2}", df$Z))) {
    # iso like "2018-09-26 09:15:00" this is how is loaded by read.csv (is NOT loaded as "2018-09-26T09:15:00Z")
    class(df$Z) = c('POSIXt', 'POSIXct')
    time_period <-unique(format(df$Z, format = "%Y"))
  } else {
    stop("Plotly error: An invalid date format was found")
  }
  
  # summarize time series
  df <- get_plot_period_data(df, length(time_period) )
  df <- data.frame(Z = index(df), coredata(df))
  
  # the plot
  p1 <- plot_ly(type = "scatter", mode = "line-marker", height=1024)  %>% 
    plotly::layout(
      title = i18n$t("Forecast_Error_Total_TITLE"),
      xaxis = list(
        title = paste(i18n$t("Z")),
        type="date",
        tickformat = "%b <br>%Y",
        automargin = T
      ),
      yaxis = list (
        title = i18n$t("Forecast_Error_Total_Y_TITLE")
      ),
      legend = list(
        title = i18n$t("Forecast_Error_Total_LEGEND_TITLE"),
        orientation="h",
        x = 0,
        y = - 0.25,
        automargin = T
      ),
      margin = list(l=100, r = 100, t=100, b=100)
    )
  # add each column as plot trace
  for(col in columns){
    p1 <- p1 %>% add_trace(x = df$Z, y = df[[col]], name = i18n$t(col), color = col, colors = "Set3")
  }
  
  p2 <- plot_ly( x = df$Z, y = df$Anbieteranzahl, name=i18n$t("Anbieteranzahl"), 
                 type = "scatter", mode = "markers") %>%
    plotly::layout(
      yaxis = list (
        title = i18n$t("Anbieteranzahl")
      ),
      margin = list(l=100, r = 100, t=100, b=100)
    )
  
  p <- subplot(list(p1, p2),nrows = 2,titleY = TRUE, shareX = TRUE)
  
  return(p)
}

#' Fehlermasse
#'
#' @description
#' Draw Fehlermasse 
#' Requires the following data columns:
#' "AM", "K", "BTU", "A1", "A2", "A3", "A4", "A5", "A6", "A7"
#' Row names are 
#' "mse", "mae", "me"
#' @param name the name of the file on disk to be used as data source  
#' @param datapath - optional, the path on disk where data is stored
#' @param i18n - required, the translator object used to personalize output according with user set language
#' 
#' @return a plotly object 
#' 
#' @examples 
#' p1 <- plot_Fehlermasse(name = i18n$t("Error_Mass"), datapath = file.path("tests/output"), i18n = i18n)
#' 
#' @export
plot_Fehlermasse <-  function(name, datapath = datapath, i18n = i18n) {
  
  df <- fread(
    paste(datapath, "/", name, ".csv",sep=""),
    header = TRUE,
  )
  
  # whatever cleaning is necessary before plot.
  # each chart may have its own issues
  df[df == "#NV"] <- NA
  
  # columns to be plotted
  columns <- list(
    "AM" = i18n$t("AM"),
    "K" = i18n$t("K"),
    "BTU" = i18n$t("BTU"),
    "A1" = i18n$t("A1"),
    "A2" = i18n$t("A2"),
    "A3" = i18n$t("A3"),
    "A4" = i18n$t("A4"),
    "A5" = i18n$t("A5"),
    "A6" = i18n$t("A6"),
    "A7" = i18n$t("A7")
  )
  df <- transpose(df)
  colnames(df) <- c("MSE", "MAE", "ME")
  p <- subplot(
    colnames(df) %>% lapply(function(col) {
      
      plt <- plot_ly(x = names(columns), y = df[[col]], type="bar", name = col,
                     color = names(columns), colors = "Set3", height=1024) %>%
        plotly::layout(
          title = i18n$t("Error_Mass_TITLE"),
          barmode = "group",
          showlegend = FALSE,
          xaxis = list(
            type="category",
            title = columns[[col]]
          ),
          yaxis = list (
            title = i18n$t(col),
            automargin = TRUE
          )
          
        )
      
      plt
      
    }), nrows = length(colnames(df)), shareY = F, titleY = T
  )
  p
}

#' Fehlermasse_J - UNDE CONSTRUCTION
#'
#' @description
#' Draw Fehlermasse_J 
#' Requires the following data columns:
#' "Jahr",
#' "installierte Leistung [MW]", "mittlere Einspeisung [MW]",
#' "Anbieteranzahl",

#' "mse_AM_J", "mse_K_J", "mse_BTU_J", "mse_A1_J", "mse_A2_J", "mse_A3_J", "mse_A4_J", "mse_A5_J", "mse_A6_J", "mse_A7_J"

#' "mae_AM_J", "mae_K_J", "mae_BTU_J", "mae_A1_J", "mae_A2_J", "mae_A3_J", "mae_A4_J", "mae_A5_J", "mae_A6_J", "mae_A7_J",

#' "me_AM_J", "me_K_J", "me_BTU_J", "me_A1_J", "me_A2_J", "me_A3_J", "me_A4_J", "me_A5_J", "me_A6_J", "me_A7_J",
#' @param name the name of the file on disk to be used as data source  
#' @param datapath - optional, the path on disk where data is stored
#' @param i18n - required, the translator object used to personalize output according with user set language
#' 
#' @return a plotly object 
#' 
#' @examples 
#' p1 <- plot_Fehlermasse_J(name = i18n$t("Error_Measures_Yearly"), datapath = file.path("tests/output"), i18n = i18n)
#' 
#' @export
plot_Fehlermasse_J <-  function(name, datapath = datapath, i18n = i18n) {
  
  # read and normalize data
  df <- fread(paste(datapath, "/", name, ".csv",sep=""), header = TRUE)
  df[ df == "#NV" ] <- NA
  for (colname in colnames(df)[-1]) {
    df[[colname]] <- as.double(df[[colname]])
  }
  df$Jahr <-zoo::as.Date(paste(as.character(df$Jahr), "01", "01", sep="-"), "%Y-%m-%d")
  # columns to be plotted
  columns <- list(
    "AM" = i18n$t("AM"),
    "K" = i18n$t("K"),
    "BTU" = i18n$t("BTU"),
    "A1" = i18n$t("A1"),
    "A2" = i18n$t("A2"),
    "A3" = i18n$t("A3"),
    "A4" = i18n$t("A4"),
    "A5" = i18n$t("A5"),
    "A6" = i18n$t("A6"),
    "A7" = i18n$t("A7")
  )
  
  
  MSE_cols <- list(
    "mse_AM_J" = i18n$t("mse_AM_J"), 
    "mse_K_J" = i18n$t("mse_K_J"), 
    "mse_BTU_J" = i18n$t("mse_BTU_J"), 
    "mse_A1_J" = i18n$t("mse_A1_J"), 
    "mse_A2_J" = i18n$t("mse_A2_J"), 
    "mse_A3_J" = i18n$t("mse_A3_J"), 
    "mse_A4_J" = i18n$t("mse_A4_J"), 
    "mse_A5_J" = i18n$t("mse_A5_J"), 
    "mse_A6_J" = i18n$t("mse_A6_J"), 
    "mse_A7_J" = i18n$t("mse_A7_J") 
    
  )
  df_MSE <- select(df, c("Jahr", names(MSE_cols)))
  
  
  MAE_cols <- list(
    "mae_AM_J" = i18n$t("mae_AM_J"), 
    "mae_K_J" = i18n$t("mae_K_J"), 
    "mae_BTU_J" = i18n$t("mae_BTU_J"), 
    "mae_A1_J" = i18n$t("mae_A1_J"), 
    "mae_A2_J" = i18n$t("mae_A2_J"), 
    "mae_A3_J" = i18n$t("mae_A3_J"), 
    "mae_A4_J" = i18n$t("mae_A4_J"), 
    "mae_A5_J" = i18n$t("mae_A5_J"), 
    "mae_A6_J" = i18n$t("mae_A6_J"), 
    "mae_A7_J" = i18n$t("mae_A7_J") 
    
  )
  df_MAE <- select(df, c("Jahr", names(MAE_cols)))
  ME_cols <- list(
    "me_AM_J" = i18n$t("me_AM_J"), 
    "me_K_J"  = i18n$t("me_K_J"), 
    "me_BTU_J" = i18n$t("me_BTU_J"), 
    "me_A1_J" = i18n$t("me_A1_J"), 
    "me_A2_J" = i18n$t("me_A2_J"), 
    "me_A3_J" = i18n$t("me_A3_J"), 
    "me_A4_J" = i18n$t("me_A4_J"), 
    "me_A5_J" = i18n$t("me_A5_J"), 
    "me_A6_J" = i18n$t("me_A6_J"), 
    "me_A7_J" = i18n$t("me_A7_J") 
    
  )
  df_ME <- select(df, c("Jahr", names(ME_cols)))
  
  p1 <- subplot({
    names(MSE_cols) %>% lapply(function(col){
      
      plt <- plot_ly(x = df_MSE$Jahr, y = df_MSE[[col]], type="bar", name = col,
                     color = df_MSE[[col]], colors = "Set3")
      
      plt %>% plotly::layout(
        barmode = 'group',
        xaxis = list(
          title = i18n$t("Jahr"),
          type="date",
          tickformat = "%Y",
          automargin = T
        )
      )
      plt
    })  
    
  }, nrows = 3, shareY = F, titleY = T) 
  
  return(p1)
}

#' Fehlermasse_M - UNDE CONSTRUCTION
#'
#' @description
#' Draw Fehlermasse_M 
#' Requires the following data columns:
#' "Jahr", "Monat",
#' "Anbieteranzahl_M",
#' "mse_AM_M", "mse_K_M", "mse_BTU_M", "mse_A1_M", "mse_A2_M", "mse_A3_M", "mse_A4_M", "mse_A5_M", "mse_A6_M", "mse_A7_M"
#' "mae_AM_M", "mae_K_M", "mae_BTU_M", "mae_A1_M", "mae_A2_M", "mae_A3_M", "mae_A4_M", "mae_A5_M", "mae_A6_M", "mae_A7_M",
#' "me_AM_M", "me_K_M", "me_BTU_M", "me_A1_M", "me_A2_M", "me_A3_M", "me_A4_M", "me_A5_M", "me_A6_M", "me_A7_M",
#'
#' @param name the name of the file on disk to be used as data source  
#' @param datapath - optional, the path on disk where data is stored
#' @param i18n - required, the translator object used to personalize output according with user set language
#' 
#' @return a plotly object 
#' 
#' @examples 
#' p1 <- plot_Fehlermasse_M(name = i18n$t("Error_Measures_Monthly"), datapath = file.path("tests/output"), i18n = i18n)
#' 
#' @export
plot_Fehlermasse_M <-  function(name, datapath = datapath, i18n = i18n) {
  
  # read and normalize data
  df <- fread(paste(datapath, "/", name, ".csv",sep=""), header = TRUE)
  
  
  # TODO: columns to be plotted
  columns <- list()
  
  # whatever cleaning is necessary before plot.
  # each chart may have its own issues
  df[ df == "#NV" ] <- NA
  # Join Jahr and Monat to create real date
  df$date <-zoo::as.Date(paste(df$Jahr, df$Monat, "01", sep="-"), "%Y-%m-%d")
  
  # the plot
  # TO DO
  NA
}

#' Descriptive_Statistics
#'
#' @description
#' Draw Descriptive_Statistics 
#' Requires the following data columns:
#' "Jahr",
#' "me_HR_J", "me_K_J", "me_A1_J", "me_A2_J", "me_A3_J", "me_A4_J", "me_A5_J", "me_A6_J", "me_A7_J",
#' "sd_HR_J", "sd_K_J", "sd_A1_J", "sd_A2_J", "sd_A3_J", "sd_A4_J", "sd_A5_J", "sd_A6_J", "sd_A7_J"
#'
#' @param name the name of the file on disk to be used as data source  
#' @param datapath - optional, the path on disk where data is stored
#' @param i18n - required, the translator object used to personalize output according with user set language
#' 
#' @return a plotly object 
#' 
#' @examples 
#' p1 <- plot_Descriptive_Statistics(name = i18n$t("Descriptive_Statistics"), datapath= file.path("tests/output"), i18n = i18n)
#' 
#' @export
plot_Descriptive_Statistics <-  function(name, datapath = datapath, i18n = i18n) {
  
  # read and normalize data
  Descriptive_Statistics <- fread(paste(datapath, "/", name, ".csv",sep=""), header = TRUE)
  
  # whatever cleaning is necessary before plot.
  # each chart may have its own issues
  
  Descriptive_Statistics[ Descriptive_Statistics == "#NV" ] <- NA
  
  me_data <- Descriptive_Statistics[, c("Jahr", "me_HR_J", "me_K_J", "me_A1_J", "me_A2_J", "me_A3_J", "me_A4_J", "me_A5_J", "me_A6_J", "me_A7_J")]
  col_me <- c("me_HR_J", "me_K_J", "me_A1_J", "me_A2_J", "me_A3_J", "me_A4_J", "me_A5_J", "me_A6_J", "me_A7_J")
  sd_data <- Descriptive_Statistics[, c("Jahr", "sd_HR_J", "sd_K_J", "sd_A1_J", "sd_A2_J", "sd_A3_J", "sd_A4_J", "sd_A5_J", "sd_A6_J", "sd_A7_J")]
  col_sd <- c("sd_HR_J", "sd_K_J", "sd_A1_J", "sd_A2_J", "sd_A3_J", "sd_A4_J", "sd_A5_J", "sd_A6_J", "sd_A7_J")
  # the plot
  
  # the plot
  p_me <- plot_ly(type = "scatter", mode = "line-marker") %>%
    plotly::layout(
      # title = i18n$t("Descriptive_Statistics_TITLE"),
      # xaxis = list(
      #   type="category",
      #   title = i18n$t("Jahr"),
      #   automargin = T
      # ),
      yaxis = list (
        title = i18n$t("ME")
      ),
      legend = list(
        title = i18n$t("Descriptive_Statistics_LEGEND_TITLE"),
        orientation="h",
        x = 0,
        y = - 0.25,
        automargin = T
      ),
      margin = list(l=100, r = 100, t=100, b=100)
    )
  # add each column as plot trace
  for(col in col_me){
    p_me <- p_me %>% add_trace(x = me_data[[1]], y = me_data[[col]], name = i18n$t(col), color = col, colors = "Spectral")
  }
  
  p_sd <- plot_ly(type = "scatter", mode = "line-marker") %>%
    plotly::layout(
      title = i18n$t("Descriptive_Statistics_TITLE"),
      xaxis = list(
        type="category",
        title = i18n$t("Jahr")
      ),
      yaxis = list (
        title = i18n$t("SD")
      ),
      legend = list(
        title = i18n$t("Descriptive_Statistics_LEGEND_TITLE"),
        orientation="h",
        x = 0,
        y = - 0.25
      ),
      margin = list(l=100, r = 100, t=100, b=100)
    )
  # add each column as plot trace
  for(col in col_sd){
    p_sd <- p_sd %>% add_trace(x = sd_data[[1]], y = sd_data[[col]], name = i18n$t(col), color = col, colors = "Spectral")
  }
  p <- subplot(list(p_me, p_sd),nrows = 2,titleY = TRUE, shareX = TRUE)
  return(p)
}


#' Save  multiple plots into many formats and many resolutions images
#'
#' @description
#' save plots using many formats and many resolutions
#' supported formats: "png", "jpeg", "pdf", " eps"
#' supported resolutions:
#' 4:3 aspect ratio resolutions: 1024x768, 1280x960, 1400x1050, 1440x1080 , 1600x1200, 1856x1392, 1920x1440, and 2048x1536.
#' 16:10 aspect ratio resolutions: 1280x800, 1440x900, 1680x1050, 1920x1200 and 2560x1600.
#' 16:9 aspect ratio resolutions: 1024x576, 1152x648, 1280x720, 1366x768, 1600x900, 1920x1080, 2560x1440 and 3840x2160.
#' @param plotdata, a list of plotly objects   
#' @param formats  an array of formats e.g. `formats = c("png", "pdf")` 
#' @param resolutions an array of resolution strings e.g. ` resolutions = c("1920x1440","1920x1200", "1920x1080")`
#' @param i18n - required, the translator object used to personalize output according with user set language
#' 
#' @examples 
#' save_plots(plotdata = plots, formats = c("png", "pdf"),  resolutions = c("1920x1440","1920x1200", "1920x1080"))
#' 
#' @export
save_plots <- function(
  plotdata = plotdata, 
  formats = formats,  
  resolutions = resolutions
) {
  names <- names(plotdata)
  for (name in names) {
    for (format in formats) {
      for (resolution in resolutions) {
        save_plot(plot = plotdata[[name]], imagename = name, format = format, resolution = resolution)
      }
    }
  }
}


# save a given plot as image
# imagename - name of the file e.g. "chart1", "my_nice_chart"
# format - the image format. c("png", "jpeg", "pdf", " eps"). Default is png
# resolution - default is "1920x1080". Other allowed values below:
# 4:3 aspect ratio resolutions: 1024x768, 1280x960, 1400x1050, 1440x1080 , 1600x1200, 1856x1392, 1920x1440, and 2048x1536.
# 16:10 aspect ratio resolutions: 1280x800, 1440x900, 1680x1050, 1920x1200 and 2560x1600.
# 16:9 aspect ratio resolutions: 1024x576, 1152x648, 1280x720, 1366x768, 1600x900, 1920x1080, 2560x1440 and 3840x2160.
save_plot <- function(plot = plot, imagename = imagename, format = "png",  resolution = "1920x1080") {
  
  # validate output parameters
  validate_file_output(format, resolution)
  
  # export plot with orca into requested format
  outName <- i18n$t(imagename)
  orca(
    plot,
    file =file.path("tests/output", paste(outName,"_", resolution, ".", format, sep="")),
    format = format,
    width = as.character(sub("x.*", "", resolution)),
    height = as.character(sub(".*x", "", resolution))
    # ...
  )
}


# subset the initial data 
# assumes a time series with column `Z` as time and a number of other columns to be plotted
# rows are ascending ordered by Z 
# @param ts - the time serties to be plotted
# @param - dateStart the start date 
# @param - endDate the end date
# @param columns, columns to be plotted 
# @example subset_time_series(ts, "2012-02-10 00:00:00", "2019-12-31 15:15:00", c("PF_A1", "PF_A3"))
subset_time_series <- function(ts, startDate, endDate, columns ) {
  
  tsStartDate <- as.POSIXct(ts$Z[1],tz="", "%Y-%m-%d %H:%M:%OS")
  tsEndDate <- as.POSIXct(ts$Z[nrow(ts)],tz="", "%Y-%m-%d %H:%M:%OS")
  
  if(exists("columns")) {
    # filter columns
    ts <- ts[c("Z", columns)]
  }
  
  ts <- xts(ts[,-1],order.by = as.POSIXct(ts$Z,tz="", "%Y-%m-%d %H:%M:%OS"))
  
  
  if (!exists("startDate")) {
    startDate <- tsStartDate
  } else {
    startDate <- as.POSIXct(startDate,tz="", "%Y-%m-%d %H:%M:%OS")
  }
  if(startDate <= tsStartDate) {
    startDate <- tsStartDate
  }
  
  if (!exists("endDate")) {
    endDate <- tsEndDate
  } else {
    endDate <- as.POSIXct(endDate,tz="", "%Y-%m-%d %H:%M:%OS")
  }
  if(endDate >= tsEndDate) {
    endDate <- tsEndDate
  }
  
  # filter rows
  ts <- ts[paste(startDate,endDate,sep="::")]
  
  df <- data.frame(Z = index(ts), coredata(ts))
  
  return(df)
  
}

# summarize time series
get_plot_period_data <- function(ts, years) {
  
  if (years >= 1 && years <= 2){
    return(apply.weekly(ts, mean))
  }
  
  if (years >= 3 && years <= 10){
    return(apply.monthly(ts, mean))
  }
  
  if (years >= 11 && years<= 20){
    
    return(apply.quarterly(ts, mean))
  }
  
  if (years >= 21){
    
    return(apply.yearly(ts, mean))
  }
}

# Validate file output parameters
# file_format: one of  "png", "jpeg", "pdf", " eps"
# resolution: default is "1920x1080". Other recommended values below:
# 4:3 aspect ratio resolutions: 1024x768, 1280x960, 1400x1050, 1440x1080 , 1600x1200, 1856x1392, 1920x1440, and 2048x1536.
# 16:10 aspect ratio resolutions: 1280x800, 1440x900, 1680x1050, 1920x1200 and 2560x1600.
# 16:9 aspect ratio resolutions: 1024x576, 1152x648, 1280x720, 1366x768, 1600x900, 1920x1080, 2560x1440 and 3840x2160.
validate_file_output <- function(file_format, resolution) {
  if (!(file_format %in% c("png", "jpeg", "pdf", " eps"))) {
    stop("Incompatible output file format")
  }
  resolutions <- c(
    "1024x768", "1280x960", "1400x1050", "1440x1080", "1600x1200", "1856x1392", "1920x1440", "2048x1536",
    "1280x800", "1440x900", "1680x1050", "1920x1200", "2560x1600",
    "1024x576", "1152x648", "1280x720", "1366x768", "1600x900", "1920x1080", "2560x1440", "3840x2160"
  )
  if (!resolution %in% resolutions) {
    stop("Requested resolution is not standard.")
  }
}
