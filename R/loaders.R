#' Load data  
#'
#' @description
#' loading data from a csv file
#' 
#' @param filename the filename on disk containing the data
#' @param schema - an optional parameters holding the data schema. 
#' The current default value corresponds to the actual processing code
#' 
#' @return data - the in-memory object containg the data
#' 
#' @examples 
#' data  <- load_input_data(filename = file.choose)
#' data <- load_input_data(filename = file.path("tests/input/sample_complete_10y.csv"))
#' 
#' @export
load_input_data <- function(filename, schema =  c("Zeit", "Jahr", "Monat", "HR", "K", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "MW_J")) {

  if(!validate_schema(filename, schema)){stop("Invalid input data.")}

  data <- read.csv(filename, header = TRUE, sep=",")
  #   no preprocessing
  return(data)
}



#' Set analysis parameters  
#'
#'@description
#' loads analysis parameters from a data file or return default values
#' 
#' @param filename an optional parameter, the filename on disk containing the data. If missing, the default values are loaded
#' @param schema - an optional parameters holding the data schema. The current default value corresponds to the actual processing code
#'
#' @return the in-memory object containg the data
#' 
#' @examples 
#' parameters  <- parameters()
#' parameters <- parameters(filename = file.path("tests/input/params.csv"))
#' 
#' @export
parameters <- function(
  filename = "./input/params.csv",   
  schema = c(
    "d", "e", "FL_d", "FX_h", "AlphaParameter",# "Periode","Datenvorbehandlung.an.aus",
    
    "Anzahl.unmoeglich.max",
    "Anzahl.leer.wdh.max",
    "Anzahl.leer.gesamt.max",
    "Anzahl.Wertwdh.Null.max",
    "Anzahl.Wertwdh.Wert.max",
    
    "Anzahl.unmoeglich.hist.max",
    "Anzahl.leer.wdh.hist.max",
    "Anzahl.leer.gesamt.hist.max",
    "Anzahl.Wertwdh.Null.hist.max",
    "Anzahl.Wertwdh.Wert.hist.max",
    "Anzahl.Wertwdh.Wert.tol.hist.max"
  )) 
{
  # default parameters as they were defined in the original code
  result <- data.frame(

    d = 1, # Starttag, erster Tag des "rolling Window"
    e = 1, # Endtag, letzter Tag des "rolling Window" von hinten gezählt
    # (z.B. 1 bedeutet bis zum letzten Tag der Datenbasis; 2 bedeutet, bis zum vorletzten Tag der Datenbasis)
    FL_d = 165, # Definition der Länge des "rolling window" in Tagen
    FX_h = 15, # Definition des Zeitpunkts der Prognoselieferung (9 Uhr) für die einzelnen day-ahead Prognosen
    AlphaParameter = 0.5,

    # Periode = NA,  # Wert in Tagen, welcher die erzeugten Prognosezeitreihen,
                    # periodisch aufteilt und eine Analyse der Ergebnisse für entsprechend
                    # kleinere Zeitabschnitte erlaubt
                    # NOT YET USED!

    # Datenvorbehandlung.an.aus =NA,# Parameter, welcher die dynamische Datenvorbehandlung aktiviert/deaktiviert
                                  # NOT YET USED!
    Anzahl.unmoeglich.max = 5,    # Maximale Anzahl der erlaubten "unmöglichen Werte" (z. B. negative Werte) bevor die Daten verworfen werden
    Anzahl.leer.wdh.max = 5,      # Maximale Anzahl fehlender Werte im Datenbereich, die aufeinander folgen
    Anzahl.leer.gesamt.max = 10,  # Maximale Anzahl fehlender Werte im Datenbereich insgesamt, Anzahl.leer.max < Anzahl.leer.gesamt.max
    Anzahl.Wertwdh.Null.max = 50, # Maximale Anzahl der Wdh. von Nullen im Datenbereich; Bsp. 0,0,0 hat 2 Wdh.
    Anzahl.Wertwdh.Wert.max = 10, # Maximale Anzahl der Wdh. des gleichen Werten im Datenbereich, Bsp. x,x,x hat 2 Wdh.

    ### Parameter für Datenvorbehandlung der historischen Daten B.hist

    Anzahl.unmoeglich.hist.max = 100, # Maximale Anzahl der erlaubten "unmöglichen Werte" (z. B. negative Werte) bevor die Daten verworfen werden
    Anzahl.leer.wdh.hist.max = 96, # Maximale Anzahl fehlender Werte im Datenbereich, die aufeinander folgen
    Anzahl.leer.gesamt.hist.max = 960, # Maximale Anzahl fehlender Werte im Datenbereich insgesamt, Anzahl.leer.max < Anzahl.leer.gesamt.max
    Anzahl.Wertwdh.Null.hist.max = 960, # Maximale Anzahl der Wdh. von Nullen im Datenbereich; Bsp. 0,0,0 hat 2 Wdh.
    Anzahl.Wertwdh.Wert.hist.max = 960, # Maximale Anzahl der Wdh. des gleichen Werten im Datenbereich, Bsp. x,x,x hat 2 Wdh.
    Anzahl.Wertwdh.Wert.tol.hist.max = 5

  )

  if (!file.exists(filename)) {
    return(result)
  }

  if (!is.na(filename) && !validate_schema(filename, schema)) {
    stop("FATAL ERROR: Invalid file structure")
    return(NA)
  }

  return(fread(filename, nrows = 1, header = TRUE))
}

# validate filename structure according with schema
validate_schema <- function(filename, schema) {
  if (!is.na(filename)) {
    header <- fread(file = filename, nrows = 0, header = TRUE, sep = ",")
    ifelse(length(header) == length(schema) && all(colnames(header) == schema), TRUE, FALSE)
  } else {
    FALSE
  }
}
