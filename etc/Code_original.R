############################################################################################
### Prognosekombination aller Anbieter aus der Datei Eingabewerte_2010-2019_komplett.csv ###
### mittels dynamischer "Elastic Net"-Regression der Prognosen auf die Istwerte (HR)     ###
############################################################################################





### Eingabe: Konfiguration, Datenaufnahme und -aufbereitung
### -------------------------------------------------------


# Installation und Aufruf des Pakete (Schritt kann �bersprungen werden, wenn Pakete bereits installiert ist)

install.packages("glmnet", repos = "http://cran.us.r-project.org")
install.packages("imputeTS")
install.packages("forecast")
install.packages("tseries")
library(glmnet)
library('tseries')
library('forecast')
library('imputeTS')


# Setzen eines Arbeitsordners (Pfad: //x/y/z) in dem erzeugte Dateien (z. B. Bilder, Ergebnisse) gespeichert werden k�nnen
# Intern kann statt Server-lsew auch 141.43.85.131 gesetzt werden.

setwd("//SERVER-LSEW/serverdaten/MitarbeiterOrdner/Kaeso/Arbeit_EW/FOCCSI/50Hertz/Modellweiterentwicklung/Code fuer Online Tool")
#setwd("Z:/MitarbeiterOrdner/Kaeso/Arbeit_EW/FOCCSI/50Hertz/Modellweiterentwicklung/Code fuer Online Tool")


# Konfiguration der BTU-Prognose


d = 1                     # Starttag, erster Tag des "rolling Window"
e = 1                     # Endtag, letzter Tag des "rolling Window" von hinten gez�hlt (z.B. 1 bedeutet bis zum letzten Tag der Datenbasis; 2 bedeutet, bis zum vorletzten Tag der Datenbasis)
FL_d = 165                # Definition der L�nge des "rolling window" in Tagen
FX_h = 15                 # Definition des Zeitpunkts der Prognoselieferung (9 Uhr) f�r die einzelnen day-ahead Prognosen
AlphaParameter = 0.5      # Bestimmt die Gewichtung der Ridge und Lasso Strafterme



# Einlesen der Zeitreihen (ZR) aus vorbereiteter semikolongetrennter csv-Datei (Pfad: //x/y/z/Datei.csv)
# Achtung: Zeitreihen m�ssen bei 0:00 Uhr beginnen und bei 24:00 Uhr enden!
# Intern kann statt Server-lsew auch 141.43.85.131 gesetzt werden.

EingabeZR=read.csv("//SERVER-LSEW/serverdaten/MitarbeiterOrdner/Kaeso/Arbeit_EW/FOCCSI/50Hertz/Modellweiterentwicklung/Code fuer Online Tool/Eingabewerte_2010-2019_komplett.csv", header=TRUE, sep=";") # , dec=","
#EingabeZR=read.csv("Z:/MitarbeiterOrdner/Kaeso/Arbeit_EW/FOCCSI/50Hertz/Modellweiterentwicklung/Code fuer Online Tool/Eingabewerte_2010-2019_komplett.csv", header=TRUE, sep=";")

# Einlesen der Daten

A1 = round(EingabeZR$A1, 0)        # Prognose Anbieter 1
A2 = round(EingabeZR$A2, 0)        # Prognose Anbieter 2
A3 = round(EingabeZR$A3, 0)        # Prognose Anbieter 3
A4 = round(EingabeZR$A4, 0)        # Prognose Anbieter 4
A5 = round(EingabeZR$A5, 0)        # Prognose Anbieter 5
A6 = round(EingabeZR$A6, 0)        # Prognose Anbieter 6
A7 = round(EingabeZR$A7, 0)        # Prognose Anbieter 7
K  = round(EingabeZR$K,  0)        # Kombiprognose 50Hertz
HR = round(EingabeZR$HR, 0)        # hochgerechnete Istwerte
Z  = EingabeZR$Zeit                # Zeitangaben
J  = EingabeZR$Jahr                # Jahreszahlen
M  = EingabeZR$Monat               # Monatszahlen
MW_J  = EingabeZR$MW_J[1:9]        # Ausbaustand: installierte Windleistung innerhalb Regelzone 50 Hertz
Ende  = length(HR)-96*(e-1)         # Berechnung der letzten Viertelstunde



### Parameter f�r Datenvorbehandlung der Folgetagsprognosen B

Max.install = max(MW_J, na.rm =T)

Anzahl.unmoeglich.max = 5     # Maximale Anzahl der erlaubten "unm�glichen Werte" (z. B. negative Werte) bevor die Daten verworfen werden
Anzahl.leer.wdh.max = 5       # Maximale Anzahl fehlender Werte im Datenbereich, die aufeinander folgen  
Anzahl.leer.gesamt.max = 10   # Maximale Anzahl fehlender Werte im Datenbereich insgesamt, Anzahl.leer.max < Anzahl.leer.gesamt.max
Anzahl.Wertwdh.Null.max = 24  # Maximale Anzahl der Wdh. von Nullen im Datenbereich; Bsp. 0,0,0 hat 2 Wdh. 
Anzahl.Wertwdh.Wert.max = 10  # Maximale Anzahl der Wdh. des gleichen Werten im Datenbereich, Bsp. x,x,x hat 2 Wdh.

### Parameter f�r Datenvorbehandlung der historischen Daten B.hist

Anzahl.unmoeglich.hist.max = 100        # Maximale Anzahl der erlaubten "unm�glichen Werte" (z. B. negative Werte) bevor die Daten verworfen werden
Anzahl.leer.wdh.hist.max = 96           # Maximale Anzahl fehlender Werte im Datenbereich, die aufeinander folgen  
Anzahl.leer.gesamt.hist.max = 960       # Maximale Anzahl fehlender Werte im Datenbereich insgesamt, Anzahl.leer.max < Anzahl.leer.gesamt.max
Anzahl.Wertwdh.Null.hist.max = 960      # Maximale Anzahl der Wdh. von Nullen im Datenbereich; Bsp. 0,0,0 hat 2 Wdh. 
Anzahl.Wertwdh.Wert.hist.max = 960      # Maximale Anzahl der Wdh. des gleichen Werten im Datenbereich, Bsp. x,x,x hat 2 Wdh. 
Anzahl.Wertwdh.Wert.tol.hist.max = 5    # Z�hlvariable f�r die Anzahl von tolerierten gleichen Werten im Datenbereich, die aufeinander folgen

Zahl.Anbieter.genutzt.tgl = 0 # Zahl der Anbieter, welche tats�chlich an einem Tag f�r die Kombination genutzt wurden



### Erzeugung einen plausiblen HR-Vektors: HRN

HRN = rep(0, times=Ende) # Einen Vektor mit gleicher L�nge wie HR voller Nullen f�r die bereinigte Zeitreihe

for (i in 1:Ende) # Kontrolliere die eingelesene Zeitreihe Wert f�r Wert!
{
  if (!is.na(HR[i])) # Wenn der Wert eine Zahl ist...
  {
    if (HR[i]>=0 && HR[i]<=max(MW_J, na.rm =T)) # ...und im plausiblen Bereich von Null bis maximale installierte Leistung der betrachteten Jahre liegt, ...
    {
      HRN[i]<-HR[i] # ...dann �bernimm diesen Wert in die bereinigte Zeitreihe!
    }
    else
    {
      HRN[i]<-mean(HR, na.rm=T) # ...Ansonsten setze zur Schadensbegrenzung einen Mittelwert ein!
    }
  }
  else {HRN[i] = 0}
}


# Erg�nzung der eingelesenen Daten durch Berechnung der Prognosefehlervektors der Einzelprognosen sowie des
# Prognosefehlervektors der 50Hz-Prognose (NA enthalten)


PF_K  = K  - HRN
PF_A1 = A1 - HRN
PF_A2 = A2 - HRN
PF_A3 = A3 - HRN
PF_A4 = A4 - HRN
PF_A5 = A5 - HRN
PF_A6 = A6 - HRN
PF_A7 = A7 - HRN


### Verarbeitung: Data-preprocessing, BTU-Kombiprognose, Zusammenfassung
### --------------------------------------------------------------------


# Definition des "Rolling Window" (anhand der anfangs gesetzten Konfiguration)

FL = FL_d*96-FX_h*4       # F�nsterl�nge des "rolling window" in Viertelstunden [165Tage*96=15.840 abzgl. der Viertelstunden, die zwischen Berechnungszeitpunkt (Vorliegen der Prognosen f�r den Folgetag, 9 Uhr des aktuellen Tages) und Prognosebeginn (0 Uhr am Folgetag) liegen]
Fstart = (d-1) * 96 + 1   # Fensterbeginn (erste Viertelstunde des Starttages)
Fend = Fstart - 1 + FL    # Fensterende (in der Praxis sp�ter: letzte Viertelstunde vor Berechnungsdurchf�hrung)
Pstart = FL_d*96 + 1      # Prognosebeginn (erste Viertelstunde des Folgetages)

# Deklarieren der BTU-Kombiprognosevektoren (BTU mit globalem i und BTUN mit lokalem i)

BTU  = rep(NA, times=Ende)

# Zahl der maximal zur Verf�gung stehenden Prognosen

Zahl.Prognosen = 7         # Zahl der maximal m�glichen Prognosen

# Erzeug Matrix zur Speicherung der Gr�nde, warum ein Anbieter nicht f�r die Kombination genutzt wird

Anbieter.stat.grund = matrix(0,(Ende/96), Zahl.Prognosen)

# Erzeug Matrix zur Speicherung der Modellkoeffizienten

Anbieter.Koeff = matrix(0,(Ende/96), (Zahl.Prognosen+1))

# tagesweise Berechnung der neuen, dynamischen Kombiprognose mit Kreuzvalidierung im prognostizierbaren Zeitraum

for (j in (FL_d+d):(Ende/96))     # j:=Tagesnummer des Tages, f�r den neue Prognosen vorliegen, ab ersten Prognosetag bis Endtag
{
  Zahl.Anbieter.genutzt = 0  # Zahl der Anbieter, welche tats�chlich f�r die Kombination genutzt werden
  
  
  ### Datenvektoren einlesen: Folgetagsprognosen und historische Prognosen sowie die historischen Hochrechnungen zum Sch�tzen der Modellkoeffizienten
  
  #j=166
  Prognosen.Folgetag = matrix(c(A1[((j-1)*96+1):(j*96)],
                                A2[((j-1)*96+1):(j*96)],
                                A3[((j-1)*96+1):(j*96)],
                                A4[((j-1)*96+1):(j*96)],
                                A5[((j-1)*96+1):(j*96)],
                                A6[((j-1)*96+1):(j*96)],
                                A7[((j-1)*96+1):(j*96)]), ncol = 7, nrow = length(A1[((j-1)*96+1):(j*96)]))
  
  Prognosen.hist = matrix(c(A1[((j-1-FL_d)*96+1):((j-1-FL_d)*96+FL)],
                            A2[((j-1-FL_d)*96+1):((j-1-FL_d)*96+FL)],
                            A3[((j-1-FL_d)*96+1):((j-1-FL_d)*96+FL)],
                            A4[((j-1-FL_d)*96+1):((j-1-FL_d)*96+FL)],
                            A5[((j-1-FL_d)*96+1):((j-1-FL_d)*96+FL)],
                            A6[((j-1-FL_d)*96+1):((j-1-FL_d)*96+FL)],
                            A7[((j-1-FL_d)*96+1):((j-1-FL_d)*96+FL)]), ncol = 7, nrow = length(A1[((j-1-FL_d)*96+1):((j-1-FL_d)*96+FL)]))
  
  Istwerte.hist = HRN[((j-1-FL_d)*96+1):((j-1-FL_d)*96+FL)]
  
  
  for (l in (1:Zahl.Prognosen))
  {
    
    ### Erzeugung von lokalen Hilfsvektoren
    #i=1
    B = Prognosen.Folgetag[,l]  # Daten von Anbieter i f�r die Folgetagsprognosen --> Erzeuge einen lokalen Datenvektor f�r die Daten des Tages um die Ausgangsdaten nicht zu ver�ndern
    B.hist = Prognosen.hist[,l] # Historische Daten von Anbieter i zur Sch�tzung der Modellkoeffizienten --> Erzeuge einen lokalen Datenvektor f�r die Historie um die Ausgangsdaten nicht zu ver�ndern
    
    ### Parameter f�r Datenvorbehandlung der Folgetagsprognosen B
    
    ### Definition der Z�hlvariablen
    
    Anzahl.unmoeglich = 0         # Z�hlvariable f�r die Anzahl der "unm�glichen Werte" (z. B. negative Werte) im aktuell betrachteten Datenbereich
    Anzahl.leer.wdh = 0           # Z�hlvariable f�r die Anzahl fehlender Werte im Datenbereich, die aufeinander folgen  
    Anzahl.leer.gesamt = 0        # Z�hlvariable f�r die Anzahl fehlender Werte im Datenbereich insgesamt
    Anzahl.Wertwdh.Null = 0       # Z�hlvariable f�r die Anzahl von Nullen im Datenbereich, die aufeinander folgen
    Anzahl.Wertwdh.Wert = 0       # Z�hlvariable f�r die Anzahl von gleichen Werten im Datenbereich, die aufeinander folgen
    
    ### Abruch der "for"-Schleife
    
    break.var = 0                 # Variable zur Kennzeichnung des Abruchs der "for"-Schleife
    
    
    
    ### Beginn der Datenvorbehandlung - �berpr�fung der Folgetagsprognosen auf Plausibili�t
    
    for (i in (1:length(B))) # F�r alle i in B
    {
      ### Pr�fung auf unm�gliche Werte in den Daten
      
      if (!is.na(B[i])) # Wenn B[i] nicht leer ist, dann...
      {
        if (B[i] < 0 || B[i] > Max.install) {Anzahl.unmoeglich = Anzahl.unmoeglich + 1} # Wenn B[i] ein unm�glicher Wert ist, dann z�hle
        if (Anzahl.unmoeglich > Anzahl.unmoeglich.max) {B.hist = rep(0, times=length(B.hist)); break.var = 1; Anbieter.stat.grund[j,l] = 1; break} # Wenn Anzahl.unmoeglich einen kritischen Wert �berschreitet, ersetze B.hist durch den Null-Vektor und verlasse die "for"-Schleife
      }
      
      
      
      ### Pr�fung auf Wiederholungen von Werten
      
      if (i > 1 && !is.na(B[i]) && !is.na(B[i-1])) # Ab dem zweiten Wert und wenn B[i] und B[i-1] nicht leer sind, dann...
      {
        if (B[i] == 0 && B[i] == B[i-1]) # Wenn B[i] gleich Null ist und identisch mit dem Vorg�nger
        {
          Anzahl.Wertwdh.Null = Anzahl.Wertwdh.Null + 1 # Z�hle die Widerholungen! Achtung nicht die Anzahl der Nullen!
          if (Anzahl.Wertwdh.Null > Anzahl.Wertwdh.Null.max) {B.hist = rep(0, times=length(B.hist)); break.var = 1; Anbieter.stat.grund[j,l] = 2; break} # Wenn Anzahl.Wertwdh.Null einen kritischen Wert �berschreitet, ersetze B.hist durch den Null-Vektor und verlasse die "for"-Schleife
        }
        else {Anzahl.Wertwdh.Null = 0} # Wenn die die Folge von Nullen abbricht ohne den kritischen Wert Anzahl.Wertwdh.Null.max zu �berschreiten, setze Anzahl.Wertwdh.Null wieder auf Null
        if (B[i] !=  0 && B[i] == B[i-1]) # Wenn B[i] ungleich Null ist und identisch mit dem Vorg�nger
        {
          Anzahl.Wertwdh.Wert = Anzahl.Wertwdh.Wert + 1 # Z�hle die Widerholungen! Achtung nicht die Anzahl der gleichen Werte!
          if (Anzahl.Wertwdh.Wert > Anzahl.Wertwdh.Wert.max) {B.hist = rep(0, times=length(B.hist)); break.var = 1; Anbieter.stat.grund[j,l] = 3; break} # Wenn Anzahl.Wertwdh.Wert einen kritischen Wert �berschreitet, ersetze B.hist durch den Null-Vektor und verlasse die "for"-Schleife
        }
        else {Anzahl.Wertwdh.Wert = 0}# Wenn die die Folge von gleichen Werten abbricht ohne den kritischen Wert Anzahl.Wertwdh.Wert.hist.max zu �berschreiten, setze Anzahl.Wertwdh.Wert.hist wieder auf Null
      }
      else {Anzahl.Wertwdh.Wert = 0; Anzahl.Wertwdh.Null = 0} # Wenn die Folge gleicher Werte durch ein NA abgebrochen wird, setze Anzahl.Wertwdh.Null.hist bzw. Anzahl.Wertwdh.Wert.hist wieder Null
      
      
      
      ### Pr�fung auf die H�ufigkeit der Leerstellen insgesamt und wieviele aufeinander folgen
      
      if (is.na(B[i])) # Wenn B[i] leer ist, dann...
      {
        Anzahl.leer.wdh = Anzahl.leer.wdh + 1 # Z�hle die Anzahl der sich wiederholenden Leerstellen
        Anzahl.leer.gesamt = Anzahl.leer.gesamt + 1 # Z�hle die Anzahl der Leerstellen
        if (Anzahl.leer.wdh > Anzahl.leer.wdh.max || Anzahl.leer.gesamt > Anzahl.leer.gesamt.max) {B.hist = rep(0, times=length(B.hist)); break.var = 1; Anbieter.stat.grund[j,l] = 4; break} # Wenn Anzahl.leer.wdh bzw. Anzahl.leer.gesamt einen kritischen Wert �berschreitet, ersetze B.hist durch den Null-Vektor und verlasse die "for"-Schleife
      }
      else {Anzahl.leer.wdh = 0} # Wenn die Folge der NA durch einen Wert abgebrochen wird, setze Anzahl.leer.wdh auf Null zur�ck
    }
    
    
    
    
    ### Aufbereitung der Prognosen - Entfernung der unm�glichen Werte und fehlender Werte in den Prognosen
    
    if (break.var == 0) # Wenn die Daten die Plausibilit�tspr�fung �berstanden haben, dann
    {
      ### Ersetzung von unplausiblen Werten
      
      for (i in (1:length(B))) # F�r alle Werte in B
      {
        if (!is.na(B[i])){if (B[i] < 0 || B[i] > Max.install) {B[i] = NA}} # Suche f�r alle nichtleeren Werte nach unm�glichen Werten und setze sie NA
      }
      #B = tsclean(B, replace.missing = TRUE, lambda = NULL) # Entferne alle Ausrei�er und interpoliere alle NA-Werte
      #B = na.interp(B, lambda = NULL) # Interpoliere alle NA-Werte
      B = na.interpolation(B, option = "linear") # Interpoliere alle NA-Werte
    }
    
    
    
    #####################################################################################################################
    
    ### Aufbereitung der historischen Daten des Anbieters mit "Einschwingen" als Spezialfall (Setze dazu die "xxx.max"-Parameter auf sehr gro�e Werte)
    
    ### Definition der Z�hlvariablen
    
    Anzahl.unmoeglich.hist = 0         # Z�hlvariable f�r die Anzahl der "unm�glichen Werte" (z. B. negative Werte) im aktuell betrachteten Datenbereich
    Anzahl.leer.wdh.hist = 0           # Z�hlvariable f�r die Anzahl fehlender Werte im Datenbereich, die aufeinander folgen  
    Anzahl.leer.gesamt.hist = 0        # Z�hlvariable f�r die Anzahl fehlender Werte im Datenbereich insgesamt
    Anzahl.Wertwdh.Null.hist = 0       # Z�hlvariable f�r die Anzahl von Nullen im Datenbereich, die aufeinander folgen
    Anzahl.Wertwdh.Wert.hist = 0       # Z�hlvariable f�r die Anzahl von gleichen Werten im Datenbereich, die aufeinander folgen
    
    
    ### Abruch der "for"-Schleife
    
    break.var.hist = 0 # Variable zur Kennzeichnung des Abruchs der "for"-Schleife
    
    
    ### Beginn der Datenvorbehandlung - �berpr�fung der historischen Daten auf Plausibili�t
    
    if (break.var == 0)  # Wenn die Folgetagsprognosen die Plausibilit�tspr�fung �berstanden haben, dann
    {
      for (i in (1:length(B.hist))) # F�r alle i in B.hist
      {
        ### Pr�fung auf unm�gliche Werte in den Daten
        
        if (!is.na(B.hist[i])) # Wenn B.hist[i] nicht leer ist, dann...
        {
          if (B.hist[i] < 0 || B.hist[i] > Max.install) {Anzahl.unmoeglich.hist = Anzahl.unmoeglich.hist + 1} # Wenn B.hist[i] ein unm�glicher Wert ist, dann z�hle
          if (Anzahl.unmoeglich.hist > Anzahl.unmoeglich.hist.max) {B.hist = rep(0, times=length(B.hist)); break.var.hist = 1; Anbieter.stat.grund[j,l] = 5; break} # Wenn Anzahl.unmoeglich einen kritischen Wert �berschreitet, ersetze B.hist durch den Null-Vektor und verlasse die "for"-Schleife
        }
        
        
        
        ### Pr�fung auf Wiederholungen von Werten
        
        if (i > 1 && !is.na(B.hist[i]) && !is.na(B.hist[i-1])) # Ab dem zweiten Wert und wenn B.hist[i] und B.hist[i-1] nicht leer ist, dann...
        {
          if (B.hist[i] == 0 && B.hist[i] == B.hist[i-1]) # Wenn B.hist[i] gleich Null ist und identisch mit dem Vorg�nger
          {
            Anzahl.Wertwdh.Null.hist = Anzahl.Wertwdh.Null.hist + 1 # Z�hle die Widerholungen! Achtung nicht die Anzahl der Nullen!
            if (Anzahl.Wertwdh.Null.hist > Anzahl.Wertwdh.Null.hist.max) {B.hist = rep(0, times=length(B.hist)); break.var.hist = 1; Anbieter.stat.grund[j,l] = 6; break} # Wenn Anzahl.Wertwdh.Null.hist einen kritischen Wert �berschreitet, ersetze B.hist durch den Null-Vektor und verlasse die "for"-Schleife
          }
          else {Anzahl.Wertwdh.Null.hist = 0} # Wenn die die Folge von Nullen abbricht ohne den kritischen Wert Anzahl.Wertwdh.Null.hist.max zu �berschreiten, setze Anzahl.Wertwdh.Null.hist wieder auf Null
          if (B.hist[i] !=  0 && B.hist[i] == B.hist[i-1]) # Wenn B.hist[i] ungleich Null ist und identisch mit dem Vorg�nger
          {
            Anzahl.Wertwdh.Wert.hist = Anzahl.Wertwdh.Wert.hist + 1 # Z�hle die Widerholungen! Achtung nicht die Anzahl der gleichen Werte!
            if (Anzahl.Wertwdh.Wert.hist > Anzahl.Wertwdh.Wert.hist.max) {B.hist = rep(0, times=length(B.hist)); break.var.hist = 1; Anbieter.stat.grund[j,l] = 7; break} # Wenn Anzahl.Wertwdh.Wert.hist einen kritischen Wert �berschreitet, ersetze B.hist durch den Null-Vektor und verlasse die "for"-Schleife
          }
          else {if (Anzahl.Wertwdh.Wert.hist > Anzahl.Wertwdh.Wert.tol.hist.max) {B.hist[(i-Anzahl.Wertwdh.Wert.hist-1):(i-1)] = 0}; Anzahl.Wertwdh.Wert.hist = 0}# Wenn die die Folge von gleichen Werten abbricht ohne den kritischen Wert Anzahl.Wertwdh.Wert.hist.max zu erreichen aber die Anzahl an tolerierten Wiederholungen �berschreitet, ersetze die Werte die gleich waren durch Null und setze dann Anzahl.Wertwdh.Null.hist wieder auf Null
        }
        else
        {
          if (i > 1 && Anzahl.Wertwdh.Wert.hist > Anzahl.Wertwdh.Wert.tol.hist.max)
          {
            B.hist[(i-Anzahl.Wertwdh.Wert.hist-1):(i-1)] = 0; Anzahl.Wertwdh.Wert.hist = 0 # Wenn die Folge gleicher Werte durch ein NA abgebrochen wird und die Anzahl an tolerierten Wiederholungen �berschritten wurde, ersetze die Werte die gleich (!=0) waren durch Null und setze dann Anzahl.Wertwdh.Wert.hist wieder Null
          }
          else {Anzahl.Wertwdh.Null.hist = 0; Anzahl.Wertwdh.Wert.hist = 0} # Wenn die Folge gleicher Werte (belibige) durch ein NA abgebrochen wird, setze Anzahl.Wertwdh.Null.hist und Anzahl.Wertwdh.Wert.hist wieder Null
        }
        
        
        ### Pr�fung auf die H�ufigkeit der Leerstellen insgesamt und wieviele aufeinander folgen
        
        if (is.na(B.hist[i])) # Wenn B.hist[[i] leer ist, dann...
        {
          Anzahl.leer.wdh.hist = Anzahl.leer.wdh.hist + 1 # Z�hle die Anzahl der sich wiederholenden Leerstellen
          Anzahl.leer.gesamt.hist = Anzahl.leer.gesamt.hist + 1 # Z�hle die Anzahl der Leerstellen
          if (Anzahl.leer.wdh.hist > Anzahl.leer.wdh.hist.max || Anzahl.leer.gesamt.hist > Anzahl.leer.gesamt.hist.max) {B.hist = rep(0, times=length(B.hist)); break.var.hist = 1; Anbieter.stat.grund[j,l] = 8; break} # Wenn Anzahl.leer.wdh.hist bzw. Anzahl.leer.gesamt.hist einen kritischen Wert �berschreitet, ersetze B.hist durch den Null-Vektor und verlasse die "for"-Schleife
        }
        else {Anzahl.leer.wdh.hist = 0} # Wenn die Folge der NA durch einen Wert abgebrochen wird, setze Anzahl.leer.wdh.hist auf Null zur�ck
      }
    }
    
    ### Aufbereitung der Historie - Entfernung der unm�glichen Werte und fehlender Werte in den Prognosen
    
    if (break.var.hist == 0) # Wenn die historischen Daten die Plausibilit�tspr�fung �berstanden haben, dann
    {
      ### Ersetzung von unplausiblen Werten
      
      for (i in (1:length(B.hist))) # F�r alle Werte in B
      {
        if (!is.na(B.hist[i])){if (B.hist[i] < 0 || B.hist[i] > Max.install) {B.hist[i] = NA}} # Suche f�r alle nichtleeren Werte nach unm�glichen und setze sie NA
      }
      #B.hist = tsclean(B.hist, replace.missing = TRUE, lambda = NULL) # Entferne alle Ausrei�er und interpoliere alle NA-Werte
      #B.hist = na.interp(B.hist, lambda = NULL) # Interpoliere alle NA-Werte
      B.hist = na.interpolation(B.hist, option = "linear") # Interpoliere alle NA-Werte
    }
    
    ### Speichern, ob der Anbieter ber�cksichtigt wurde oder nicht
    
    if (break.var == 0 && break.var.hist == 0) {Zahl.Anbieter.genutzt = Zahl.Anbieter.genutzt + 1}
    
    ### Speichern der �berarbeiteten Anbieterdaten (Historische Prognosen und Folgetagsprognosen)
    
    Prognosen.Folgetag[,l] = B # Daten von Anbieter i f�r die Folgetagsprognosen --> Erzeuge einen lokalen Datenvektor f�r die Daten des Tages um die Ausgangsdaten nicht zu ver�ndern
    Prognosen.hist[,l] = B.hist # Historische Daten von Anbieter i zur Sch�tzung der Modellkoeffizienten --> Erzeuge einen lokalen Datenvektor f�r die Historie um die Ausgangsdaten nicht zu ver�ndern
    
  }
  
  Zahl.Anbieter.genutzt.tgl[j] = Zahl.Anbieter.genutzt
  
  
  # Anwendung des Kombinationsmodells "cross validation" cvKM mit Konfiguration des Alpha-Parameters und des Fehlerma�es
  
  cvKM = cv.glmnet(Prognosen.hist, Istwerte.hist, alpha = AlphaParameter, type.measure = "mse")   
  
  # Auslesen des Lambda-Parameters aus cvKM
  
  LambdaParameter = cvKM$lambda.min
  
  # Auslesen der Modellkoeffizienten aus cvKM
  
  Anbieter.Koeff.temp = t(as.matrix(coef(cvKM, s = "lambda.min")))
  Anbieter.Koeff[j,] = Anbieter.Koeff.temp[1,]
  
  # Erstellung der BTU-Prognose der 96 Viertelstunden f�r den Tag j
  
  r = 0
  
  for (k in ((j-1)*96+1):(j*96) )   # k:= Viertelstundennummer des Tages j, von 1 bis 96 ###
  {
    # f�r den Tag j relevante Prognosen; dabei wieder tempor�re Ersetzung zu unplausibler Anbieter durch fiktiven Anbieter 0
    # hier noch �berlegen: 1.) Muss A0N hier aufgef�hrt werden? 2.) M�ssen Unplausible hier extra aussortiert werden (erhalten ja eh kein Gewicht oder)?
    
    # neue Kombinationsprognose auf Basis von cvKM
    
    r = r + 1
    
    BTU[k] = round(predict(cvKM, newx = t(Prognosen.Folgetag[r,]), s = LambdaParameter), 0)
    #if (BTU[k]<0) {BTU[k]=0}
    
  }
}

# Berechnung des Prognosefehlers der BTU-Prognose

PF_BTU = rep(NA, times=Ende)

for (i in Pstart:Ende)
{
  if (BTU[i] < 0) {BTU[i] = 0}
  PF_BTU[i] = BTU[i] - HRN[i]
}

# # Berechnung der AM Prognose und AM Prognosefehlers der BTU-Prognose

###AM

#Number of forecasts available: Anbieterzahl

Anbieteranzahl = NA
for (i in 1:length(HR))     # i:=Viertelstundennummer
{   
  Anbieteranzahl[i] = 0
  
  #if (!is.na(A1[i])&&A1[i]<0) {A1[i]<-NA}
  if (!is.na(A1[i])) {Anbieteranzahl[i]=Anbieteranzahl[i]+1}
  #if (!is.na(A2[i])&&A2[i]<0) {A2[i]<-NA}
  if (!is.na(A2[i])) {Anbieteranzahl[i]=Anbieteranzahl[i]+1}
  #if (!is.na(A3[i])&&A3[i]<=0) {A3[i]<-NA}
  if (!is.na(A3[i])) {Anbieteranzahl[i]=Anbieteranzahl[i]+1}
  #if (!is.na(A4[i])&&A4[i]<0) {A4[i]<-NA}
  if (!is.na(A4[i])) {Anbieteranzahl[i]=Anbieteranzahl[i]+1}
  #if (!is.na(A5[i])&&A5[i]<0) {A5[i]<-NA}
  if (!is.na(A5[i])) {Anbieteranzahl[i]=Anbieteranzahl[i]+1}
  #if (!is.na(A6[i])&&A6[i]<0) {A6[i]<-NA}
  if (!is.na(A6[i])) {Anbieteranzahl[i]=Anbieteranzahl[i]+1}
  #if (!is.na(A7[i])&&A6[i]<0) {A6[i]<-NA}
  if (!is.na(A7[i])) {Anbieteranzahl[i]=Anbieteranzahl[i]+1}
}

#summing ins presence of NAs: sum(1:5, NA, na.rm = TRUE) # this command ignores NAs

AM = rep(NA, times=Ende) ####has to be calculated within the loop
A_total = rep(NA, times=Ende)

for (i in Pstart:Ende) { #i:Viertelstundenummer Zahl.Anbieter.genutzt.tgl ist jetzt vierst�ndlich
  A_total[i]  = sum(A1[i], A2[i], A3[i], A4[i], A5[i], A6[i], A7[i], na.rm = TRUE)
  AM[i] = (1/Anbieteranzahl[i])*A_total[i]
  AM[i] <- round(AM[i])
}

PF_AM = rep(NA, times=Ende)
for (i in Pstart:Ende)
{
  PF_AM[i] = AM[i] - HRN[i]
}

#mean(PF_BTU, na.rm =T)^2+var(PF_BTU, na.rm =T)

### Ausgabe: Ergebnistabellen, Fehlerma�e, Grafiken
### -----------------------------------------------


# Speichern der Prognosefehler der Anbieterprognosen

Prognosefehler_anb = matrix(c(Z[1:Ende],
                              PF_A1[1:Ende],
                              PF_A2[1:Ende],
                              PF_A3[1:Ende],
                              PF_A4[1:Ende],
                              PF_A5[1:Ende],
                              PF_A6[1:Ende],
                              PF_A7[1:Ende]), nrow=Ende, ncol=8)
colnames(Prognosefehler_anb) <- c("Z", "PF_A1", "PF_A2", "PF_A3", "PF_A4", "PF_A5", "PF_A6", "PF_A7")
write.csv(Prognosefehler_anb, "Prognosefehler_anb.csv", row.names=FALSE, na="#NV")

# Speichern der Prognosefehler der Kombiprognosen

Prognosefehler_ges = matrix(c(Z[1:Ende],
                              Anbieteranzahl[1:Ende],
                              AM[1:Ende],
                              K[1:Ende],
                              BTU[1:Ende],
                              PF_AM[1:Ende],
                              PF_K[1:Ende],
                              PF_BTU[1:Ende]), nrow=Ende, ncol=8)
colnames(Prognosefehler_ges) <- c("Z", "Anbieteranzahl", "AM", "K", "BTU", "PF_AM", "PF_K", "PF_BTU")
write.csv(Prognosefehler_ges, "Prognosefehler_ges.csv", row.names=FALSE, na="#NV")

# Berechnung der Fehlerma�e: mittlerer quadratischer Fehler (MSE),
# mittlerer absoluter Fehler (MAE) und mittlerer Fehler (ME)

# Berechnen der Gesamtfehlerma�e

mse_AM =  round(mean(PF_AM[1:Ende]^2,    na.rm =T), 0)
mae_AM =  round(mean(abs(PF_AM[1:Ende]), na.rm =T), 0)
me_AM  =  round(mean(PF_AM[1:Ende],      na.rm =T), 0)

mse_K =  round(mean(PF_K[1:Ende]^2,    na.rm =T), 0)
mae_K =  round(mean(abs(PF_K[1:Ende]), na.rm =T), 0)
me_K  =  round(mean(PF_K[1:Ende],      na.rm =T), 0)

mse_BTU =  round(mean(PF_BTU[1:Ende]^2,    na.rm =T), 0)
mae_BTU =  round(mean(abs(PF_BTU[1:Ende]), na.rm =T), 0)
me_BTU  =  round(mean(PF_BTU[1:Ende],      na.rm =T), 0)

mse_A1 =  round(mean(PF_A1[1:Ende]^2,    na.rm =T), 0)
mae_A1 =  round(mean(abs(PF_A1[1:Ende]), na.rm =T), 0)
me_A1  =  round(mean(PF_A1[1:Ende],      na.rm =T), 0)

mse_A2 =  round(mean(PF_A2[1:Ende]^2,    na.rm =T), 0)
mae_A2 =  round(mean(abs(PF_A2[1:Ende]), na.rm =T), 0)
me_A2  =  round(mean(PF_A2[1:Ende],      na.rm =T), 0)

mse_A3 =  round(mean(PF_A3[1:Ende]^2,    na.rm =T), 0)
mae_A3 =  round(mean(abs(PF_A3[1:Ende]), na.rm =T), 0)
me_A3  =  round(mean(PF_A3[1:Ende],      na.rm =T), 0)

mse_A4 =  round(mean(PF_A4[1:Ende]^2,    na.rm =T), 0)
mae_A4 =  round(mean(abs(PF_A4[1:Ende]), na.rm =T), 0)
me_A4  =  round(mean(PF_A4[1:Ende],      na.rm =T), 0)

mse_A5 =  round(mean(PF_A5[1:Ende]^2,    na.rm =T), 0)
mae_A5 =  round(mean(abs(PF_A5[1:Ende]), na.rm =T), 0)
me_A5  =  round(mean(PF_A5[1:Ende],      na.rm =T), 0)

mse_A6 =  round(mean(PF_A6[1:Ende]^2,    na.rm =T), 0)
mae_A6 =  round(mean(abs(PF_A6[1:Ende]), na.rm =T), 0)
me_A6  =  round(mean(PF_A6[1:Ende],      na.rm =T), 0)

mse_A7 =  round(mean(PF_A7[1:Ende]^2,    na.rm =T), 0)
mae_A7 =  round(mean(abs(PF_A7[1:Ende]), na.rm =T), 0)
me_A7  =  round(mean(PF_A7[1:Ende],      na.rm =T), 0)

# Speichern der Gesamtfehlerma�e

mse <- c(mse_AM, mse_K, mse_BTU, mse_A1, mse_A2, mse_A3, mse_A4, mse_A5, mse_A6, mse_A7)
mae <- c(mae_AM, mae_K, mae_BTU, mae_A1, mae_A2, mae_A3, mae_A4, mae_A5, mae_A6, mae_A7)
me  <- c(me_AM, me_K, me_BTU, me_A1, me_A2, me_A3, me_A4, me_A5, me_A6, me_A7)
Fehlerma�e = rbind(mse, mae, me)
colnames(Fehlerma�e) <- c("AM", "K", "BTU", "A1", "A2", "A3", "A4", "A5", "A6", "A7")
rownames(Fehlerma�e) <- c("mse", "mae", "me")
write.csv(Fehlerma�e, "Fehlerma�e.csv", row.names=FALSE, na="#NV")

# Auslesen der Jahresanf�nge

JAnz         = max(J) - min(J) + 1
JBeg         = rep(1, times=(JAnz+1))
JBeg[JAnz+1] = Ende + 1
JNum         = 2
JLis         = rep(J[1], times=JAnz)

for (i in 2:Ende)
{
  if (J[i]!=J[i-1]) {JBeg[JNum]<-i
  JLis[JNum]<-J[i]
  JNum = JNum + 1}
}

# Berechnen der Jahresfehlerma�e

mse_AM_J  =  rep(NA, times=JAnz)
mse_K_J   =  rep(NA, times=JAnz)
mse_BTU_J =  rep(NA, times=JAnz)

mae_AM_J  =  rep(NA, times=JAnz)
mae_K_J   =  rep(NA, times=JAnz)
mae_BTU_J =  rep(NA, times=JAnz)

me_AM_J   =  rep(NA, times=JAnz)
me_K_J    =  rep(NA, times=JAnz)
me_BTU_J  =  rep(NA, times=JAnz)

mse_A1_J  =  rep(NA, times=JAnz)
mse_A2_J  =  rep(NA, times=JAnz)
mse_A3_J  =  rep(NA, times=JAnz)
mse_A4_J  =  rep(NA, times=JAnz)
mse_A5_J  =  rep(NA, times=JAnz)
mse_A6_J  =  rep(NA, times=JAnz)
mse_A7_J  =  rep(NA, times=JAnz)

mae_A1_J  =  rep(NA, times=JAnz)
mae_A2_J  =  rep(NA, times=JAnz)
mae_A3_J  =  rep(NA, times=JAnz)
mae_A4_J  =  rep(NA, times=JAnz)
mae_A5_J  =  rep(NA, times=JAnz)
mae_A6_J  =  rep(NA, times=JAnz)
mae_A7_J  =  rep(NA, times=JAnz)

me_A1_J  =  rep(NA, times=JAnz)
me_A2_J  =  rep(NA, times=JAnz)
me_A3_J  =  rep(NA, times=JAnz)
me_A4_J  =  rep(NA, times=JAnz)
me_A5_J  =  rep(NA, times=JAnz)
me_A6_J  =  rep(NA, times=JAnz)
me_A7_J  =  rep(NA, times=JAnz)

#mean_HRN_J = 0
#sd_HRN_J = 0


for (l in 1:JAnz) 
{
  mse_AM_J[l] =  round(mean(PF_AM[JBeg[l]:(JBeg[l+1]-1)]^2,    na.rm =T), 0)
  mae_AM_J[l] =  round(mean(abs(PF_AM[JBeg[l]:(JBeg[l+1]-1)]), na.rm =T), 0)
  me_AM_J[l]  =  round(mean(PF_AM[JBeg[l]:(JBeg[l+1]-1)],      na.rm =T), 0)
  
  mse_K_J[l] =  round(mean(PF_K[JBeg[l]:(JBeg[l+1]-1)]^2,    na.rm =T), 0)
  mae_K_J[l] =  round(mean(abs(PF_K[JBeg[l]:(JBeg[l+1]-1)]), na.rm =T), 0)
  me_K_J[l]  =  round(mean(PF_K[JBeg[l]:(JBeg[l+1]-1)],      na.rm =T), 0)
  
  mse_BTU_J[l] =  round(mean(PF_BTU[JBeg[l]:(JBeg[l+1]-1)]^2,    na.rm =T), 0)
  mae_BTU_J[l] =  round(mean(abs(PF_BTU[JBeg[l]:(JBeg[l+1]-1)]), na.rm =T), 0)
  me_BTU_J[l]  =  round(mean(PF_BTU[JBeg[l]:(JBeg[l+1]-1)],      na.rm =T), 0)
  
  mse_A1_J[l] =  round(mean(PF_A1[JBeg[l]:(JBeg[l+1]-1)]^2,    na.rm =T), 0)
  mae_A1_J[l] =  round(mean(abs(PF_A1[JBeg[l]:(JBeg[l+1]-1)]), na.rm =T), 0)
  me_A1_J[l]  =  round(mean(PF_A1[JBeg[l]:(JBeg[l+1]-1)],      na.rm =T), 0)
  
  mse_A2_J[l] =  round(mean(PF_A2[JBeg[l]:(JBeg[l+1]-1)]^2,    na.rm =T), 0)
  mae_A2_J[l] =  round(mean(abs(PF_A2[JBeg[l]:(JBeg[l+1]-1)]), na.rm =T), 0)
  me_A2_J[l]  =  round(mean(PF_A2[JBeg[l]:(JBeg[l+1]-1)],      na.rm =T), 0)
  
  mse_A3_J[l] =  round(mean(PF_A3[JBeg[l]:(JBeg[l+1]-1)]^2,    na.rm =T), 0)
  mae_A3_J[l] =  round(mean(abs(PF_A3[JBeg[l]:(JBeg[l+1]-1)]), na.rm =T), 0)
  me_A3_J[l]  =  round(mean(PF_A3[JBeg[l]:(JBeg[l+1]-1)],      na.rm =T), 0)
  
  mse_A4_J[l] =  round(mean(PF_A4[JBeg[l]:(JBeg[l+1]-1)]^2,    na.rm =T), 0)
  mae_A4_J[l] =  round(mean(abs(PF_A4[JBeg[l]:(JBeg[l+1]-1)]), na.rm =T), 0)
  me_A4_J[l]  =  round(mean(PF_A4[JBeg[l]:(JBeg[l+1]-1)],      na.rm =T), 0)
  
  mse_A5_J[l] =  round(mean(PF_A5[JBeg[l]:(JBeg[l+1]-1)]^2,    na.rm =T), 0)
  mae_A5_J[l] =  round(mean(abs(PF_A5[JBeg[l]:(JBeg[l+1]-1)]), na.rm =T), 0)
  me_A5_J[l]  =  round(mean(PF_A5[JBeg[l]:(JBeg[l+1]-1)],      na.rm =T), 0)
  
  mse_A6_J[l] =  round(mean(PF_A6[JBeg[l]:(JBeg[l+1]-1)]^2,    na.rm =T), 0)
  mae_A6_J[l] =  round(mean(abs(PF_A6[JBeg[l]:(JBeg[l+1]-1)]), na.rm =T), 0)
  me_A6_J[l]  =  round(mean(PF_A6[JBeg[l]:(JBeg[l+1]-1)],      na.rm =T), 0)
  
  mse_A7_J[l] =  round(mean(PF_A7[JBeg[l]:(JBeg[l+1]-1)]^2,    na.rm =T), 0)
  mae_A7_J[l] =  round(mean(abs(PF_A7[JBeg[l]:(JBeg[l+1]-1)]), na.rm =T), 0)
  me_A7_J[l]  =  round(mean(PF_A7[JBeg[l]:(JBeg[l+1]-1)],      na.rm =T), 0)
  
  #mean_HRN_J[l] =  round(mean(HRN[JBeg[l]:(JBeg[l+1]-1)],    na.rm =T), 0)
  #sd_HRN_J[l] =  round(sd(HRN[JBeg[l]:(JBeg[l+1]-1)],    na.rm =T), 0)
}

# Berechnen der durchschnittlichen viertelst�ndlichen Einspeiseleistung je Jahr (HR oder HRN benutzen)

JMW  =  rep(NA, times=JAnz)

for (l in 1:JAnz) 
{
  JMW[l] = round((sum(HRN[JBeg[l]:(JBeg[l+1]-1)], na.rm =T) * 1/((JBeg[l+1])-JBeg[l])), 0)
}

# Mitteln der Jahresanbieteranzahl

Anbieteranzahl_J = rep(NA, times=JAnz)

for (l in 1:JAnz)
{
  Anbieteranzahl_J[l] = round(mean(Anbieteranzahl[JBeg[l]:(JBeg[l+1]-1)], na.rm =T))
}

# Speichern der Jahresehlerma�e

Fehlerma�e_J = matrix(c(JLis[1:JAnz],
                        MW_J[1:JAnz],
                        JMW[1:JAnz],
                        Anbieteranzahl_J[1:JAnz],
                        mse_AM_J[1:JAnz],
                        mse_K_J[1:JAnz],
                        mse_BTU_J[1:JAnz],
                        mae_AM_J[1:JAnz],
                        mae_K_J[1:JAnz],
                        mae_BTU_J[1:JAnz],
                        me_AM_J[1:JAnz],
                        me_K_J[1:JAnz],
                        me_BTU_J[1:JAnz],
                        me_A1_J[1:JAnz],
                        me_A2_J[1:JAnz],
                        me_A3_J[1:JAnz],
                        me_A4_J[1:JAnz],
                        me_A5_J[1:JAnz],
                        me_A6_J[1:JAnz],
                        me_A7_J[1:JAnz],
                        mae_A1_J[1:JAnz],
                        mae_A2_J[1:JAnz],
                        mae_A3_J[1:JAnz],
                        mae_A4_J[1:JAnz],
                        mae_A5_J[1:JAnz],
                        mae_A6_J[1:JAnz],
                        mae_A7_J[1:JAnz],
                        mse_A1_J[1:JAnz],
                        mse_A2_J[1:JAnz],
                        mse_A3_J[1:JAnz],
                        mse_A4_J[1:JAnz],
                        mse_A5_J[1:JAnz],
                        mse_A6_J[1:JAnz],
                        mse_A7_J[1:JAnz]), nrow=JAnz, ncol=34)
colnames(Fehlerma�e_J) <- c("Jahr", "installierte Leistung [MW]", "mittlere Einspeisung [MW]", "Anbieteranzahl", "mse_AM_J", "mse_K_J", "mse_BTU_J", "mae_AM_J", "mae_K_J", "mae_BTU_J", "me_AM_J", "me_K_J", "me_BTU_J",
                            "me_A1_J", "me_A2_J", "me_A3_J", "me_A4_J", "me_A5_J", "me_A6_J", "me_A7_J",
                            "mae_A1_J", "mae_A2_J", "mae_A3_J", "mae_A4_J", "mae_A5_J", "mae_A6_J", "mae_A7_J",
                            "mse_A1_J", "mse_A2_J", "mse_A3_J", "mse_A4_J", "mse_A5_J", "mse_A6_J", "mse_A7_J")
write.csv(Fehlerma�e_J, "Fehlerma�e_J.csv", row.names=FALSE, na="#NV")







# Auslesen der Monatsanf�nge

MAnz         = 12*(JAnz-2) + (12-M[1]+1) + M[Ende]
MBeg         = rep(1, times=(MAnz+1))
MBeg[MAnz+1] = Ende + 1
MNum         = 2
MLis         = rep(M[1], times=MAnz)
JLis_M       = rep(J[1], times=MAnz)

for (i in 2:Ende)
{
  if (M[i]!=M[i-1]) {MBeg[MNum]<-i
  MLis[MNum]<-M[i]
  JLis_M[MNum]<-J[i]
  MNum = MNum + 1}
}

# Berechnen der Monatsfehlerma�e

mse_AM_M  =  rep(NA, times=MAnz)
mse_K_M   =  rep(NA, times=MAnz)
mse_BTU_M =  rep(NA, times=MAnz)

mae_AM_M  =  rep(NA, times=MAnz)
mae_K_M   =  rep(NA, times=MAnz)
mae_BTU_M =  rep(NA, times=MAnz)

me_AM_M   =  rep(NA, times=MAnz)
me_K_M    =  rep(NA, times=MAnz)
me_BTU_M  =  rep(NA, times=MAnz)

mse_A1_M  =  rep(NA, times=MAnz)
mse_A2_M  =  rep(NA, times=MAnz)
mse_A3_M  =  rep(NA, times=MAnz)
mse_A4_M  =  rep(NA, times=MAnz)
mse_A5_M  =  rep(NA, times=MAnz)
mse_A6_M  =  rep(NA, times=MAnz)
mse_A7_M  =  rep(NA, times=MAnz)

mae_A1_M  =  rep(NA, times=MAnz)
mae_A2_M  =  rep(NA, times=MAnz)
mae_A3_M  =  rep(NA, times=MAnz)
mae_A4_M  =  rep(NA, times=MAnz)
mae_A5_M  =  rep(NA, times=MAnz)
mae_A6_M  =  rep(NA, times=MAnz)
mae_A7_M  =  rep(NA, times=MAnz)

me_A1_M  =  rep(NA, times=MAnz)
me_A2_M  =  rep(NA, times=MAnz)
me_A3_M  =  rep(NA, times=MAnz)
me_A4_M  =  rep(NA, times=MAnz)
me_A5_M  =  rep(NA, times=MAnz)
me_A6_M  =  rep(NA, times=MAnz)
me_A7_M  =  rep(NA, times=MAnz)

for (m in 1:MAnz) 
{
  mse_AM_M[m] =  round(mean(PF_AM[MBeg[m]:(MBeg[m+1]-1)]^2,    na.rm =T), 0)
  mae_AM_M[m] =  round(mean(abs(PF_AM[MBeg[m]:(MBeg[m+1]-1)]), na.rm =T), 0)
  me_AM_M[m]  =  round(mean(PF_AM[MBeg[m]:(MBeg[m+1]-1)],      na.rm =T), 0)
  
  mse_K_M[m] =  round(mean(PF_K[MBeg[m]:(MBeg[m+1]-1)]^2,    na.rm =T), 0)
  mae_K_M[m] =  round(mean(abs(PF_K[MBeg[m]:(MBeg[m+1]-1)]), na.rm =T), 0)
  me_K_M[m]  =  round(mean(PF_K[MBeg[m]:(MBeg[m+1]-1)],      na.rm =T), 0)
  
  mse_BTU_M[m] =  round(mean(PF_BTU[MBeg[m]:(MBeg[m+1]-1)]^2,    na.rm =T), 0)
  mae_BTU_M[m] =  round(mean(abs(PF_BTU[MBeg[m]:(MBeg[m+1]-1)]), na.rm =T), 0)
  me_BTU_M[m]  =  round(mean(PF_BTU[MBeg[m]:(MBeg[m+1]-1)],      na.rm =T), 0)
  
  mse_A1_M[m] =  round(mean(PF_A1[MBeg[m]:(MBeg[m+1]-1)]^2,    na.rm =T), 0)
  mae_A1_M[m] =  round(mean(abs(PF_A1[MBeg[m]:(MBeg[m+1]-1)]), na.rm =T), 0)
  me_A1_M[m]  =  round(mean(PF_A1[MBeg[m]:(MBeg[m+1]-1)],      na.rm =T), 0)
  
  mse_A2_M[m] =  round(mean(PF_A2[MBeg[m]:(MBeg[m+1]-1)]^2,    na.rm =T), 0)
  mae_A2_M[m] =  round(mean(abs(PF_A2[MBeg[m]:(MBeg[m+1]-1)]), na.rm =T), 0)
  me_A2_M[m]  =  round(mean(PF_A2[MBeg[m]:(MBeg[m+1]-1)],      na.rm =T), 0)
  
  mse_A3_M[m] =  round(mean(PF_A3[MBeg[m]:(MBeg[m+1]-1)]^2,    na.rm =T), 0)
  mae_A3_M[m] =  round(mean(abs(PF_A3[MBeg[m]:(MBeg[m+1]-1)]), na.rm =T), 0)
  me_A3_M[m]  =  round(mean(PF_A3[MBeg[m]:(MBeg[m+1]-1)],      na.rm =T), 0)
  
  mse_A4_M[m] =  round(mean(PF_A4[MBeg[m]:(MBeg[m+1]-1)]^2,    na.rm =T), 0)
  mae_A4_M[m] =  round(mean(abs(PF_A4[MBeg[m]:(MBeg[m+1]-1)]), na.rm =T), 0)
  me_A4_M[m]  =  round(mean(PF_A4[MBeg[m]:(MBeg[m+1]-1)],      na.rm =T), 0)
  
  mse_A5_M[m] =  round(mean(PF_A5[MBeg[m]:(MBeg[m+1]-1)]^2,    na.rm =T), 0)
  mae_A5_M[m] =  round(mean(abs(PF_A5[MBeg[m]:(MBeg[m+1]-1)]), na.rm =T), 0)
  me_A5_M[m]  =  round(mean(PF_A5[MBeg[m]:(MBeg[m+1]-1)],      na.rm =T), 0)
  
  mse_A6_M[m] =  round(mean(PF_A6[MBeg[m]:(MBeg[m+1]-1)]^2,    na.rm =T), 0)
  mae_A6_M[m] =  round(mean(abs(PF_A6[MBeg[m]:(MBeg[m+1]-1)]), na.rm =T), 0)
  me_A6_M[m]  =  round(mean(PF_A6[MBeg[m]:(MBeg[m+1]-1)],      na.rm =T), 0)
  
  mse_A7_M[m] =  round(mean(PF_A7[MBeg[m]:(MBeg[m+1]-1)]^2,    na.rm =T), 0)
  mae_A7_M[m] =  round(mean(abs(PF_A7[MBeg[m]:(MBeg[m+1]-1)]), na.rm =T), 0)
  me_A7_M[m]  =  round(mean(PF_A7[MBeg[m]:(MBeg[m+1]-1)],      na.rm =T), 0)
}

# Mitteln der Monatsanbieteranzahl

Anbieteranzahl_M = rep(NA, times=MAnz)

for (m in 1:MAnz)
{
  Anbieteranzahl_M[m] = round(mean(Anbieteranzahl[MBeg[m]:(MBeg[m+1]-1)], na.rm =T))
}

# Speichern der Monatsfehlerma�e

Fehlerma�e_M = matrix(c(JLis_M[1:MAnz],
                        MLis[1:MAnz],
                        Anbieteranzahl_M[1:MAnz],
                        mse_AM_M[1:MAnz],
                        mse_K_M[1:MAnz],
                        mse_BTU_M[1:MAnz],
                        mae_AM_M[1:MAnz],
                        mae_K_M[1:MAnz],
                        mae_BTU_M[1:MAnz],
                        me_AM_M[1:MAnz],
                        me_K_M[1:MAnz],
                        me_BTU_M[1:MAnz],
                        me_A1_M[1:MAnz],
                        me_A2_M[1:MAnz],
                        me_A3_M[1:MAnz],
                        me_A4_M[1:MAnz],
                        me_A5_M[1:MAnz],
                        me_A6_M[1:MAnz],
                        me_A7_M[1:MAnz],
                        mae_A1_M[1:MAnz],
                        mae_A2_M[1:MAnz],
                        mae_A3_M[1:MAnz],
                        mae_A4_M[1:MAnz],
                        mae_A5_M[1:MAnz],
                        mae_A6_M[1:MAnz],
                        mae_A7_M[1:MAnz],
                        mse_A1_M[1:MAnz],
                        mse_A2_M[1:MAnz],
                        mse_A3_M[1:MAnz],
                        mse_A4_M[1:MAnz],
                        mse_A5_M[1:MAnz],
                        mse_A6_M[1:MAnz],
                        mse_A7_M[1:MAnz]), nrow=MAnz, ncol=33)
colnames(Fehlerma�e_M) <- c("JLis", "MLis", "Anbieteranzahl_M", "mse_AM_M", "mse_K_M", "mse_BTU_M", "mae_AM_M", "mae_K_M", "mae_BTU_M", "me_AM_M", "me_K_M", "me_BTU_M",
                            "me_A1_M", "me_A2_M", "me_A3_M", "me_A4_M", "me_A5_M", "me_A6_M", "me_A7_M",
                            "mae_A1_M", "mae_A2_M", "mae_A3_M", "mae_A4_M", "mae_A5_M", "mae_A6_M", "mae_A7_M",
                            "mse_A1_M", "mse_A2_M", "mse_A3_M", "mse_A4_M", "mse_A5_M", "mse_A6_M", "mse_A7_M")
write.csv(Fehlerma�e_M, "Fehlerma�e_M.csv", row.names=FALSE, na="#NV")


# Grafik 1: 1. Anbieterfehler, installierte und durchschnittliche Leistung, j�hrlich

windows(width=16, height=8)
par(mfrow=c(1,1))

plot(mse_A1_J, type="o", col="red", lwd=2, lty=2, xlab="Jahr", ylab="mse", main = "Anbieterfehler, installierte und durchschnittliche Leistung, j�hrlich")
lines(mse_A2_J, type="o", col="orange", lwd=2, lty=2)
lines(mse_A3_J, type="o", col="yellow", lwd=2, lty=2)
lines(mse_A4_J, type="o", col="green", lwd=2, lty=2)
lines(mse_A5_J, type="o", col="brown", lwd=2, lty=2)
lines(mse_A6_J, type="o", col="violet", lwd=2, lty=2)
lines(mse_A7_J, type="o", col="pink", lwd=2, lty=2)
lines(MW_J, type="o", col="pink", lwd=2, lty=2)
lines(JMW, type="o", col="pink", lwd=2, lty=2)

#savePlot(filename="//Server-lsew/serverdaten/MitarbeiterOrdner/Kaeso/Arbeit_EW/FOCCSI/50Hertz/Modellweiterentwicklung/Modell mit Datenvorbehandlung/Diagramm_B.jpg", type="jpeg")

# Speichern der Werte zu Grafik 1

Grafik1 = matrix(c(JLis,
                   mse_A1_J,
                   mse_A2_J,
                   mse_A3_J,
                   mse_A4_J,
                   mse_A5_J,
                   mse_A6_J,
                   mse_A7_J,
                   MW_J,
                   JMW), nrow=JAnz, ncol=10)
colnames(Grafik1) <- c("Jahr", "mse_A1_J","mse_A2_J", "mse_A3_J", "mse_A4_J", "mse_A5_J", "mse_A6_J", "mse_A7_J", "MW_J", "JMW")
write.csv(Grafik1, "Grafik1.csv", row.names=FALSE, na="#NV")

# Grafik2 : Anbietervergleich, j�hrlich (7 MSE's jeweils geteilt durch durchschnittliche Erzeugung)

#windows(width=16, height=8)
#par(mfrow=c(1,1))
#plot(mse_A1_J/JMW, type="o", col="red", lwd=2, lty=2, xlab="Jahr", ylab="mse/MW", main = "Anbietervergleich, j�hrlich (7 MSE's jeweils geteilt durch durchschnittliche Erzeugung)")
#lines(mse_A2_J/JMW, type="o", col="orange", lwd=2, lty=2)
#lines(mse_A3_J/JMW, type="o", col="yellow", lwd=2, lty=2)
#lines(mse_A4_J/JMW, type="o", col="green", lwd=2, lty=2)
#lines(mse_A5_J/JMW, type="o", col="brown", lwd=2, lty=2)
#lines(mse_A6_J/JMW, type="o", col="violet", lwd=2, lty=2)
#lines(mse_A7_J/JMW, type="o", col="pink", lwd=2, lty=2)
#savePlot(filename="//Server-lsew/serverdaten/MitarbeiterOrdner/Kaeso/Arbeit_EW/FOCCSI/50Hertz/Modellweiterentwicklung/Modell mit Datenvorbehandlung/Diagramm_A.jpg", type="jpeg")

# Speichern der Werte zu Grafik 2

Grafik2 = matrix(c(JLis,
                   round(mse_A1_J/JMW, 0),
                   round(mse_A2_J/JMW, 0),
                   round(mse_A3_J/JMW, 0),
                   round(mse_A4_J/JMW, 0),
                   round(mse_A5_J/JMW, 0),
                   round(mse_A6_J/JMW, 0),
                   round(mse_A7_J/JMW, 0)), nrow=JAnz, ncol=8)
colnames(Grafik2) <- c("Jahr", "mse_A1_J/JMW","mse_A2_J/JMW", "mse_A3_J/JMW", "mse_A4_J/JMW", "mse_A5_J/JMW", "mse_A6_J/JMW", "mse_A7_J/JMW")
write.csv(Grafik2, "Grafik2.csv", row.names=FALSE, na="#NV")

# Grafik 3: Methodenfehler, monatlich

# Speichern der Werte zu Grafik 3

JLis_M = rep(NA, times=MAnz)
JLis_M[1] = JLis[1]
for (m in 2:MAnz)
{
  if (MLis[m-1]==12) {JLis_M[m]=JLis_M[m]+1} else {JLis_M[m]=JLis_M[m]}
}

Grafik3 = matrix(c(MLis,
                   mse_AM_M,
                   mse_K_M,
                   mse_BTU_M,
                   Anbieteranzahl_M), nrow=MAnz, ncol=5)
colnames(Grafik3) <- c("Monat", "mse_AM","mse_K", "mse_BTU", "Anbieteranzahl")
write.csv(Grafik3, "Grafik3.csv", row.names=FALSE, na="#NV")

# Grafik 4v: Prognosefehler der BTU-Prognose, viertelst�ndlich (Einh�llende)
#
# lokale Maxima (obere Einh�llende) [oder doch lieber monatlich?]
#
#maxH = rep(NA, times=Ende)
#for (i in (Pstart+1):(Ende-1))
#{
#  if (!is.na(PF_BTU[i])&&PF_BTU[i]>=PF_BTU[i-1]&&PF_BTU[i]>=PF_BTU[i+1]) {maxH[i]=PF_BTU[i]}
#}
#
# lokale Minima (untere Einh�llende)
#
#minH = rep(NA, times=Ende)
#for (i in (Pstart+1):(Ende-1))
#{
#  if (!is.na(PF_BTU[i])&&PF_BTU[i]>=PF_BTU[i-1]&&PF_BTU[i]>=PF_BTU[i+1]) {minH[i]=PF_BTU[i]}
#}
#
#Grafik4 = matrix(c(Z,
#                   PF_BTU,
#                   maxH,
#                   minH), nrow=Ende, ncol=4)
#colnames(Grafik4) <- c("Zeit", "PF_BTU", "Obere", "Untere")
#write.csv(Grafik4, "Grafik4.csv", sep = ";", dec = ",", row.names=FALSE, na="#NV")

# Grafik 4: Prognosefehler der BTU-Prognose, monatlich (Einh�llende)

#
#
#

# H�lkurven

maxH_M = rep(NA, times=MAnz)
minH_M = rep(NA, times=MAnz)

for (m in 1:MAnz) 
{
  maxH_M[m] =  max(PF_BTU[MBeg[m]:(MBeg[m+1]-1)], na.rm =T)
  minH_M[m] =  min(PF_BTU[MBeg[m]:(MBeg[m+1]-1)], na.rm =T)
}

# Speichern der Werte zu Grafik 4

Grafik4 = matrix(c(MLis,
                   maxH_M,
                   minH_M,
                   me_BTU_M,
                   Anbieteranzahl_M), nrow=MAnz, ncol=5)
colnames(Grafik4) <- c("Monat", "Obere", "Untere", "me_BTU_M", "Anbieter")
write.csv(Grafik4, "Grafik4.csv", row.names=FALSE, na="#NV")

# Grafik 5: Methodenvergleich, monatlich

Vergleich_M    = round((100*(mse_BTU_M[1:MAnz]-mse_K_M[1:MAnz])/mse_K_M[1:MAnz]), 0)
Durchschnitt_M = 0
AnzVerb_M        = 0
Verb_M           = 0
DurVerb_M        = 0
AnzVers_M        = 0
Vers_M           = 0
DurVers_M        = 0

for (m in 1:MAnz)
{
  Durchschnitt_M[m] = round(mean(Vergleich_M, na.rm=T), 0)
  if (!is.na(Vergleich_M[m])&&Vergleich_M[m]<0) {AnzVerb_M = AnzVerb_M +1
  Verb_M = Verb_M + Vergleich_M[m]}
  if (!is.na(Vergleich_M[m])&&Vergleich_M[m]>0) {AnzVers_M = AnzVers_M +1
  Vers_M = Vers_M + Vergleich_M[m]}
}

DurVerb_M = round(Verb_M/AnzVerb_M, 0)
DurVers_M = round(Vers_M/AnzVers_M, 0)

DurVerb2_M = rep(DurVerb_M, times=MAnz)
AnzVerb2_M = rep(AnzVerb_M, times=MAnz)
DurVers2_M = rep(DurVers_M, times=MAnz)
AnzVers2_M = rep(AnzVers_M, times=MAnz)

# Darstellung der Gr�nde, warum ein Anbieter in der Kombination ignoriert wird

windows(width=16, height=8)
par(mfrow=c(1,1))

plot(Anbieter.stat.grund[(FL_d+1):(Ende/96),7], ylim = c(0,8))


# Darstellung der t�glich genutzten Anbieter

windows(width=16, height=8)
par(mfrow=c(1,1))

plot(Zahl.Anbieter.genutzt.tgl[(FL_d+1):(Ende/96)])


# Darstellung der Modellkoeffizienten

windows(width=16, height=8)
par(mfrow=c(1,1))

plot(Anbieter.Koeff[(FL_d+1):(Ende/96),2], type="l", col="red", lwd=2, ylim=c(-0.2,1.0))
lines(Anbieter.Koeff[(FL_d+1):(Ende/96),3], type="l", col="blue", lwd=2)
lines(Anbieter.Koeff[(FL_d+1):(Ende/96),4], type="l", col="green", lwd=2)
lines(Anbieter.Koeff[(FL_d+1):(Ende/96),5], type="l", col="orange", lwd=2)
lines(Anbieter.Koeff[(FL_d+1):(Ende/96),6], type="l", col="violet", lwd=2)
lines(Anbieter.Koeff[(FL_d+1):(Ende/96),7], type="l", col="black", lwd=2)
lines(Anbieter.Koeff[(FL_d+1):(Ende/96),8], type="l", col="grey", lwd=2)

# # Code f�r png-Export der Gewichte (von Dragana)

png("Zeitliche Entwicklung der Gewichte 2010-2019.png", width = 876, height = 551, units = "px", pointsize = 14,
    bg = "white", res = NA, family = "", restoreConsole = TRUE)
plot(Anbieter.Koeff[(FL_d+1):(Ende/96),2], type="l", col="orange", lwd=2, ylab = "Weight", xlab = "Remaining days 2010-2019", ylim=c(-0.1,1))
lines(Anbieter.Koeff[(FL_d+1):(Ende/96),3], type="l", col="green", lwd=2)
lines(Anbieter.Koeff[(FL_d+1):(Ende/96),4], type="l", col="pink", lwd=2)
lines(Anbieter.Koeff[(FL_d+1):(Ende/96),5], type="l", col="magenta", lwd=2)
lines(Anbieter.Koeff[(FL_d+1):(Ende/96),6], type="l", col="black", lwd=2)
lines(Anbieter.Koeff[(FL_d+1):(Ende/96),7], type="l", col="blue", lwd=2)
lines(Anbieter.Koeff[(FL_d+1):(Ende/96),8], type="l", col="grey", lwd=2)
legend("topright", legend=c("W1","W2","W3","W4","W5","W6", "W7"), lwd=c(2,2), col=c("orange","green", "pink", "magenta", "black", "blue", "grey"))
dev.off()




windows(width=16, height=8)
par(mfrow=c(1,1))

hist(HRN, breaks = 200)

#plot(Zahl.Anbieter.genutzt.tgl)

plot(Vergleich_M, type="o", col="blue", lwd=2, xlab="Jahr", ylab="Verschlechterung [%]", main = "monatlicher Vergleich (mse_BTU - mse_K) / mse_K")
lines(Durchschnitt_M, type="l", col="red", lwd=2, lty=2)
lines(+10, type="l", col="grey", lwd=2)
lines(+5, type="l", col="grey", lwd=2)
lines(+0, type="l", col="black", lwd=2)
lines(-5, type="l", col="grey", lwd=2)
lines(-10, type="l", col="grey", lwd=2)
savePlot(filename="//Server-lsew/serverdaten/MitarbeiterOrdner/Kaeso/Arbeit_EW/FOCCSI/50Hertz/Modellweiterentwicklung/Modell mit Datenvorbehandlung2/Diagramm_M.jpg", type="jpeg")

# Speichern der Werte zu Grafik 5

Grafik5 = matrix(c(MLis,
                   Vergleich_M,
                   Durchschnitt_M,
                   DurVerb2_M,
                   AnzVerb2_M,
                   DurVers2_M,
                   AnzVers2_M), nrow=MAnz, ncol=7)
colnames(Grafik5) <- c("Monat", "Vergleich", "Durchschnitt", "Verbesserung", "Anzahl", "Verschlechterung", "Anzahl")
write.csv(Grafik5, "Grafik5.csv", row.names=FALSE, na="#NV")

# Grafik 6: Methodenvergleich, j�hrlich

Vergleich_J    = round((100*(mse_BTU_J[1:JAnz]-mse_K_J[1:JAnz])/mse_K_J[1:JAnz]), 0)
Durchschnitt_J = 0
AnzVerb_J        = 0
Verb_J             = 0
DurVerb_J          = 0
AnzVers_J          = 0
Vers_J             = 0
DurVers_J          = 0

for (l in 1:JAnz)
{
  Durchschnitt_J[l] = round(mean(Vergleich_J, na.rm =T), 0)
  if (!is.na(Vergleich_J[l])&&Vergleich_J[l]<0) {AnzVerb_J = AnzVerb_J +1
  Verb_J = Verb_J + Vergleich_J[l]}
  if (!is.na(Vergleich_J[l])&&Vergleich_J[l]>0) {AnzVers_J = AnzVers_J +1
  Vers_J = Vers_J + Vergleich_J[l]}
}

DurVerb_J = round(Verb_J/AnzVerb_J, 0)
DurVers_J = round(Vers_J/AnzVers_J, 0)

DurVerb2_J = rep(DurVerb_J, times=JAnz)
AnzVerb2_J = rep(AnzVerb_J, times=JAnz)
DurVers2_J = rep(DurVers_J, times=JAnz)
AnzVers2_J = rep(AnzVers_J, times=JAnz)

windows(width=16, height=8)
par(mfrow=c(1,1))
plot(BTU)
plot(Vergleich_J, type="o", col="blue", lwd=2, xlab="Jahr", ylab="Verschlechterung [%]", main = "j�hrlicher Vergleich (mse_BTU - mse_K) / mse_K")
lines(Durchschnitt_J, type="l", col="red", lwd=2, lty=2)
lines(+10, type="l", col="grey", lwd=2)
lines(+5, type="l", col="grey", lwd=2)
lines(+0, type="l", col="black", lwd=2)
lines(-5, type="l", col="grey", lwd=2)
lines(-10, type="l", col="grey", lwd=2)
savePlot(filename="//Server-lsew/serverdaten/MitarbeiterOrdner/Kaeso/Arbeit_EW/FOCCSI/50Hertz/Modellweiterentwicklung/Modell mit Datenvorbehandlung2/Diagramm_J.jpg", type="jpeg")

Grafik6 = matrix(c(JLis,
                   Vergleich_J,
                   Durchschnitt_J,
                   DurVerb2_J,
                   AnzVerb2_J,
                   DurVers2_J,
                   AnzVers2_J), nrow=JAnz, ncol=7)
colnames(Grafik6) <- c("Jahr", "Vergleich", "Durchschnitt", "Verbesserung", "Anzahl", "Verschlechterung", "Anzahl")
write.csv(Grafik6, "Grafik6.csv", row.names=FALSE, na="#NV")


# # Mean und SD von HR, K und den Einzelprognosen

sd_HR_J = rep(NA, times=JAnz)
me_HR_J = rep(NA, times=JAnz)
sd_K_J = rep(NA, times=JAnz)
me_K_J = rep(NA, times=JAnz)
sd_A1_J = rep(NA, times=JAnz)
me_A1_J = rep(NA, times=JAnz)
sd_A2_J = rep(NA, times=JAnz)
me_A2_J = rep(NA, times=JAnz)
sd_A3_J = rep(NA, times=JAnz)
me_A3_J = rep(NA, times=JAnz)
sd_A4_J = rep(NA, times=JAnz)
me_A4_J = rep(NA, times=JAnz)
sd_A5_J = rep(NA, times=JAnz)
me_A5_J = rep(NA, times=JAnz)
sd_A6_J = rep(NA, times=JAnz)
me_A6_J = rep(NA, times=JAnz)
sd_A7_J = rep(NA, times=JAnz)
me_A7_J = rep(NA, times=JAnz)



for (l in 1:JAnz) 
{
  sd_HR_J[l] =  round(sd(HR[JBeg[l]:(JBeg[l+1]-1)],    na.rm =T), 0)
  me_HR_J[l] =  round(mean(HR[JBeg[l]:(JBeg[l+1]-1)], na.rm =T), 0)
  sd_K_J[l] =  round(sd(K[JBeg[l]:(JBeg[l+1]-1)],    na.rm =T), 0)
  me_K_J[l] =  round(mean(K[JBeg[l]:(JBeg[l+1]-1)], na.rm =T), 0)
  sd_A1_J[l] =  round(sd(A1[JBeg[l]:(JBeg[l+1]-1)],    na.rm =T), 0)
  me_A1_J[l] =  round(mean(A1[JBeg[l]:(JBeg[l+1]-1)], na.rm =T), 0)
  sd_A2_J[l] =  round(sd(A2[JBeg[l]:(JBeg[l+1]-1)],    na.rm =T), 0)
  me_A2_J[l] =  round(mean(A2[JBeg[l]:(JBeg[l+1]-1)], na.rm =T), 0)
  sd_A3_J[l] =  round(sd(A3[JBeg[l]:(JBeg[l+1]-1)],    na.rm =T), 0)
  me_A3_J[l] =  round(mean(A3[JBeg[l]:(JBeg[l+1]-1)], na.rm =T), 0)
  sd_A4_J[l] =  round(sd(A4[JBeg[l]:(JBeg[l+1]-1)],    na.rm =T), 0)
  me_A4_J[l] =  round(mean(A4[JBeg[l]:(JBeg[l+1]-1)], na.rm =T), 0)
  sd_A5_J[l] =  round(sd(A5[JBeg[l]:(JBeg[l+1]-1)],    na.rm =T), 0)
  me_A5_J[l] =  round(mean(A5[JBeg[l]:(JBeg[l+1]-1)], na.rm =T), 0)
  sd_A6_J[l] =  round(sd(A6[JBeg[l]:(JBeg[l+1]-1)],    na.rm =T), 0)
  me_A6_J[l] =  round(mean(A6[JBeg[l]:(JBeg[l+1]-1)], na.rm =T), 0)
  sd_A7_J[l] =  round(sd(A7[JBeg[l]:(JBeg[l+1]-1)],    na.rm =T), 0)
  me_A7_J[l] =  round(mean(A7[JBeg[l]:(JBeg[l+1]-1)], na.rm =T), 0)
}



Descript.Stat = matrix(c(me_HR_J[1:JAnz],
                         me_K_J[1:JAnz],
                         me_A1_J[1:JAnz],
                         me_A2_J[1:JAnz],
                         me_A3_J[1:JAnz],
                         me_A4_J[1:JAnz],
                         me_A5_J[1:JAnz],
                         me_A6_J[1:JAnz],
                         me_A7_J[1:JAnz],
                         sd_HR_J[1:JAnz],
                         sd_K_J[1:JAnz],
                         sd_A1_J[1:JAnz],
                         sd_A2_J[1:JAnz],
                         sd_A3_J[1:JAnz],
                         sd_A4_J[1:JAnz],
                         sd_A5_J[1:JAnz],
                         sd_A6_J[1:JAnz],
                         sd_A7_J[1:JAnz]), nrow=JAnz, ncol=18)
colnames(Descript.Stat) <- c("me_HR_J", "me_K_J", "me_A1_J", "me_A2_J", "me_A3_J", "me_A4_J", "me_A5_J", "me_A6_J",
                             "me_A7_J", "sd_HR_J", "sd_K_J", "sd_A1_J", "sd_A2_J", "sd_A3_J", "sd_A4_J",
                             "sd_A5_J", "sd_A6_J", "sd_A7_J")
write.csv(Descript.Stat, "Descript.Stat.csv", row.names=FALSE, na="#NV")

S = sort(HRN)
quantil.lim.1.3 = S[round(length(S)/3,0)]
quantil.lim.2.3 = S[round(2*length(S)/3,0)]
