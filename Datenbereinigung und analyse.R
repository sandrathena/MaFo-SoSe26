#Bei Bedarf Pakete installieren

library(ggbreak)
library(WRS2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(car)
library(afex)
library(emmeans)
library(rstatix)
library(pwr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(mediation)



# am Ende entfernen
###############################################################################################################
#### Automatisches Speichern der Grafiken #####################################################################
###############################################################################################################

# Alle im Skript erzeugten Grafiken nummeriert als PNG speichern
grafik_ordner <- "Grafiken"
dir.create(grafik_ordner, showWarnings = FALSE, recursive = TRUE)

grafik_counter <- 0

speichere_grafik <- function(plot, name = "Grafik") {
  grafik_counter <<- grafik_counter + 1
  sicherer_name <- gsub(
    "[^A-Za-z0-9ÄÖÜäöüß_-]+", "_", name)
  dateiname <- file.path(
    grafik_ordner,
    sprintf(
      "%03d_%s.png",
      grafik_counter,
      sicherer_name))
  ggsave(
    filename = dateiname,
    plot = plot,
    width = 10,
    height = 7,
    dpi = 300)
  invisible(plot)}

# Für Grafiken, die im ursprünglichen Code direkt erzeugt und angezeigt werden,
# wird die Grafik gespeichert und anschließend wie bisher als Ergebnis zurückgegeben.
zeige_und_speichere_grafik <- function(plot, name = "Grafik") {
  speichere_grafik(plot, name)
  plot
}
###############################################################################################################



# Laden der Daten
salienz <- read.table("C:/Users/sandr/Documents/Uni_Winfo/MaFo/MaFo Final/Final/Salienz Endstand Raw Data.csv", header = FALSE, sep = ";")

# Es sind 255 obs., davon sind 3 Header
nrow(salienz)





###############################################################################################################
###############################################################################################################
#### Data Preperation & Cleaning ##############################################################################
###############################################################################################################
###############################################################################################################


#### Leere Zeilen löschen #####################################################################################

# Bei Bedarf: Weil die Datei in der Mail beim Öffnen manchmal (aber nicht immer?) in jeder zweiten Zeile leer ist, 
# löschen wir erstmal jede zweite (also leere) Zeile
# salienz <- salienz[-seq(2, nrow(salienz), by = 2), ]


#### Header festlegen #########################################################################################

# Extrahieren der zweiten Zeile als Header
new_header <- as.character(salienz[2, ])
print(new_header)

# Entfernen der ersten beiden Zeilen (alte Header und neue Headerzeile)
salienz <- salienz[-c(1, 2), ]

# Zuweisen der neuen Headerzeilen als Spaltennamen
colnames(salienz) <- new_header

# Entfernen erster Zeile
salienz <- salienz[-c(1), ]

# Zuweisung von unique col names
colnames(salienz) <- make.names(colnames(salienz), unique = TRUE)

# Es sind 252 Teilnehmer
nrow(salienz)


#### Testteilnehmer entfernen #################################################################################

# Entfernen jener 5 Einträge, die vor der finalen Umfrage als Test-Teilnehmer teilgenommen haben und die
# Matheaufgaben nicht beantwortet haben -> dort steht aber kein N/A sondern -77 wegen unipark
class(salienz$Plus.Rechnung.2)
salienz <- salienz %>%
  filter(Plus.Rechnung.2 != "-77")

# Es bleiben 247 Teilnehmer
nrow(salienz)




###############################################################################################################
###############################################################################################################
#### Stichproben Demografie ###################################################################################
###############################################################################################################
###############################################################################################################


#### Altersverteilung ###########################################################################################

# Alter in numerische Variable umwandeln
salienz <- salienz %>%
  mutate(Alter = as.numeric(Alter))

# Häufigkeiten pro Alter berechnen
alter_plot <- salienz %>%
  filter(!is.na(Alter)) %>%
  count(Alter)

# Balkendia
zeige_und_speichere_grafik(
  ggplot(alter_plot, aes(x = Alter, y = n)) +
    geom_col(fill = "#EED5B7", width = 0.8) +
    scale_x_continuous(
      breaks = c(seq(15, 100, by = 5), 990, 997)) +
    scale_x_break(c(100, 985)) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10),
      expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = "Altersverteilung der Stichprobe n = 247",
      x = "Alter in Jahren",
      y = "Anzahl der Teilnehmenden") +
    theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5)),
  "Altersverteilung der Stichprobe"
)


#### Geschlechtsverteilung ######################################################################################

# Häufigkeiten und Prozentwerte berechnen
geschlecht_plot <- salienz %>%
  filter(!is.na(Geschlecht)) %>%
  count(Geschlecht) %>%
  mutate(
    Prozent = n / sum(n) * 100,
    Geschlecht_Label = factor(
      as.character(Geschlecht),
      levels = as.character(1:4),
      labels = c(
        "weiblich",
        "männlich",
        "divers",
        "keine Angabe")))

# Balkendiagramm
zeige_und_speichere_grafik(
  ggplot(geschlecht_plot, aes(x = Geschlecht_Label, y = n)) +
    geom_col(fill = "#EED5B7", width = 0.6) +
    geom_text(
      aes(label = paste0(n, " (", round(Prozent, 1), " %)")),
      vjust = -0.5,
      size = 4) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10),
      expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Geschlechterverteilung der Stichprobe n = 247",
      x = "Geschlecht",
      y = "Anzahl der Teilnehmenden") +
    theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5)),
  "Geschlechterverteilung der Stichprobe"
)



#### Einkommensverteilung #######################################################################################

# Reihenfolge der Einkommenskategorien festlegen
einkommen_plot <- salienz %>%
  filter(!is.na(Einkommen)) %>%
  count(Einkommen) %>%
  mutate(
    Prozent = n / sum(n) * 100,
    Einkommen_Label = factor(
      as.character(Einkommen),
      levels = as.character(1:9),
      labels = c(
        "< 500 €",
        "500 - 999 €",
        "1000 - 1499 €",
        "1500 - 1999 €",
        "2000 - 2999 €",
        "3000 - 3999 €",
        "4000 - 4999 €",
        "> 5000 €",
        "keine Angabe")))

# Balkendiagramm
zeige_und_speichere_grafik(
  ggplot(einkommen_plot, aes(x = Einkommen_Label, y = n)) +
    geom_col(fill = "#EED5B7", width = 0.7) +
    geom_text(
      aes(label = paste0(n, " (", round(Prozent, 1), " %)")),
      vjust = -0.5,
      size = 3.5) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10),
      expand = expansion(
        mult = c(0, 0.15))) +
    labs(
      title = "Einkommensverteilung der Stichprobe n = 247",
      x = "Monatliches Netto-Haushaltseinkommen",
      y = "Anzahl der Teilnehmenden") +
    theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1)),
  "Einkommensverteilung der Stichprobe"
)



#### Tätigkeitsverteilung #####################################################################################

# Häufigkeiten und Prozentwerte berechnen
taetigkeit_plot <- salienz %>%
  filter(!is.na(Beruf)) %>%
  count(Beruf) %>%
  mutate(
    Prozent = n / sum(n) * 100,
    Beruf_Label = factor(
      as.character(Beruf),
      levels = as.character(1:3),
      labels = c(
        "Student/in",
        "Berufstätig",
        "Sonstiges")))

# Balkendiagramm
zeige_und_speichere_grafik(
  ggplot(taetigkeit_plot, aes(x = Beruf_Label, y = n)) +
    geom_col(fill = "#EED5B7", width = 0.6) +
    geom_text(
      aes(label = paste0(n, " (", round(Prozent, 1), " %)")),
      vjust = -0.5,
      size = 4) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10),
      expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Tätigkeitsverteilung der Stichprobe n = 247",
      x = "Aktuelle Haupttätigkeit",
      y = "Anzahl der Teilnehmenden") +
    theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5)),
  "Tätigkeitsverteilung der Stichprobe"
)




###############################################################################################################
###############################################################################################################
#### Data Preperation & Cleaning Contin. ######################################################################
###############################################################################################################
###############################################################################################################


#### Mobile Geräte ausschließen ##############################################################################

# Mit Hilfe von browser.id bestimmen, ob PC/Laptop oder mobiles Gerät genutzt wurde
salienz <- salienz %>%
  mutate( Geraetekategorie = case_when(
    # Mobiles Endgerät
    grepl(
      "Mobile|Android|iPhone|iPad|iPod|Windows Phone|Tablet",
      browser.id,
      ignore.case = TRUE) ~ "Mobilgerät",
    # PC/Laptop Betriebssysteme
    grepl(
      "Windows NT|Macintosh|X11|Linux|CrOS",
      browser.id,
      ignore.case = TRUE) ~ "PC / Laptop",
    # Extra-Kategorie falls ein browser.id nicht zugeordnet werden kann und dann manuell prüfen
    TRUE ~ "Unklar"))

# Die Geräte auswählen und Anteile berechnen
geraete_plot <- salienz %>%
  filter(Geraetekategorie %in% c("PC / Laptop", "Mobilgerät")) %>%
  count(Geraetekategorie) %>%
  mutate(Prozent = n / sum(n) * 100)

# Balkendiagramm für Verteilung der Engeräte erstellen
zeige_und_speichere_grafik(
  ggplot(
    geraete_plot,
    aes(x = Geraetekategorie, y = n, fill = Geraetekategorie)) +
    geom_col(width = 0.6) +
    geom_text(
      aes(label = paste0(n," (",round(Prozent, 1)," %)")), vjust = -0.5, size = 4) +
    scale_fill_manual(values = c(
      "PC / Laptop" = "#EED5B7",
      "Mobilgerät" = "#FFEFDB")) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10),
      expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "Verteilung der verwendeten Endgeräte",
      x = "Gerätekategorie",
      y = "Anzahl der Teilnehmenden") +
    theme_minimal() +
    theme(
      legend.position = "none", # Legende weg weil Balken schon beschriftet
      plot.title = element_text(hjust = 0.5)),
  "Verteilung der verwendeten Endgeräte"
)


# mobile Endgeräte aus dem Datensatz salienz entfernen
salienz <- salienz %>%
  filter(Geraetekategorie == "PC / Laptop")

# Es bleiben 151 Teilnehmer
nrow(salienz)


#### Gesamtbearbeitungszeit filtern ############################################################################

# Gesamtbearbeitungszeit von Sekunden in Minuten
salienz <- salienz %>%
  mutate(Bearbeitungszeit_Minuten = as.numeric(time.to.complete.survey) / 60)

# Balkendia der Gesamtbearbeitungszeiten erstellen
zeige_und_speichere_grafik(
  ggplot(salienz, aes(x = Bearbeitungszeit_Minuten)) +
    geom_histogram(binwidth = 2, fill = "#EED5B7", color = "white") +
    geom_vline(xintercept = median(salienz$Bearbeitungszeit_Minuten, na.rm = TRUE), linetype = "dashed", linewidth = 0.8) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10),expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = "Verteilung der Gesamtbearbeitungszeiten",
      subtitle = "Gestrichelte Linie = Median der Gesamtbearbeitungszeiten",
      x = "Gesamtbearbeitungszeit der Umfrage [in Minuten]",
      y = "Anzahl der Teilnehmenden [pro 2-Minuten-Intervall]") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(
        hjust = 0.5),
      plot.subtitle = element_text(
        hjust = 0.5)),
  "Verteilung der Gesamtbearbeitungszeiten"
)

# Berechnen der Bearbeitungszeiten der fünf Outfits, die relative timestamps sind Zeitstempel
# -> also muss immer der der vorhergehenden Seite abgezogen werden
salienz <- salienz %>%
  mutate(
    Zeit_7698435 = as.numeric(relative.timestamp.for.page.7698435) - as.numeric(relative.timestamp.for.page.7698434),
    Zeit_7698431 = as.numeric(relative.timestamp.for.page.7698431) - as.numeric(relative.timestamp.for.page.7698430),
    Zeit_7698433 = as.numeric(relative.timestamp.for.page.7698433) - as.numeric(relative.timestamp.for.page.7698432),
    Zeit_7698438 = as.numeric(relative.timestamp.for.page.7698438) - as.numeric(relative.timestamp.for.page.7698437),
    Zeit_7698427 = as.numeric(relative.timestamp.for.page.7698427) - as.numeric(relative.timestamp.for.page.7698426))

# Speichern der Namen der fünf Bearbeitungszeitvariablen
zeit_spalten <- c("Zeit_7698435", "Zeit_7698431", "Zeit_7698433", "Zeit_7698438", "Zeit_7698427")

# Bestimmen des Medians der Bearbeitungszeit für jedes Outfit
seiten_mediane <- sapply(
  salienz[, zeit_spalten],
  median,
  na.rm = TRUE)
print(seiten_mediane)

# Berechnen der relativen Speed Faktoren nach Leiner (2019) mit der Formel:
# Speed = Median der Seite / individuelle Bearbeitungszeit und auf maximal 3 festlegen
salienz <- salienz %>%
  mutate(
    Speed_7698435 = pmin(seiten_mediane["Zeit_7698435"] / Zeit_7698435, 3),
    Speed_7698431 = pmin(seiten_mediane["Zeit_7698431"] / Zeit_7698431, 3),
    Speed_7698433 = pmin(seiten_mediane["Zeit_7698433"] / Zeit_7698433, 3),
    Speed_7698438 = pmin(seiten_mediane["Zeit_7698438"] / Zeit_7698438, 3),
    Speed_7698427 = pmin(seiten_mediane["Zeit_7698427"] / Zeit_7698427,3))

# Bilden der durchschnittlichen Relative Completion Speed Indize
salienz <- salienz %>%
  mutate(
    Relative_Speed_Index = rowMeans(
      across(
        c(Speed_7698435, Speed_7698431, Speed_7698433, Speed_7698438, Speed_7698427)),
      na.rm = FALSE))

# Identifizieren von auffällig schnellen Teilnehmern
salienz <- salienz %>%
  mutate(Antwortzeit_auffaellig = Relative_Speed_Index > 2)

# Anzahl anzeigen
table(salienz$Antwortzeit_auffaellig)

# Betrachten der Verteilung der mit Relative Speed Index > 2.0
zeige_und_speichere_grafik(
  salienz %>%
    filter(Relative_Speed_Index > 2) %>%
    ggplot(aes(x = Relative_Speed_Index, y = reorder(as.factor(number), Relative_Speed_Index))) +
    geom_point(size = 3) +
    geom_vline(xintercept = 2, linetype = "dashed", linewidth = 0.8) +
    scale_x_continuous(
      breaks = seq(2, ceiling(max(salienz$Relative_Speed_Index, na.rm = TRUE) * 10) / 10, by = 0.1)) +
    labs(
      title = "Teilnehmende mit auffälliger Antwortgeschwindigkeit",
      subtitle = "Relative Completion Speed Index > 2.0 nach Leiner (2019)",
      x = "Relative Completion Speed Index",
      y = "Teilnehmer-ID") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)),
  "Teilnehmende mit auffälliger Antwortgeschwindigkeit"
)

# Betroffene Teilnehmer anzeigen
salienz %>%
  filter(Antwortzeit_auffaellig) %>%
  dplyr::select(
    number,
    Bearbeitungszeit_Minuten,
    Relative_Speed_Index,
    all_of(zeit_spalten)
  ) %>%
  arrange(desc(Relative_Speed_Index))

# Betroffene Teilnehmer in der Gesamtbearbeitungsverteilung farbig markieren
zeige_und_speichere_grafik(
  ggplot(salienz, aes(x = Bearbeitungszeit_Minuten, fill = Antwortzeit_auffaellig)) +
    geom_histogram(binwidth = 2, color = "white") +
    # Median der Gesamtbearbeitungszeit
    geom_vline(xintercept = median(salienz$Bearbeitungszeit_Minuten, na.rm = TRUE), linetype = "dashed", linewidth = 0.8) +
    # Farben für unauffällige und auffällige Personen
    scale_fill_manual(
      values = c("FALSE" = "#EED5B7", "TRUE" = "grey50"), 
      labels = c("FALSE" = "Nicht auffällig", "TRUE" = "Auffällig, da Relative Speed Index > 2.0")) +
    scale_x_continuous(
      breaks = scales::pretty_breaks(n = 15)) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10),
      expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = "Auffällige Gesamtbearbeitungszeiten",
      x = "Gesamtbearbeitungszeit der Umfrage [in Minuten]",
      y = "Anzahl der Teilnehmenden [pro 2-Minuten-Intervall]",
      fill = "Legende") +
    guides(
      fill = guide_legend(
        nrow = 1,
        byrow = TRUE)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)),
  "Auffällige Gesamtbearbeitungszeiten"
)

# Auffällige Teilnehmer aus dem Datensatz salienz entfernen
salienz <- salienz %>%
  filter(!Antwortzeit_auffaellig)

# Es bleiben 136 Teilnehmer
nrow(salienz)



#### Suspicion Probe scannen ##################################################################################

# Kommentare alle anzeigen und speichern um sie manuell zu prüfen
print(salienz$Suspicion.Probe)

suspicionprobe_antworten <- salienz %>%
  dplyr::select(
    number,
    Suspicion.Probe)
write.csv2(
  suspicionprobe_antworten,
  "SuspicionProbe_Antworten.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8")


#### Unnötge Spalten entfernen ################################################################################

# get rid of unnecessary columns von 2-20 und ab 186, weil ab Spalte http refer wurde auch
# beim Pretest alles entfernt und erst ab Spalte 21 sind wichtige Inhalte
# Die Spalten 237 bis 241 bleiben aber weil sie die Randomisierungsreihenfolge der fünf Outfits enthalten
salienz <- salienz %>%
  dplyr::select(
    -c(
      2:20,
      186:236,
      242:ncol(salienz)))


#### Spalten übergreifend umbenennen ##########################################################################

# Konsistenz
names(salienz)[grepl("uneinheitlich", names(salienz))] <- "Konsistenz1"
names(salienz)[grepl("unstimmig", names(salienz))] <- "Konsistenz2"
names(salienz)[grepl("unharmonisch", names(salienz))] <- "Konsistenz3"

# Interesse
names(salienz)[grepl("langweilig", names(salienz))] <- "Interesse1"
names(salienz)[grepl("uninteressant", names(salienz))] <- "Interesse2"

# Liking
names(salienz)[grepl("Das.Outfit.gefällt.mir.gut", names(salienz))] <- "Liking1"
names(salienz)[grepl("Ich.mag.das.Outfit", names(salienz))] <- "Liking2"
names(salienz)[grepl("Ich.finde.das.Outfit.ansprechend", names(salienz))] <- "Liking3"

# Fluency
names(salienz)[grepl("das.Outfit.zu.bewerten", names(salienz))] <- "Fluency1"
names(salienz)[grepl("das.Outfit.visuell.schnell.zu.erfassen", names(salienz))] <- "Fluency2"
names(salienz)[grepl("geschlossenen.Augen", names(salienz))] <- "Fluency3"
names(salienz)[grepl("einer.anderen.Person.zu.beschreiben", names(salienz))] <- "Fluency4"

# Kreativität
names(salienz)[grepl("wirkt.kreativ", names(salienz))] <- "Kreativitaet1"
names(salienz)[grepl("wirkt.originell", names(salienz))] <- "Kreativitaet2"
names(salienz)[grepl("wirkt.einfallsreich", names(salienz))] <- "Kreativitaet3"
names(salienz)[grepl("hebt.sich.von.modischen.Normen.ab", names(salienz))] <- "Kreativitaet4"
names(salienz)[grepl("ist.unkonventionell", names(salienz))] <- "Kreativitaet5"

# Authentizität
names(salienz)[grepl("Zusammenstellung.glaubwürdig", names(salienz))] <- "Authentizitaet1"
names(salienz)[grepl("nicht.künstlich.erzwungen", names(salienz))] <- "Authentizitaet2"
names(salienz)[grepl("eigenständigen.Charakter", names(salienz))] <- "Authentizitaet3"
names(salienz)[grepl("nicht.aufgesetzt", names(salienz))] <- "Authentizitaet4"

# Sophisticated Taste
names(salienz)[grepl("gekonnt.zusammengestellt", names(salienz))] <- "SophisticatedTaste1"
names(salienz)[grepl("stilvoll.kleidet", names(salienz))] <- "SophisticatedTaste2"
names(salienz)[grepl("kleidet.sich.bewusst", names(salienz))] <- "SophisticatedTaste3"
names(salienz)[grepl("raffinierten.Geschmack", names(salienz))] <- "SophisticatedTaste4"

# Manipulations-Stilcheck
names(salienz)[grepl("Minimalismus", names(salienz))] <- "Minimalismus"
names(salienz)[grepl("Hip.Hop", names(salienz))] <- "HipHopActiveWear"
names(salienz)[grepl("Hippie", names(salienz))] <- "HippieBoho"

# Matheaufgabe
mathe_spalten <- grep("Plus.Rechnung", names(salienz))
# Kontrolle dass fünf Blöcke mit jeweils zwei Aufgaben
length(mathe_spalten)
names(salienz)[mathe_spalten] <- rep(
  c("Matheaufgabe1", "Matheaufgabe2"),
  length.out = length(mathe_spalten))

# CVPA
names(salienz)[grepl("hervorragendes.Design", names(salienz))] <- "CVPA1"
names(salienz)[grepl("Auslagen.von.Produkten", names(salienz))] <- "CVPA2"
names(salienz)[grepl("Quelle.der.Freude", names(salienz))] <- "CVPA3"
names(salienz)[grepl("Welt.lebenswerter", names(salienz))] <- "CVPA4"
names(salienz)[grepl("feine.Unterschiede.im.Produktdesign", names(salienz))] <- "CVPA5"
names(salienz)[grepl("anderen.meist.entgehen", names(salienz))] <- "CVPA6"
names(salienz)[grepl("zu.anderen.Dingen.passt", names(salienz))] <- "CVPA7"
names(salienz)[grepl("besser.aussehen.lässt", names(salienz))] <- "CVPA8"
names(salienz)[grepl("in.seinen.Bann.zieht", names(salienz))] <- "CVPA9"

# Randomisierung
names(salienz)[167:171] <- c(
  "Randomisierung_1",
  "Randomisierung_2",
  "Randomisierung_3",
  "Randomisierung_4",
  "Randomisierung_5")
names(salienz)[167:171]

# Rename Columns by Block sodass man ablesen kann zu welchem Outifit es gehört
rename_columns_with_suffix <- function(df, start, end, suffix) {
  names(df)[start:end] <- paste0(names(df)[start:end], suffix)
  return(df)}

# Start at column 2 bis column 31 ist erstes Outfit
start <- 2

# Define block sizes (number of columns per block, wir haben 5 Blöcke mit je 30 Fragen)
block_sizes <- c(30, 30, 30, 30, 30)  
suffixes <- c("_Jacke-stark-dezentral",
              "_Jacke-leicht-dezentral", 
              "_Schuh-stark-dezentral", 
              "_Jacke-stark-zentral",
              "_Min-Baseline")

for (i in seq_along(suffixes)) {
  end <- start + block_sizes[i] - 1
  # Check if end exceeds total columns
  if (end > ncol(salienz)) end <- ncol(salienz)
  # Rename the columns
  salienz <- rename_columns_with_suffix(
    salienz,
    start,
    end,
    suffixes[i])
  # Next block starts after this one
  start <- end + 1
  if (start > ncol(salienz)) break}

# Die fünf experimentellen Bedingungen
conditions <- c(
  "Jacke-stark-dezentral",
  "Jacke-leicht-dezentral",
  "Schuh-stark-dezentral",
  "Jacke-stark-zentral",
  "Min-Baseline")

# Regulärer Ausdruck für die fünf Suffixe
condition_regex <- paste(conditions, collapse = "|")

# Alle Spalten ermitteln, die zu einem Outfitblock gehören
outfit_columns <- grep(
  paste0("_(", condition_regex, ")$"),
  names(salienz),
  value = TRUE)
print(outfit_columns)
length(outfit_columns)



#### Auffällige Antwortmuster ###################################################################################

# Jene speichern, die innerhalb einer Outfit-Frageseite straightlinen
straightlining_pro_outfit <- sapply(conditions, function(condition) {
  # Alle Spalten der jeweiligen Outfit-Frageseite
  aktuelle_spalten <- grep(
    paste0("_", condition, "$"),
    names(salienz),
    value = TRUE)
  # Ohne Matheaufgaben
  aktuelle_spalten <- aktuelle_spalten[!grepl("^Matheaufgabe", aktuelle_spalten)]
  # Für jeden Teilnehmer prüfen ob innerhalb dieser Outfit-Seite alle Antworten gleich sind
  apply(salienz[, aktuelle_spalten, drop = FALSE], 1, function(antworten) {all(length(unique(antworten)) == 1)})})

# Prüfen, ob ein Teilnehmer auf ALLEN fünf Outfit-Seiten gestraightlined hat
straightlining_alle_outfits <- apply(straightlining_pro_outfit, 1, all)

# Anzahl der betroffenen Teilnehmer anzeigen
table(straightlining_alle_outfits)

# Betroffene Teilnehmer speichern zum Angucken seiner Antworten
straightliner_teilnehmer <- salienz[straightlining_alle_outfits, , drop = FALSE]

# Teilnehmer-IDs separat speichern
straightliner_ids <- salienz$number[straightlining_alle_outfits]

# Betroffene Teilnehmer-IDs anzeigen
print(straightliner_ids)

# Anzahl der betroffenen Teilnehmer anzeigen
length(straightliner_ids)



#### Altersverteilung ###########################################################################################

# Alter in numerische Variable umwandeln
salienz <- salienz %>%
  mutate(Alter = as.numeric(Alter))

# Häufigkeiten pro Alter berechnen
alter_plot <- salienz %>%
  filter(!is.na(Alter)) %>%
  count(Alter)

# Balkendiagramm machen
zeige_und_speichere_grafik(
  ggplot(alter_plot, aes(x = Alter, y = n)) +
    geom_col(fill = "#EED5B7", width = 0.8) +
    scale_x_continuous(
      breaks = seq(
        floor(min(alter_plot$Alter)),
        ceiling(max(alter_plot$Alter)),
        by = 5)) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10),
      expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = "Altersverteilung der Stichprobe n = 136",
      x = "Alter [in Jahren]",
      y = "Anzahl der Teilnehmenden") +
    theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5)),
  "Altersverteilung der Stichprobe"
)



#### Geschlechtsverteilung ######################################################################################

# Häufigkeiten und Prozentwerte berechnen
geschlecht_plot <- salienz %>%
  filter(!is.na(Geschlecht)) %>%
  count(Geschlecht) %>%
  mutate(
    Prozent = n / sum(n) * 100,
    Geschlecht_Label = factor(
      as.character(Geschlecht),
      levels = as.character(1:4),
      labels = c(
        "weiblich",
        "männlich",
        "divers",
        "keine Angabe")))

# Balkendiagramm
zeige_und_speichere_grafik(
  ggplot(geschlecht_plot, aes(x = Geschlecht_Label, y = n)) +
    geom_col(fill = "#EED5B7", width = 0.6) +
    geom_text(
      aes(label = paste0(n, " (", round(Prozent, 1), " %)")),
      vjust = -0.5,
      size = 4) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10),
      expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Geschlechterverteilung der Stichprobe n = 136",
      x = "Geschlecht",
      y = "Anzahl der Teilnehmenden") +
    theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5)),
  "Geschlechterverteilung der Stichprobe"
)



#### Einkommensverteilung #######################################################################################

# Reihenfolge der Einkommenskategorien festlegen
einkommen_plot <- salienz %>%
  filter(!is.na(Einkommen)) %>%
  count(Einkommen) %>%
  mutate(
    Prozent = n / sum(n) * 100,
    Einkommen_Label = factor(
      as.character(Einkommen),
      levels = as.character(1:9),
      labels = c(
        "< 500 €",
        "500 - 999 €",
        "1000 - 1499 €",
        "1500 - 1999 €",
        "2000 - 2999 €",
        "3000 - 3999 €",
        "4000 - 4999 €",
        "> 5000 €",
        "keine Angabe")))

# Balkendiagramm
zeige_und_speichere_grafik(
  ggplot(einkommen_plot, aes(x = Einkommen_Label, y = n)) +
    geom_col(fill = "#EED5B7", width = 0.7) +
    geom_text(
      aes(label = paste0(n, " (", round(Prozent, 1), " %)")),
      vjust = -0.5,
      size = 3.5) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10),
      expand = expansion(
        mult = c(0, 0.15))) +
    labs(
      title = "Einkommensverteilung der Stichprobe n = 136",
      x = "Monatliches Netto-Haushaltseinkommen",
      y = "Anzahl der Teilnehmenden") +
    theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1)),
  "Einkommensverteilung der Stichprobe"
)



#### Tätigkeitsverteilung #####################################################################################

# Häufigkeiten und Prozentwerte berechnen
taetigkeit_plot <- salienz %>%
  filter(!is.na(Beruf)) %>%
  count(Beruf) %>%
  mutate(
    Prozent = n / sum(n) * 100,
    Beruf_Label = factor(
      as.character(Beruf),
      levels = as.character(1:3),
      labels = c(
        "Student/in",
        "Berufstätig",
        "Sonstiges")))

# Balkendiagramm
zeige_und_speichere_grafik(
  ggplot(taetigkeit_plot, aes(x = Beruf_Label, y = n)) +
    geom_col(fill = "#EED5B7", width = 0.6) +
    geom_text(
      aes(label = paste0(n, " (", round(Prozent, 1), " %)")),
      vjust = -0.5,
      size = 4) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10),
      expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Tätigkeitsverteilung der Stichprobe n = 136",
      x = "Aktuelle Haupttätigkeit",
      y = "Anzahl der Teilnehmenden") +
    theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5)),
  "Tätigkeitsverteilung der Stichprobe"
)



#### Jetzt die demografischen Werte vom Verdächtigen mit ID 32 von der Antwortmusteranalyse betrachten ##########
salienz %>%
  filter(as.character(number) == "32") %>%
  dplyr::select(
    number,
    Alter,
    Geschlecht,
    Einkommen,
    Beruf,
    Suspicion.Probe
  )
# Sein Alter und Suspicion Probe sind sehr suspicious. Grund genug, den Datensatz zu entfernen.



##### Teilnehmer mit ID 32 aus dem Datensatz entfernen ##########################################################
salienz <- salienz %>%
  filter(as.character(number) != "32")

# Es bleiben 135 Teilnehmer 
nrow(salienz)



#### Finale Daten reshapen ins Long-Format ######################################################################

# Eine Zeile pro Person und Outfit erzeugen
salienz_reshaped <- salienz %>%
  pivot_longer(
    cols = all_of(outfit_columns),
    # Der Teil vorm letzten Unterstrich wird zum Spaltenname
    # Der Teil danach wird zur Condition
    names_to = c(".value", "Condition"),
    names_pattern = paste0(
      "^(.*)_(",
      condition_regex,
      ")$"))

# Anzeigen der reshaped Data
print(salienz_reshaped)



#### Konsistenz zu Inkonsistenz umpolen ########################################################################

# Die ursprünglichen Konsistenz-Items sind so kodiert:
# 1 = hohe Inkonsistenz
# 5 = hohe Konsistenz
# Für die Forschungsfrage polen wir sie aber der Einfachheithalber um:
# 1 = hohe Konsistenz 
# 5 = hohe Inkonsistenz
# Bei unserer Skala von 1 bis 5 müssen wir also immer 6 - ursprünglicher Wert rechnen
salienz_reshaped <- salienz_reshaped %>%
  mutate(
    across(
      c(Konsistenz1, Konsistenz2, Konsistenz3),
      ~ 6 - as.numeric(.x)))

# Umbenennung der Variablen Konsistenz zu Inkonsistenz
salienz_reshaped <- salienz_reshaped %>%
  rename(
    Inkonsistenz1 = Konsistenz1,
    Inkonsistenz2 = Konsistenz2,
    Inkonsistenz3 = Konsistenz3)

# Zeilenanzahl prüfen, 135 x 5 = 675
nrow(salienz_reshaped)


###############################################################################################################
#### Cronbach Alpha ###########################################################################################
###############################################################################################################


# Prüfen ob die Variablen in einer Form sind mit der wir arbeiten können
summary(salienz_reshaped)


#### Umwandlung in numerisch notwendig! #######################################################################

# Inkonsistenz
salienz_reshaped <- salienz_reshaped %>% mutate(across(Inkonsistenz1:Inkonsistenz3, as.numeric))

# Interesse
salienz_reshaped <- salienz_reshaped %>% mutate(across(Interesse1:Interesse2, as.numeric))

# Liking
salienz_reshaped <- salienz_reshaped %>% mutate(across(Liking1:Liking3, as.numeric))

# Fluency
salienz_reshaped <- salienz_reshaped %>% mutate(across(Fluency1:Fluency4, as.numeric))

# Kreativität
salienz_reshaped <- salienz_reshaped %>% mutate(across(Kreativitaet1:Kreativitaet5, as.numeric))

# Authentizität
salienz_reshaped <- salienz_reshaped %>% mutate(across(Authentizitaet1:Authentizitaet4, as.numeric))

# Sophisticated Taste
salienz_reshaped <- salienz_reshaped %>% mutate(across(SophisticatedTaste1:SophisticatedTaste4, as.numeric))

# CVPA
salienz_reshaped <- salienz_reshaped %>% mutate(across(CVPA1:CVPA9, as.numeric))


#### Berechnung von Cronbach Alpha und Interpretation nach Faustregel nach (Blanz, 2015): ####################
#### Wir ziehlen auf einen Wert von min. 0.8 #################################################################


#### EInmal über alle Outfits hinweg #########################################################################

# Inkonsistenz
inkonsistenz_items <- salienz_reshaped %>%
  dplyr::select(Inkonsistenz1, Inkonsistenz2, Inkonsistenz3)
# Berechnung von Cronbach's Alpha
cronbach_alpha_inkonsistenz <- psych::alpha(inkonsistenz_items)
# Ergebnis anzeigen
print(cronbach_alpha_inkonsistenz)
# 0.90 (raw_alpha) 
# Sehr gute Konsistenz der Skala. Aber potenziell redundante Items. Wir können die Skala so verwenden. 

# Interesse
interesse_items <- salienz_reshaped %>%
  dplyr::select(Interesse1, Interesse2)
# Berechnung von Cronbach's Alpha
cronbach_alpha_interesse <- psych::alpha(interesse_items)
# Ergebnis anzeigen
print(cronbach_alpha_interesse)
# 0.87 (raw_alpha) 
# Gute Konsistenz der Skala.Wir können die Skala so verwenden. 

# Liking
liking_items <- salienz_reshaped %>%
  dplyr::select(Liking1, Liking2, Liking3)
# Berechnung von Cronbach's Alpha
cronbach_alpha_liking <- psych::alpha(liking_items)
# Ergebnis anzeigen
print(cronbach_alpha_liking)
# 0.95 (raw_alpha) 
# Sehr gute Konsistenz der Skala. Aber potenziell redundante Items. Wir können die Skala so verwenden. 

# Fluency
fluency_items <- salienz_reshaped %>%
  dplyr::select(Fluency1, Fluency2, Fluency3, Fluency4)
# Berechnung von Cronbach's Alpha
cronbach_alpha_fluency <- psych::alpha(fluency_items)
# Ergebnis anzeigen
print(cronbach_alpha_fluency)
# 0.85 (raw_alpha) 
# Gute Konsistenz der Skala. Wir können die Skala so verwenden. 

# Kreativitaet
creativity_items <- salienz_reshaped %>%
  dplyr::select(Kreativitaet1, Kreativitaet2, Kreativitaet3, Kreativitaet4, Kreativitaet5)
# Berechnung von Cronbach's Alpha
cronbach_alpha_creativity <- psych::alpha(creativity_items)
# Ergebnis anzeigen
print(cronbach_alpha_creativity)
# 0.87 (raw_alpha) 
# Gute Konsistenz der Skala.Wir können die Skala so verwenden. 

# Authentizität
authentizitaet_items <- salienz_reshaped %>%
  dplyr::select(Authentizitaet1, Authentizitaet2, Authentizitaet3, Authentizitaet4)
# Berechnung von Cronbach's Alpha
cronbach_alpha_authentizitaet<- psych::alpha(authentizitaet_items)
# Ergebnis anzeigen
print(cronbach_alpha_authentizitaet)
# 0.84 (raw_alpha) 
# Gute Konsistenz der Skala.Wir können die Skala so verwenden. 

# SophisticatedTaste
sophisticated_items <- salienz_reshaped %>%
  dplyr::select(SophisticatedTaste1, SophisticatedTaste2, SophisticatedTaste3, SophisticatedTaste4)
# Berechnung von Cronbach's Alpha
cronbach_alpha_sophisticated <- psych::alpha(sophisticated_items)
# Ergebnis anzeigen
print(cronbach_alpha_sophisticated)
# 0.86 (raw_alpha) 
# Gute Konsistenz der Skala. Wir können die Skala so verwenden.

# CVPA
cvpa_items <- salienz_reshaped %>%
  dplyr::select(CVPA1, CVPA2,CVPA3,CVPA4,CVPA5,CVPA6,CVPA7,CVPA8,CVPA9)
# Berechnung von Cronbach's Alpha
cronbach_alpha_cvpa <- psych::alpha(cvpa_items)
# Ergebnis anzeigen
print(cronbach_alpha_cvpa)
# 0.84 (raw_alpha) 
# Gute Konsistenz der Skala. Wir können die Skala so verwenden.



#### Und eInmal für jedes Outfit getrennt mit for ##################################################################

# Konstrukte definieren
konstrukte <- list(
  Inkonsistenz = c("Inkonsistenz1", "Inkonsistenz2", "Inkonsistenz3"),
  Interesse = c("Interesse1", "Interesse2"),
  Liking = c("Liking1", "Liking2", "Liking3"),
  Fluency = c("Fluency1", "Fluency2", "Fluency3", "Fluency4"),
  Kreativitaet = c("Kreativitaet1", "Kreativitaet2", "Kreativitaet3", "Kreativitaet4", "Kreativitaet5"),
  Authentizitaet = c("Authentizitaet1", "Authentizitaet2", "Authentizitaet3", "Authentizitaet4"),
  SophisticatedTaste = c("SophisticatedTaste1", "SophisticatedTaste2", "SophisticatedTaste3", "SophisticatedTaste4"))

# Leeren Ergebnis-Datensatz erstellen
cronbach_pro_outfit <- data.frame()

# Cronbach Alpha für jedes Outfit und jedes Konstrukt berechnen mit loop
for (outfit in conditions) {
  for (konstrukt in names(konstrukte)) {
    daten <- salienz_reshaped %>%
      filter(
        Condition == outfit
      ) %>%
      dplyr::select(
        all_of(konstrukte[[konstrukt]]))
    # Cronbach Alpha berechnen und speichern
    alpha_result <- psych::alpha(daten, warnings = FALSE)
    cronbach_pro_outfit <- rbind(
      cronbach_pro_outfit,
      data.frame(
        Outfit = outfit,
        Konstrukt = konstrukt,
        Cronbach_Alpha = alpha_result$total$raw_alpha))}}

# Cronbach Alpha auf drei Nachkommastellen runden
cronbach_pro_outfit$Cronbach_Alpha <- round(cronbach_pro_outfit$Cronbach_Alpha, 3)

# Interpretation ergänzen nach Blanz (2015) damit bisschen übersichtlicher
cronbach_pro_outfit <- cronbach_pro_outfit %>%
  mutate(
    Interpretation = case_when(
      Cronbach_Alpha > 0.9 ~ "Sehr gut",
      Cronbach_Alpha > 0.8 ~ "Gut",
      Cronbach_Alpha > 0.7 ~ "Akzeptabel",
      Cronbach_Alpha > 0.6 ~ "Fragwürdig",
      TRUE ~ "Nicht ausreichend"))
print(cronbach_pro_outfit)


# Alle Skalen sind konsistent wir können Mittelwerte bilden !



#### Mittelwerte bilden ####################################################################################

# Bilde den Mittelwert zwischen den Konsistenz Spalten
salienz_reshaped$Mittelwert_Inkonsistenz <- rowMeans(salienz_reshaped[, c("Inkonsistenz1", "Inkonsistenz2", "Inkonsistenz3")], na.rm = TRUE)

# Bilde den Mittelwert zwischen den Interesse Spalten
salienz_reshaped$Mittelwert_Interesse <- rowMeans(salienz_reshaped[, c("Interesse1", "Interesse2")], na.rm = TRUE)

# Bilde den Mittelwert zwischen den Liking Spalten
salienz_reshaped$Mittelwert_Liking <- rowMeans(salienz_reshaped[, c("Liking1", "Liking2", "Liking3")], na.rm = TRUE)

# Bilde den Mittelwert zwischen den Fluency Spalten
salienz_reshaped$Mittelwert_Fluency<- rowMeans(salienz_reshaped[, c("Fluency1", "Fluency2", "Fluency3", "Fluency4")], na.rm = TRUE)

# Bilde den Mittelwert zwischen den Kreativitaet Spalten
salienz_reshaped$Mittelwert_Kreativitaet<- rowMeans(salienz_reshaped[, c("Kreativitaet1", "Kreativitaet2", "Kreativitaet3", "Kreativitaet4", "Kreativitaet5")], na.rm = TRUE)

# Bilde den Mittelwert zwischen den Authentizität Spalten
salienz_reshaped$Mittelwert_Authentizitaet<- rowMeans(salienz_reshaped[, c("Authentizitaet1", "Authentizitaet2", "Authentizitaet3", "Authentizitaet4")], na.rm = TRUE)

# Bilde den Mittelwert zwischen den SophisticatedTaste Spalten
salienz_reshaped$Mittelwert_SophisticatedTaste<- rowMeans(salienz_reshaped[, c("SophisticatedTaste1", "SophisticatedTaste2", "SophisticatedTaste3", "SophisticatedTaste4")], na.rm = TRUE)



###############################################################################################################
#### Boxplots: Bewertung der fünf Outfits explorativ vergleichen ##############################################
###############################################################################################################


# Mittelwerte wieder ins Long-Format bringen um alle Konstrukte zusammen zu plotten
bewertungen_long <- salienz_reshaped %>%
  pivot_longer(
    cols = c(
      Mittelwert_Inkonsistenz,
      Mittelwert_Interesse,
      Mittelwert_Liking,
      Mittelwert_Fluency,
      Mittelwert_Kreativitaet,
      Mittelwert_Authentizitaet,
      Mittelwert_SophisticatedTaste
    ), names_to = "Variable", values_to = "Wert")

bewertungen_long <- bewertungen_long %>%
  mutate(
    Variable = dplyr::recode(
      Variable,
      "Mittelwert_Inkonsistenz" = "Inkonsistenz",
      "Mittelwert_Interesse" = "Interesse",
      "Mittelwert_Liking" = "Liking",
      "Mittelwert_Fluency" = "Fluency",
      "Mittelwert_Kreativitaet" = "Kreativität",
      "Mittelwert_Authentizitaet" = "Authentizität",
      "Mittelwert_SophisticatedTaste" = "Sophisticated Taste"))

# Festlegen der Reihenfolge der Outfits
bewertungen_long$Condition <- factor(
  bewertungen_long$Condition,
  levels = c(
    "Min-Baseline",
    "Jacke-leicht-dezentral",
    "Jacke-stark-dezentral",
    "Jacke-stark-zentral",
    "Schuh-stark-dezentral"))

# Alle Boxplots in eine Abbildung
boxplots_outfits <- ggplot(
  bewertungen_long,
  aes(x = Condition, y = Wert, fill = Condition)) +
  geom_boxplot() +
  facet_wrap(~ Variable, ncol = 2) +
  scale_fill_manual(
    values = c(
      "Min-Baseline" = "#EED5B7",
      "Jacke-leicht-dezentral" = "#CDAA7D",
      "Jacke-stark-dezentral" = "#EED5B7",
      "Jacke-stark-zentral" = "#CDAA7D",
      "Schuh-stark-dezentral" = "#EED5B7")) +
  scale_y_continuous(breaks = 1:5, limits = c(1, 5)) +
  labs(
    title = "Bewertung der fünf Outfits",
    subtitle = "Verteilung der Skalenmittelwerte nach Outfit",
    x = "Outfit",
    y = "Bewertung") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(
      hjust = 0.5),
    plot.subtitle = element_text(
      hjust = 0.5),
    axis.text.x = element_text(
      angle = 45,
      hjust = 0.5))

# Boxplots anzeigen
print(boxplots_outfits)

# Boxplots als PNG speichern
ggsave(filename = "Boxplots_Bewertung_aller_Outfits.png", plot = boxplots_outfits, width = 14, height = 12, dpi = 300)






###############################################################################################################
#### Datenset schreiben #######################################################################################
###############################################################################################################


write.csv(
  salienz_reshaped,
  "pretest_salienz-reshaped.csv",
  row.names = FALSE
)







# am Ende entfernen
###############################################################################################################
#### p-Werte in Berechnungsreihenfolge speichern ##############################################################
###############################################################################################################

# Die Tabelle wird während der Analyse fortlaufend befüllt.
# Dadurch entspricht die Reihenfolge der Zeilen exakt der Reihenfolge,
# in der die jeweiligen Tests im Code ausgeführt werden.
p_werte_tabelle <- data.frame(
  Reihenfolge = integer(),
  Objekt = character(),
  Bestandteil = character(),
  Zeile = character(),
  p_Typ = character(),
  p_Wert = numeric(),
  stringsAsFactors = FALSE
)

p_wert_zaehler <- 0L

# Leere Hilfstabelle
leere_p_wert_tabelle <- function() {
  data.frame(
    Objekt = character(),
    Bestandteil = character(),
    Zeile = character(),
    p_Typ = character(),
    p_Wert = numeric(),
    stringsAsFactors = FALSE
  )
}

# p-Werte aus einem Testergebnis extrahieren
extrahiere_p_werte <- function(objekt, objektname, bestandteil = "", tiefe = 0) {
  if (tiefe > 6) {
    return(leere_p_wert_tabelle())
  }
  
  # Klassische Tests aus stats, z. B. Shapiro-Wilk, t-Test, Wilcoxon oder Friedman
  if (inherits(objekt, "htest")) {
    return(
      data.frame(
        Objekt = objektname,
        Bestandteil = ifelse(bestandteil == "", objekt$method, bestandteil),
        Zeile = "",
        p_Typ = "p.value",
        p_Wert = as.numeric(objekt$p.value),
        stringsAsFactors = FALSE
      )
    )
  }
  
  # afex-ANOVA
  if (inherits(objekt, "afex_aov")) {
    return(
      extrahiere_p_werte(
        as.data.frame(objekt$anova_table),
        objektname,
        ifelse(bestandteil == "", "anova_table", paste0(bestandteil, "$anova_table")),
        tiefe + 1
      )
    )
  }
  
  # Tabellen und Matrizen, z. B. Levene-, Post-hoc- oder Mauchly-Tabellen
  if (is.data.frame(objekt) || is.matrix(objekt)) {
    daten <- as.data.frame(objekt)
    
    p_spalten <- names(daten)[
      grepl(
        "(^p($|[._ -])|[._ -]p($|[._ -])|Pr\\(|p-value)",
        names(daten),
        ignore.case = TRUE
      )
    ]
    
    if (length(p_spalten) == 0 || nrow(daten) == 0) {
      return(leere_p_wert_tabelle())
    }
    
    beschriftungs_spalten <- intersect(
      c(
        "Effect", "effect", "group1", "group2", ".y.",
        "Outfit", "Konstrukt", "Condition", "Reihenfolge", "Test"
      ),
      names(daten)
    )
    
    ergebnis <- list()
    k <- 1L
    
    # Zeilenweise durchgehen, damit auch innerhalb einer Ergebnistabelle
    # die sichtbare Reihenfolge erhalten bleibt.
    for (i in seq_len(nrow(daten))) {
      zeilen_name <- rownames(daten)[i]
      
      if (length(beschriftungs_spalten) > 0) {
        zeilen_name <- paste(
          as.character(daten[i, beschriftungs_spalten, drop = TRUE]),
          collapse = " | "
        )
      }
      
      for (p_spalte in p_spalten) {
        p_wert <- suppressWarnings(
          as.numeric(as.character(daten[[p_spalte]][i]))
        )
        
        if (!is.na(p_wert)) {
          ergebnis[[k]] <- data.frame(
            Objekt = objektname,
            Bestandteil = bestandteil,
            Zeile = zeilen_name,
            p_Typ = p_spalte,
            p_Wert = p_wert,
            stringsAsFactors = FALSE
          )
          k <- k + 1L
        }
      }
    }
    
    if (length(ergebnis) == 0) {
      return(leere_p_wert_tabelle())
    }
    
    return(bind_rows(ergebnis))
  }
  
  # Zusammenfassungsobjekte, insbesondere für Sphärizitäts-/Mauchly-Ausgaben
  if (is.list(objekt) && grepl("summary|Anova", paste(class(objekt), collapse = " "), ignore.case = TRUE)) {
    teile <- names(objekt)
    if (is.null(teile)) {
      teile <- as.character(seq_along(objekt))
    }
    
    return(
      bind_rows(
        lapply(
          seq_along(objekt),
          function(i) {
            neuer_bestandteil <- if (bestandteil == "") {
              teile[i]
            } else {
              paste0(bestandteil, "$", teile[i])
            }
            
            extrahiere_p_werte(
              objekt[[i]],
              objektname,
              neuer_bestandteil,
              tiefe + 1
            )
          }
        )
      )
    )
  }
  
  leere_p_wert_tabelle()
}

# Ein Testergebnis sofort in die Gesamttabelle schreiben.
# Weil diese Funktion direkt nach dem jeweiligen Test aufgerufen wird,
# bleibt die Berechnungsreihenfolge erhalten.
speichere_p_werte <- function(objekt, objektname, bestandteil = "") {
  neue_p_werte <- extrahiere_p_werte(
    objekt,
    objektname,
    bestandteil
  )
  
  if (nrow(neue_p_werte) > 0) {
    neue_p_werte$Reihenfolge <- seq.int(
      p_wert_zaehler + 1L,
      p_wert_zaehler + nrow(neue_p_werte)
    )
    
    p_wert_zaehler <<- p_wert_zaehler + nrow(neue_p_werte)
    
    neue_p_werte <- neue_p_werte %>%
      dplyr::select(
        Reihenfolge,
        Objekt,
        Bestandteil,
        Zeile,
        p_Typ,
        p_Wert
      )
    
    p_werte_tabelle <<- bind_rows(
      p_werte_tabelle,
      neue_p_werte
    )
  }
  
  invisible(objekt)
}

# Einzelnen p-Wert speichern, z. B. einen erst nachträglich Holm-korrigierten p-Wert.
speichere_p_wert_einzeln <- function(objektname, bestandteil, zeile, p_typ, p_wert) {
  if (!is.na(p_wert)) {
    p_wert_zaehler <<- p_wert_zaehler + 1L
    
    p_werte_tabelle <<- bind_rows(
      p_werte_tabelle,
      data.frame(
        Reihenfolge = p_wert_zaehler,
        Objekt = objektname,
        Bestandteil = bestandteil,
        Zeile = zeile,
        p_Typ = p_typ,
        p_Wert = as.numeric(p_wert),
        stringsAsFactors = FALSE
      )
    )
  }
}







###############################################################################################################
###############################################################################################################
#### Data Analysis ############################################################################################
###############################################################################################################
###############################################################################################################


###############################################################################################################
#### Statistische Tests für Inkonsistenz ######################################################################
###############################################################################################################

#### Hilfsfunktionen ###########################################################################################

# Daten für Conditions zum Plotten vorbereiten
prepare_plot_data <- function(conditions, levels, labels) {
  
  salienz_reshaped %>%
    filter(Condition %in% conditions) %>%
    drop_na(Mittelwert_Inkonsistenz) %>%
    mutate(
      Condition = factor(
        Condition,
        levels = levels,
        labels = labels
      )
    )
}


# Daten für zwei Conditions für gepaarte Tests vorbereiten
prepare_paired_data <- function(condition1, condition2) {
  
  salienz_reshaped %>%
    filter(Condition %in% c(condition1, condition2)) %>%
    dplyr::select(number, Condition, Mittelwert_Inkonsistenz) %>%
    pivot_wider(
      names_from = Condition,
      values_from = Mittelwert_Inkonsistenz
    ) %>%
    drop_na(all_of(c(condition1, condition2)))
}


# Box und Density Plots erstellen
plot_box_density <- function(data, farben, subtitle) {
  
  # Boxplot
  boxplot <- ggplot(
    data,
    aes(
      x = Condition,
      y = Mittelwert_Inkonsistenz,
      fill = Condition
    )
  ) +
    geom_boxplot() +
    scale_fill_manual(
      values = rep(
        c("#EED5B7", "#CDAA7D"),
        length.out = length(unique(data$Condition))
      )
    ) +
    labs(
      title = "Boxplot der Inkonsistenzbewertung",
      subtitle = subtitle,
      x = "Condition",
      y = "Wahrgenommene stilistische Inkonsistenz"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  speichere_grafik(
    boxplot,
    paste0("Boxplot_Inkonsistenz_", subtitle)
  )
  print(boxplot)
  
  # Density Plot
  density_plot <- ggplot(
    data,
    aes(
      x = Mittelwert_Inkonsistenz,
      color = Condition
    )
  ) +
    geom_density(linewidth = 1) +
    scale_color_manual(values = farben) +
    labs(
      title = "Density Plot der Inkonsistenzbewertung",
      subtitle = subtitle,
      x = "Wahrgenommene stilistische Inkonsistenz",
      y = "Dichte",
      color = "Condition"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  speichere_grafik(
    density_plot,
    paste0("Density_Inkonsistenz_", subtitle)
  )
  print(density_plot)
}


# Deskriptive Kennwerte erstellen
deskriptive_kennwerte <- function(data) {
  
  data %>%
    group_by(Condition) %>%
    summarise(
      n = n(),
      Median = median(Mittelwert_Inkonsistenz, na.rm = TRUE),
      IQR = IQR(Mittelwert_Inkonsistenz, na.rm = TRUE),
      Mittelwert = mean(Mittelwert_Inkonsistenz, na.rm = TRUE),
      SD = sd(Mittelwert_Inkonsistenz, na.rm = TRUE)
    )
}


# Normalverteilung graphisch prüfen
plot_differenz <- function(data, subtitle) {
  
  # Density Plot
  density_plot <- ggplot(
    data,
    aes(x = Differenz_Inkonsistenz)
  ) +
    geom_density(
      fill = "#EED5B7",
      color = "#8B6F47",
      alpha = 0.4,
      linewidth = 1
    ) +
    labs(
      title = "Density Plot der Differenzwerte",
      subtitle = subtitle,
      x = "Differenz der Inkonsistenzbewertung",
      y = "Dichte"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  speichere_grafik(
    density_plot,
    paste0("Density_Differenzwerte_", subtitle)
  )
  print(density_plot)
  
  # QQ-Plot
  qq_plot <- ggplot(
    data,
    aes(sample = Differenz_Inkonsistenz)
  ) +
    stat_qq() +
    stat_qq_line() +
    labs(
      title = "QQ-Plot der Differenzwerte",
      subtitle = subtitle,
      x = "Theoretische Quantile",
      y = "Beobachtete Quantile"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  speichere_grafik(
    qq_plot,
    paste0("QQ_Differenzwerte_", subtitle)
  )
  print(qq_plot)
  
  invisible(list(
    Density = density_plot,
    QQ = qq_plot
  ))
}


###############################################################################################################
#### Modell Inkonsistenz: Jacke-stark-dezentral vs Schuh-stark-dezentral ######################################
# DV: Stilistsiche Inkonsistenz ###############################################################################
# IV: Conditions/ Outfits (Groß vs klein -> Kategorisch 2 Ausprägungen) #######################################
# Within-Subjects daher dependent sample ######################################################################
# -> Dependent/Gepaarter-t-Test ###############################################################################
###############################################################################################################

# Daten für die beiden Conditions auswählen
plot_groesse <- prepare_plot_data(
  conditions = c(
    "Jacke-stark-dezentral",
    "Schuh-stark-dezentral"
  ),
  levels = c(
    "Schuh-stark-dezentral",
    "Jacke-stark-dezentral"
  ),
  labels = c(
    "Schuh stark dezentral",
    "Jacke stark dezentral"
  )
)

# Box und Density erstellen
plot_box_density(
  plot_groesse,
  farben = c(
    "Schuh stark dezentral" = "#EED5B7",
    "Jacke stark dezentral" = "#CDAA7D"
  ),
  subtitle = "Großes vs. kleines stilistisch abweichendes Produkt"
)

# Deskriptive Kennwerte
deskriptiv_groesse <- deskriptive_kennwerte(plot_groesse)
deskriptiv_groesse

# Daten für die beiden Conditions auswählen
inkonsistenz_groesse <- prepare_paired_data(
  "Jacke-stark-dezentral",
  "Schuh-stark-dezentral"
)

# Differenzwert bilden:
# positive Werte = Jacke wurde inkonsistenter bewertet als Schuh
inkonsistenz_groesse <- inkonsistenz_groesse %>%
  mutate(
    Differenz_Inkonsistenz =
      `Jacke-stark-dezentral` -
      `Schuh-stark-dezentral`
  )

# Anzahl vollständiger Paare kontrollieren
nrow(inkonsistenz_groesse)

# Normalverteilung graphisch prüfen
plot_differenz(
  inkonsistenz_groesse,
  "Jacke stark dezentral − Schuh stark dezentral"
)

# Normalverteilung mit Shapiro-Wilk prüfen
# H0: Die Differenzwerte sind normalverteilt
# H1: Die Differenzwerte sind nicht normalverteilt
shapiro_auto_01 <- shapiro.test(
  inkonsistenz_groesse$Differenz_Inkonsistenz
)
speichere_p_werte(shapiro_auto_01, "shapiro_auto_01")
shapiro_auto_01
# p > 0.05 -> H0 nicht verwerfen also keine statistisch signifikante Abweichung von der Normalverteilung

# Normalverteilungsannahme ist erfüllt, gepaarter t-Test ist okkk!
t_test_groesse <- t.test(
  inkonsistenz_groesse$`Jacke-stark-dezentral`,
  inkonsistenz_groesse$`Schuh-stark-dezentral`,
  paired = TRUE,
  alternative = "greater"
)
speichere_p_werte(t_test_groesse, "t_test_groesse")
t_test_groesse

# p < 0.05 -> H0 verwerfen:
# physisch groß wird statistsich signifikant inkonsistenter wahrgenommen



###############################################################################################################
#### Modell Inkonsistenz: Jacke-stark-zentral vs Jacke-stark-dezentral ########################################
# DV: Stilistsiche Inkonsistenz ###############################################################################
# IV: Conditions/ Outfits (Zentral vs dezentral -> Kategorisch 2 Ausprägungen) ################################
# Within-Subjects daher dependent sample ######################################################################
# -> Dependent-t-Test ##########################################################################################
###############################################################################################################

# Daten für die beiden Conditions auswählen
plot_position <- prepare_plot_data(
  conditions = c(
    "Jacke-stark-zentral",
    "Jacke-stark-dezentral"
  ),
  levels = c(
    "Jacke-stark-dezentral",
    "Jacke-stark-zentral"
  ),
  labels = c(
    "Jacke stark dezentral",
    "Jacke stark zentral"
  )
)

inkonsistenz_position <- prepare_paired_data(
  "Jacke-stark-zentral",
  "Jacke-stark-dezentral"
)

# Box und Density Plots erstellen
plot_box_density(
  plot_position,
  farben = c(
    "Jacke stark dezentral" = "#EED5B7",
    "Jacke stark zentral" = "#CDAA7D"
  ),
  subtitle = "Zentrale vs. dezentrale Positionierung"
)

# Deskriptive Kennwerte erstellen
deskriptiv_position <- deskriptive_kennwerte(plot_position)
deskriptiv_position

# Differenzwert bilden:
# positive Werte = zentrale Jacke wurde inkonsistenter bewertet
# als dezentrale Jacke
inkonsistenz_position <- inkonsistenz_position %>%
  mutate(
    Differenz_Inkonsistenz =
      `Jacke-stark-zentral` -
      `Jacke-stark-dezentral`
  )

# Anzahl vollständiger Paare kontrollieren
nrow(inkonsistenz_position)

# Normalverteilung graphisch prüfen
plot_differenz(
  inkonsistenz_position,
  "Jacke stark zentral − Jacke stark dezentral"
)

# Normalverteilung mit Shapiro-Wilk prüfen
# H0: Die Differenzwerte sind normalverteilt
# H1: Die Differenzwerte sind nicht normalverteilt
shapiro_auto_02 <- shapiro.test(
  inkonsistenz_position$Differenz_Inkonsistenz
)
speichere_p_werte(shapiro_auto_02, "shapiro_auto_02")
shapiro_auto_02
# p < 0.05 -> H0 verwerfen also kann man nicht sagen,
# dass keine statistisch signifikante Abweichung von der Normalverteilung vorliegt

# -> Wilcoxon-Vorzeichen-Rang-Test durchgeführt
wilcoxon_position <- wilcox.test(
  inkonsistenz_position$`Jacke-stark-zentral`,
  inkonsistenz_position$`Jacke-stark-dezentral`,
  paired = TRUE,
  alternative = "greater",
  exact = FALSE
)
speichere_p_werte(wilcoxon_position, "wilcoxon_position")
wilcoxon_position



###############################################################################################################
#### Modell Inkonsistenz: Jacke-stark-dezentral vs Jacke-schwach-dezentral ####################################
# DV: Stilistsiche Inkonsistenz ###############################################################################
# IV: Conditions/ Outfits (Stark vs schwach -> Kategorisch 2 Ausprägungen) ####################################
# Within-Subjects daher dependent sample ######################################################################
# -> Dependent-t-Test ##########################################################################################
###############################################################################################################

# Daten für die beiden Conditions auswählen
plot_staerke <- prepare_plot_data(
  conditions = c(
    "Jacke-stark-dezentral",
    "Jacke-leicht-dezentral"
  ),
  levels = c(
    "Jacke-leicht-dezentral",
    "Jacke-stark-dezentral"
  ),
  labels = c(
    "Jacke leicht dezentral",
    "Jacke stark dezentral"
  )
)

inkonsistenz_staerke <- prepare_paired_data(
  "Jacke-stark-dezentral",
  "Jacke-leicht-dezentral"
)

# Box und Density Plots erstellen
plot_box_density(
  plot_staerke,
  farben = c(
    "Jacke leicht dezentral" = "#EED5B7",
    "Jacke stark dezentral" = "#CDAA7D"
  ),
  subtitle = "Leichte vs. starke stilistische Abweichung"
)

# Deskriptive Kennwerte erstellen
deskriptiv_staerke_zwei <- deskriptive_kennwerte(plot_staerke)
deskriptiv_staerke_zwei

# Differenzwert bilden:
# positive Werte = stark abweichende Jacke wurde inkonsistenter bewertet
# als leicht abweichende Jacke
inkonsistenz_staerke <- inkonsistenz_staerke %>%
  mutate(
    Differenz_Inkonsistenz =
      `Jacke-stark-dezentral` -
      `Jacke-leicht-dezentral`
  )

# Anzahl vollständiger Paare kontrollieren
nrow(inkonsistenz_staerke)

# Normalverteilung graphisch prüfen
plot_differenz(
  inkonsistenz_staerke,
  "Jacke stark dezentral − Jacke leicht dezentral"
)

# Normalverteilung mit Shapiro-Wilk prüfen
# H0: Die Differenzwerte sind normalverteilt
# H1: Die Differenzwerte sind nicht normalverteilt
shapiro_auto_03 <- shapiro.test(
  inkonsistenz_staerke$Differenz_Inkonsistenz
)
speichere_p_werte(shapiro_auto_03, "shapiro_auto_03")
shapiro_auto_03
# p > 0.05 -> H0 nicht verwerfen also keine statistisch signifikante Abweichung von der Normalverteilung

# Normalverteilungsannahme ist erfüllt, gepaarter t-Test ok!
t_test_staerke <- t.test(
  inkonsistenz_staerke$`Jacke-stark-dezentral`,
  inkonsistenz_staerke$`Jacke-leicht-dezentral`,
  paired = TRUE,
  alternative = "greater"
)
speichere_p_werte(t_test_staerke, "t_test_staerke")
t_test_staerke



###############################################################################################################
#### Zusätzliches Modell Inkonsistenz: Min Jacke vs schwach Jacke vs stark Jacke ##############################
# DV: Stilistsiche Inkonsistenz ###############################################################################
# IV: Conditions/ Outfits (Stark vs schwach vs gar nicht -> Kategorisch 3 Ausprägungen) #######################
# Within-Subjects daher dependent sample ######################################################################
# -> One-way repeated measures ANOVA ##########################################################################
###############################################################################################################

# die drei Conditions auswählen
inkonsistenz_anova_staerke <- prepare_plot_data(
  conditions = c(
    "Min-Baseline",
    "Jacke-leicht-dezentral",
    "Jacke-stark-dezentral"
  ),
  levels = c(
    "Min-Baseline",
    "Jacke-leicht-dezentral",
    "Jacke-stark-dezentral"
  ),
  labels = c(
    "Jacke minimalistisch dezentral",
    "Jacke leicht dezentral",
    "Jacke stark dezentral"
  )
)

# Box und Density Plots erstellen
plot_box_density(
  inkonsistenz_anova_staerke,
  farben = c(
    "Jacke minimalistisch dezentral" = "#FFF5E6",
    "Jacke leicht dezentral" = "#EED5B7",
    "Jacke stark dezentral" = "#CDAA7D"
  ),
  subtitle = "Baseline vs. leichte vs. starke stilistische Abweichung"
)

# Deskriptive Kennwerte erstellen
deskriptiv_staerke <- deskriptive_kennwerte(
  inkonsistenz_anova_staerke
)
deskriptiv_staerke

# Repeated-Measures-ANOVA zunächst schätzen,
# damit anschließend die Residuen geprüft werden können
anova_staerke <- aov_ez(
  id = "number",
  dv = "Mittelwert_Inkonsistenz",
  within = "Condition",
  data = inkonsistenz_anova_staerke
)
speichere_p_werte(anova_staerke, "anova_staerke")

# Residuen extrahieren
residuen_staerke <- residuals(anova_staerke$lm)

# Normalverteilung graphisch prüfen: Density Plot
zeige_und_speichere_grafik(
  ggplot(
    data.frame(Residuen = as.numeric(residuen_staerke)),
    aes(x = Residuen)
  ) +
    geom_density(
      fill = "#EED5B7",
      color = "#8B6F47",
      alpha = 0.4,
      linewidth = 1
    ) +
    labs(
      title = "Density Plot der Residuen",
      subtitle = "Repeated-Measures-ANOVA: Stärke",
      x = "Residuen",
      y = "Dichte"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ),
  "Density Plot der Residuen - Stärke"
)

# Normalverteilung mit Shapiro-Wilk prüfen
shapiro_auto_04 <- shapiro.test(residuen_staerke)
speichere_p_werte(shapiro_auto_04, "shapiro_auto_04")
shapiro_auto_04

# Normalverteilung graphisch prüfen: QQ-Plot
zeige_und_speichere_grafik(
  ggplot(
    data.frame(Residuen = as.numeric(residuen_staerke)),
    aes(sample = Residuen)
  ) +
    stat_qq() +
    stat_qq_line() +
    labs(
      title = "QQ-Plot der Residuen",
      subtitle = "Repeated-Measures-ANOVA: Stärke",
      x = "Theoretische Quantile",
      y = "Beobachtete Quantile"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ),
  "QQ-Plot der Residuen - Stärke"
)

# p < 0.05 -> H0 verwerfen -> Friedman-Test

# Friedman-Test
friedman_staerke <- friedman.test(
  Mittelwert_Inkonsistenz ~ Condition | number,
  data = inkonsistenz_anova_staerke
)
speichere_p_werte(friedman_staerke, "friedman_staerke")
friedman_staerke

# H0: Inkonsistenz unterscheidet sich zwischen den dreien nicht
# p < 0.05 H0 verwerfen

# Um festzustellen, zwischen welchen Bedingungen Unterschiede bestehen,
# werden paarweise Wilcoxon-Vorzeichen-Rang-Tests durchgeführt mit Holm Korrektur
paarweise_staerke <- inkonsistenz_anova_staerke %>%
  pairwise_wilcox_test(
    Mittelwert_Inkonsistenz ~ Condition,
    paired = TRUE,
    p.adjust.method = "holm"
  )
paarweise_staerke
speichere_p_werte(paarweise_staerke, "paarweise_staerke")

# keine Abweichung -> leichte Abweichung -> starke Abweichung
# führt zu einer zunehmend höheren wahrgenommenen stilistischen Inkonsistenz.



###############################################################################################################
#### Zusätzliches Modell Inkonsistenz: Min Schuh vs stark Hiphop Schuh ########################################
# DV: Stilistsiche Inkonsistenz ###############################################################################
# IV: Conditions/ Outfits (Stark vs gar nicht -> Kategorisch 2 Ausprägungen) ##################################
# Within-Subjects daher dependent sample ######################################################################
# -> Dependent-t-Test ##########################################################################################
###############################################################################################################

# Daten für die beiden Conditions auswählen
plot_schuh <- prepare_plot_data(
  conditions = c(
    "Min-Baseline",
    "Schuh-stark-dezentral"
  ),
  levels = c(
    "Min-Baseline",
    "Schuh-stark-dezentral"
  ),
  labels = c(
    "Minimalistischer Schuh",
    "Starker Hip-Hop-Schuh"
  )
)

inkonsistenz_schuh <- prepare_paired_data(
  "Min-Baseline",
  "Schuh-stark-dezentral"
)

# Box und Density Plots erstellen
plot_box_density(
  plot_schuh,
  farben = c(
    "Minimalistischer Schuh" = "#EED5B7",
    "Starker Hip-Hop-Schuh" = "#CDAA7D"
  ),
  subtitle = "Minimalistische Baseline vs. stark abweichender Hip-Hop-Schuh"
)

# Deskriptive Kennwerte erstellen
deskriptiv_schuh <- deskriptive_kennwerte(plot_schuh)
deskriptiv_schuh

# Differenzwert bilden:
# positive Werte = stark abweichender Hip-Hop-Schuh wurde
# inkonsistenter bewertet als die minimalistische Baseline
inkonsistenz_schuh <- inkonsistenz_schuh %>%
  mutate(
    Differenz_Inkonsistenz =
      `Schuh-stark-dezentral` -
      `Min-Baseline`
  )

# Anzahl vollständiger Paare kontrollieren
nrow(inkonsistenz_schuh)

# Normalverteilung graphisch prüfen
plot_differenz(
  inkonsistenz_schuh,
  "Schuh stark dezentral − Min-Baseline"
)

# Normalverteilung mit Shapiro-Wilk prüfen
# H0: Die Differenzwerte sind normalverteilt
# H1: Die Differenzwerte sind nicht normalverteilt
shapiro_schuh <- shapiro.test(
  inkonsistenz_schuh$Differenz_Inkonsistenz
)
speichere_p_werte(shapiro_schuh, "shapiro_schuh")
shapiro_schuh

# p < 0.05 -> H0 verwerfen:
# statistisch signifikante Abweichung von der Normalverteilung

# -> Wilcoxon-Vorzeichen-Rang-Test
wilcoxon_schuh <- wilcox.test(
  inkonsistenz_schuh$`Schuh-stark-dezentral`,
  inkonsistenz_schuh$`Min-Baseline`,
  paired = TRUE,
  alternative = "greater",
  exact = FALSE
)
speichere_p_werte(wilcoxon_schuh, "wilcoxon_schuh")
wilcoxon_schuh
# p < 0.05
# Damit wird H₀ verworfen. Der stark stilistisch abweichende Hip-Hop-Schuh wird
# signifikant inkonsistenter wahrgenommen als der minimalistische Baseline-Schuh



###############################################################################################################
#### Power-Analyse / Fehler 1. und 2. Grades ##################################################################
###############################################################################################################

alpha <- 0.05

#### Hilfsfunktion 1: Gepaarter t-Test
power_t_paired <- function(differenz, modell, alpha = 0.05) {
  differenz <- na.omit(differenz)
  # Cohen's dz
  d <- mean(differenz) / sd(differenz)
  # Power
  power <- pwr.t.test(
    n = length(differenz),
    d = d,
    sig.level = alpha,
    type = "paired",
    alternative = "greater"
  )$power
  data.frame(
    Modell = modell,
    Test = "Gepaarter t-Test",
    n = length(differenz),
    Cohen_dz = d,
    Alpha = alpha,
    Power = power,
    Beta = 1 - power)}

#### Hilfsfunktion 2: Wilcoxon
power_wilcoxon <- function(x, y, modell,
                           alpha = 0.05,
                           B = 5000,
                           seed = 123) {
  # Nur vollständige Paare
  komplett <- complete.cases(x, y)
  x <- x[komplett]
  y <- y[komplett]
  n <- length(x)
  set.seed(seed)
  p_werte <- replicate(
    B,
    {index <- sample(
      seq_len(n),
      size = n,
      replace = TRUE)
    wilcox.test(
      x[index],
      y[index],
      paired = TRUE,
      alternative = "greater",
      exact = FALSE
    )$p.value})
  power <- mean(p_werte < alpha, na.rm = TRUE)
  data.frame(
    Modell = modell,
    Test = "Wilcoxon",
    n = n,
    Cohen_dz = NA,
    Alpha = alpha,
    Power = power,
    Beta = 1 - power)}

#### Hilfsfunktion 3: Friedman
power_friedman_test <- function(matrix,
                                modell,
                                alpha = 0.05,
                                B = 1000,
                                seed = 123) {
  matrix <- matrix[complete.cases(matrix), ]
  n <- nrow(matrix)
  set.seed(seed)
  p_werte <- replicate(
    B,
    {
      index <- sample(
        seq_len(n),
        size = n,
        replace = TRUE )
      friedman.test(
        matrix[index, , drop = FALSE]
      )$p.value } )
  power <- mean(p_werte < alpha)
  data.frame(
    Modell = modell,
    Test = "Friedman",
    n = n,
    Cohen_dz = NA,
    Alpha = alpha,
    Power = power,
    Beta = 1 - power)}

#### Fall 1: Größe ############################################################################################
power_groesse <- power_t_paired(
  inkonsistenz_groesse$Differenz_Inkonsistenz,
  "Größe: Jacke vs. Schuh")

#### Fall 2: Position #########################################################################################
power_position <- power_wilcoxon(
  inkonsistenz_position$`Jacke-stark-zentral`,
  inkonsistenz_position$`Jacke-stark-dezentral`,
  "Position: zentral vs. dezentral")

#### Fall 3: Stärke ###########################################################################################
power_staerke <- power_t_paired(
  inkonsistenz_staerke$Differenz_Inkonsistenz,
  "Stärke: stark vs. leicht")

#### Fall 4: Baseline vs. leicht vs. stark ####################################################################
friedman_matrix <- inkonsistenz_anova_staerke %>%
  dplyr::select(
    number,
    Condition,
    Mittelwert_Inkonsistenz
  ) %>%
  pivot_wider(
    names_from = Condition,
    values_from = Mittelwert_Inkonsistenz
  ) %>%
  dplyr::select(
    `Jacke minimalistisch dezentral`,
    `Jacke leicht dezentral`,
    `Jacke stark dezentral`
  ) %>%
  drop_na() %>%
  as.matrix()
power_friedman <- power_friedman_test(
  friedman_matrix,
  "Stärke: Baseline vs. leicht vs. stark")

#### Fall 5: Baseline vs. starker Hip-Hop-Schuh ###############################################################
power_schuh <- power_wilcoxon(
  inkonsistenz_schuh$`Schuh-stark-dezentral`,
  inkonsistenz_schuh$`Min-Baseline`,
  "Schuh: Baseline vs. Hip-Hop-Schuh")

#### Übersicht #################################################################################################
power_uebersicht <- bind_rows(
  power_groesse,
  power_position,
  power_staerke,
  power_friedman,
  power_schuh
) %>%
  mutate(
    Alpha_Prozent = round(Alpha * 100, 2),
    Power_Prozent = round(Power * 100, 2),
    Beta_Prozent = round(Beta * 100, 2),
    Cohen_dz = round(Cohen_dz, 3))
power_uebersicht



#############################################################################################################
#### Unterschiedliche Gruppen: Geschlechtsunterschiede #######################################################
#############################################################################################################

# Die wiederkehrenden Arbeitsschritte werden in Hilfsfunktionen zusammengefasst.
# Methodik, Filterung, Faktorstufen, Tests, gespeicherte p-Werte und Grafiken bleiben unverändert.

#### Hilfsfunktionen #########################################################################################

bereite_geschlechtsdaten_vor <- function(conditions, faktor, zuordnung, stufen) {
  salienz_reshaped %>%
    filter(Condition %in% conditions) %>%
    mutate(
      Geschlecht = case_when(
        as.character(Geschlecht) %in% c("1", "weiblich") ~ "weiblich",
        as.character(Geschlecht) %in% c("2", "männlich") ~ "männlich",
        TRUE ~ NA_character_
      ),
      !!rlang::sym(faktor) := unname(zuordnung[as.character(Condition)])
    ) %>%
    filter(!is.na(Geschlecht), !is.na(Mittelwert_Inkonsistenz)) %>%
    mutate(
      Geschlecht = factor(Geschlecht, levels = c("weiblich", "männlich")),
      !!rlang::sym(faktor) := factor(.data[[faktor]], levels = stufen)
    ) %>%
    group_by(number) %>%
    filter(n_distinct(.data[[faktor]]) == length(stufen)) %>%
    ungroup()
}

erstelle_boxplot_geschlecht <- function(data, faktor, stufe, subtitle,
                                        manuelle_farben = FALSE,
                                        titel_zentriert = FALSE) {
  p <- data %>%
    filter(.data[[faktor]] == stufe) %>%
    ggplot(aes(
      x = Geschlecht,
      y = Mittelwert_Inkonsistenz,
      fill = Geschlecht
    )) +
    geom_boxplot() +
    scale_fill_manual(
      values = c(
        "weiblich" = "#EED5B7",
        "männlich" = "#CDAA7D"
      )
    )
  p <- p +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5)) +
    labs(
      title = "Inkonsistenzbewertung nach Geschlecht",
      subtitle = subtitle,
      x = "Geschlecht",
      y = "Wahrgenommene stilistische Inkonsistenz"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  if (titel_zentriert) {
    p <- p + theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  }
  
  p
}

deskriptiv_geschlecht <- function(data, faktor) {
  data %>%
    group_by(Geschlecht, !!rlang::sym(faktor)) %>%
    summarise(
      n = n(),
      Median = median(Mittelwert_Inkonsistenz, na.rm = TRUE),
      IQR = IQR(Mittelwert_Inkonsistenz, na.rm = TRUE),
      Mittelwert = mean(Mittelwert_Inkonsistenz, na.rm = TRUE),
      SD = sd(Mittelwert_Inkonsistenz, na.rm = TRUE),
      .groups = "drop"
    )
}

schaetze_mixed_anova <- function(data, faktor, name) {
  modell <- aov_ez(
    id = "number",
    dv = "Mittelwert_Inkonsistenz",
    within = faktor,
    between = "Geschlecht",
    data = data
  )
  speichere_p_werte(modell, name)
  modell
}

density_residuen <- function(residuen, subtitle, farbig = TRUE, zentriert = TRUE) {
  p <- ggplot(
    data.frame(Residuen = as.numeric(residuen)),
    aes(x = Residuen)
  )
  
  if (farbig) {
    p <- p + geom_density(
      fill = "#EED5B7",
      color = "#8B6F47",
      alpha = 0.4,
      linewidth = 1
    )
  } else {
    p <- p + geom_density(
      fill = "#EED5B7",
      color = "#8B6F47",
      alpha = 0.4,
      linewidth = 1
    )
  }
  
  p <- p +
    labs(
      title = "Density Plot der Residuen",
      subtitle = subtitle,
      x = "Residuen",
      y = "Dichte"
    ) +
    theme_minimal()
  
  if (zentriert) {
    p <- p + theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  }
  
  p
}

qq_residuen <- function(residuen, subtitle,
                        titel = "QQ-Plot der Residuen",
                        zentriert = TRUE) {
  p <- ggplot(
    data.frame(Residuen = as.numeric(residuen)),
    aes(sample = Residuen)
  ) +
    stat_qq() +
    stat_qq_line() +
    labs(
      title = titel,
      subtitle = subtitle,
      x = "Theoretische Quantile",
      y = "Beobachtete Quantile"
    ) +
    theme_minimal()
  
  if (zentriert) {
    p <- p + theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  }
  
  p
}

shapiro_residuen <- function(residuen, name) {
  test <- shapiro.test(as.numeric(residuen))
  speichere_p_werte(test, name)
  test
}

levene_stufe <- function(data, faktor, stufe, name) {
  test <- leveneTest(
    Mittelwert_Inkonsistenz ~ Geschlecht,
    data = data %>% filter(.data[[faktor]] == stufe)
  )
  speichere_p_werte(test, name)
  test
}


#############################################################################################################
#### 1. Position × Geschlecht ################################################################################
#############################################################################################################

inkonsistenz_position_geschlecht <- bereite_geschlechtsdaten_vor(
  conditions = c("Jacke-stark-dezentral", "Jacke-stark-zentral"),
  faktor = "Position",
  zuordnung = c(
    "Jacke-stark-dezentral" = "dezentral",
    "Jacke-stark-zentral" = "zentral"
  ),
  stufen = c("dezentral", "zentral")
)

# Kontrolle
table(inkonsistenz_position_geschlecht$number)
inkonsistenz_position_geschlecht %>%
  distinct(number, Geschlecht) %>%
  count(Geschlecht)

# Boxplots
boxplot_position_dezentral <- erstelle_boxplot_geschlecht(
  inkonsistenz_position_geschlecht, "Position", "dezentral",
  "Jacke stark dezentral",
  manuelle_farben = TRUE,
  titel_zentriert = TRUE
)
speichere_grafik(boxplot_position_dezentral, "Inkonsistenzbewertung nach Geschlecht")
print(boxplot_position_dezentral)

boxplot_position_zentral <- erstelle_boxplot_geschlecht(
  inkonsistenz_position_geschlecht, "Position", "zentral",
  "Jacke stark zentral",
  manuelle_farben = TRUE,
  titel_zentriert = TRUE
)
speichere_grafik(boxplot_position_zentral, "Inkonsistenzbewertung nach Geschlecht")
print(boxplot_position_zentral)

# Deskriptive Kennwerte
deskriptiv_position_geschlecht <- deskriptiv_geschlecht(
  inkonsistenz_position_geschlecht, "Position"
)
deskriptiv_position_geschlecht

# Mixed ANOVA zunächst schätzen, um Residuen zu prüfen
anova_position_geschlecht <- schaetze_mixed_anova(
  inkonsistenz_position_geschlecht,
  "Position",
  "anova_position_geschlecht"
)

residuen_position_geschlecht <- residuals(anova_position_geschlecht$lm)

zeige_und_speichere_grafik(
  density_residuen(
    residuen_position_geschlecht,
    "Factorial Mixed ANOVA: Position × Geschlecht",
    farbig = TRUE,
    zentriert = TRUE
  ),
  "Density Plot der Residuen"
)

shapiro_auto_05 <- shapiro_residuen(
  residuen_position_geschlecht,
  "shapiro_auto_05"
)
shapiro_auto_05

zeige_und_speichere_grafik(
  qq_residuen(
    residuen_position_geschlecht,
    "Factorial Mixed ANOVA: Position × Geschlecht",
    titel = "Normalverteilung der Residuen",
    zentriert = TRUE
  ),
  "Normalverteilung der Residuen"
)

# Varianzhomogenität
levene_auto_01 <- levene_stufe(
  inkonsistenz_position_geschlecht, "Position", "dezentral", "levene_auto_01"
)
levene_auto_01

levene_auto_02 <- levene_stufe(
  inkonsistenz_position_geschlecht, "Position", "zentral", "levene_auto_02"
)
levene_auto_02

# Mixed ANOVA berechnen
anova_position_geschlecht <- schaetze_mixed_anova(
  inkonsistenz_position_geschlecht,
  "Position",
  "anova_position_geschlecht"
)
anova_position_geschlecht


#############################################################################################################
#### 2. Größe × Geschlecht ###################################################################################
#############################################################################################################

inkonsistenz_groesse_geschlecht <- bereite_geschlechtsdaten_vor(
  conditions = c("Schuh-stark-dezentral", "Jacke-stark-dezentral"),
  faktor = "Groesse",
  zuordnung = c(
    "Schuh-stark-dezentral" = "klein",
    "Jacke-stark-dezentral" = "gross"
  ),
  stufen = c("klein", "gross")
)

# Kontrolle
table(inkonsistenz_groesse_geschlecht$number)
inkonsistenz_groesse_geschlecht %>%
  distinct(number, Geschlecht) %>%
  count(Geschlecht)

# Boxplots
boxplot_groesse_klein <- erstelle_boxplot_geschlecht(
  inkonsistenz_groesse_geschlecht, "Groesse", "klein",
  "Kleines stilistisch abweichendes Produkt: Schuh stark dezentral"
)
speichere_grafik(boxplot_groesse_klein, "Inkonsistenzbewertung nach Geschlecht")
print(boxplot_groesse_klein)

boxplot_groesse_gross <- erstelle_boxplot_geschlecht(
  inkonsistenz_groesse_geschlecht, "Groesse", "gross",
  "Großes stilistisch abweichendes Produkt: Jacke stark dezentral"
)
speichere_grafik(boxplot_groesse_gross, "Inkonsistenzbewertung nach Geschlecht")
print(boxplot_groesse_gross)

# Deskriptive Kennwerte
deskriptiv_groesse_geschlecht <- deskriptiv_geschlecht(
  inkonsistenz_groesse_geschlecht, "Groesse"
)
deskriptiv_groesse_geschlecht

# Mixed ANOVA zunächst schätzen
anova_groesse_geschlecht <- schaetze_mixed_anova(
  inkonsistenz_groesse_geschlecht,
  "Groesse",
  "anova_groesse_geschlecht"
)

residuen_groesse_geschlecht <- residuals(anova_groesse_geschlecht$lm)

zeige_und_speichere_grafik(
  density_residuen(
    residuen_groesse_geschlecht,
    "Factorial Mixed ANOVA: Größe × Geschlecht"
  ),
  "Density Plot der Residuen"
)

shapiro_auto_06 <- shapiro_residuen(
  residuen_groesse_geschlecht,
  "shapiro_auto_06"
)
shapiro_auto_06

zeige_und_speichere_grafik(
  qq_residuen(
    residuen_groesse_geschlecht,
    "Factorial Mixed ANOVA: Größe × Geschlecht"
  ),
  "QQ-Plot der Residuen"
)

# Varianzhomogenität
levene_auto_03 <- levene_stufe(
  inkonsistenz_groesse_geschlecht, "Groesse", "klein", "levene_auto_03"
)
levene_auto_03

levene_auto_04 <- levene_stufe(
  inkonsistenz_groesse_geschlecht, "Groesse", "gross", "levene_auto_04"
)
levene_auto_04

# Mixed ANOVA berechnen
anova_groesse_geschlecht <- schaetze_mixed_anova(
  inkonsistenz_groesse_geschlecht,
  "Groesse",
  "anova_groesse_geschlecht"
)
anova_groesse_geschlecht


#############################################################################################################
#### 3. Stärke × Geschlecht ##################################################################################
#############################################################################################################

inkonsistenz_staerke_geschlecht <- bereite_geschlechtsdaten_vor(
  conditions = c("Jacke-leicht-dezentral", "Jacke-stark-dezentral"),
  faktor = "Staerke",
  zuordnung = c(
    "Jacke-leicht-dezentral" = "leicht",
    "Jacke-stark-dezentral" = "stark"
  ),
  stufen = c("leicht", "stark")
)

# Kontrolle
table(inkonsistenz_staerke_geschlecht$number)
inkonsistenz_staerke_geschlecht %>%
  distinct(number, Geschlecht) %>%
  count(Geschlecht)

# Boxplots
boxplot_staerke_leicht <- erstelle_boxplot_geschlecht(
  inkonsistenz_staerke_geschlecht, "Staerke", "leicht",
  "Leichte stilistische Abweichung: Jacke leicht dezentral"
)
speichere_grafik(boxplot_staerke_leicht, "Inkonsistenzbewertung nach Geschlecht")
print(boxplot_staerke_leicht)

boxplot_staerke_stark <- erstelle_boxplot_geschlecht(
  inkonsistenz_staerke_geschlecht, "Staerke", "stark",
  "Starke stilistische Abweichung: Jacke stark dezentral"
)
speichere_grafik(boxplot_staerke_stark, "Inkonsistenzbewertung nach Geschlecht")
print(boxplot_staerke_stark)

# Deskriptive Kennwerte
deskriptiv_staerke_geschlecht <- deskriptiv_geschlecht(
  inkonsistenz_staerke_geschlecht, "Staerke"
)
deskriptiv_staerke_geschlecht

# Mixed ANOVA zunächst schätzen
anova_staerke_geschlecht <- schaetze_mixed_anova(
  inkonsistenz_staerke_geschlecht,
  "Staerke",
  "anova_staerke_geschlecht"
)

residuen_staerke_geschlecht <- residuals(anova_staerke_geschlecht$lm)

zeige_und_speichere_grafik(
  density_residuen(
    residuen_staerke_geschlecht,
    "Factorial Mixed ANOVA: Stärke × Geschlecht",
    farbig = TRUE,
    zentriert = TRUE
  ),
  "Density Plot der Residuen"
)

shapiro_auto_07 <- shapiro_residuen(
  residuen_staerke_geschlecht,
  "shapiro_auto_07"
)
shapiro_auto_07

zeige_und_speichere_grafik(
  qq_residuen(
    residuen_staerke_geschlecht,
    "Factorial Mixed ANOVA: Stärke × Geschlecht"
  ),
  "QQ-Plot der Residuen"
)

# Varianzhomogenität
levene_auto_05 <- levene_stufe(
  inkonsistenz_staerke_geschlecht, "Staerke", "leicht", "levene_auto_05"
)
levene_auto_05

levene_auto_06 <- levene_stufe(
  inkonsistenz_staerke_geschlecht, "Staerke", "stark", "levene_auto_06"
)
levene_auto_06

# Mixed ANOVA berechnen
anova_staerke_geschlecht <- schaetze_mixed_anova(
  inkonsistenz_staerke_geschlecht,
  "Staerke",
  "anova_staerke_geschlecht"
)
anova_staerke_geschlecht


#############################################################################################################
#### 4. Schuh-Stärke × Geschlecht ############################################################################
#############################################################################################################

inkonsistenz_staerke_schuh_geschlecht <- bereite_geschlechtsdaten_vor(
  conditions = c("Min-Baseline", "Schuh-stark-dezentral"),
  faktor = "Staerke",
  zuordnung = c(
    "Min-Baseline" = "baseline",
    "Schuh-stark-dezentral" = "stark"
  ),
  stufen = c("baseline", "stark")
)

# Kontrolle
table(inkonsistenz_staerke_schuh_geschlecht$number)
inkonsistenz_staerke_schuh_geschlecht %>%
  distinct(number, Geschlecht) %>%
  count(Geschlecht)

# Boxplots
boxplot_staerke_schuh_baseline <- erstelle_boxplot_geschlecht(
  inkonsistenz_staerke_schuh_geschlecht, "Staerke", "baseline",
  "Minimalismus-Baseline"
)
speichere_grafik(boxplot_staerke_schuh_baseline, "Inkonsistenzbewertung nach Geschlecht")
print(boxplot_staerke_schuh_baseline)

boxplot_staerke_schuh_stark <- erstelle_boxplot_geschlecht(
  inkonsistenz_staerke_schuh_geschlecht, "Staerke", "stark",
  "Starker stilistisch abweichender Schuh"
)
speichere_grafik(boxplot_staerke_schuh_stark, "Inkonsistenzbewertung nach Geschlecht")
print(boxplot_staerke_schuh_stark)

# Deskriptive Kennwerte
deskriptiv_staerke_schuh_geschlecht <- deskriptiv_geschlecht(
  inkonsistenz_staerke_schuh_geschlecht, "Staerke"
)
deskriptiv_staerke_schuh_geschlecht

# Klassische Mixed ANOVA zunächst nur zur Annahmenprüfung schätzen
anova_staerke_schuh_geschlecht <- schaetze_mixed_anova(
  inkonsistenz_staerke_schuh_geschlecht,
  "Staerke",
  "anova_staerke_schuh_geschlecht"
)

residuen_staerke_schuh_geschlecht <- residuals(
  anova_staerke_schuh_geschlecht$lm
)

zeige_und_speichere_grafik(
  density_residuen(
    residuen_staerke_schuh_geschlecht,
    "Factorial Mixed ANOVA: Schuh-Stärke × Geschlecht"
  ),
  "Density Plot der Residuen"
)

shapiro_auto_08 <- shapiro_residuen(
  residuen_staerke_schuh_geschlecht,
  "shapiro_auto_08"
)
shapiro_auto_08

zeige_und_speichere_grafik(
  qq_residuen(
    residuen_staerke_schuh_geschlecht,
    "Factorial Mixed ANOVA: Schuh-Stärke × Geschlecht"
  ),
  "QQ-Plot der Residuen"
)

# Varianzhomogenität
levene_auto_07 <- levene_stufe(
  inkonsistenz_staerke_schuh_geschlecht, "Staerke", "baseline", "levene_auto_07"
)
levene_auto_07

levene_auto_08 <- levene_stufe(
  inkonsistenz_staerke_schuh_geschlecht, "Staerke", "stark", "levene_auto_08"
)
levene_auto_08

# Robuste deskriptive Kennwerte: bwtrim() verwendet 20 % getrimmte Mittelwerte
deskriptiv_staerke_schuh_robust <- inkonsistenz_staerke_schuh_geschlecht %>%
  group_by(Geschlecht, Staerke) %>%
  summarise(
    n = n(),
    Getrimmter_Mittelwert = mean(
      Mittelwert_Inkonsistenz,
      trim = 0.20,
      na.rm = TRUE
    ),
    Median = median(Mittelwert_Inkonsistenz, na.rm = TRUE),
    IQR = IQR(Mittelwert_Inkonsistenz, na.rm = TRUE),
    .groups = "drop"
  )

deskriptiv_staerke_schuh_robust

# Robuste Factorial Mixed ANOVA
robust_anova_staerke_schuh_geschlecht <- bwtrim(
  Mittelwert_Inkonsistenz ~ Geschlecht * Staerke,
  id = number,
  data = inkonsistenz_staerke_schuh_geschlecht,
  tr = 0.20
)
robust_anova_staerke_schuh_geschlecht


#############################################################################################################
#### 5. Stärke mit drei Stufen × Geschlecht ##################################################################
#############################################################################################################

inkonsistenz_staerke3_geschlecht <- bereite_geschlechtsdaten_vor(
  conditions = c(
    "Min-Baseline",
    "Jacke-leicht-dezentral",
    "Jacke-stark-dezentral"
  ),
  faktor = "Staerke",
  zuordnung = c(
    "Min-Baseline" = "baseline",
    "Jacke-leicht-dezentral" = "leicht",
    "Jacke-stark-dezentral" = "stark"
  ),
  stufen = c("baseline", "leicht", "stark")
)

# Kontrolle
table(inkonsistenz_staerke3_geschlecht$number)
inkonsistenz_staerke3_geschlecht %>%
  distinct(number, Geschlecht) %>%
  count(Geschlecht)

# Boxplots
boxplot_staerke3_baseline <- erstelle_boxplot_geschlecht(
  inkonsistenz_staerke3_geschlecht, "Staerke", "baseline",
  "Minimalismus-Baseline"
)
speichere_grafik(boxplot_staerke3_baseline, "Inkonsistenzbewertung nach Geschlecht")
print(boxplot_staerke3_baseline)

boxplot_staerke3_leicht <- erstelle_boxplot_geschlecht(
  inkonsistenz_staerke3_geschlecht, "Staerke", "leicht",
  "Leichte stilistische Abweichung: Jacke leicht dezentral"
)
speichere_grafik(boxplot_staerke3_leicht, "Inkonsistenzbewertung nach Geschlecht")
print(boxplot_staerke3_leicht)

boxplot_staerke3_stark <- erstelle_boxplot_geschlecht(
  inkonsistenz_staerke3_geschlecht, "Staerke", "stark",
  "Starke stilistische Abweichung: Jacke stark dezentral"
)
speichere_grafik(boxplot_staerke3_stark, "Inkonsistenzbewertung nach Geschlecht")
print(boxplot_staerke3_stark)

# Deskriptive Kennwerte
deskriptiv_staerke3_geschlecht <- deskriptiv_geschlecht(
  inkonsistenz_staerke3_geschlecht, "Staerke"
)
deskriptiv_staerke3_geschlecht

# Modell zunächst schätzen, um Residuen und Sphärizität zu prüfen
anova_staerke3_geschlecht <- schaetze_mixed_anova(
  inkonsistenz_staerke3_geschlecht,
  "Staerke",
  "anova_staerke3_geschlecht"
)

residuen_staerke3_geschlecht <- residuals(anova_staerke3_geschlecht$lm)

zeige_und_speichere_grafik(
  density_residuen(
    residuen_staerke3_geschlecht,
    "Factorial Mixed ANOVA: Stärke × Geschlecht"
  ),
  "Density Plot der Residuen"
)

shapiro_auto_09 <- shapiro_residuen(
  residuen_staerke3_geschlecht,
  "shapiro_auto_09"
)
shapiro_auto_09

zeige_und_speichere_grafik(
  qq_residuen(
    residuen_staerke3_geschlecht,
    "Factorial Mixed ANOVA: Stärke × Geschlecht"
  ),
  "QQ-Plot der Residuen"
)

# Varianzhomogenität für alle drei Stufen
levene_auto_09 <- levene_stufe(
  inkonsistenz_staerke3_geschlecht, "Staerke", "baseline", "levene_auto_09"
)
levene_auto_09

levene_auto_10 <- levene_stufe(
  inkonsistenz_staerke3_geschlecht, "Staerke", "leicht", "levene_auto_10"
)
levene_auto_10

levene_auto_11 <- levene_stufe(
  inkonsistenz_staerke3_geschlecht, "Staerke", "stark", "levene_auto_11"
)
levene_auto_11

# Sphärizität prüfen
summary_anova_staerke3_geschlecht <- summary(anova_staerke3_geschlecht)
speichere_p_werte(
  summary_anova_staerke3_geschlecht,
  "summary_anova_staerke3_geschlecht"
)
summary_anova_staerke3_geschlecht

# ANOVA erneut berechnen und ausgeben
anova_staerke3_geschlecht <- schaetze_mixed_anova(
  inkonsistenz_staerke3_geschlecht,
  "Staerke",
  "anova_staerke3_geschlecht"
)
anova_staerke3_geschlecht




#############################################################################################################
#### CVPA-Unterschiede #######################################################################################
#############################################################################################################

#### CVPA-Mittelwert und Gruppen #############################################################################

salienz_reshaped <- salienz_reshaped %>%
  mutate(Mittelwert_CVPA = rowMeans(across(CVPA1:CVPA9), na.rm = TRUE))

cvpa_personen <- salienz_reshaped %>%
  distinct(number, Mittelwert_CVPA)

cvpa_gesamtmittelwert <- mean(cvpa_personen$Mittelwert_CVPA, na.rm = TRUE)
cvpa_gesamtmittelwert

cvpa_personen <- cvpa_personen %>%
  mutate(
    CVPA_Gruppe = case_when(
      Mittelwert_CVPA < cvpa_gesamtmittelwert ~ "unter Mittelwert",
      Mittelwert_CVPA > cvpa_gesamtmittelwert ~ "über Mittelwert",
      Mittelwert_CVPA == cvpa_gesamtmittelwert ~ "genau Mittelwert"
    )
  )

cvpa_personen %>% count(CVPA_Gruppe)

salienz_reshaped <- salienz_reshaped %>%
  left_join(
    cvpa_personen %>% dplyr::select(number, CVPA_Gruppe),
    by = "number"
  )

salienz_reshaped %>%
  distinct(number, Mittelwert_CVPA, CVPA_Gruppe) %>%
  count(CVPA_Gruppe)


#############################################################################################################
#### Hilfsfunktionen #########################################################################################
#############################################################################################################

bereite_cvpa_daten_vor <- function(conditions, faktor, zuordnung, stufen) {
  salienz_reshaped %>%
    filter(Condition %in% conditions) %>%
    mutate(
      "{faktor}" := unname(zuordnung[as.character(Condition)]),
      CVPA_Gruppe = factor(
        CVPA_Gruppe,
        levels = c("unter Mittelwert", "über Mittelwert")
      ),
      "{faktor}" := factor(.data[[faktor]], levels = stufen)
    ) %>%
    filter(
      !is.na(CVPA_Gruppe),
      !is.na(.data[[faktor]]),
      !is.na(Mittelwert_Inkonsistenz)
    ) %>%
    group_by(number) %>%
    filter(n_distinct(.data[[faktor]]) == length(stufen)) %>%
    ungroup()
}

erstelle_cvpa_boxplot <- function(data, faktor, stufe, subtitle) {
  data %>%
    filter(.data[[faktor]] == stufe) %>%
    ggplot(
      aes(
        x = CVPA_Gruppe,
        y = Mittelwert_Inkonsistenz,
        fill = CVPA_Gruppe
      )
    ) +
    geom_boxplot() +
    scale_fill_manual(
      values = c(
        "unter Mittelwert" = "#EED5B7",
        "über Mittelwert" = "#CDAA7D"
      )
    ) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5)) +
    labs(
      title = "Inkonsistenzbewertung nach CVPA",
      subtitle = subtitle,
      x = "CVPA-Gruppe",
      y = "Wahrgenommene stilistische Inkonsistenz"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

deskriptiv_cvpa <- function(data, faktor) {
  data %>%
    group_by(CVPA_Gruppe, !!rlang::sym(faktor)) %>%
    summarise(
      n = n(),
      Median = median(Mittelwert_Inkonsistenz, na.rm = TRUE),
      IQR = IQR(Mittelwert_Inkonsistenz, na.rm = TRUE),
      Mittelwert = mean(Mittelwert_Inkonsistenz, na.rm = TRUE),
      SD = sd(Mittelwert_Inkonsistenz, na.rm = TRUE),
      .groups = "drop"
    )
}

deskriptiv_cvpa_robust <- function(data, faktor) {
  data %>%
    group_by(CVPA_Gruppe, !!rlang::sym(faktor)) %>%
    summarise(
      n = n(),
      Getrimmter_Mittelwert = mean(
        Mittelwert_Inkonsistenz,
        trim = 0.20,
        na.rm = TRUE
      ),
      Median = median(Mittelwert_Inkonsistenz, na.rm = TRUE),
      IQR = IQR(Mittelwert_Inkonsistenz, na.rm = TRUE),
      .groups = "drop"
    )
}

schaetze_cvpa_anova <- function(data, faktor, objektname) {
  modell <- aov_ez(
    id = "number",
    dv = "Mittelwert_Inkonsistenz",
    within = faktor,
    between = "CVPA_Gruppe",
    data = data
  )
  speichere_p_werte(modell, objektname)
  modell
}

zeige_residuenplots_cvpa <- function(residuen, subtitle) {
  zeige_und_speichere_grafik(
    ggplot(
      data.frame(Residuen = as.numeric(residuen)),
      aes(x = Residuen)
    ) +
      geom_density(
        fill = "#EED5B7",
        color = "#8B6F47",
        alpha = 0.4,
        linewidth = 1
      ) +
      labs(
        title = "Density Plot der Residuen",
        subtitle = subtitle,
        x = "Residuen",
        y = "Dichte"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      ),
    "Density Plot der Residuen"
  )
  
  zeige_und_speichere_grafik(
    ggplot(
      data.frame(Residuen = as.numeric(residuen)),
      aes(sample = Residuen)
    ) +
      stat_qq() +
      stat_qq_line() +
      labs(
        title = "QQ-Plot der Residuen",
        subtitle = subtitle,
        x = "Theoretische Quantile",
        y = "Beobachtete Quantile"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      ),
    "QQ-Plot der Residuen"
  )
}

shapiro_residuen_cvpa <- function(residuen, objektname) {
  test <- shapiro.test(as.numeric(residuen))
  speichere_p_werte(test, objektname)
  test
}

levene_cvpa <- function(data, faktor, stufe, objektname) {
  test <- leveneTest(
    Mittelwert_Inkonsistenz ~ CVPA_Gruppe,
    data = data %>% filter(.data[[faktor]] == stufe)
  )
  speichere_p_werte(test, objektname)
  test
}


#############################################################################################################
#### 1. Position × CVPA ######################################################################################
#############################################################################################################

inkonsistenz_position_cvpa <- bereite_cvpa_daten_vor(
  conditions = c("Jacke-stark-dezentral", "Jacke-stark-zentral"),
  faktor = "Position",
  zuordnung = c(
    "Jacke-stark-dezentral" = "dezentral",
    "Jacke-stark-zentral" = "zentral"
  ),
  stufen = c("dezentral", "zentral")
)

table(inkonsistenz_position_cvpa$number)

boxplot_position_dezentral_cvpa <- erstelle_cvpa_boxplot(
  inkonsistenz_position_cvpa,
  "Position",
  "dezentral",
  "Starke stilistische Abweichung: Jacke dezentral"
)
speichere_grafik(boxplot_position_dezentral_cvpa, "Inkonsistenzbewertung nach CVPA")
print(boxplot_position_dezentral_cvpa)

boxplot_position_zentral_cvpa <- erstelle_cvpa_boxplot(
  inkonsistenz_position_cvpa,
  "Position",
  "zentral",
  "Starke stilistische Abweichung: Jacke zentral"
)
speichere_grafik(boxplot_position_zentral_cvpa, "Inkonsistenzbewertung nach CVPA")
print(boxplot_position_zentral_cvpa)

deskriptiv_position_cvpa <- deskriptiv_cvpa(
  inkonsistenz_position_cvpa,
  "Position"
)
deskriptiv_position_cvpa

# Modell zunächst schätzen und Voraussetzungen prüfen
anova_position_cvpa <- schaetze_cvpa_anova(
  inkonsistenz_position_cvpa,
  "Position",
  "anova_position_cvpa"
)

residuen_position_cvpa <- residuals(anova_position_cvpa$lm)

# Density Plot
zeige_und_speichere_grafik(
  ggplot(
    data.frame(Residuen = as.numeric(residuen_position_cvpa)),
    aes(x = Residuen)
  ) +
    geom_density(
      fill = "#EED5B7",
      color = "#8B6F47",
      alpha = 0.4,
      linewidth = 1
    ) +
    labs(
      title = "Density Plot der Residuen",
      subtitle = "Factorial Mixed ANOVA: Position × CVPA",
      x = "Residuen",
      y = "Dichte"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ),
  "Density Plot der Residuen"
)

shapiro_auto_10 <- shapiro_residuen_cvpa(
  residuen_position_cvpa,
  "shapiro_auto_10"
)
shapiro_auto_10

# QQ-Plot
zeige_und_speichere_grafik(
  ggplot(
    data.frame(Residuen = as.numeric(residuen_position_cvpa)),
    aes(sample = Residuen)
  ) +
    stat_qq() +
    stat_qq_line() +
    labs(
      title = "QQ-Plot der Residuen",
      subtitle = "Factorial Mixed ANOVA: Position × CVPA",
      x = "Theoretische Quantile",
      y = "Beobachtete Quantile"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ),
  "QQ-Plot der Residuen"
)

levene_auto_12 <- levene_cvpa(
  inkonsistenz_position_cvpa, "Position", "dezentral", "levene_auto_12"
)
levene_auto_12

levene_auto_13 <- levene_cvpa(
  inkonsistenz_position_cvpa, "Position", "zentral", "levene_auto_13"
)
levene_auto_13

# 2 × 2 Factorial Mixed ANOVA
anova_position_cvpa <- schaetze_cvpa_anova(
  inkonsistenz_position_cvpa,
  "Position",
  "anova_position_cvpa"
)
anova_position_cvpa


#############################################################################################################
#### 2. Größe × CVPA #########################################################################################
#############################################################################################################

inkonsistenz_groesse_cvpa <- bereite_cvpa_daten_vor(
  conditions = c("Schuh-stark-dezentral", "Jacke-stark-dezentral"),
  faktor = "Groesse",
  zuordnung = c(
    "Schuh-stark-dezentral" = "klein",
    "Jacke-stark-dezentral" = "gross"
  ),
  stufen = c("klein", "gross")
)

table(inkonsistenz_groesse_cvpa$number)

boxplot_groesse_klein_cvpa <- erstelle_cvpa_boxplot(
  inkonsistenz_groesse_cvpa,
  "Groesse",
  "klein",
  "Kleines stilistisch abweichendes Produkt: Schuh stark dezentral"
)
speichere_grafik(boxplot_groesse_klein_cvpa, "Inkonsistenzbewertung nach CVPA")
print(boxplot_groesse_klein_cvpa)

boxplot_groesse_gross_cvpa <- erstelle_cvpa_boxplot(
  inkonsistenz_groesse_cvpa,
  "Groesse",
  "gross",
  "Großes stilistisch abweichendes Produkt: Jacke stark dezentral"
)
speichere_grafik(boxplot_groesse_gross_cvpa, "Inkonsistenzbewertung nach CVPA")
print(boxplot_groesse_gross_cvpa)

deskriptiv_groesse_cvpa <- deskriptiv_cvpa(
  inkonsistenz_groesse_cvpa,
  "Groesse"
)
deskriptiv_groesse_cvpa

# Erste Voraussetzungenprüfung wie im Original
anova_groesse_cvpa <- schaetze_cvpa_anova(
  inkonsistenz_groesse_cvpa,
  "Groesse",
  "anova_groesse_cvpa"
)
residuen_groesse_cvpa <- residuals(anova_groesse_cvpa$lm)

# Density Plot
zeige_und_speichere_grafik(
  ggplot(
    data.frame(Residuen = as.numeric(residuen_groesse_cvpa)),
    aes(x = Residuen)
  ) +
    geom_density(
      fill = "#EED5B7",
      color = "#8B6F47",
      alpha = 0.4,
      linewidth = 1
    ) +
    labs(
      title = "Density Plot der Residuen",
      subtitle = "Factorial Mixed ANOVA: Größe × CVPA",
      x = "Residuen",
      y = "Dichte"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ),
  "Density Plot der Residuen"
)

shapiro_auto_11 <- shapiro_residuen_cvpa(
  residuen_groesse_cvpa,
  "shapiro_auto_11"
)
shapiro_auto_11

# QQ-Plot
zeige_und_speichere_grafik(
  ggplot(
    data.frame(Residuen = as.numeric(residuen_groesse_cvpa)),
    aes(sample = Residuen)
  ) +
    stat_qq() +
    stat_qq_line() +
    labs(
      title = "QQ-Plot der Residuen",
      subtitle = "Factorial Mixed ANOVA: Größe × CVPA",
      x = "Theoretische Quantile",
      y = "Beobachtete Quantile"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ),
  "QQ-Plot der Residuen"
)

levene_auto_14 <- levene_cvpa(
  inkonsistenz_groesse_cvpa, "Groesse", "klein", "levene_auto_14"
)
levene_auto_14

levene_auto_15 <- levene_cvpa(
  inkonsistenz_groesse_cvpa, "Groesse", "gross", "levene_auto_15"
)
levene_auto_15

# Erneute identische Voraussetzungenprüfung wie im Original
anova_groesse_cvpa <- schaetze_cvpa_anova(
  inkonsistenz_groesse_cvpa,
  "Groesse",
  "anova_groesse_cvpa"
)
residuen_groesse_cvpa <- residuals(anova_groesse_cvpa$lm)

# Density Plot
zeige_und_speichere_grafik(
  ggplot(
    data.frame(Residuen = as.numeric(residuen_groesse_cvpa)),
    aes(x = Residuen)
  ) +
    geom_density(
      fill = "#EED5B7",
      color = "#8B6F47",
      alpha = 0.4,
      linewidth = 1
    ) +
    labs(
      title = "Density Plot der Residuen",
      subtitle = "Factorial Mixed ANOVA: Größe × CVPA",
      x = "Residuen",
      y = "Dichte"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ),
  "Density Plot der Residuen"
)

shapiro_auto_12 <- shapiro_residuen_cvpa(
  residuen_groesse_cvpa,
  "shapiro_auto_12"
)
shapiro_auto_12

# QQ-Plot
zeige_und_speichere_grafik(
  ggplot(
    data.frame(Residuen = as.numeric(residuen_groesse_cvpa)),
    aes(sample = Residuen)
  ) +
    stat_qq() +
    stat_qq_line() +
    labs(
      title = "QQ-Plot der Residuen",
      subtitle = "Factorial Mixed ANOVA: Größe × CVPA",
      x = "Theoretische Quantile",
      y = "Beobachtete Quantile"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ),
  "QQ-Plot der Residuen"
)

levene_auto_16 <- levene_cvpa(
  inkonsistenz_groesse_cvpa, "Groesse", "klein", "levene_auto_16"
)
levene_auto_16

levene_auto_17 <- levene_cvpa(
  inkonsistenz_groesse_cvpa, "Groesse", "gross", "levene_auto_17"
)
levene_auto_17

# Robuste deskriptive Kennwerte und robuste 2 × 2 Mixed ANOVA
deskriptiv_groesse_cvpa_robust <- deskriptiv_cvpa_robust(
  inkonsistenz_groesse_cvpa,
  "Groesse"
)
deskriptiv_groesse_cvpa_robust

robust_anova_groesse_cvpa <- bwtrim(
  Mittelwert_Inkonsistenz ~ CVPA_Gruppe * Groesse,
  id = number,
  data = inkonsistenz_groesse_cvpa,
  tr = 0.20
)
robust_anova_groesse_cvpa


#############################################################################################################
#### 3. Stärke × CVPA ########################################################################################
#############################################################################################################

inkonsistenz_staerke_cvpa <- bereite_cvpa_daten_vor(
  conditions = c("Jacke-leicht-dezentral", "Jacke-stark-dezentral"),
  faktor = "Staerke",
  zuordnung = c(
    "Jacke-leicht-dezentral" = "leicht",
    "Jacke-stark-dezentral" = "stark"
  ),
  stufen = c("leicht", "stark")
)

table(inkonsistenz_staerke_cvpa$number)

inkonsistenz_staerke_cvpa %>%
  distinct(number, CVPA_Gruppe) %>%
  count(CVPA_Gruppe)

boxplot_staerke_leicht_cvpa <- erstelle_cvpa_boxplot(
  inkonsistenz_staerke_cvpa,
  "Staerke",
  "leicht",
  "Leichte stilistische Abweichung: Jacke dezentral"
)
speichere_grafik(boxplot_staerke_leicht_cvpa, "Inkonsistenzbewertung nach CVPA")
print(boxplot_staerke_leicht_cvpa)

boxplot_staerke_stark_cvpa <- erstelle_cvpa_boxplot(
  inkonsistenz_staerke_cvpa,
  "Staerke",
  "stark",
  "Starke stilistische Abweichung: Jacke dezentral"
)
speichere_grafik(boxplot_staerke_stark_cvpa, "Inkonsistenzbewertung nach CVPA")
print(boxplot_staerke_stark_cvpa)

deskriptiv_staerke_cvpa <- deskriptiv_cvpa(
  inkonsistenz_staerke_cvpa,
  "Staerke"
)
deskriptiv_staerke_cvpa

anova_staerke_cvpa <- schaetze_cvpa_anova(
  inkonsistenz_staerke_cvpa,
  "Staerke",
  "anova_staerke_cvpa"
)
residuen_staerke_cvpa <- residuals(anova_staerke_cvpa$lm)

# Density Plot
zeige_und_speichere_grafik(
  ggplot(
    data.frame(Residuen = as.numeric(residuen_staerke_cvpa)),
    aes(x = Residuen)
  ) +
    geom_density(
      fill = "#EED5B7",
      color = "#8B6F47",
      alpha = 0.4,
      linewidth = 1
    ) +
    labs(
      title = "Density Plot der Residuen",
      subtitle = "Factorial Mixed ANOVA: Stärke × CVPA",
      x = "Residuen",
      y = "Dichte"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ),
  "Density Plot der Residuen"
)

shapiro_auto_13 <- shapiro_residuen_cvpa(
  residuen_staerke_cvpa,
  "shapiro_auto_13"
)
shapiro_auto_13

# QQ-Plot
zeige_und_speichere_grafik(
  ggplot(
    data.frame(Residuen = as.numeric(residuen_staerke_cvpa)),
    aes(sample = Residuen)
  ) +
    stat_qq() +
    stat_qq_line() +
    labs(
      title = "QQ-Plot der Residuen",
      subtitle = "Factorial Mixed ANOVA: Stärke × CVPA",
      x = "Theoretische Quantile",
      y = "Beobachtete Quantile"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ),
  "QQ-Plot der Residuen"
)

levene_auto_18 <- levene_cvpa(
  inkonsistenz_staerke_cvpa, "Staerke", "leicht", "levene_auto_18"
)
levene_auto_18

levene_auto_19 <- levene_cvpa(
  inkonsistenz_staerke_cvpa, "Staerke", "stark", "levene_auto_19"
)
levene_auto_19

anova_staerke_cvpa <- schaetze_cvpa_anova(
  inkonsistenz_staerke_cvpa,
  "Staerke",
  "anova_staerke_cvpa"
)
anova_staerke_cvpa


#############################################################################################################
#### 4. Stärke mit Baseline × CVPA ###########################################################################
#############################################################################################################

inkonsistenz_staerke3_cvpa <- bereite_cvpa_daten_vor(
  conditions = c(
    "Min-Baseline",
    "Jacke-leicht-dezentral",
    "Jacke-stark-dezentral"
  ),
  faktor = "Staerke",
  zuordnung = c(
    "Min-Baseline" = "baseline",
    "Jacke-leicht-dezentral" = "leicht",
    "Jacke-stark-dezentral" = "stark"
  ),
  stufen = c("baseline", "leicht", "stark")
)

table(inkonsistenz_staerke3_cvpa$number)

inkonsistenz_staerke3_cvpa %>%
  distinct(number, CVPA_Gruppe) %>%
  count(CVPA_Gruppe)

# Gemeinsamer Boxplot für alle drei Stärkestufen
zeige_und_speichere_grafik(
  ggplot(
    inkonsistenz_staerke3_cvpa,
    aes(
      x = Staerke,
      y = Mittelwert_Inkonsistenz,
      fill = CVPA_Gruppe
    )
  ) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    scale_fill_manual(
      values = c(
        "unter Mittelwert" = "#EED5B7",
        "über Mittelwert" = "#CDAA7D"
      )
    ) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5)) +
    labs(
      title = "Inkonsistenzbewertung nach Stärke und CVPA",
      subtitle = "Baseline, leichte und starke stilistische Abweichung",
      x = "Stärke der stilistischen Abweichung",
      y = "Wahrgenommene stilistische Inkonsistenz",
      fill = "CVPA-Gruppe"
    ) +
    theme_minimal(),
  "Inkonsistenzbewertung nach Stärke und CVPA"
)

deskriptiv_staerke3_cvpa <- deskriptiv_cvpa(
  inkonsistenz_staerke3_cvpa,
  "Staerke"
)
deskriptiv_staerke3_cvpa

anova_staerke3_cvpa <- schaetze_cvpa_anova(
  inkonsistenz_staerke3_cvpa,
  "Staerke",
  "anova_staerke3_cvpa"
)
residuen_staerke3_cvpa <- residuals(anova_staerke3_cvpa$lm)

# Density Plot
zeige_und_speichere_grafik(
  ggplot(
    data.frame(Residuen = as.numeric(residuen_staerke3_cvpa)),
    aes(x = Residuen)
  ) +
    geom_density(
      fill = "#EED5B7",
      color = "#8B6F47",
      alpha = 0.4,
      linewidth = 1
    ) +
    labs(
      title = "Density Plot der Residuen",
      subtitle = "Factorial Mixed ANOVA: Stärke × CVPA",
      x = "Residuen",
      y = "Dichte"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ),
  "Density Plot der Residuen"
)

shapiro_auto_14 <- shapiro_residuen_cvpa(
  residuen_staerke3_cvpa,
  "shapiro_auto_14"
)
shapiro_auto_14

# QQ-Plot
zeige_und_speichere_grafik(
  ggplot(
    data.frame(Residuen = as.numeric(residuen_staerke3_cvpa)),
    aes(sample = Residuen)
  ) +
    stat_qq() +
    stat_qq_line() +
    labs(
      title = "QQ-Plot der Residuen",
      subtitle = "Factorial Mixed ANOVA: Stärke × CVPA",
      x = "Theoretische Quantile",
      y = "Beobachtete Quantile"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ),
  "QQ-Plot der Residuen"
)

levene_auto_20 <- levene_cvpa(
  inkonsistenz_staerke3_cvpa, "Staerke", "baseline", "levene_auto_20"
)
levene_auto_20

levene_auto_21 <- levene_cvpa(
  inkonsistenz_staerke3_cvpa, "Staerke", "leicht", "levene_auto_21"
)
levene_auto_21

levene_auto_22 <- levene_cvpa(
  inkonsistenz_staerke3_cvpa, "Staerke", "stark", "levene_auto_22"
)
levene_auto_22

# Sphärizität prüfen
summary_anova_staerke3_cvpa <- summary(anova_staerke3_cvpa)
speichere_p_werte(
  summary_anova_staerke3_cvpa,
  "summary_anova_staerke3_cvpa"
)
summary_anova_staerke3_cvpa


#############################################################################################################
#### 5. Stärke Schuh × CVPA ##################################################################################
#############################################################################################################

inkonsistenz_staerke_schuh_cvpa <- bereite_cvpa_daten_vor(
  conditions = c("Min-Baseline", "Schuh-stark-dezentral"),
  faktor = "Staerke",
  zuordnung = c(
    "Min-Baseline" = "baseline",
    "Schuh-stark-dezentral" = "stark"
  ),
  stufen = c("baseline", "stark")
)

table(inkonsistenz_staerke_schuh_cvpa$number)

inkonsistenz_staerke_schuh_cvpa %>%
  distinct(number, CVPA_Gruppe) %>%
  count(CVPA_Gruppe)

boxplot_staerke_schuh_baseline_cvpa <- erstelle_cvpa_boxplot(
  inkonsistenz_staerke_schuh_cvpa,
  "Staerke",
  "baseline",
  "Minimalistische Baseline"
)
speichere_grafik(
  boxplot_staerke_schuh_baseline_cvpa,
  "Inkonsistenzbewertung nach CVPA"
)
print(boxplot_staerke_schuh_baseline_cvpa)

boxplot_staerke_schuh_stark_cvpa <- erstelle_cvpa_boxplot(
  inkonsistenz_staerke_schuh_cvpa,
  "Staerke",
  "stark",
  "Stark stilistisch abweichender Schuh: dezentral"
)
speichere_grafik(
  boxplot_staerke_schuh_stark_cvpa,
  "Inkonsistenzbewertung nach CVPA"
)
print(boxplot_staerke_schuh_stark_cvpa)

deskriptiv_staerke_schuh_cvpa <- deskriptiv_cvpa(
  inkonsistenz_staerke_schuh_cvpa,
  "Staerke"
)
deskriptiv_staerke_schuh_cvpa

anova_staerke_schuh_cvpa <- schaetze_cvpa_anova(
  inkonsistenz_staerke_schuh_cvpa,
  "Staerke",
  "anova_staerke_schuh_cvpa"
)
residuen_staerke_schuh_cvpa <- residuals(anova_staerke_schuh_cvpa$lm)

# Density Plot
zeige_und_speichere_grafik(
  ggplot(
    data.frame(Residuen = as.numeric(residuen_staerke_schuh_cvpa)),
    aes(x = Residuen)
  ) +
    geom_density(
      fill = "#EED5B7",
      color = "#8B6F47",
      alpha = 0.4,
      linewidth = 1
    ) +
    labs(
      title = "Density Plot der Residuen",
      subtitle = "Factorial Mixed ANOVA: Stärke Schuh × CVPA",
      x = "Residuen",
      y = "Dichte"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ),
  "Density Plot der Residuen"
)

shapiro_auto_15 <- shapiro_residuen_cvpa(
  residuen_staerke_schuh_cvpa,
  "shapiro_auto_15"
)
shapiro_auto_15

# QQ-Plot
zeige_und_speichere_grafik(
  ggplot(
    data.frame(Residuen = as.numeric(residuen_staerke_schuh_cvpa)),
    aes(sample = Residuen)
  ) +
    stat_qq() +
    stat_qq_line() +
    labs(
      title = "QQ-Plot der Residuen",
      subtitle = "Factorial Mixed ANOVA: Stärke Schuh × CVPA",
      x = "Theoretische Quantile",
      y = "Beobachtete Quantile"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ),
  "QQ-Plot der Residuen"
)

levene_auto_23 <- levene_cvpa(
  inkonsistenz_staerke_schuh_cvpa, "Staerke", "baseline", "levene_auto_23"
)
levene_auto_23

levene_auto_24 <- levene_cvpa(
  inkonsistenz_staerke_schuh_cvpa, "Staerke", "stark", "levene_auto_24"
)
levene_auto_24

deskriptiv_staerke_schuh_cvpa_robust <- deskriptiv_cvpa_robust(
  inkonsistenz_staerke_schuh_cvpa,
  "Staerke"
)
deskriptiv_staerke_schuh_cvpa_robust

library(WRS2)

robust_anova_staerke_schuh_cvpa <- bwtrim(
  Mittelwert_Inkonsistenz ~ CVPA_Gruppe * Staerke,
  id = number,
  data = inkonsistenz_staerke_schuh_cvpa,
  tr = 0.20
)
robust_anova_staerke_schuh_cvpa





#############################################################################################################
#### Ermüdungseffekt ########################################################################################
#############################################################################################################

# Wird ein Outfit anders bewertet, wenn es als erstes vs. als letztes gezeigt wird?

# Gespeicherte Randomisierung der 10-Sekunden-Bilder prüfen
unique(salienz$Randomisierung_1)
unique(salienz$Randomisierung_2)
unique(salienz$Randomisierung_3)
unique(salienz$Randomisierung_4)
unique(salienz$Randomisierung_5)

# Unipark-IDs den Conditions zuordnen
randomisierung_zu_condition <- function(x) {
  x <- as.character(x)
  case_when(
    x == "7698430" ~ "Jacke-stark-dezentral",
    x == "7698432" ~ "Schuh-stark-dezentral",
    x == "7698426" ~ "Jacke-stark-zentral",
    x == "7698437" ~ "Jacke-leicht-dezentral",
    x == "7698434" ~ "Min-Baseline",
    TRUE ~ NA_character_
  )
}

# Erstes und letztes Outfit pro Person bestimmen
salienz_reshaped <- salienz_reshaped %>%
  mutate(
    Erstes_Outfit = randomisierung_zu_condition(Randomisierung_1),
    Letztes_Outfit = randomisierung_zu_condition(Randomisierung_5)
  )

# Zuordnung prüfen
salienz_reshaped %>%
  distinct(number, Erstes_Outfit) %>%
  count(Erstes_Outfit)

salienz_reshaped %>%
  distinct(number, Letztes_Outfit) %>%
  count(Letztes_Outfit)

# Erstes und letztes Outfit auswählen
ermuedung_plot <- salienz_reshaped %>%
  mutate(
    Reihenfolge = case_when(
      Condition == Erstes_Outfit ~ "Als erstes gezeigt",
      Condition == Letztes_Outfit ~ "Als letztes gezeigt",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Reihenfolge), !is.na(Mittelwert_Inkonsistenz)) %>%
  mutate(
    Reihenfolge = factor(
      Reihenfolge,
      levels = c("Als erstes gezeigt", "Als letztes gezeigt")
    )
  )


#### Deskriptive Analyse ####################################################################################

deskriptiv_ermuedung <- ermuedung_plot %>%
  group_by(Condition, Reihenfolge) %>%
  summarise(
    n = n(),
    Median = median(Mittelwert_Inkonsistenz, na.rm = TRUE),
    IQR = IQR(Mittelwert_Inkonsistenz, na.rm = TRUE),
    Mittelwert = mean(Mittelwert_Inkonsistenz, na.rm = TRUE),
    SD = sd(Mittelwert_Inkonsistenz, na.rm = TRUE),
    .groups = "drop"
  )

print(deskriptiv_ermuedung)


#### Boxplots ################################################################################################

outfit_namen <- c(
  "Min-Baseline" = "Baseline",
  "Jacke-leicht-dezentral" = "Jacke leicht dezentral",
  "Jacke-stark-dezentral" = "Jacke stark dezentral",
  "Jacke-stark-zentral" = "Jacke stark zentral",
  "Schuh-stark-dezentral" = "Schuh stark dezentral"
)

boxplots_ermuedung <- list()

for (outfit in conditions) {
  
  plot_daten <- ermuedung_plot %>%
    filter(Condition == outfit)
  
  plot <- ggplot(
    plot_daten,
    aes(
      x = Reihenfolge,
      y = Mittelwert_Inkonsistenz,
      fill = Reihenfolge
    )
  ) +
    geom_boxplot(width = 0.6) +
    scale_fill_manual(
      values = c(
        "Als erstes gezeigt" = "#EED5B7",
        "Als letztes gezeigt" = "#CDAA7D"
      )
    ) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5)) +
    labs(
      title = paste("Inkonsistenzbewertung:", outfit_namen[outfit]),
      subtitle = "Als erstes vs. als letztes gezeigt",
      x = "Position in der Befragung",
      y = "Wahrgenommene stilistische Inkonsistenz"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  speichere_grafik(plot, paste0("Ermuedung_", outfit))
  boxplots_ermuedung[[outfit]] <- plot
  print(plot)
}


#### Normalverteilung mit Shapiro-Wilk ######################################################################

shapiro_ermuedung <- ermuedung_plot %>%
  group_by(Condition, Reihenfolge) %>%
  shapiro_test(Mittelwert_Inkonsistenz)

print(shapiro_ermuedung)
speichere_p_werte(shapiro_ermuedung, "shapiro_ermuedung")

# Normalverteilung grafisch prüfen: Density Plots
zeige_und_speichere_grafik(
  ggplot(ermuedung_plot, aes(x = Mittelwert_Inkonsistenz)) +
    geom_density(
      fill = "#EED5B7",
      color = "#8B6F47",
      alpha = 0.4,
      linewidth = 1
    ) +
    facet_grid(Condition ~ Reihenfolge) +
    labs(
      title = "Density Plots der Inkonsistenzbewertung",
      subtitle = "Erstes vs. letztes gezeigtes Outfit",
      x = "Wahrgenommene stilistische Inkonsistenz",
      y = "Dichte"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ),
  "Density Plots der Inkonsistenzbewertung"
)

# Normalverteilung grafisch prüfen: QQ-Plots
zeige_und_speichere_grafik(
  ggplot(ermuedung_plot, aes(sample = Mittelwert_Inkonsistenz)) +
    stat_qq() +
    stat_qq_line() +
    facet_grid(Condition ~ Reihenfolge) +
    labs(
      title = "QQ-Plots der Inkonsistenzbewertung",
      subtitle = "Erstes vs. letztes gezeigtes Outfit",
      x = "Theoretische Quantile",
      y = "Beobachtete Quantile"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ),
  "QQ-Plots der Inkonsistenzbewertung"
)


#### Levene-Test ############################################################################################

levene_ermuedung <- ermuedung_plot %>%
  group_by(Condition) %>%
  levene_test(Mittelwert_Inkonsistenz ~ Reihenfolge)

print(levene_ermuedung)
speichere_p_werte(levene_ermuedung, "levene_ermuedung")


#### Hypothesentest für jedes Outfit ########################################################################

ergebnisse_ermuedung <- data.frame()

for (outfit in conditions) {
  
  # Daten des aktuellen Outfits
  daten_outfit <- ermuedung_plot %>%
    filter(Condition == outfit) %>%
    droplevels()
  
  # Gruppen getrennt speichern
  zuerst <- daten_outfit %>%
    filter(Reihenfolge == "Als erstes gezeigt") %>%
    pull(Mittelwert_Inkonsistenz)
  
  zuletzt <- daten_outfit %>%
    filter(Reihenfolge == "Als letztes gezeigt") %>%
    pull(Mittelwert_Inkonsistenz)
  
  # Normalverteilung prüfen
  shapiro_zuerst <- shapiro.test(zuerst)
  speichere_p_werte(shapiro_zuerst, "shapiro_zuerst", outfit)
  
  shapiro_zuletzt <- shapiro.test(zuletzt)
  speichere_p_werte(shapiro_zuletzt, "shapiro_zuletzt", outfit)
  
  # Normalverteilung grafisch prüfen: Density Plot
  zeige_und_speichere_grafik(
    ggplot(
      daten_outfit,
      aes(x = Mittelwert_Inkonsistenz)
    ) +
      geom_density(
        fill = "#EED5B7",
        color = "#8B6F47",
        alpha = 0.4,
        linewidth = 1
      ) +
      facet_wrap(~ Reihenfolge) +
      labs(
        title = paste("Density Plot:", outfit_namen[outfit]),
        subtitle = "Als erstes vs. als letztes gezeigt",
        x = "Wahrgenommene stilistische Inkonsistenz",
        y = "Dichte"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      ),
    paste0("Density_Normalverteilung_Ermuedung_", outfit)
  )
  
  # Normalverteilung grafisch prüfen: QQ-Plot
  zeige_und_speichere_grafik(
    ggplot(
      daten_outfit,
      aes(sample = Mittelwert_Inkonsistenz)
    ) +
      stat_qq() +
      stat_qq_line() +
      facet_wrap(~ Reihenfolge) +
      labs(
        title = paste("QQ-Plot:", outfit_namen[outfit]),
        subtitle = "Als erstes vs. als letztes gezeigt",
        x = "Theoretische Quantile",
        y = "Beobachtete Quantile"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      ),
    paste0("QQ_Normalverteilung_Ermuedung_", outfit)
  )
  
  # Varianzhomogenität prüfen
  levene_result <- car::leveneTest(
    Mittelwert_Inkonsistenz ~ Reihenfolge,
    data = daten_outfit
  )
  levene_p <- levene_result$`Pr(>F)`[1]
  speichere_p_werte(levene_result, "levene_result", outfit)
  
  # Test abhängig von Voraussetzungen auswählen
  if (shapiro_zuerst$p.value > 0.05 &&
      shapiro_zuletzt$p.value > 0.05) {
    if (levene_p > 0.05) {
      test_result <- t.test(
        zuerst,
        zuletzt,
        paired = FALSE,
        var.equal = TRUE,
        alternative = "two.sided"
      )
      verwendeter_test <- "Unabhängiger t-Test"
    } else {
      test_result <- t.test(
        zuerst,
        zuletzt,
        paired = FALSE,
        var.equal = FALSE,
        alternative = "two.sided"
      )
      verwendeter_test <- "Welch-t-Test"
    }
    
  } else {
    test_result <- wilcox.test(
      zuerst,
      zuletzt,
      paired = FALSE,
      alternative = "two.sided",
      exact = FALSE
    )
    verwendeter_test <- "Mann-Whitney-U-Test"
  }
  
  speichere_p_werte(test_result, verwendeter_test, outfit)
  
  # Ergebnisse speichern
  ergebnisse_ermuedung <- rbind(
    ergebnisse_ermuedung,
    data.frame(
      Outfit = outfit,
      n_zuerst = length(zuerst),
      n_zuletzt = length(zuletzt),
      Mittelwert_zuerst = mean(zuerst, na.rm = TRUE),
      Mittelwert_zuletzt = mean(zuletzt, na.rm = TRUE),
      Differenz = mean(zuletzt, na.rm = TRUE) - mean(zuerst, na.rm = TRUE),
      Shapiro_p_zuerst = shapiro_zuerst$p.value,
      Shapiro_p_zuletzt = shapiro_zuletzt$p.value,
      Levene_p = levene_p,
      Test = verwendeter_test,
      Teststatistik = unname(test_result$statistic),
      p_Wert = test_result$p.value
    )
  )
}


#### Holm-Korrektur für fünf Tests ##########################################################################

ergebnisse_ermuedung <- ergebnisse_ermuedung %>%
  mutate(
    p_Wert_Holm = p.adjust(p_Wert, method = "holm"),
    Signifikant_nach_Holm = ifelse(p_Wert_Holm < 0.05, "Ja", "Nein")
  )

# Holm-korrigierte p-Werte separat speichern
for (i in seq_len(nrow(ergebnisse_ermuedung))) {
  speichere_p_wert_einzeln(
    objektname = "ergebnisse_ermuedung",
    bestandteil = "Holm-Korrektur",
    zeile = ergebnisse_ermuedung$Outfit[i],
    p_typ = "p_Wert_Holm",
    p_wert = ergebnisse_ermuedung$p_Wert_Holm[i]
  )
}
print(ergebnisse_ermuedung)




#############################################################################################################
#### Mediation ##############################################################################################
#############################################################################################################

#############################################################################################################
#### Hypothese 1: Größe -> Fluency -> Inkonsistenz
#############################################################################################################

<<<<<<< HEAD
#### IRGENDWAS MIT DEN LMER PAKETEN STIMMT HIE RNICHT BRUH


#############################################################################################################
#### Hypothese 1: Größe -> Fluency -> Inkonsistenz
#############################################################################################################

=======
>>>>>>> 3d36fecd146c084a91beb55ca0de3540f548f7c3
# Relevante Bedingungen auswählen
mediation_groesse <- salienz_reshaped %>%
  filter(Condition %in% c("Schuh-stark-dezentral", "Jacke-stark-dezentral")) %>%
  mutate(Groesse = ifelse(Condition == "Jacke-stark-dezentral", 1, 0)) %>%
  drop_na(number, Mittelwert_Fluency, Mittelwert_Inkonsistenz)

#### Voraussetzungen der Regression prüfen
model_groesse <- lmer(
  Mittelwert_Inkonsistenz ~ Groesse + Mittelwert_Fluency + (1 | number),
  data = mediation_groesse
)
summary(model_groesse)

# 1. Ausreißer / einflussreiche Personen
influence_groesse <- influence(model_groesse, groups = "number")
cooks_groesse <- cooks.distance(influence_groesse)

plot(
  cooks_groesse,
  type = "h",
  main = "Cook's Distance – Größe",
  xlab = "Versuchsperson",
  ylab = "Cook's Distance"
)

# 2. Normalverteilung der Residuen
residuen_groesse <- residuals(model_groesse)
shapiro.test(residuen_groesse)
qqnorm(residuen_groesse)
qqline(residuen_groesse)

# 3. Homoskedastizität der Residuen
plot(
  fitted(model_groesse),
  residuen_groesse,
  xlab = "Vorhergesagte Werte",
  ylab = "Residuen",
  main = "Residuen vs. vorhergesagte Werte – Größe"
)
abline(h = 0, lty = 2)

# 4. Multikollinearität
vif(model_groesse)

# 5. Unabhängigkeit der Beobachtungen
# Abhängigkeit wiederholter Messungen wird durch (1 | number) berücksichtigt.

#### Mediation

# 1. Effekt von UV auf M = Pfad A
pfad_a_groesse <- lmer(
  Mittelwert_Fluency ~ Groesse + (1 | number),
  data = mediation_groesse
)
summary(pfad_a_groesse)

# 2. Effekt von UV und M auf AV
pfad_b_c_groesse <- lmer(
  Mittelwert_Inkonsistenz ~ Groesse + Mittelwert_Fluency + (1 | number),
  data = mediation_groesse
)
summary(pfad_b_c_groesse)

# 3. Untersuchung des Mediationseffektes
set.seed(123)
results_groesse <- mediate(
  pfad_a_groesse,
  pfad_b_c_groesse,
  treat = "Groesse",
  mediator = "Mittelwert_Fluency",
  sims = 5000,
  boot = FALSE,
  group.out = "number"
)
<<<<<<< HEAD
class(pfad_a_groesse)
=======
>>>>>>> 3d36fecd146c084a91beb55ca0de3540f548f7c3
summary(results_groesse)

#############################################################################################################
#### Hypothese 2: Stärke -> Fluency -> Inkonsistenz
#############################################################################################################

# Relevante Bedingungen auswählen
mediation_staerke <- salienz_reshaped %>%
  filter(Condition %in% c("Jacke-leicht-dezentral", "Jacke-stark-dezentral")) %>%
  mutate(Staerke = ifelse(Condition == "Jacke-stark-dezentral", 1, 0)) %>%
  drop_na(number, Mittelwert_Fluency, Mittelwert_Inkonsistenz)

#### Voraussetzungen der Regression prüfen
model_staerke <- lmer(
  Mittelwert_Inkonsistenz ~ Staerke + Mittelwert_Fluency + (1 | number),
  data = mediation_staerke
)
summary(model_staerke)

# 1. Ausreißer / einflussreiche Personen
influence_staerke <- influence(model_staerke, groups = "number")
cooks_staerke <- cooks.distance(influence_staerke)

plot(
  cooks_staerke,
  type = "h",
  main = "Cook's Distance – Stärke",
  xlab = "Versuchsperson",
  ylab = "Cook's Distance"
)

# 2. Normalverteilung der Residuen
residuen_staerke <- residuals(model_staerke)
shapiro.test(residuen_staerke)
qqnorm(residuen_staerke)
qqline(residuen_staerke)

# 3. Homoskedastizität der Residuen
plot(
  fitted(model_staerke),
  residuen_staerke,
  xlab = "Vorhergesagte Werte",
  ylab = "Residuen",
  main = "Residuen vs. vorhergesagte Werte – Stärke"
)
abline(h = 0, lty = 2)

# 4. Multikollinearität
vif(model_staerke)

# 5. Unabhängigkeit der Beobachtungen
# Abhängigkeit wiederholter Messungen wird durch (1 | number) berücksichtigt.

#### Mediation

# 1. Effekt von UV auf M = Pfad A
pfad_a_staerke <- lmer(
  Mittelwert_Fluency ~ Staerke + (1 | number),
  data = mediation_staerke
)
summary(pfad_a_staerke)

# 2. Effekt von UV und M auf AV
pfad_b_c_staerke <- lmer(
  Mittelwert_Inkonsistenz ~ Staerke + Mittelwert_Fluency + (1 | number),
  data = mediation_staerke
)
summary(pfad_b_c_staerke)

# 3. Untersuchung des Mediationseffektes
set.seed(123)
results_staerke <- mediate(
  pfad_a_staerke,
  pfad_b_c_staerke,
  treat = "Staerke",
  mediator = "Mittelwert_Fluency",
  sims = 5000,
  boot = FALSE,
  group.out = "number"
)
summary(results_staerke)

#############################################################################################################
#### Hypothese 3: Position -> Fluency -> Inkonsistenz
#############################################################################################################

# Relevante Bedingungen auswählen
mediation_position <- salienz_reshaped %>%
  filter(Condition %in% c("Jacke-stark-dezentral", "Jacke-stark-zentral")) %>%
  mutate(Position = ifelse(Condition == "Jacke-stark-zentral", 1, 0)) %>%
  drop_na(number, Mittelwert_Fluency, Mittelwert_Inkonsistenz)

#### Voraussetzungen der Regression prüfen
model_position <- lmer(
  Mittelwert_Inkonsistenz ~ Position + Mittelwert_Fluency + (1 | number),
  data = mediation_position
)
summary(model_position)

# 1. Ausreißer / einflussreiche Personen
influence_position <- influence(model_position, groups = "number")
cooks_position <- cooks.distance(influence_position)

plot(
  cooks_position,
  type = "h",
  main = "Cook's Distance – Position",
  xlab = "Versuchsperson",
  ylab = "Cook's Distance"
)

# 2. Normalverteilung der Residuen
residuen_position <- residuals(model_position)
shapiro.test(residuen_position)
qqnorm(residuen_position)
qqline(residuen_position)

# 3. Homoskedastizität der Residuen
plot(
  fitted(model_position),
  residuen_position,
  xlab = "Vorhergesagte Werte",
  ylab = "Residuen",
  main = "Residuen vs. vorhergesagte Werte – Position"
)
abline(h = 0, lty = 2)

# 4. Multikollinearität
vif(model_position)

# 5. Unabhängigkeit der Beobachtungen
# Abhängigkeit wiederholter Messungen wird durch (1 | number) berücksichtigt.

#### Mediation

# 1. Effekt von UV auf M = Pfad A
pfad_a_position <- lmer(
  Mittelwert_Fluency ~ Position + (1 | number),
  data = mediation_position
)
summary(pfad_a_position)

# 2. Effekt von UV und M auf AV
pfad_b_c_position <- lmer(
  Mittelwert_Inkonsistenz ~ Position + Mittelwert_Fluency + (1 | number),
  data = mediation_position
)
summary(pfad_b_c_position)

# 3. Untersuchung des Mediationseffektes
set.seed(123)
results_position <- mediate(
  pfad_a_position,
  pfad_b_c_position,
  treat = "Position",
  mediator = "Mittelwert_Fluency",
  sims = 5000,
  boot = FALSE,
  group.out = "number"
)
summary(results_position)





#############################################################################################################
#### Ausblick ###############################################################################################
#############################################################################################################


#### Stilzuordnung prüfen #####################################################################################

# Stilbewertungen ins Long-Format bringen
stilzuordnung_long <- salienz_reshaped %>%
  dplyr::select(
    number,
    Condition,
    Minimalismus,
    HipHopActiveWear,
    HippieBoho
  ) %>%
  pivot_longer(
    cols = c(
      Minimalismus,
      HipHopActiveWear,
      HippieBoho
    ),
    names_to = "Stil",
    values_to = "Bewertung"
  ) %>%
  mutate(
    Bewertung = as.numeric(Bewertung),
    
    Stil = factor(
      Stil,
      levels = c(
        "Minimalismus",
        "HipHopActiveWear",
        "HippieBoho"
      ),
      labels = c(
        "Minimalismus",
        "HipHopActiveWear",
        "HippieBoho"
      )
    ),
    
    Condition = factor(
      Condition,
      levels = c(
        "Jacke-stark-dezentral",
        "Jacke-leicht-dezentral",
        "Schuh-stark-dezentral",
        "Jacke-stark-zentral",
        "Min-Baseline"
      ),
      labels = c(
        "Jacke stark\ndezentral",
        "Jacke leicht\ndezentral",
        "Schuh stark\ndezentral",
        "Jacke stark\nzentral",
        "Minimalistische\nBaseline"
      )
    )
  )

# Mittelwerte und Standardfehler berechnen
stilzuordnung_summary <- stilzuordnung_long %>%
  group_by(Condition, Stil) %>%
  summarise(
    Mittelwert = mean(Bewertung, na.rm = TRUE),
    SE = sd(Bewertung, na.rm = TRUE) / sqrt(sum(!is.na(Bewertung))),
    .groups = "drop"
  )

# Liniengraph mit Fehlerbalken
zeige_und_speichere_grafik(
  ggplot(
    stilzuordnung_summary,
    aes(
      x = Condition,
      y = Mittelwert,
      color = Stil,
      group = Stil
    )
  ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    geom_errorbar(
      aes(
        ymin = Mittelwert - SE,
        ymax = Mittelwert + SE
      ),
      width = 0.08,
      linewidth = 0.7
    ) +
    scale_y_continuous(
      breaks = seq(1, 5, by = 0.5),
      limits = c(1, 5.2)
    ) +
    scale_color_manual(
      values = c(
        "Minimalismus" = "#8B6F47",
        "HipHopActiveWear" = "#CDAA7D",
        "HippieBoho" = "#EED5B7"
      ),
      labels = c(
        "Minimalismus",
        "Hip Hop / Active Wear",
        "Hippie / Boho"
      )
    ) +
    labs(
      title = "Überprüfung der wahrgenommenen Stilzuordnung",
      x = NULL,
      y = "Stilbewertung (trifft nicht zu 1 – 5 trifft zu)",
      color = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5
      ),
      legend.position = "top"
    ),
  "Manipulationscheck der wahrgenommenen Stilzuordnung"
)




#### Mediation mit Stilistischer Inkonsistenz als Mediator ##################################################















# am Ende entfernen
###############################################################################################################
#### Alle berechneten p-Werte speichern #######################################################################
###############################################################################################################

# Die Tabelle ist bereits in Berechnungsreihenfolge aufgebaut.
# Es wird hier ausdrücklich NICHT mehr mit ls() nach Objektnamen gesucht,
# da ls() alphabetisch sortieren würde.
print(p_werte_tabelle)

write.csv2(
  p_werte_tabelle,
  "p_Werte_gesamt.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)