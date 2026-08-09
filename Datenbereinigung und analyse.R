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


###############################################################################################################
#### Automatisches Speichern der Grafiken #####################################################################
###############################################################################################################

# Alle im Skript erzeugten Grafiken werden zusätzlich als PNG gespeichert.
# Die laufende Nummer verhindert, dass gleich benannte Grafiken überschrieben werden.
grafik_ordner <- "Grafiken"
dir.create(grafik_ordner, showWarnings = FALSE, recursive = TRUE)

grafik_counter <- 0

speichere_grafik <- function(plot, name = "Grafik") {
  grafik_counter <<- grafik_counter + 1
  
  sicherer_name <- gsub(
    "[^A-Za-z0-9ÄÖÜäöüß_-]+",
    "_",
    name
  )
  
  dateiname <- file.path(
    grafik_ordner,
    sprintf(
      "%03d_%s.png",
      grafik_counter,
      sicherer_name
    )
  )
  
  ggsave(
    filename = dateiname,
    plot = plot,
    width = 10,
    height = 7,
    dpi = 300
  )
  
  invisible(plot)
}

# Für Grafiken, die im ursprünglichen Code direkt erzeugt und angezeigt werden,
# wird die Grafik gespeichert und anschließend wie bisher als Ergebnis zurückgegeben.
zeige_und_speichere_grafik <- function(plot, name = "Grafik") {
  speichere_grafik(plot, name)
  plot
}


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
      title = "Altersverteilung der Stichprobe",
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
      title = "Geschlechterverteilung der Stichprobe",
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
      title = "Einkommensverteilung der Stichprobe",
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
      title = "Tätigkeitsverteilung der Stichprobe",
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
  select(
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
  select(
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
      title = "Altersverteilung der Stichprobe",
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
      title = "Geschlechterverteilung der Stichprobe",
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
      title = "Einkommensverteilung der Stichprobe",
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
      title = "Tätigkeitsverteilung der Stichprobe",
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
  select(
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
      "Min-Baseline" = "#EEC591",
      "Jacke-leicht-dezentral" = "#FFE4C4",
      "Jacke-stark-dezentral" = "#CD853F",
      "Jacke-stark-zentral" = "#8B7355",
      "Schuh-stark-dezentral" = "#8B4513")) +
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
