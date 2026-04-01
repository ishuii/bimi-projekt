# Projekt: Clusterverfahren
Thema: Semantische Integration medizinischer Daten \
Treffen: (quasi) jeden Freitag, 14:55 bis 16:30 Uhr

Hier noch eine Seite, die eine hilfreiche Anleitung für die gemeinsame Nutzung von Git, 
GitHub und RStudio beinhaltet: https://happygitwithr.com

## Projektumfang
- GUI (Adrika, Andreas, Johanna)
  - Auswahl: Parameter, Datensätze (Messungen, Patienten), Farbpaletten
  - Einlesen von Datensätzen, Sichern von Einstellungen, Grafikexport (PDF), Dropdown für Merkmalsselektion
- Grafikpanel (mit eigener Heatmap, keine fertige nutzen) (Saliha, Domi, Johanna)
  - Darstellung: Heatmap, Dendrogramme, Beschriftungen (Patienten-ID, Messungen), Farbpaletten
  - Eisen-Paper mit Hinweisen zur klassischen Darstellung
- Clusteralgorithmen (Wiki, Alisa, Rosina)
  - soll verschiedene Arten von Clusterung zulassen
  - agglomerative Algorithmen: Single-Linkage, Average-Linkage, Complete-Linkage, (agglomerativer Basisalgorithmus)
  - Lance-Paper
  - Funktion zum Einlesen der Daten + die Erkennung der Merkmalgruppen
  
### Sonstige Aufgaben
- Testen (mind. 4 Wochen)
- Dokumentation (auch im Code)
- Bericht (20 Seiten)
  - theoretische Erklärung des Algorithmus
  - warum ist das interessant?
  - biologische Hintergründe
  - Vorgehen erläutern
- Vortrag (10 Minuten pro Gruppe)

*Empfehlung*: funktionierender Prototyp, der vier Wochen vor Abgabe fertig ist

## Ordnerstruktur
```
bimi-projekt/
│
├── app/                      # GUI Team (das ist die Shiny App)
│   ├── ui.R
│   ├── server.R
│   └── app.R              
│
├── R/                        # geteilte Logik
│   ├── clustering/           # Cluster Team
│       ├── single_linkage.R 
│       └── etc...     
│   ├── visualization/        # Design Team
│       ├── heatmap.R 
│       └── etc...  
│   └── utils/                # für kleine Hilfsfunktionen, die von beiden Teams geteilt werden
│
├── tests/                    # Ordner für Tests
│
├── data/                    # Ordner für Datensätze
│
├── docs/                     # Dokumentation
│   ├── workflow.md           # beinhaltet einen Workflow für das Arbeiten mit Git
│   └── name_directory.md     # Verzeichnis der Funktionen und Variablen        
|
├── README.md
└── .gitignore               
```

