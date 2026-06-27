## Code-Regeln
- Code-Funktionen sollen einen englischen Namen haben
- wir nutzen Snake-Style, d.h. single_linkage (Trennung mit Unterstrichen) und nicht z.B. singleLinkage
- Code-Dokumentation sollte auf Englisch geschrieben sein

## Benötigte Parameter für die Implementierung der Visualisierungs-Funktionen
-  **Merge-Matrix / Merge-Liste**
    - normalerweise arbeitet R auf einer Merge-Matrix mit folgendem Layout:
    [1]  -1  -2    -> Originalelement 1 und 2 werden gemerged => Cluster 1
    [2]  -3  -4    -> Originalelement 3 und 4 werden gemerged => Cluster 2
    [3]   1   2    -> Cluster 1(Zeile 1) + Cluster 2(Zeile 2) => Cluster 3 entsteht

    
        - negative Zahlen beschreiben dabei Originalemente
        - positive Zahlen stellen die sich ergebenden Cluster dar
        - Zeilen werden von oben nach unten abgearbeitet

    - für unser Projekt wäre auch eine Merge-Liste denkbar -> übersichtlicher, lesbarer
      - *merge = [
      (1, 2),   # → Cluster 6
      (3, 4),   # → Cluster 7
      (6, 5),   # → Cluster 8
      (7, 8)    # → Cluster 9 (Wurzel)]*

      - ABER:
        - in dem Fall müssen klare Regeln definiert werden wie die IDs der Originalelemente und Cluster vergeben werden
             - bspw. Originalelemente: 1 - N , Cluster: N+1, N+2, ...
        - klare,feste Regeln für den Aufbau der Merges und den Aufbau des Baumes
          => Vislualisierungsteam muss genau wissen, welches Cluster nach links und welches Cluster nach rechts kommt


-  **Order-Vektor**
    - Liste von Indizes, die die Reihenfolge der Zeilen und Spalten vorgibt, die angezeigt werden soll
    - Reihenfolge der Blätter im Baum = Order-Vector
    - Durch die Order kann die Matrix dann in der richtigen Reihenfolge sortiert werden
    - Benötigt werden **zwei Order-Vektoren**: *row_order* , *col_order*

-  **Height-Vektor**
    - Distanzen des Merges werden in diesen Vektor geschrieben
 
## Distanzfunktionen - Abfrage in GUI
Es stehen sechs Distanzfunktionen zur Auswahl: "minkowski", "euclidean", "manhattan", "canberra", "pearson" und "angular".

#### Besonderheiten bei "minkowski"
- die Minkowski-Distanz erfodert die Eingabe eines Parameters p
- p muss ein Integer > 0 sein
- d.h. Werte 0 oder kleiner, sowie nicht-Integer (z.B. 1.5) dürfen nicht eingegeben werden
- Sonderfall p = 1
  - hat der Nutzer 1 eingegeben, entspricht die Minkowski-Distanz der Manhattan-Distanz
  - der Nutzer sollte darüber informiert werden (z.B. ein Info-Feld)
- Sonderfall p = 2
  - hat der Nutzer 2 eingegebem, entspricht die Minkowski-Distanz der euklidischen Distanz
  - der Nutzer sollte darüber informiert werden (z.B. ein Info-Feld)
- Sonderfall p ~ Unendlich, bzw. o ist sehr groß
  - ist p extrem groß, spricht man von der Chebyshev-Distanz
  - bislang noch nicht getestet, aber die Auswahl sollte dem Nutzer ermöglicht werden


## Datenschnittstelle zum GUI-Team

Unter `data/database_functions_v4.r` steht die Funktion `run_data_integration` zur Verfügung. Vor dem Aufruf müssen folgende Voraussetzungen erfüllt sein:

- Die Pakete `RSQLite` und `DBI` müssen installiert und geladen sein
- Eine Datenbankverbindung muss über `dbConnect` hergestellt worden sein
- Der Datensatz muss zunächst durch `preprocess_general` vorverarbeitet und danach übergeben werden
- um bei Auswahl eine Matrix über die Coverage im Datensatz pro Datensatz zu erhalten muss `analyze_pathway_coverage` aufgerufen werden. Daraufhin sollte der User entscheiden können, ob er die Pathway haben will, oder sie doch ändern möchte

**Inputparameter:**
```
run_data_integration(dataset, chosen_pathways, con)
# dataset: vorverarbeiteter Datensatz nach preprocess_general
# chosen_pathways: character Vektor der gewählten Pathway-Namen
# con: Datenbank Connection Objekt
```

**Rückgabe:** benannte Liste mit folgendem Inhalt:
```
# filtered_dataset: gefilterter Datensatz
# meta_data:        Metadaten
# gene_vector:      Vektor der Entrez IDs
# gene_names:       Vektor der Gennamen
# matrix_unused:    Matrix mit Coverage-Informationen pro Pathway
# ids_unused:       Vektor der IDs die nicht im Datensatz vorkommen,
#                   aber zum gewählten Pathway gehören
```

Für die Auswahl der Pathways in der GUI sollte zunächst `get_pathwaynames_from_database(con)` aufgerufen werden. Die Funktion gibt einen character Vektor aller verfügbaren Pathway Namen zurück. Dieser Vektor sollte direkt als Grundlage für die Checkboxen verwendet werden, da so sichergestellt ist, dass die Namen exakt mit den Datenbankeinträgen übereinstimmen. 

Workflow: 
preprocess_general ==> analyze_pathway_coverage ==> run_data_integration