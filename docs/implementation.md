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
