# Infos für die Visualisierung

### Ansatz:
- Wir erstellen Funktionen bspw. create_cluster_heatmap, create_dendrogram, etc..
- GUI Team kriegt von uns Funktionen und baut diese in die Oberfläche mit ein
- Vorgefertige Funktionen wären heatmap() und as.dendrogram()
- Selbst schreiben:
    - Aufteilung in Heatmap, Dendrogramm, Farbpalette, Panel
  
### Benötigte Datenformate:
- Wir benötigen
    - **Datenmatrix** ODER **Dataframe** -> Heatmap (wird von Herr Lausser ja schon bereit gestellt)
    - **Baumstruktur bzw. Mergestruktur des Baumes** -> Dendrogramm  (Clusterstruktur)
        - merge matrix + heigt vector + leaf order + (optional: labels)
        - für jeden Algorithmus benötigen wir eine Chlusterstruktur im selben Format
    - Beispiel: cluster_result <- list(
                    merge = merge_matrix,
                    height = height_vector,
                    order = leaf_order)    -> **Cluster-Objekt**

### Heatmap:
- normalerweise: heatmap(data, col=cluster_colors)
- Input:
    - Datenmatrix (numerische Werte, Zeilen=Objekte, Spalten=Features)  ODER Dataframe, welches zu einer MAtrix konvertiert werden kann
    - Reihenfolge der Zeilen aus dem Clustering
    - Farbpalette
- muss nach Clusterreihenfolge sortiert werden -> Dendrogramm stimmt sonst nicht überein
  
### Output:
- **Visualisierungsfunktionen** (entweder zusammengefasst als eine Funktion oder zwei/drei unterschiedliche Funktionen)
    - im Falle einer Funktion: Nutzung des Clusterings -> Sortierung der Heatmap -> Zeichnen des Dendrogramms -> durch Layout kombinieren
    - im Falle einzelner: Heatmap, Dandrogramm, Panel
 
## Wichtige Fragen an das restliche Team:
1. Welche Datenstruktur liefert der Algorithmus?
   1) merge matrix, height vector und leaf order 
   2) eine Baumstruktur (Nodes mit left/right children und height)?

2. Könnt ihr euer Ergebnis optional als hclust-ähnliche Struktur liefern?
3. Ist die Matrix schon sortiert oder nicht

4. war tolll
  
