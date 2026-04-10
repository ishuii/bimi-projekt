# Infos für die Visualisierung

### Ansatz:
- Wir erstellen Funktionen bspw. create_cluster_heatmap, create_dendrogram, etc..
- GUI Team kriegt von uns Funktionen und baut diese in die Oberfläche mit ein
- Vorgefertige Funktionen wären heatmap() und as.dendrogram()
- Selbst schreiben:
    - Aufteilung in Heatmap, Dendrogramm, Farbpalette, Panel
  
### Benötigte Datenformate:
- Wir benötigen
    - **Datenmatrix** ODER **Dataframe** -> Heatmap
    - **Baumstruktur bzw. Mergestruktur des Baumes** -> Dendrogramm
        - merge matrix + heigt vector + leaf order 
    - Beispiel: cluster_result <- list(
                    merge = merge_matrix,
                    height = height_vector,
                    order = leaf_order)

### Heatmap:
- normalerweise: heatmap(data, col=cluster_colors)
- Input:
    - Datenmatrix (numerische Werte, Zeilen=Objekte, Spalten=Features)  ODER Dataframe, welches zu einer MAtrix konvertiert werden kann
    - Reiehnfolge der Zeilen aus dem Clustering
    - Farbpalette
- muss nach Clusterreihenfolge sortiert werden -> Dendrogramm stimmt sonst nicht überein
  
### Output:
- **Visualisierungsfunktionen** (entweder zusammengefasst als eine Funktion oder zwei/drei unterschiedliche Funktionen)
    - im Falle einer Funktion: Nutzung des Clusterings -> Sortierung der Heatmap -> Zeichnen des Dendrogramms -> durch Layout kombinieren
    - im Falle einzelner: Heatmap, Dandrogramm, Panel
 
## Wichtige Fragen an Algorithmus Team:
1. Welche Struktur liefert der Algorithmus? Merge, Height, Order oder Baumstruktur?
2. Was würde für das GUI Team mehr Sinn ergeben? Eine finale Visualisierungsfunktion oder drei einzelne kleine Funktionen?
  
