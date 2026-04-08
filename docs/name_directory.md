Hier sollen alle Funktionen und Variablen abgelegt werden, inkl. Input und Output Formaten, sowie einer kurzen Erklärung, um was es sich handelt. 
Dies ist v.a. relevant für das Outputformat der Clusteranalyse und den Input für die Visualisierungen. 

### Clustering

#### dist
Berechnet eine Distanzmatrix aus einem Datensatz. Für den Fall, dass wir die bereits existierende dist-Funktion aus stats nicht nutzen dürfen. 
```
dist(data_set, method)
# data_set ist ein Datensatz als dataframe
# method ist die Methode für die Berechnung von Distanzen, übergeben als String. Bislang existieren nur "euclidian" und "manhattan"
# bei Bedarf werden weitere Distanzmetriken ergänzt werden

# Beispielanwendung:
dist_mat <- dist(t(df_normalized), method = "euclidean")
```

#### single_linkage
Führt eine Clusteranalyse nach der Single-Linkage Methode durch. 
```
single_linkage(dist_mat, initial_clusters)
# dist_mat: Distanzmatrix, hier das Ergebnis der dist-Funktion
# initial_clusters: Vektor, der die ursprünglichen Cluster enthält. Am Anfang steht jede Spalte für ein eigenes Cluster. Daher:
initial_clusters <- c(1:ncol(df))

# output: eine Liste mit zwei Elementen, nämlich...
# ... matched_at: ein Vektor, der die Werte enthält, an denen die Cluster zusammengefügt wurden (hierbei immer die minimale Distanz an jedem Schritt)
# ... cluster_history: eine Liste aus Listen von Vektoren. Jede Vektorliste repräsentiert hier den Stand der Cluster an jedem Schritt. Daher ist initial_clusters das erste Element dieser Liste. Das letzte Element sollte nur einen Vektor enthalten (nur noch ein großes Cluster übrig).

# Anwendung:
cluster_res <- single_linkage(dist_mat, initial_clusters)
# Ergebnis wird in cluster_res gespeichert
