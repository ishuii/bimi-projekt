Hier sollen alle Funktionen und Variablen abgelegt werden, inkl. Input und Output Formaten, sowie einer kurzen Erklärung, um was es sich handelt. 
Dies ist v.a. relevant für das Outputformat der Clusteranalyse und den Input für die Visualisierungen. 

### Clustering

#### dist_cpp
Berechnet eine Distanzmatrix aus einem Datensatz. Code in C++ implementiert über die Library Rcpp. 
```
dist(df, method, p)
# df: der Datensatz, als numerische Matrix
# method ist die Methode für die Berechnung von Distanzen, übergeben als String
# Optionen für method: "euclidean", "manhattan", "minkowski", "canberra", "pearson", "angular"
# p: int Parameter für Minkowski. Default-Wert ist 2 (entspricht der euklidischen Distanz). Für p = 1 wird Manhattan berechnet.

# Beispielanwendung:
dist_mat <- dist_cpp(t(df_normalized), method = "euclidean")
```

#### single_linkage
Führt eine Clusteranalyse nach der Single-Linkage Methode durch. 
```
single_linkage(dist_mat)
# dist_mat: Distanzmatrix, d.h. das Ergebnis der dist_cpp-Funktion

# output: eine Liste mit zwei Elementen, nämlich...
# ... matched_at: ein Vektor, der die Werte enthält, an denen die Cluster zusammengefügt wurden (hierbei immer die minimale Distanz an jedem Schritt)
# ... merge: eine n-1 mal 2 Matrix, die in jeder Zeile die zusammengefügten Cluster-Indexe enthält. Prinzipiell wie bei hclust, allerdings u.U. andere Reihenfolge innerhalb der Zeile. 

# Anwendung:
cluster_res <- single_linkage(dist_mat)
cluster_merge <- cluster_res$merge            # für Zugriff auf merge-Matrix
cluster_height <- cluster_res$matched_at      # für Zugriff auf matched-at Vektor
