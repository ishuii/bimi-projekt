Hier sollen alle Funktionen und Variablen abgelegt werden, inkl. Input und Output Formaten, sowie einer kurzen Erklärung, um was es sich handelt. 
Dies ist v.a. relevant für das Outputformat der Clusteranalyse und den Input für die Visualisierungen. 

### Generell
Es ist nun eine renv-Datei vorhanden, die dafür sorgt, dass wir alle die gleichen Versionen von Paketen verwenden. Jedre muss die folgenden Befehle in der Console ausführen (einmalig): 
```
install.packages("renv")
renv::restore()
```
Wenn ein neues Paket hinzugefügt wird, d.h. man ruft library(irgendwas) auf, dann muss in der Console ausgeführt werden: 
```
renv::snapshot()
```
Dann muss die Datein renv.lock auch committed werden (und natürlich gepusht). \\
WICHTIG: Nun sollte ``` renv::restore()``` regelmäßig nach einem pull durchgeführt werden, VOR ALLEM, wenn renv verändert wurde (d.h. es hat jemand ein neues Paket hinzugefügt). 

### Clustering

#### dist_cpp
Berechnet eine Distanzmatrix aus einem Datensatz. Code in C++ implementiert über ein eigenes Repo. 
```
# pak::pak("ishuii/bimi-projekt-distance")
library(distRcpp)
dist_cpp(df, method, p)
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
```
####  preprocess_dataset_meta
Bearbeitet den geladenen Datensatz, welcher IDs enthält und gibt einmal Metadaten und den Datensatz ohne Metadaten zurück. Der Datensatz enthält hierbei in der ersten Spalte die Entrez IDs. Außerdem wird die Spalte in integer umgewandelt 
```
preprocess_dataset_meta(data)
# data: der Datensatz, welcher über read.csv geladen wird
# output: Liste (Dataframe ohne Metadaten, Dataframe nur Metadaten)
```
#### preprocess_dataset_meta_gennames
Bearbeitet den geladenen Datensatz, welcher Gennamen enthält und gibt einmal Metadaten und den Datensatz ohne Metadaten zurück. Die Gennamen werden hier in IDs umgewandelt und die erste Spalte ebenfalls in Entrez ID umbenannt. Außerdem wird die Spalte in integer umgewandelt 

preprocess_dataset_meta_gennames(data,con)
data: Der Datensatz, der über read.csv geladen wird
con: Datenbank Connection Objekt
output: Liste (Dataframe ohne Metadaten, Dataframe nur Metadaten)

#### get_chosen_gennames_from_database
Gibt die Gennamen zurück, welche den entsprechend gewählten IDs entsprechen

get_chosen_gennames_from_database(con, entrez_ids)
con: Datenbank Objekt 
entrez_ids: Vektor von Entrez IDs in integer

#### get_chosen_IDs_from_database

#### get_pathwaynames_from_database

#### get_genes_for_pathways

#### extract_relevant_genes

#### rename_duplikate_genes

#### run_data_integration 
Funktion welche alle Hilfsfunktionen unter einen Hut bringt und eine benannte Liste zurückgibt. Auf die mit $ zugegriffen werden kann 





