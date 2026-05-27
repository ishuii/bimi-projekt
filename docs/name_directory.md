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
#### hierarchical_clustering
Die allgemeine Funktion für die Clusteranalyse.
```
# Input: Distanzmatrix, Clustering-Funktion als String, optional eine benannte Parameter-Liste
hierarchical_clustering(dist_mat, method = "single", custom_params = NULL)

# output: eine Liste mit zwei Elementen, nämlich...
# ... matched_at: ein Vektor, der die Werte enthält, an denen die Cluster zusammengefügt wurden (hierbei immer die minimale Distanz an jedem Schritt)
# ... merge: eine n-1 mal 2 Matrix, die in jeder Zeile die zusammengefügten Cluster-Indexe enthält. Prinzipiell wie bei hclust, allerdings u.U. andere Reihenfolge innerhalb der Zeile. 

# Anwendung:
cluster_res <- hierarchical_clustering(dist_mat, "single")
cluster_merge <- cluster_res$merge            # für Zugriff auf merge-Matrix
cluster_height <- cluster_res$matched_at      # für Zugriff auf matched-at Vektor
```
Hinter ```method``` versteckt sich eine Liste mit vier Elementen: alpha_a, alpha_b, beta, und gamma. Wird demnach eine Funktion wie ```"single"```
eingegeben, so macht sie nichts anderes als eine Liste mit diesen vier Elementen zurückzugeben.\\
Es wäre daher möglich, auch eine Custom-Eingabe zu erlauben, bei der der Nutzer vier Werte eingeben kann: alpha_a, alpha_b, beta, und gamma (die ```custom_params```).
Intern wird dann die Funktion ```custom_linkage``` aufgerufen, die aus den vier Parametern eine geeignete Methode erstellt, die dann genauso wie die anderen Funktionen
verwendet werden kann.

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

#### preprocess_general
Bereinigt den Datensatz indem Zeilen mit fehlenden, leeren oder null Werten in der ersten Spalte entfernt werden. Gibt zusätzlich Statistiken über die entfernten Zeilen zurück.

```
preprocess_general(data)
# data: der Datensatz, welcher über read.csv geladen wird
# output: Liste (Anzahl NAs in erster Spalte, Anzahl Nullen in erster Spalte, Anzahl leere Werte in erster Spalte, Anzahl entfernter Zeilen (wenn die komplette Zeile NA ist), bereinigter Datensatz)
```
#### preprocess_dataset_meta
Bearbeitet den von preprocess_general zurückgegebenen Datensatz, welcher IDs enthält und gibt einmal Metadaten und den Datensatz ohne Metadaten zurück. Der Datensatz enthält hierbei in der ersten Spalte die Entrez IDs. Außerdem wird die Spalte in integer umgewandelt.
```
preprocess_dataset_meta(data)
# data: der Datensatz, welcher über read.csv geladen wird
# output: Liste (Dataframe ohne Metadaten, Dataframe nur Metadaten)
```
#### preprocess_dataset_meta_gennames
Bearbeitet den von preprocess_general zurückgegebenen Datensatz, welcher Gennamen enthält und gibt einmal Metadaten und den Datensatz ohne Metadaten zurück.
```
preprocess_dataset_meta_gennames(data)
# data: der Datensatz, welcher über read.csv geladen wird
# output: Liste (Dataframe ohne Metadaten, Dataframe nur Metadaten)
```
#### get_chosen_gennames_from_database
Gibt die Gennamen zurück, welche den entsprechend gewählten IDs entsprechen. Die Reihenfolge der Eingabe wird beibehalten.
```
get_chosen_gennames_from_database(con, entrez_ids)
# con: Datenbank Connection Objekt
# entrez_ids: Vektor von Entrez IDs als integer
# output: character Vektor der Gennamen
```
#### get_chosen_IDs_from_database
Gibt die Entrez IDs zurück, welche den entsprechend gewählten Gennamen entsprechen. Die Reihenfolge der Eingabe wird beibehalten.
```
get_chosen_IDs_from_database(con, gene_names)
# con: Datenbank Connection Objekt
# gene_names: character Vektor von Gennamen
# output: integer Vektor der Entrez IDs
```
#### get_pathwaynames_from_database
Gibt alle in der Datenbank gespeicherten Pathway Namen zurück. Wird für die Auswahl in der GUI verwendet, sodass die Namen exakt mit den Datenbankeinträgen übereinstimmen.
```
get_pathwaynames_from_database(con)
# con: Datenbank Connection Objekt
# output: character Vektor aller in der Datenbank gespeicherten Pathways
```
#### get_geneIDS_for_pathways
Gibt alle Entrez IDs zurück, die einem oder mehreren gewählten Pathways zugeordnet sind. Duplikate werden entfernt.
```
get_geneIDS_for_pathways(chosen_pathways, con)
# chosen_pathways: character Vektor der gewählten Pathway-Namen
# con: Datenbank Connection Objekt
# output: integer Vektor der Entrez IDs ohne Duplikate
```
#### get_gene_names_for_pathways
Gibt alle Gennamen zurück, die einem oder mehreren gewählten Pathways zugeordnet sind. Duplikate werden entfernt.
```
get_gene_names_for_pathways(chosen_pathways, con)
# chosen_pathways: character Vektor der gewählten Pathway-Namen
# con: Datenbank Connection Objekt
# output: character Vektor eindeutiger Gennamen
```
#### extract_relevant_genes
Filtert den Datensatz und gibt nur die Zeilen zurück, deren erste Spalte den gewählten Genen oder IDs entspricht.
```
extract_relevant_genes(extracted_genes, original_data)
# extracted_genes: Vektor von Entrez IDs oder Gennamen
# original_data: bereinigter Dataframe ohne Metadaten
# output: gefilterter Dataframe
```
#### rename_duplikate_genes
Behandelt doppelte Entrez IDs im Datensatz, indem ein numerisches Suffix angehängt mittels Unterstrich.
```
rename_duplikate_genes(extracted_dataset)
# extracted_dataset: Dataframe mit möglicherweise doppelten Entrez IDs
# output: Dataframe mit eindeutigen Entrez IDs
```
#### analyze_pathways_coverage
Berechnet für jeden gewählten Pathway wie viele der zugehörigen Gene im Datensatz vorhanden sind. Gibt zusätzlich einen Vektor aller fehlenden Entrez IDs zurück. 
```
analyze_pathways_coverage(chosen_pathways, dataset_cleaned, con)
# chosen_pathways: character Vektor der gewählten Pathway Namen in der GUI
# dataset_cleaned: Datensatz, welcher der Output ist von preprocess_general 
# con: Datenbank Connection Objekt
# output: Liste (Matrix mit Total/Found/Missing/Coverage pro Pathway, Vektor fehlender Entrez IDs)
```
#### run_data_integration
Hauptfunktion die alle Schritte kombiniert: Preprocessing, Pathway Filterung, Coverage Analyse und Rückgabe des finalen Datensatzes. Erkennt automatisch ob die erste Spalte Entrez IDs oder Gennamen enthält.
```
run_data_integration(dataset, chosen_pathways, con)
# dataset: der Datensatz, welcher über read.csv geladen wird
# chosen_pathways: character Vektor der gewählten Pathway-Namen
# con: Datenbank Connection Objekt
# output: Liste (gefilterter Datensatz, Metadaten, Gen-Vektor, Gennamen,
#                Coverage-Matrix, Vektor fehlender IDs)  
```
