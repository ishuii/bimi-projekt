# read.csv like in Andreas' server-function

df <- read.csv("data/TCGA_kidney_unnormalized.csv", header = TRUE, 
               stringsAsFactors = FALSE, na.strings = c("", " ", "NA", "NaN", "NULL", "N/A"))


df[df == ""] <- NA

na_gesamt <- sum(is.na(df))
zeilen_mit_na <- !complete.cases(df)
anzahl_zeilen_mit_na <- sum(zeilen_mit_na)
na_pro_spalte <- colSums(is.na(df))


res <- list(na_gesamt = na_gesamt, zeilen_mit_na = anzahl_zeilen_mit_na, zeilen_gesamt = nrow(df),
     spalten_gesamt = ncol(df), na_pro_spalte = na_pro_spalte, bereits_bereinigt = FALSE)
