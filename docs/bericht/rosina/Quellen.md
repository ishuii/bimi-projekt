Quellen für Normalization funktionen

log2 transformation:

https://www.researchgate.net/publication/264500976_Log-transformation_and_its_implications_for_data_analysis
@article{article,
author = {Feng, Changyong and Hongyue, Wang and Lu, Naiji and Chen, Tian and He, Hua and Lu, Ying and Tu, Xin},
year = {2014},
month = {04},
pages = {105-9},
title = {Log-transformation and its implications for data analysis},
volume = {26},
journal = {Shanghai archives of psychiatry},
doi = {10.3969/j.issn.1002-0829.2014.02.009}
}

log2 und zscore (nicht zusammen):

https://pmc.ncbi.nlm.nih.gov/articles/PMC12235674/
Deng, F., Feng, C. H., Gao, N., & Zhang, L. (2025). Normalization and Selecting Non-Differentially Expressed Genes Improve Machine Learning Modelling of Cross-Platform Transcriptomic Data. Transactions on artificial intelligence, 1(1), 5. https://doi.org/10.53941/tai.2025.100005

log2 und zscore zusammen:

https://link.springer.com/article/10.1186/s13059-021-02337-8?utm_source=chatgpt.com
Methods like hierarchical clustering, k-means clustering, and self-organizing maps can be used to identify clusters of coordinately regulated genes with similar expression patterns [107, 108] (Fig. 4). The representative expression pattern for each of these clusters can be identified by taking the average of the z-score of the log-transformed expression values for each of the sample. The z-score is the number of standard deviations that a value for a given gene in a given sample is away from the mean of all the values for all the samples for the same gene. A z-score of -2 means that this value is 2 standard deviations lower than the mean across all the samples. It is an effective tool for normalizing prior to visualization particularly when there is not a clear reference sample. When a reference sample is available that all samples are compared to, the log-fold change can be shown relative to the reference.
Chung, M., Bruno, V.M., Rasko, D.A. et al. Best practices on the differential expression analysis of multi-species RNA-seq. Genome Biol 22, 121 (2021). https://doi.org/10.1186/s13059-021-02337-8
