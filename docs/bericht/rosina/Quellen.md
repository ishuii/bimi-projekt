Quellen für Normalization funktionen

Normalisierung grundsätzlich:
The idea behind normalization is to reduce any kind of noise and source of errors that pull away the results from the true value, i.e., bias, and to preserve only the biological variability. Some factors of uncontrolled variability can come from sample handling, changing conditions such as slight fluctuations in temperature, sample degradation, technical variability linked to instrumentation, among others. Generally, those variations are not easy to measure nor to quantify, which increases the difficulty of proper normalization (Mertens, 2017; De Livera et al., 2015).
https://www.sciencedirect.com/science/article/pii/S0303264722000533

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

log2 median-centering:

https://www.sciencedirect.com/science/article/pii/S0303264722000533
In particular, quantile sample normalization, RUV, mean and median centering showed very good performances, while quantile protein normalization provided worse results than those obtained with unnormalized data.

The aim of the mean/median normalization is to center the data to the mean/median of the distribution of each sample (Valikangas et al., 2018; Callister et al., 2006) as follows:
(7)
where 
 represents the value of the protein j in the ith sample, 
 the mean or median of all protein value sin the ith sample j and 
 the normalized value of protein j in sample i.
Generally, centering by the median is preferred over the mean as the median is more robust against outliers.
Etienne Dubois, Antonio Núñez Galindo, Loïc Dayon, Ornella Cominetti,
Assessing normalization methods in mass spectrometry-based proteome profiling of clinical samples,
Biosystems,
Volumes 215–216,
2022,
104661,
ISSN 0303-2647,
https://doi.org/10.1016/j.biosystems.2022.104661.
(https://www.sciencedirect.com/science/article/pii/S0303264722000533)


log2 MAD:

https://pmc.ncbi.nlm.nih.gov/articles/PMC7641762/
Bhuva, D. D., Cursons, J., & Davis, M. J. (2020). Stable gene expression for normalisation and single-sample scoring. Nucleic acids research, 48(19), e113. https://doi.org/10.1093/nar/gkaa802
erklärt MAD sogar mit log2, sehr gut
