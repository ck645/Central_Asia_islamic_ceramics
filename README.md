# Central_Asia_islamic_ceramics

This repository contains the data and scripts used in the manuscript:

Klesner, C.E., Ilyasova, S.R., Ilyasov, J., Torgoev, A., Parchuto, V., Mirzaakhmedov, J. K., Mirzaakhmedov, S. J., MacDonald, B.L., and Stark, S. Production of Glazed Ceramics across Mā Warāʾ an-Nahr: a compositional analysis

The repository is organised into four main directories: 

***Scripts*** ... Contains R scripts for analysing the data in this study

***Data*** ... Contains the raw compositional data for this study (csv files) as well as all summary data outputs generated in the analysis

***Figures*** ... Contains all Figures generated from the R scripts as well as figures included in the manuscript prepared by other methods

***Supplementary Materials*** ... Contains supplementary data and reports for the manuscripts referenced as such throughout the text

## Overview

The script **Bukhara_and_Tashkent_paste_analysis.R** performs the following tasks:

*Data Import and Cleaning* the raw compositional data from **paste_compositional_data.csv** obtained through Neutron Activation Analysis at the University of Missouri Research Reactor. The samples' 'NAA Group' assignments were initially determined through analysis of 31 elements, using biplots, principal component analysis (PCA), hierarchical clustering, and group membership validation through Mahalanobis distance calculations based on principal components. Squared-mean Euclidean Distance (ED) calculations for determining the nearest neighbors in NAA databases were also performed. These analyses were conducted using the statistical software Gauss 8.0, relying on the MURRAP statistical regimes created by Michael Glascock (version date: Nov 14, 2022). The results of these regimes are summarized in **Supplementary Material 1. NAA Compositional data**.

*Statistical Analysis* including ... summary Statistics for each compositional group, saved as **Data/NAA_group_stats.csv** and Pairwise t-tests for each element between different compositional groups to identify statistically significant differences in means, saved as **Data/pairwise_t_test_results.txt**

*Data Visualization* of the relationships between elements for each compositional groups, a summary compositonal biplot (Figure3.jpg) and pairwise biplots within **Table_4**, the latter of which were selected after examining pairwise scatterplot matrices between compositional groups contained in **Supplementary_materials/supplementary_materials_pairwise_comparisons**. 


The script **Bukhara_and_Tashkent_glaze_analysis.R** performs the following tasks: 



## File Structure

### Analysis
Bukhara_and_Tashkent_paste_analysis.R
R script for paste data processing, statistical analysis, and visualization.

paste_compositional_data.csv
Raw compositional data obtained from NAA analysis at the University of Missouri Research Reactor.

### Data
Contains output files including summary statistics and statistical test results:

NAA_group_stats.csv – Summary statistics for each compositional group.

pairwise_t_test_results.txt – Results of pairwise t-tests for each element.

significant_df.csv – List of elements with significant differences between groups.

### Figures
Contains generated visualizations:

Figure3.jpg – Biplot visualizing Fe vs. La for selected compositional groups.

Figure3.png – PNG version of the biplot (same as Figure3.jpg).

Table_4/ – Contains pairwise scatterplot matrices and additional biplots.

### Supplementary_materials/
Contains supplementary analysis files, including data generated through R:

supplementary_materials_pairwise_comparisons/ – Contains pairwise comparison scatterplot matrices.

and data obtained through other methods: 




## R Session Info



## Funding 

This project has received funding from the the Institute for the Study of the Ancient World (ISAW) at New York University, and the National Science Foundation (Grant number 2208558).
