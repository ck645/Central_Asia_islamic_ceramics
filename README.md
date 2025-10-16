# Central_Asia_islamic_ceramics

This repository contains the data and scripts used in the manuscript:

Klesner, C.E., Ilyasova, S.R., Ilyasov, J., Torgoev, A., Parchuto, V., Mirzaakhmedov, J. K., Mirzaakhmedov, S. J., MacDonald, B.L., and Stark, S. Production of Glazed Ceramics across Mā Warāʾ an-Nahr: a compositional analysis

The repository is organised into four main directories: 

***Scripts*** ... Contains R scripts for analysing the data in this study

***Data*** ... Contains the raw compositional data for this study (csv files) as well as all summary data outputs generated in the analysis

***Figures*** ... Contains all Figures generated from the R scripts as well as figures included in the manuscript prepared by other methods

***Supplementary Materials*** ... Contains supplementary data and reports for the manuscripts referenced as such throughout the text

## Overview

The script [**Bukhara_and_Tashkent_paste_analysis.R**](Script/Bukhara_and_Tashkent_paste_analysis.R) performs the following tasks:

*Data Import and Cleaning* the raw compositional data from [**paste_compositional_data**](Data/paste_compositional_data.csv) obtained through Neutron Activation Analysis at the University of Missouri Research Reactor. The samples' 'NAA Group' assignments were initially determined through analysis of 31 elements, using biplots, principal component analysis (PCA), hierarchical clustering, and group membership validation through Mahalanobis distance calculations based on principal components. Squared-mean Euclidean Distance (ED) calculations for determining the nearest neighbors in NAA databases were also performed. These analyses were conducted using the statistical software Gauss 8.0, relying on the MURRAP statistical regimes created by Michael Glascock (version date: Nov 14, 2022). The results of these regimes are summarized in **Supplementary Material 1. NAA Compositional data**.

*Statistical Analysis* including summary Statistics for each compositional group, saved as [**NAA_group_stats.csv**](Data/NAA_group_stats.csv) and Pairwise t-tests for each element between different compositional groups to identify statistically significant differences in means, saved as [**Data/pairwise_t_test_results.txt**](Data/pairwise_t_test_results.txt)

*Data Visualization* of the relationships between elements for each compositional groups, a summary compositonal biplot ([**Figure3.jpg**](Figures/Figure3.jpg)) and pairwise biplots within [**Table_4**](Figures/Table_4), the latter of which were selected after examining pairwise scatterplot matrices between compositional groups contained in [**Supplementary_materials/supplementary_materials_pairwise_comparisons**](Supplementary_materials/supplementary_materials_pairwise_comparisons). 


The script **Bukhara_and_Tashkent_glaze_analysis.R** performs the following tasks: 

*Data Import and Cleaning* the raw glaze and slip compositional datasets (**glaze_data.csv** and **slip_data.csv**) generated through SEM-EDS analysis, which are subsequently cleaned. Comparative datasets are compiled from multiple site-level CSV files (found in **Data/comparative_EDS_data**), compiled into a single dataset for comparative analysis, which is saved as **Comp_Glaze.csv** and **Comp_Slip.csv** for subsequent analysis. Normalized oxide data (excluding lead oxide and colorants) were also generated to compare slip and glaze compositions on the same vessels.

*Statistical Analysis* including generation of summary statistics of glaze and slip types, including the distribution of transparent and opaque glaze technologies by site and ware type. 

*Data Visualization* of glaze types using Ternary diagrams (**FIgure4.jpg** and those found in **supplementary_figures**), and a PCA applied to major glaze oxides for transparent glazes to assess technological variability across sites, ware types, and provenance groups identified in this study (**Figure6a.jpg** and **Figure6b.jpg**). Enrichment-Depletion plots of normalised glaze and slip concentrations were generated to visualize technological relationships between slip and glaze layers (**Figure9a.jpg** and **Figure9b.jpg**), as well as slip compositional data (**Figure9c.jpg**). 


### Analysis
Bukhara_and_Tashkent_paste_analysis.R
R script for paste data processing, statistical analysis, and visualization.

Bukhara_and_Tashkent_glaze_analysis.R
R script for glaze and slip data processing, statistical analysis, and visualization.

paste_compositional_data.csv
Raw compositional data obtained from NAA analysis at the University of Missouri Research Reactor.

### Data
Contains output files including summary statistics and statistical test results:

NAA_group_stats.csv – Summary statistics for each compositional group.

pairwise_t_test_results.txt – Results of pairwise t-tests for each element.

significant_df.csv – List of elements with significant differences between groups.

Comp_Glaze.csv - Compiled normalised compositional data for all glazes in the comparative datasets assessed in this study. 

Comp_Slip.csv - Compiled normalised compositional data for all slips in the comparative datasets assessed in this study. 


### Figures
Contains generated visualizations:

Figure3.jpg – Biplot visualizing Fe vs. La for selected compositional groups.

Figure3.png – PNG version of the biplot (same as Figure3.jpg).

Table_4/ – Contains pairwise scatterplot matrices and additional biplots.

Figure4.jpg - Ternary diagram of opaque glaze compositions

Figure6a.jpg and Figure6b.jpg - biplots of the PCA of transparent glaze compositions 

Figure9a.jpg and Figure9b.jpg - Enrichment-depletion plot of glaze compositions, versus underlying white slip compositions, after subtraction of PbO and colorants and normalization to 100 wt% for major elements: SiO2 and Al2O3

Figure9c.jpg - Biplot of the measured composition of Al2O3 versus CaO for the white slips identified by their produciton/recovery site and ware type.   

As well as those prepared by other means: 

Figure1.jpg - Map of sites in this study in relation to other sites with comparative data, prepared in QGIS.

Figure2.jpg - Figure showing ths different types of glazes in this study, compiled from images of samples under investigation.

Figure4_amended.jpg - Annotated version of generated Figure4.

Figure5.jpg - Compiled Backscattered electron (BSE) images obtained from SEM analysis of opaque glazes.

Figure6.jpg - Compiled version of generated plots.

Figure7.jpg - Compiled Backscattered electron (BSE) images obtained from SEM analysis of splashed sgraffiato wares.

Figure8.jpg - Compiled and annotated images of Samanid-style glazed ceramics obtained through optical and electron microscopy.

Figure9.jpg - Compiled version of generated plots.

Figure10.jpg - Map of sites in this study showing the circulation of glazed ceramics as determined through paste compositional analysis. 


### Supplementary_materials/
Contains supplementary analysis files, including data generated through R:

supplementary_materials_pairwise_comparisons/ – Contains pairwise comparison scatterplot matrices.

and data obtained through other methods: 

Supplementary Abstracts_Russian and Uzbek.pdf contains the translated abstract of this article into Russian and Uzbek for accessibility.

Supplementary Material 0. Sample Metadata.xlsx - contains information on the samples understudy, as well as information from publications on samples used in the comparative analyses. 

Supplementary Material 1. NAA Compositional data.xlsx - contains the raw paste compositional data obtained by NAA< as well as the resutls of the statistical analyses conducted using the murrap.gcg program in Gauss8.0. 

Supplementary Material 2. EDS Compositional data.xlsx - contains the metadata and  processed glaze compositional data obtained from SEM-EDS analysis. 

Supplementary Material 3. EDS performance.docs - summary document assessing the performance of the EDS. 

supplementary_figures/ - Contains ternary diagrams showing glaze compositions generated through R as well as the dendrogram showing the resutls of the heirarchical cluster analysis conducted using the murrap.gcg program in Gauss8.0.


## R Session Info



## Funding 

This project has received funding from the the Institute for the Study of the Ancient World (ISAW) at New York University, and the National Science Foundation (Grant number 2208558).
