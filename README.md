# Central_Asia_islamic_ceramics

This repository contains the data and scripts used in the manuscript:

Klesner, C.E., Ilyasova, S.R., Ilyasov, J., Torgoev, A., Parchuto, V., Mirzaakhmedov, J. K., Mirzaakhmedov, S. J., MacDonald, B.L., and Stark, S. Production of Glazed Ceramics across Mā Warāʾ an-Nahr: a compositional analysis

The repository is organised into four main directories: 

***Scripts*** ... Contains R scripts for analysing the data in this study

***Data*** ... Contains the raw compositional data for this study (csv files) as well as all summary data outputs generated in the analysis

***Figures*** ... Contains all Figures generated from the R scripts as well as figures included in the manuscript prepared by other methods

***Supplementary Materials*** ... Contains supplementary data and reports for the manuscripts referenced as such throughout the text

## Overview

The script [**Bukhara_and_Tashkent_paste_analysis.R**](Scripts/Bukhara_and_Tashkent_paste_analysis.R) performs the following tasks:

*Data Import and Cleaning* the raw compositional data from [**paste_compositional_data**](Data/paste_compositional_data.csv) obtained through Neutron Activation Analysis at the University of Missouri Research Reactor. The samples' 'NAA Group' assignments were initially determined through analysis of 31 elements, using biplots, principal component analysis (PCA), hierarchical clustering, and group membership validation through Mahalanobis distance calculations based on principal components. Squared-mean Euclidean Distance (ED) calculations for determining the nearest neighbors in NAA databases were also performed. These analyses were conducted using the statistical software Gauss 8.0, relying on the [MURRAP statistical regimes](https://archaeometry.missouri.edu/gauss.html) created by Michael Glascock (version date: Nov 14, 2022). The results of these regimes are summarized in [**Supplementary Material 1. NAA Compositional data**](Supplementary_materials/Supplementary_Material_1_20NAA_Compositional_data.xlsx).

*Statistical Analysis* including summary Statistics for each compositional group, saved as [**NAA_group_stats.csv**](Data/NAA_group_stats.csv) and Pairwise t-tests for each element between different compositional groups to identify statistically significant differences in means, saved as [**Data/pairwise_t_test_results.txt**](Data/pairwise_t_test_results.txt)

*Data Visualization* of the relationships between elements for each compositional groups, a summary compositonal biplot ([**Figure3.jpg**](Figures/Figure3.jpg)) and pairwise biplots within [**Table_4**](Figures/Table_4), the latter of which were selected after examining pairwise scatterplot matrices between compositional groups contained in [**Supplementary_materials/supplementary_materials_pairwise_comparisons**](Supplementary_materials/supplementary_materials_pairwise_comparisons). 


The script [**Bukhara_and_Tashkent_glaze_analysis.R**](Scripts/Bukhara_and_Tashkent_glaze_analysis.R)  performs the following tasks: 

*Data Import and Cleaning* the raw glaze and slip compositional datasets ([**glaze_data.csv**](Data/glaze_data.csv)) and [**slip_data.csv**](Data/slip_data.csv)) generated through SEM-EDS analysis, which are subsequently cleaned. Comparative datasets are compiled from multiple site-level CSV files (found in [**Data/comparative_EDS_data**](Data/comparative_EDS_data)), compiled into a single dataset for comparative analysis, which is saved as [**Comp_Glaze.csv**](Data/Comp_Glaze.csv), [**Comp_Slip.csv**](Data/Comp_Slip.csv), and [**Comp_colored_Slips.csv**](Data/Comp_colored_Slips.csv) for subsequent analysis. Normalized oxide data (excluding lead oxide and colorants) were also generated to compare slip and glaze compositions on the same vessels.

*Statistical Analysis* including generation of summary statistics of glaze and slip types, including the distribution of transparent and opaque glaze technologies by site and ware type. 

*Data Visualization* of glaze types using Ternary diagrams ([**Figure4.jpg**](Figures/Figure4.jpg) and those found in [**supplementary_figures**](Supplementary_materials/supplementary_figures)), and a PCA applied to major glaze oxides for transparent glazes to assess technological variability across sites, ware types, and provenance groups identified in this study ([**Figure9a.jpg**](Figures/Figure9a.jpg) and [**Figure9b.jpg**](Figures/Figure9b.jpg)). Enrichment-Depletion plots of normalised glaze and slip concentrations were generated to visualize technological relationships between slip and glaze layers ([**Figure7a.jpg**](Figures/Figure7a.jpg) and [**Figure7b.jpg**](Figures/Figure7b.jpg)), as well as slip compositional data ([**Figure7c.jpg**](Figures/Figure7c.jpg)). 


### Analysis
`Bukhara_and_Tashkent_paste_analysis.R`
R script for paste data processing, statistical analysis, and visualization.

`Bukhara_and_Tashkent_glaze_analysis.R`
R script for glaze and slip data processing, statistical analysis, and visualization.

`paste_compositional_data.csv`
Raw compositional data obtained from NAA analysis at the University of Missouri Research Reactor.

### Data
Contains output files including summary statistics and statistical test results:

`NAA_group_stats.csv` – Summary statistics for each compositional group.

`pairwise_t_test_results.txt` – Results of pairwise t-tests for each element.

`significant_df.csv` – List of elements with significant differences between groups.

`Comp_Glaze.csv` - Compiled normalised compositional data for all glazes in the comparative datasets assessed in this study. 

`Comp_Slip.csv` - Compiled normalised compositional data for all white slips in the comparative datasets assessed in this study. 

`Comp_colored_Slip.csv` - Compiled normalised compositional data for all colored slips in the comparative datasets assessed in this study. 

As well as data files used in the analysis: 

`paste_compositional_data.csv` - Compositional data obtained through Neutron Activation Analysis.

`glaze_data.csv` - Glaze compositional data obtained through electron dispersive spectroscopy.  

`slip_data.csv` - Slip compositional data obtained through electron dispersive spectroscopy. 

`opaque_database.csv`-  Normalised glaze compositional data obtained through electron dispersive spectroscopy for samples in this study as well as comparative material. 

### Figures
Contains generated visualizations:

`Figure3.jpg` – Biplot visualizing Fe vs. La for selected compositional groups.

`Figure3.png` – PNG version of the biplot (same as Figure3.jpg).

`Table_4/` – Contains pairwise scatterplot matrices and additional biplots.

`Figure4.jpg` - Ternary diagram of opaque glaze compositions

`Figure7a.jpg` and `Figure7b.jpg` - Enrichment-depletion plot of glaze compositions, versus underlying white slip compositions, after subtraction of PbO and colorants and normalization to 100 wt% for major elements: SiO2 and Al2O3

`Figure7c.jpg` - Biplot of the measured composition of Al2O3 versus CaO for the white slips identified by their production/recovery site and ware type.   

`Figure9a.jpg` and `Figure9b.jpg` - biplots of the PCA of transparent glaze compositions 

`Figure10_a.jpg` - Biplot of the measured alkali versus PbO concentration for the glazes identified by their producttion group and ware type, highlighting the splashed ceramics.

As well as those prepared by other means: 

`Figure1.jpg` - Map of sites in this study in relation to other sites with comparative data, prepared in QGIS.

`Figure2.jpg` - Figure showing ths different types of glazes in this study, compiled from images of samples under investigation.

`Figure4_amended.jpg` - Annotated version of generated Figure4.

`Figure5.jpg` - Compiled Backscattered electron (BSE) images obtained from SEM analysis of opaque glazes.

`Figure6.png` - Compiled and annotated images of slipware glazed ceramics obtained through optical and electron microscopy.

`Figure7.jpg` - Compiled version of generated plots.

`Figure8.jpg` - Compiled and annotated Backscattered electron (BSE) images of slipware glazed ceramics show casing different colored slip applications.

`Figure9.jpg` - Compiled version of generated plots.

`Figure10.png` - Compiled generated biplot with the Backscattered electron (BSE) images obtained from SEM analysis of splashed sgraffiato wares.

`Figure11.jpg` - Map of sites in this study showing the circulation of glazed ceramics as determined through paste compositional analysis. 


### Supplementary_materials/
Contains supplementary analysis files, including data generated through R:

`supplementary_materials_pairwise_comparisons/` – Contains pairwise comparison scatterplot matrices.

and data obtained through other methods: 

`Supplementary_Abstracts_Russian_and_Uzbek.pdf` contains the translated abstract of this article into Russian and Uzbek for accessibility.

`Supplementary_Material_0_Sample_Metadata.xlsx` - contains information on the samples understudy, as well as information from publications on samples used in the comparative analyses. 

`Supplementary_Material_1_NAA_Compositional_data.xlsx` - contains the raw paste compositional data obtained by NAA< as well as the resutls of the statistical analyses conducted using the murrap.gcg program in Gauss8.0. 

`Supplementary_Material_2_EDS_Compositional_data.xlsx` - contains the metadata and  processed glaze compositional data obtained from SEM-EDS analysis. 

`Supplementary_Material_3_EDS_performance.docx` - summary document assessing the performance of the EDS. 

`supplementary_figures/` - Contains ternary diagrams showing glaze compositions generated through R as well as the dendrogram showing the resutls of the heirarchical cluster analysis conducted using the murrap.gcg program in Gauss8.0.


## R Session Info

The code was run using R version 4.5.1 (2025-06-13 ucrt)

The following R packages are required to execute the code:

* scales_1.4.0 (Wickham, Pedersen, and Seidel, 2025)

* ggtern_3.5.0  (Wickham and Ferry, 2018)

* ggplot2_3.5.2 (Wickham, 2016)

* dplyr_1.1.4   (Wickham et al., 2023)

* rio_1.2.4  (Chan et al., 2023)  

* GGally_2.4.0  (Schloerke et al., 2025)

* tidyr_1.3.1   (Wickham et al., 2024)

Citations: 

  Chan C, Leeper T, Becker J, Schoch D (2023). _rio: A Swiss-army knife for data file I/O_.
  <https://cran.r-project.org/package=rio>.

  Wickham H, François R, Henry L, Müller K, Vaughan D (2023). _dplyr: A Grammar of Data
  Manipulation_. doi:10.32614/CRAN.package.dplyr <https://doi.org/10.32614/CRAN.package.dplyr>, R
  package version 1.1.4, <https://CRAN.R-project.org/package=dplyr>.

  H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.

  Hamilton NE, Ferry M (2018). “ggtern: Ternary Diagrams Using ggplot2.” _Journal of Statistical
  Software, Code Snippets_, *87*(3), 1-17. doi:10.18637/jss.v087.c03
  <https://doi.org/10.18637/jss.v087.c03>.

  Wickham H, Pedersen T, Seidel D (2025). _scales: Scale Functions for Visualization_.
  doi:10.32614/CRAN.package.scales <https://doi.org/10.32614/CRAN.package.scales>, R package version
  1.4.0, <https://CRAN.R-project.org/package=scales>.

  Schloerke B, Cook D, Larmarange J, Briatte F, Marbach M, Thoen E, Elberg A, Crowley J (2025).
  _GGally: Extension to 'ggplot2'_. doi:10.32614/CRAN.package.GGally
  <https://doi.org/10.32614/CRAN.package.GGally>, R package version 2.4.0,
  <https://CRAN.R-project.org/package=GGally>.

  Wickham H, Vaughan D, Girlich M (2024). _tidyr: Tidy Messy Data_. doi:10.32614/CRAN.package.tidyr
  <https://doi.org/10.32614/CRAN.package.tidyr>, R package version 1.3.1,
  <https://CRAN.R-project.org/package=tidyr>.



## Funding 

This project has received funding from the the Institute for the Study of the Ancient World (ISAW) at New York University, and the National Science Foundation (Grant number 2208558).
