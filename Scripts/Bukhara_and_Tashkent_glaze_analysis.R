##Uzbekistan glazed ceramics - Glaze and Slip analysis
# C. Klesner
# 2025

install.packages(c("rio", "dplyr", "ggplot2", "ggtern", "scales"))

library(rio)
library(dplyr)
library(ggplot2)
library(ggtern)
library(scales)

shape_values <- c(18, 15, 1, 16, 2, 0, 3, 17, 4, 5, 6)

Group_colors <- c(
  "BUK" = "#CC6677", "BUK A" = "#332288", "Outlier" = "#000000", 
  "NISH" = "#DAA520", "PAY 1" = "#999933", "PAY 2" = "#88CCEE", 
  "PAY 3" = "#AA4499", "SAMK" = "#882255", "TASH" = "#44AA99", 
  "TAZ - Group 3" = "#117733", "unassigned" = "#bbbbbb"
)

#--Importing and cleaning Glaze compositional analysis generated in this project----

# Importing the dataset
glaze_database <- rio::import("./Data/glaze_data.csv")
slip_database <-  rio::import("./Data/slip_data.csv")

#cleaning the dataset
glaze_database$Sample <- as.factor(glaze_database$Sample)
glaze_database$Ware <- as.factor(glaze_database$Ware)
glaze_database$Provenance <- as.factor(glaze_database$Provenance)
glaze_database$Glaze_or_slip_color <- as.factor(glaze_database$Glaze_or_slip_color)
slip_database$Sample <- as.factor(slip_database$Sample)
slip_database$Ware <- as.factor(slip_database$Ware)
slip_database$Provenance <- as.factor(slip_database$Provenance)
slip_database$Glaze_or_slip_color <- as.factor(slip_database$Glaze_or_slip_color)
slip_database$Component <- as.factor(slip_database$Component)

glaze_database <- glaze_database %>%
  mutate(Ware = recode(Ware, "Samanid - Slipware" = "Slipware - Samanid"))
slip_database <- slip_database %>%
  mutate(Ware = recode(Ware, "Samanid - Slipware" = "Slipware - Samanid"))

unique_glaze_type_per_Site <- glaze_database %>%
  group_by(Site, Ware) %>%
  summarise(unique_sample_count = n_distinct(Sample)) %>%
  ungroup()
print(unique_glaze_type_per_Site, n=13)







#-------------------------- visualising glaze types ---------------------------

### Preparing opaque glaze data in context with comparative data for ternary

O_database <- rio::import("./Data/opaque_database.csv")

#cleaning the dataset
O_database$ID <- as.factor(O_database$ID)
O_database$Color <- as.factor(O_database$Color)
O_database$Group <- as.factor(O_database$Group)

O_db <- filter(O_database, `Group` %in% c('BUK A', 'N/A', 'SAMK', 'TASH'))
O_comp <- filter(O_database, `Group` %in% c('Central Asia','Egypt and Levant', 'Mesopotamian'))



# Opaque Glaze ternary showing glaze types 

combined_ternary <- ggtern(data=O_db, aes(x=PbO, y=SiO2, z=alkali, fill=Color, shape=Group)) +
  geom_point(size=2.5, stroke = 1, alpha = 1) +
  labs(x = "PbO (%)", y = "SiO2 (%)", z= "alkali (%)") +
  geom_point(data=O_comp, aes(x=PbO, y=SiO2, z=alkali, fill=Color, shape = "Comparative samples"),
             color="black", size=3, stroke=0.5, alpha=0.3) +
  scale_fill_manual(
    name = "Opaque Glaze Color",
    values = c("Yellow" = "maroon", "White" = "grey70", "Ishkor" = "royalblue3")
  ) +
  scale_shape_manual(name = "Paste Compositional Group", values = c(
    "BUK A" = 21, "SAMK" = 22, "TASH" = 23, "N/A" = 24,
    "Comparative samples" = 25
  )) +
  guides(
    fill  = guide_legend(override.aes = list(shape = 21, color = "black", alpha = 1, stroke = 0.7)),
    shape = guide_legend(order = 1),
    fill  = guide_legend(order = 2)
  ) +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    tern.panel.background = element_rect(fill = "transparent", color = NA),
    tern.plot.background = element_rect(fill = "transparent", color = NA), # ternary plot background
    plot.title = element_text(hjust = 0.5),
    tern.axis.line.T = element_line(color = "black", size = 1.2),
    tern.axis.line.L = element_line(color = "black", size = 1.2),
    tern.axis.line.R = element_line(color = "black", size = 1.2),
    tern.panel.grid.major.T = element_blank(),
    tern.panel.grid.major.L = element_blank(),
    tern.panel.grid.major.R = element_blank(),
    # Remove minor grid lines
    tern.panel.grid.minor.T = element_blank(),
    tern.panel.grid.minor.L = element_blank(),
    tern.panel.grid.minor.R = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )

plot(combined_ternary)

jpeg(filename = "./Figures/Figure4.jpg", width = 3600, height = 2400, res=300)
plot(combined_ternary)
dev.off()


### Preparing transparent glaze data for ternary

T_database <- filter(glaze_database, `Ware` %in% c('Monochrome', 'Slipware - Samanid', 
                                                   'Slipware - Qarakhanid', 'Splashed'))
T_database <- filter(T_database, `Component` %in% c('Glaze'))
T_database$Alkali <- rowSums(T_database[, c("Na2O", "MgO", "CaO", "K2O", "Al2O3")], na.rm = TRUE)


# Opaque Glaze ternary showing glaze types

transparent_ternary <- ggtern(data=T_database, aes(x=PbO, y=SiO2, z=Alkali, color=Ware, shape=`Provenance`)) +
  geom_point(size=2) +
  labs(title = "Transparent Glaze ternary", x = "PbO (%)", y = "SiO2 (%)", z= "alkali (%)") +
  scale_color_brewer(name = "Ware", palette = "Set1") +
  scale_shape_manual(name = "Group", values = c(21, 22, 23, 24, 25, 7, 8, 9)) +
  theme(legend.position = "right")  +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    tern.panel.background = element_rect(fill = "transparent", color = NA),
    tern.axis.line.T = element_line(color = "black", size = 1.2),
    tern.axis.line.L = element_line(color = "black", size = 1.2),
    tern.axis.line.R = element_line(color = "black", size = 1.2),
    # Remove major grid lines
    tern.panel.grid.major.T = element_blank(),
    tern.panel.grid.major.L = element_blank(),
    tern.panel.grid.major.R = element_blank(),
    # Remove minor grid lines
    tern.panel.grid.minor.T = element_blank(),
    tern.panel.grid.minor.L = element_blank(),
    tern.panel.grid.minor.R = element_blank()
  )

plot(transparent_ternary)

jpeg(filename = "./Supplementary_materials/supplementary_figures/transparent_ternary.jpg", width = 3600, height = 2400, res=300)
plot(transparent_ternary)
dev.off()



#----------importing and cleaning comparative ceramics datasets----------------


# Define the folder path containing all the CSV files
folder_path <- "./Data/comparative_EDS_data"  

# Get a list of all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)


## joining comparative datasets
to_num <- function(x) {
  x <- gsub(",", "", x)
  x <- gsub("%", "", x)
  x <- gsub("[^0-9eE.+\\-]", "", x)
  suppressWarnings(as.numeric(x))
}

# selecting non-oxide metadata
factor_columns   <- c("Sample","Site","Region","Instrument","Glaze_or_slip_color",
                      "Component","Ware","Incision","LIA Group","Provenance")
metadata_columns <- c(factor_columns, "Date","Count", "Surface")

#scanning files for common oxides
all_oxides <- character(0)

for (file_path in csv_files) {
  message(paste("Scanning columns in:", file_path))
  df <- read.csv(file_path, check.names = FALSE, stringsAsFactors = FALSE)
  df <- df[, colnames(df) != "", drop = FALSE]  # drop empty-name columns
  oxides_here <- setdiff(colnames(df), c(metadata_columns, "Total"))
  all_oxides <- union(all_oxides, oxides_here)
}
oxide_columns_master <- sort(all_oxides)

# process, align, and create a normalized dataset
raw_data_list <- list()
normalized_data_list <- list()

for (file_path in csv_files) {
  message(paste("Processing file:", file_path))
  samples <- read.csv(file_path, check.names = FALSE, stringsAsFactors = FALSE)
  samples <- samples[, colnames(samples) != "", drop = FALSE]
  
  for (col in factor_columns) if (col %in% colnames(samples)) samples[[col]] <- as.character(samples[[col]])
  
  keep_cols <- unique(c(metadata_columns, oxide_columns_master, "Total"))
  samples <- samples[, intersect(keep_cols, colnames(samples)), drop = FALSE]
  
  missing_oxides <- setdiff(oxide_columns_master, colnames(samples))
  if (length(missing_oxides)) for (m in missing_oxides) samples[[m]] <- NA_real_
  
  ordered_columns <- c(
    metadata_columns[metadata_columns %in% colnames(samples)],
    oxide_columns_master,
    intersect("Total", colnames(samples))
  )
  samples <- samples[, ordered_columns, drop = FALSE]
  
  numeric_targets <- intersect(c(oxide_columns_master, "Total"), colnames(samples))
  samples[numeric_targets] <- lapply(samples[numeric_targets], to_num)
  raw_data_list[[file_path]] <- samples
  
  normalized_samples <- samples
  for (col in numeric_targets) normalized_samples[[col]] <- to_num(normalized_samples[[col]])
  
  if (!"Total" %in% colnames(normalized_samples)) {
    normalized_samples$Total <- rowSums(normalized_samples[, oxide_columns_master, drop = FALSE], na.rm = TRUE)
  } else {
    bad_total <- is.na(normalized_samples$Total) | !is.finite(normalized_samples$Total) | normalized_samples$Total <= 0
    fallback  <- rowSums(normalized_samples[, oxide_columns_master, drop = FALSE], na.rm = TRUE)
    normalized_samples$Total[bad_total] <- fallback[bad_total]
  }
  
  normalized_samples <- normalized_samples[!is.na(normalized_samples$Total) & normalized_samples$Total > 0, , drop = FALSE]
  
  if (nrow(normalized_samples) > 0) {
    for (col in oxide_columns_master) {
      normalized_samples[[col]] <- (normalized_samples[[col]] * 100) / normalized_samples$Total
    }
    
    row_sums <- rowSums(normalized_samples[, oxide_columns_master, drop = FALSE], na.rm = TRUE)
    cf <- 100 / row_sums
    cf[!is.finite(cf)] <- 1
    for (col in oxide_columns_master) {
      normalized_samples[[col]] <- normalized_samples[[col]] * cf
    }
    
    normalized_samples$Total <- 100
    
    round_cols <- intersect(c(oxide_columns_master, "Total"), colnames(normalized_samples))
    normalized_samples[round_cols] <- lapply(normalized_samples[round_cols], function(v) signif(v, 3))
  }
  
  normalized_data_list[[file_path]] <- normalized_samples
}

comparative_raw_data         <- dplyr::bind_rows(raw_data_list)
comparative_normalized_data  <- dplyr::bind_rows(normalized_data_list)



### Cleaning data
comparative_normalized_data$Ware <- as.factor(comparative_normalized_data$Ware)
comparative_normalized_data$Provenance <- as.factor(comparative_normalized_data$Provenance)

# renaming same paste compositional groups
comparative_normalized_data <- comparative_normalized_data %>%
  mutate(
    Provenance = case_when(
      Provenance %in% c("TAZ", "KAZ3") ~ "TAZ - Group 3",
      TRUE ~ as.character(Provenance) 
    ),
    Provenance = factor(Provenance)
  )

# examining samples distribution by Ware
unique_samples_per_Ware <- tapply(
  comparative_normalized_data$Sample,
  comparative_normalized_data$Ware,
  function(x) length(unique(x))
)
unique_samples_per_Ware <- data.frame(
  Ware = names(unique_samples_per_Ware),
  unique_sample_count = as.integer(unique_samples_per_Ware),
  row.names = NULL
)

unique_samples_per_Ware


# examining samples distribution by Site
unique_samples_per_Site <- tapply(
  comparative_normalized_data$Sample,
  comparative_normalized_data$Site,
  function(x) length(unique(x))
)
unique_samples_per_Site <- data.frame(
  Site = names(unique_samples_per_Site),
  unique_sample_count = as.integer(unique_samples_per_Site),
  row.names = NULL
)

unique_samples_per_Site




#----------------examining Comparative ceramic datasets ----------------------

Comp_Glaze <- filter(comparative_normalized_data, `Component` %in% c('Glaze'))
Comp_Glaze <- Comp_Glaze %>% filter(!Glaze_or_slip_color == "Weathered")
Comp_slip <- filter(comparative_normalized_data, `Component` %in% c('White Slip'))
Comp_colored_slip <- filter(comparative_normalized_data, `Component` %in% 
                              c('Red Slip','Brown Slip', 'Black Slip', 'Olive Slip'))

# calculate mean compositions for samples with multiple measurements 
Comp_Glaze <- Comp_Glaze %>%
  group_by(Sample, Site, Region, Glaze_or_slip_color, Ware, Provenance) %>%
  summarise(
    across(all_of(oxide_columns_master), ~ mean(.x, na.rm = TRUE)),
    Count = sum(Count, na.rm = TRUE),
    .groups = "drop"
  )
Comp_slip <- Comp_slip %>%
  group_by(Sample, Site, Region, Glaze_or_slip_color, Ware, Provenance) %>%
  summarise(
    across(all_of(oxide_columns_master), ~ mean(.x, na.rm = TRUE)),
    Count = sum(Count, na.rm = TRUE),
    .groups = "drop"
  )
Comp_colored_slip <- Comp_colored_slip %>%
  group_by(Sample, Site, Region, Glaze_or_slip_color, Ware, Provenance) %>%
  summarise(
    across(all_of(oxide_columns_master), ~ mean(.x, na.rm = TRUE)),
    Count = sum(Count, na.rm = TRUE),
    .groups = "drop"
  )


# clean averaged data
Comp_Glaze <- Comp_Glaze %>%
  rename_with(~ gsub("_Mean$", "", .x), ends_with("_Mean")) %>%
  mutate(
    alkali = rowSums(select(., Na2O, MgO, Al2O3, K2O, CaO), na.rm = TRUE), 
    PbO    = coalesce(PbO, 1e-5)   
  )



# examine the data by Ware type (transparent or opaque)

opaque_set       <- c("Alkali", "Ishkor", "Lustreware", "Opaque")
transparent_set  <- c("Slipware - Qarakhanid", "Slipware - Samanid", "Slipware", "Splashed", "Underglaze", "Monochrome")
other_set        <- c("Kiln Furniture", "Undetermined", "Unknown")

Comp_Glaze <- Comp_Glaze %>%
  mutate(
    Ware_type = case_when(
      Ware %in% opaque_set      ~ "Opaque",
      Ware %in% transparent_set ~ "Transparent",
      Ware %in% other_set       ~ "Other",
      TRUE                      ~ "Other"   
    ),
    Ware_type = factor(Ware_type, levels = c("Opaque","Transparent","Other"))
  )


# examine glaze type distribution by site

unique_glaze_type_per_Site <- Comp_Glaze %>%
  group_by(Site, Ware_type) %>%
  summarise(unique_sample_count = n_distinct(Sample)) %>%
  ungroup()
print(unique_glaze_type_per_Site, n=36)


# Save compiled comparative datasets

write.csv(Comp_Glaze, "./Data/Comp_Glaze.csv", row.names = FALSE)
write.csv(Comp_slip, "./Data/Comp_Slip.csv", row.names = FALSE)
write.csv(Comp_colored_slip, "./Data/Comp_colored_Slip.csv", row.names = FALSE)

# visualising glaze typs across 

Comp_glaze_ternary <- ggtern(
  data = Comp_Glaze, aes(x = PbO, y = SiO2, z = alkali, 
                         color = Ware, shape = Region)) +
  geom_point(size = 2.5, alpha = 0.7, stroke = 0.5) +
  labs(
    L = "PbO (%)",
    T = "SiO2 (%)",
    R = "Alkali (%)"
  ) +
  scale_shape_manual(name = "Region", values = c(15,1,2,3,8,10,6,17)) + 
  scale_color_manual(name = "Ware", values = c("#1F77B4", "#FF7F0E",  "#2CA02C",  "#D62728", "#9467BD",
    "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#000000", "#AEC7E8", "#FFBB78")) +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    tern.panel.background = element_rect(fill = "transparent", color = NA),
    tern.plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5),
    tern.axis.line.T = element_line(color = "black", size = 1.2),
    tern.axis.line.L = element_line(color = "black", size = 1.2),
    tern.axis.line.R = element_line(color = "black", size = 1.2),
    tern.panel.grid.major.T = element_blank(),
    tern.panel.grid.major.L = element_blank(),
    tern.panel.grid.major.R = element_blank(),
    tern.panel.grid.minor.T = element_blank(),
    tern.panel.grid.minor.L = element_blank(),
    tern.panel.grid.minor.R = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )

png(filename = "./Supplementary_materials/supplementary_figures/Comp_glaze_ternary.png", width = 3600, height = 2400, res=300)
plot(Comp_glaze_ternary)
dev.off()


Comp_glaze_ternary_ware_type <- ggtern(
  data = Comp_Glaze, aes(x = PbO, y = SiO2, z = alkali, 
                         color = Ware_type, shape = Region)) +
  geom_point(size = 2.5, alpha = 0.7, stroke = 0.5) +
  labs(
    L = "PbO (%)",
    T = "SiO2 (%)",
    R = "Alkali (%)"
  ) +
  scale_shape_manual(name = "Region", values = c(15,1,2,3,8,10,6,17)) + 
  scale_color_manual(name = "Ware Type", values = c("#1F77B4","#FF7F0E","#000000")) +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    tern.panel.background = element_rect(fill = "transparent", color = NA),
    tern.plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5),
    tern.axis.line.T = element_line(color = "black", size = 1.2),
    tern.axis.line.L = element_line(color = "black", size = 1.2),
    tern.axis.line.R = element_line(color = "black", size = 1.2),
    tern.panel.grid.major.T = element_blank(),
    tern.panel.grid.major.L = element_blank(),
    tern.panel.grid.major.R = element_blank(),
    tern.panel.grid.minor.T = element_blank(),
    tern.panel.grid.minor.L = element_blank(),
    tern.panel.grid.minor.R = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )

png(filename = "./Supplementary_materials/supplementary_figures/Comp_glaze_ternary_Ware.png", width = 3600, height = 2400, res=300)
plot(Comp_glaze_ternary_ware_type)
dev.off()



### Transparent glaze Principal Component Analysis to assess major trends 

Comp_Glaze_transparent <- filter(Comp_Glaze, `Ware_type` %in% c('Transparent'))

Comp_Glaze_transparent <- Comp_Glaze_transparent %>%
  filter(PbO >= 30)

#clean transparent glaze data to ensure their are no N/A values in major elements
detection_limit <- 0.1
replacement_value <- (2/3) * detection_limit

Comp_Glaze_transparent <- Comp_Glaze_transparent %>%
  mutate(across(c(MgO, Na2O, FeO, CaO, K2O),
                ~ ifelse(is.na(.x), replacement_value, .x)))

#select major elements (Pb, Si, Al, Na, Mg, Ca, Fe, K) for PCA

pc <- prcomp(Comp_Glaze_transparent[,c("PbO", "SiO2", "Al2O3", "Na2O", "MgO", "CaO", "FeO", "K2O")],
             center = TRUE,
             scale. = TRUE) 

pc.dataframe <- as.data.frame(pc$x)
pc.dataframe <- data.frame(pc.dataframe, Ware = Comp_Glaze_transparent$Ware, 
                           Provenance = Comp_Glaze_transparent$Provenance, 
                           Site = Comp_Glaze_transparent$Site, 
                           Sample = Comp_Glaze_transparent$Sample,
                           Region = Comp_Glaze_transparent$Region)

# Create element loadings scaled for visualization
loadings <- as.data.frame(pc$rotation)
loadings_scaled <- loadings*8
loadings_scaled$Variable <- rownames(loadings)

# Calculate percentage contribution of each PC
total_variance <- sum(pc$sdev^2)  
pc_contributions <- (pc$sdev^2 / total_variance) * 100
print(pc_contributions)


##subset data for plotting

NAA_pc.dataframe <- pc.dataframe %>% 
  filter(Provenance %in% c("BUK", "TASH", "SAMK", "TAZ - Group 3", "PAY 2", "N/A"))

Site_pc.dataframe <- pc.dataframe %>% 
  filter(Site %in% c("Akhsiket", "Dandanakan", "Kuva", "Termez", "Aktobe", "Bektobe",
                     "Kulan", "Lower Barskhan", "Tamdy", "Taraz", "Laskhar-i Bazar",
                     "Bust")| Provenance == "N/A")

ellipse_NAA_pc.dataframe <- pc.dataframe %>% 
  filter(Provenance %in% c("BUK", "TASH")) 

ellipse_region_pc.dataframe <- pc.dataframe %>% 
  filter(Region %in% c("Chach", "Transoxiana")) 

ellipse_ware_pc.dataframe <- ellipse_NAA_pc.dataframe %>% 
  filter(Ware %in% c("Slipware - Samanid", "Slipware - Qarakhanid")) 


#plot by paste compositional group with element loadings
PCregion <- ggplot() +
  geom_point(
    data = Site_pc.dataframe,
    aes(PC1, PC2, color = Ware, shape = Site),
    show.legend = c(color = TRUE, shape = TRUE)
  ) +
  geom_point(
    data = NAA_pc.dataframe,
    aes(PC1, PC2, color = Ware, shape = Provenance)
  ) +
  stat_ellipse(
    data = ellipse_ware_pc.dataframe,
    aes(PC1, PC2, group = Ware, color = Ware)
  ) +
  theme_minimal() +
  labs(x = "PC1 (43.3%)", y = "PC2 (16.2%)") +
  coord_cartesian(xlim = c(-5, 7.5), ylim = c(-5, 4)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1),
    legend.position = "right",
    legend.box = "horizontal"  
  ) +
  scale_shape_manual(
    name = "Provenance/Site",
    values = c(17, 15, 16, 18, 1,2,3,4,5,6,8,9,10,11,12,13,14,7,0)
  ) +
  scale_color_manual(
    name = "Ware",
    values = c("#E41A1C", "#999999", "#4DAF4A", "#377EB8", "#984EA3", "#FF7F00")
  ) +
  guides(
    shape = guide_legend(order = 1, direction = "vertical", ncol = 1),
    color = guide_legend(order = 2, direction = "vertical", ncol = 1)
  )

PCregion
jpeg(filename = "./Figures/Figure6a.jpg", width = 2800, height = 1600, res=300)
print(PCregion) + theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1)) +
  geom_segment(data = loadings_scaled, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_text(data = loadings_scaled, aes(x = PC1, y = PC2, label = Variable), hjust = 0.25, vjust = 1.25, size = 5, color = "black")
dev.off()


PCComp <- ggplot() +
  geom_point(data = pc.dataframe, aes(x = PC1, y = PC2, color = "comparative site", shape = Ware)) +
  geom_point(data = NAA_pc.dataframe, aes(x =PC1, y = PC2, color = Provenance, shape = Ware))+
  theme_minimal() +
  stat_ellipse(data = ellipse_NAA_pc.dataframe, aes(x =PC1, y = PC2, group = Provenance, color = Provenance))+
  labs(x = "PC1 (43.3%)", y = "PC2 (16.2%)") +
  coord_cartesian(xlim = c(-5, 7.5), ylim = c(-5, 4)) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  scale_color_manual(
    name   = "Provenance",
    breaks = c("BUK", "SAMK", "TASH", "TAZ - Group 3", "comparative site"),  
    values = c("BUK" = "#CC6677",
               "SAMK" = "#882255",
               "TASH" = "#44AA99",
               "TAZ - Group 3" = "#117733",
               "comparative site" = "gray"),
    drop = FALSE 
  ) +
  scale_shape_manual(name = "Ware", values = c(1, 6, 16, 17, 8, 7)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1),
    legend.position = "right",
    legend.box = "horizontal"  
  ) 

print(PCComp) + theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1)) 

jpeg(filename = "./Figures/Figure6b.jpg", width = 2800, height = 1600, res=300)
print(PCComp) + theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1))
dev.off()












#----------------------Comparative White slip ---------------------------------

# renormalizing data after removing PbO and colorants

subs <- c("PbO", "MnO", "FeO", "CoO", "CuO", "Cr2O3")

elts <- c("SiO2", "Al2O3", "K2O", "CaO")
elts_norm <- c("SiO2_norm", "Al2O3_norm", "K2O_norm", "CaO_norm")


# Normalization of slip and glaze 
Comp_PbO_removed <- comparative_normalized_data %>%
  rowwise() %>%
  mutate(
    total = sum(c_across(all_of(elts)), na.rm = TRUE) + sum(c_across(all_of(subs)), na.rm = TRUE),
    denominator = total - sum(c_across(all_of(subs)), na.rm = TRUE),
    across(all_of(elts), ~ .x / denominator * 100, .names = "{.col}_norm")
  ) %>%
  ungroup() %>%
  select(-total, -denominator)


Comp_Slips_glaze <- Comp_PbO_removed %>%
  filter(Component == "Glaze", Glaze_or_slip_color != "Not over slip") %>%
  group_by(Sample, Site, Region, Ware, Provenance) %>%
  summarise(
    across(all_of(elts), ~ mean(.x, na.rm = TRUE), .names = "Raw_{.col}_glaze"),
    across(all_of(elts_norm), ~ mean(.x, na.rm = TRUE), .names = "{.col}_glaze"),
    Count = sum(Count, na.rm = TRUE),
    .groups = "drop"
  )


Comparative_slip <- Comp_PbO_removed %>%
  filter(Component == "White Slip") %>%
  filter(!Glaze_or_slip_color %in% c('Not under glaze')) %>%
  select(Sample, Site, Region, Ware, Component, Provenance, Glaze_or_slip_color, all_of(elts)) %>%
  group_by(Sample, Site, Region, Ware, Provenance) %>%
  summarise(
    across(all_of(elts), ~ mean(.x, na.rm = TRUE), .names = "Raw_{.col}_slip"),
    .groups = "drop"
  )


Comparative_slip_norm <- Comp_PbO_removed %>%
  filter(Component == "White Slip") %>%
  filter(!Glaze_or_slip_color %in% c('Not under glaze')) %>%
  select(Sample, Site, Region, Ware, Component, Provenance, Glaze_or_slip_color, all_of(elts_norm)) %>%
  group_by(Sample, Site, Region, Ware, Provenance) %>%
  summarise(
    # values for slip
    across(all_of(elts_norm), ~ mean(.x, na.rm = TRUE), .names = "{.col}_slip"),
    .groups = "drop"
  )


# Join raw and normalized data for glaze and slip
wide_slip_data <- Comp_Slips_glaze %>%
  inner_join(Comparative_slip, by = c("Sample", "Site", "Region", "Ware", "Provenance")) %>%
  inner_join(Comparative_slip_norm, by = c("Sample", "Site", "Region", "Ware", "Provenance"))

# Final cleaning
wide_slip_data <- wide_slip_data %>%
  mutate(
    across(starts_with("SiO2"), as.numeric),
    across(starts_with("Al2O3"), as.numeric),
    across(starts_with("K2O"), as.numeric),
    across(starts_with("CaO"), as.numeric)
  )

## group by provenance type (NAA group versus recovery site for comparative ceramics)

provenanced_ceramics_data <- wide_slip_data
provenanced_ceramics_data$Provenance <- as.factor(provenanced_ceramics_data$Provenance)
provenanced_ceramics_data <- provenanced_ceramics_data %>% filter(Provenance %in% c("BUK", "TASH", "TAZ - Group 3", "SAMK"))
provenanced_ceramics_data_ellipse <- provenanced_ceramics_data %>% filter(Provenance %in% c("BUK", "TASH"))


recovery_ceramics_data <- wide_slip_data
recovery_ceramics_data$Site <- as.factor(recovery_ceramics_data$Site)
recovery_ceramics_data <- recovery_ceramics_data %>% filter(Site %in% c("Akhsiket", "Dandanakan", "Kuva", "Termez", "Aktobe", "Bektobe",
                                      "Kulan", "Lower Barskhan", "Tamdy", "Taraz", "Laskhar-i Bazar",
                                      "Bust")| Provenance == "N/A")
recovery_ceramics_data <- recovery_ceramics_data %>% filter(!(Provenance %in% c("BUK", "TASH", "TAZ - Group 3", "SAMK")))


## visualising glaze and slip interactions
combined_plot_Al2O3 <- ggplot() +
  geom_point(data = provenanced_ceramics_data, aes(x = Al2O3_norm_slip, y = Al2O3_norm_glaze, 
                                                   color = Provenance, shape = Provenance),
             size = 4) +
  geom_point(data = recovery_ceramics_data, aes(x = Al2O3_norm_slip, y = Al2O3_norm_glaze, color = "comparative site", shape = Site),
             size = 2) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  stat_ellipse(data = provenanced_ceramics_data_ellipse, 
               aes(x = Al2O3_norm_slip, y = Al2O3_norm_glaze, group = Provenance, color = Provenance), 
               level = 0.90, geom = "path") +
  labs(x = "White Slip (Al2O3* wt%)",
       y = "Glaze (Al2O3* wt%)") +
  scale_shape_manual(name = "Provenance/Site", values = c(17, 15, 16, 18, 1,2,3,4,5,6,8,9,10,11,12,13,14,7,0)) +
  scale_color_manual(
    name   = " ",
    breaks = c("BUK", "SAMK", "TASH", "TAZ - Group 3", "comparative site"), 
    values = c("BUK" = "#CC6677",
               "SAMK" = "#882255",
               "TASH" = "#44AA99",
               "TAZ - Group 3" = "#117733",
               "comparative site" = "gray")) +
  coord_cartesian(xlim = c(0, 42), ylim = c(0, 42)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1),
    legend.position = "right",
    legend.box = "horizontal"  
  )  +
  guides(
    color = guide_legend(order = 2, direction = "vertical"),
    shape = guide_legend(order = 1, direction = "vertical")
  )

print(combined_plot_Al2O3) 

jpeg(filename = "./Figures/Figure9a.jpg", width = 2800, height = 1600, res=300)
print(combined_plot_Al2O3) 
dev.off()



combined_plot_SiO2 <- ggplot() +
  geom_point(data = provenanced_ceramics_data, aes(x = SiO2_norm_slip, y = SiO2_norm_glaze, color = Provenance, shape = Provenance),
             size = 4) + 
  geom_point(data = recovery_ceramics_data, aes(x = SiO2_norm_slip, y = SiO2_norm_glaze, color = "comparative site", shape = Site),
             size = 2) +  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  stat_ellipse(data = provenanced_ceramics_data_ellipse, 
               aes(x = SiO2_norm_slip, y = SiO2_norm_glaze, group = Provenance, color = Provenance), 
               level = 0.90, geom = "path") +
  labs(x = "White Slip (SiO2* wt%)",
       y = "Glaze (SiO2* wt%)") +
  scale_shape_manual(name = "Provenance/Site", values = c(17, 15, 16, 18, 1,2,3,4,5,6,8,9,10,11,12,13,14,7,0)) +
  scale_color_manual(
    name   = " ",
    breaks = c("BUK", "SAMK", "TASH", "TAZ - Group 3", "comparative site"),  # desired order
    values = c("BUK" = "#CC6677",
               "SAMK" = "#882255",
               "TASH" = "#44AA99",
               "TAZ - Group 3" = "#117733",
               "comparative site" = "gray")) +
  coord_cartesian(xlim = c(50,100), ylim = c(50,100)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1),
    legend.position = "right",
    legend.box = "horizontal"  
  )  +
  guides(
    color = guide_legend(order = 2, direction = "vertical"),
    shape = guide_legend(order = 1, direction = "vertical")
  )

print(combined_plot_SiO2)

jpeg(filename = "./Figures/Figure9b.jpg", width = 2800, height = 1600, res=300)
print(combined_plot_SiO2) 
dev.off()




combined_plot_CaO_Al2O3 <- ggplot() +
  geom_point(data = provenanced_ceramics_data,
             aes(x = Raw_CaO_slip, y = Raw_Al2O3_slip, color = Ware, shape = Provenance),
             size = 4) +
  geom_point(data = recovery_ceramics_data,
             aes(x = Raw_CaO_slip, y = Raw_Al2O3_slip, color = Ware, shape = Site),
             size = 2) +
  labs(x = "log(CaO slip (wt%))", 
       y = "Al2O3 slip (wt%)") +
  scale_shape_manual(name = "Provenance/Site",
                     values = c(17,15,16,18,1,2,3,4,5,6,8,9,10,11,12,13,14,7,0)) +
  scale_color_manual(name = "Ware",
                     values = c("#4DAF4A","#377EB8","#984EA3","#FF7F00",
                                "#a65628","#E41A1C","#999999","#000000")) +
  scale_x_log10(
    breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 40),   # pick ticks that fit your data
    labels = label_number(accuracy = 0.1, trim = TRUE)
  ) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linewidth = 1),
        legend.position = "right",
        legend.box = "horizontal") +
  guides(color = guide_legend(order = 2, direction = "vertical"),
         shape = guide_legend(order = 1, direction = "vertical"))

combined_plot_CaO_Al2O3

jpeg(filename = "./Figures/Figure9c.jpg", width = 2800, height = 1600, res=300)
print(combined_plot_CaO_Al2O3) 
dev.off()


# ------------------------------ citations -------------------------------------

citation("rio")
citation("dplyr")
citation("ggplot2")
citation("ggtern")
citation("scales")
