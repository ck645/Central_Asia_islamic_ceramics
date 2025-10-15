##Uzbekistan glazed ceramics - Glaze and Slip analysis
# C. Klesner
# 2025

#load necessary packages

library(compositions)
library(rio)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggbiplot)
library(ggtern)

#Setting shape lists for 11
shape_values <- c(18, 15, 1, 16, 2, 0, 3, 17, 4, 5, 6)

#------------Glaze compositional analysis generated in this project-------------

# Importing the dataset
glaze_database <- rio::import(".Central_Asia_islamic_ceramics/Data/glaze_data.csv")
slip_database <-  rio::import(".Central_Asia_islamic_ceramics/Data/slip_data.csv")

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



### Opaque comparison data
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






### Transparent glazes

T_database <- filter(glaze_database, `Ware` %in% c('Monochrome', 'Slipware - Samanid', 
                                                   'Slipware - Qarakhanid', 'Splashed'))
T_database <- filter(T_database, `Component` %in% c('Glaze'))


head(T_database)

T_database$Alkali <- rowSums(T_database[, c("Na2O", "MgO", "CaO", "K2O", "Al2O3")], na.rm = TRUE)

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



#biplots

ggplot(T_database, aes(x = PbO, y = SiO2, color = Ware, shape = Provenance)) +
  geom_point(size=2) +
  theme_minimal() +
  labs(title = "PbO vs SiO2", x = "PbO (%)", y = "SiO2 (%)") +
  scale_color_brewer(name = "Ware", palette = "Set1") +
  scale_shape_manual(name = "Group", values = c(21, 22, 23, 24, 25, 7, 8, 9)) +
  theme(legend.position = "right")

### PCA ###

library(devtools) 
library(ggbiplot) 
library("corrr")
library(ggbiplot)


#ensure no N/A values in major elements

detection_limit_Mg <- 0.1
replacement_value_Mg <- (2/3) * detection_limit_Mg

T_database <- T_database %>%
  mutate(MgO = ifelse(is.na(MgO), replacement_value_Mg, MgO))

detection_limit_Na <- 0.1
replacement_value_Na <- (2/3) * detection_limit_Na

T_database <- T_database %>%
  mutate(Na2O = ifelse(is.na(Na2O), replacement_value_Na, Na2O))

detection_limit_Fe <- 0.1
replacement_value_Fe <- (2/3) * detection_limit_Fe

T_database <- T_database %>%
  mutate(FeO = ifelse(is.na(FeO), replacement_value_Fe, FeO))

head(T_database)

#select major elements (Pb, Si, Al, Na, Mg, Ca, Fe, K)
pc <- prcomp(T_database[,c("PbO", "SiO2", "Al2O3", "Na2O", "MgO", "CaO", "FeO", "K2O")],
             center = TRUE,
             scale. = TRUE) 

attributes(pc) 

g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = T_database$Ware,
              ellipse = TRUE, 
              ellipse.prob = 0.90) 

g <- g + scale_color_discrete(name = '') 

print(g)

pc.dataframe <- as.data.frame(pc$x)

pc.dataframe <- data.frame(pc.dataframe, Ware = T_database$Ware, NAA = T_database$`Provenance`, Sample = T_database$`Sample`)

summary(pc.dataframe$Ware)
summary(pc.dataframe$NAA)


# Loadings scaled for visualization
loadings <- as.data.frame(pc$rotation)

loadings_scaled <- loadings*8

loadings_scaled$Variable <- rownames(loadings)

# Calculate percentage contribution of each PC
total_variance <- sum(pc$sdev^2)  # Total variance (sum of eigenvalues)
pc_contributions <- (pc$sdev^2 / total_variance) * 100

# Print the percentage contributions
print(pc_contributions)

##Plot with loadings and PC contributions -- Remember to manually change the PC % on labels



ellipse_slip_pc.dataframe <- pc.dataframe %>% 
  filter(Ware %in% c("Slipware - Samanid", "Slipware - Qarakhanid")) 

g5 <- ggplot(pc.dataframe, aes(x = PC1, y = PC2, color = Ware, shape = NAA)) +
  geom_point(size=2) +
  scale_shape_manual(values = c(16, 25, 4, 3, 5, 1, 15, 22)) +
  stat_ellipse(data = ellipse_slip_pc.dataframe, aes(group = Ware, color = Ware), level = 0.90, geom = "path") +
  scale_color_manual(name = "Transparent Glaze Ware",
                    values = c("Monochrome" = "gray", "Slipware - Samanid" = "blue", "Slipware - Qarakhanid" = "red", "Splashed" = "gray")) +
  theme_minimal() +
  labs(title = "PCA Biplot for select elements - 90% confidence ellipses", x = "PC1 (46.29%)", y = "PC2 (15.72%)") +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "right")

print(g5) + theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1)) 



#plot by compositional group with loadings

ellipse_NAA_pc.dataframe <- pc.dataframe %>% 
  filter(NAA %in% c("BUK", "TASH")) 

g4 <- ggplot(pc.dataframe, aes(x = PC1, y = PC2, color = NAA)) +
  geom_point(size=2) +
  theme_minimal() +
  stat_ellipse(data = ellipse_NAA_pc.dataframe, aes(group = NAA, color = NAA), level = 0.90, geom = "path") +
  geom_segment(data = loadings_scaled, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_text(data = loadings_scaled, aes(x = PC1, y = PC2, label = Variable), hjust = 0.25, vjust = 1.25, size = 5, color = "black") +
  labs(title = "PCA Biplot for select elements - 90% confidence ellipses", x = "PC1 (46.3%)", y = "PC2 (15.7%)") +
  xlim(-6, 7) +
  ylim(-5, 6) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme(legend.position = "right")

print(g4) + theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1))


png(filename = "PCA_transparent_with_loadings.png", width = 2400, height = 1600, res=300)
print(g4) + theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1))
dev.off()



ellipse_pc.dataframe <- pc.dataframe %>% 
  filter(Ware %in% c("Slipware - Samanid", "Slipware - Qarakhanid", "Splashed", "Monochrome")) 

g2 <- ggplot(pc.dataframe, aes(x = PC1, y = PC2, color = Ware)) +
  geom_point(size=2) +
  theme_minimal() +
  stat_ellipse(data = ellipse_pc.dataframe, aes(group = Ware, color = Ware), level = 0.90, geom = "path") +
  labs(title = "PCA Biplot for select elements - 90% confidence ellipses", x = "PC1 (46.3%)", y = "PC2 (15.7%)") +
  xlim(-6, 7) +
  ylim(-5, 6) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  scale_color_brewer(name = "Ware", palette = "Set1") +
  theme(legend.position = "right")

print(g2) + theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1))

png(filename = "PCA_transparent_by_ware.png", width = 2400, height = 1600, res=300)
print(g2) + theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1))
dev.off()



#### ----------------------------comparative ceramics ------------------------------------


# Define the folder path containing all the CSV files
folder_path <- "~/Uzbekistan/comparative EDS data"  

# Get a list of all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# Load required libraries
library(dplyr)
library(readr)

oxide_mapping <- read.csv("Element_to_Oxide_relabelling.csv", stringsAsFactors = FALSE)


to_num <- function(x) {
  # strip anything that's not part of a typical numeric literal
  x <- gsub(",", "", x)                 # remove thousands separators
  x <- gsub("%", "", x)                 # remove percent signs if any
  x <- gsub("[^0-9eE.+\\-]", "", x)     # keep digits, decimal, sign, exponent
  suppressWarnings(as.numeric(x))
}



# Initialize empty lists to store both datasets
raw_data_list <- list()
normalized_data_list <- list()

for (file_path in csv_files) {
  print(paste("Processing file:", file_path))
  
  samples <- read.csv(file_path, check.names = FALSE, stringsAsFactors = FALSE)
  
  print("Column names in dataset:")
  print(colnames(samples))
  
  # Remove any columns with empty names
  samples <- samples[, colnames(samples) != ""]
  
  # Convert only known metadata columns to character
  factor_columns <- c("Sample","Site","Region","Instrument","Glaze_or_slip_color",
                      "Component","Ware","Incision","LIA Group","Provenance")
  for (col in factor_columns) if (col %in% colnames(samples)) samples[[col]] <- as.character(samples[[col]])
  
  metadata_columns <- c("Sample","Site","Region","Instrument","Glaze_or_slip_color",
                        "Component","Ware","Incision","LIA Group","Provenance","Date","Count")
  
  # oxide columns present in this file
  oxide_columns <- intersect(oxide_mapping$Oxide, colnames(samples))
  
  print("Column names after renaming:")
  print(colnames(samples))
  
  # Reorder (only existing)
  ordered_columns <- c(metadata_columns[metadata_columns %in% colnames(samples)],
                       oxide_mapping$Oxide[oxide_mapping$Oxide %in% colnames(samples)],
                       intersect("Total", colnames(samples)))
  samples <- samples[, ordered_columns, drop = FALSE]
  
  # Save raw
  raw_data_list[[file_path]] <- samples
  
  # ----- NORMALIZATION -----
  normalized_samples <- samples
  
  # Ensure oxide columns AND Total are numeric
  numeric_targets <- unique(c(oxide_columns, "Total"))
  numeric_targets <- intersect(numeric_targets, colnames(normalized_samples))
  for (col in numeric_targets) normalized_samples[[col]] <- to_num(normalized_samples[[col]])
  
  # Remove rows with NA or non-positive Total to avoid division problems
  normalized_samples <- normalized_samples[!is.na(normalized_samples$Total) & normalized_samples$Total > 0, , drop = FALSE]
  
  # Only normalize columns that actually exist (and are numeric)
  existing_oxide_columns <- intersect(oxide_columns, colnames(normalized_samples))
  
  if (length(existing_oxide_columns) > 0) {
    # Normalize to 100%
    for (col in existing_oxide_columns) {
      normalized_samples[[col]] <- (normalized_samples[[col]] * 100) / normalized_samples$Total
    }
    
    # Correction factor to force exact 100
    row_sums <- rowSums(normalized_samples[, existing_oxide_columns, drop = FALSE], na.rm = TRUE)
    correction_factor <- 100 / row_sums
    correction_factor[!is.finite(correction_factor)] <- 1
    
    for (col in existing_oxide_columns) {
      normalized_samples[[col]] <- normalized_samples[[col]] * correction_factor
    }
  }
  
  # Recalculate total
  if ("Total" %in% colnames(normalized_samples)) normalized_samples$Total <- 100
  
  # Store normalized
  normalized_data_list[[file_path]] <- normalized_samples
}

# Combine all processed data into final datasets
raw_data_list <- lapply(raw_data_list, function(df) {
  cols <- intersect(numeric_targets, names(df))
  df[cols] <- lapply(df[cols], to_num)
  df
})

comparative_raw_data <- dplyr::bind_rows(raw_data_list)

comparative_normalized_data <- bind_rows(normalized_data_list)



### Examining samples

factor_columns <- c("Sample","Site","Region","Instrument","Glaze_or_slip_color",
                    "Component","Ware","Incision","LIA Group","Provenance")

comparative_normalized_data$Ware <- as.factor(comparative_normalized_data$Ware)
comparative_normalized_data$Provenance <- as.factor(comparative_normalized_data$Provenance)

summary(comparative_normalized_data$Ware)
summary(comparative_normalized_data$Provenance)

comparative_normalized_data <- comparative_normalized_data %>%
  mutate(
    Provenance = case_when(
      Provenance %in% c("TAZ", "KAZ3") ~ "TAZ - Group 3",
      TRUE ~ as.character(Provenance) 
    ),
    Provenance = factor(Provenance)
  )

unique_samples_per_Ware <- tapply(
  comparative_normalized_data$Sample,
  comparative_normalized_data$Ware,
  function(x) length(unique(x))
)

# Convert to data frame for a nice table
unique_samples_per_Ware <- data.frame(
  Ware = names(unique_samples_per_Ware),
  unique_sample_count = as.integer(unique_samples_per_Ware),
  row.names = NULL
)

unique_samples_per_Ware


# Convert to data frame for a nice table
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




### ------------------------Comparative glazes --------------------------------

Comp_Glaze <- filter(comparative_normalized_data, `Component` %in% c('Glaze'))
Comp_Glaze <- Comp_Glaze %>% filter(!Glaze_or_slip_color == "Weathered")

Comp_Glaze <- Comp_Glaze %>%
  group_by(Sample, Site, Region, Glaze_or_slip_color, Ware, Provenance) %>%
  summarise(
    across(all_of(oxide_columns), ~ mean(.x, na.rm = TRUE)),
    Count = sum(Count, na.rm = TRUE),
    .groups = "drop"
  )

Comp_slip <- filter(comparative_normalized_data, `Component` %in% c('White Slip'))

Comp_slip <- Comp_slip %>%
  group_by(Sample, Site, Region, Glaze_or_slip_color, Ware, Provenance) %>%
  summarise(
    across(all_of(oxide_columns), ~ mean(.x, na.rm = TRUE)),
    Count = sum(Count, na.rm = TRUE),
    .groups = "drop"
  )


Comp_Glaze <- Comp_Glaze %>%
  rename_with(~ gsub("_Mean$", "", .x), ends_with("_Mean")) %>%
  mutate(
    alkali = rowSums(select(., Na2O, MgO, Al2O3, K2O, CaO), na.rm = TRUE), 
    PbO    = coalesce(PbO, 1e-5)   
  )

summary(Comp_Glaze$Ware)

opaque_set       <- c("Alkali", "Ishkor", "Lustreware", "Opaque")
transparent_set  <- c("Slipware - Qarakhanid", "Slipware - Samanid", "Slipware", "Splashed", "Underglaze", "Monochrome")
other_set        <- c("Kiln Furniture", "Undetermined", "Unknown")

Comp_Glaze <- Comp_Glaze %>%
  mutate(
    Ware_type = case_when(
      Ware %in% opaque_set      ~ "Opaque",
      Ware %in% transparent_set ~ "Transparent",
      Ware %in% other_set       ~ "Other",
      TRUE                      ~ "Other"   # fallback for any unexpected labels
    ),
    Ware_type = factor(Ware_type, levels = c("Opaque","Transparent","Other"))
  )

summary(Comp_Glaze$Ware_type)

Comp_Glaze$Site <- as.factor(Comp_Glaze$Site)

summary(Comp_Glaze$Site)

Comp_Glaze$Region <- as.factor(Comp_Glaze$Region)

summary(Comp_Glaze$Region)

unique_glaze_type_per_Site <- Comp_Glaze %>%
  group_by(Site, Ware_type) %>%
  summarise(unique_sample_count = n_distinct(Sample)) %>%
  ungroup()

# View the result
print(unique_glaze_type_per_Site, n=36)



write.csv(Comp_Glaze, "Comp_Glaze.csv", row.names = FALSE)
write.csv(Comp_slip, "Comp_Slip.csv", row.names = FALSE)


Comp_glaze_ternary <- ggtern(
  data = Comp_Glaze, aes(x = PbO, y = SiO2, z = alkali, 
                         color = Ware, shape = Region)) +
  geom_point(size = 2.5, alpha = 0.7, stroke = 0.5) +
  labs(
    title = "Glaze ternary",
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

plot(Comp_glaze_ternary)


png(filename = "Comp_glaze_ternary.png", width = 3600, height = 2400, res=300)
plot(Comp_glaze_ternary)
dev.off()


Comp_glaze_ternary_ware_type <- ggtern(
  data = Comp_Glaze, aes(x = PbO, y = SiO2, z = alkali, 
                         color = Ware_type, shape = Region)) +
  geom_point(size = 2.5, alpha = 0.7, stroke = 0.5) +
  labs(
    title = "Glaze ternary",
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

plot(Comp_glaze_ternary_ware_type)

png(filename = "Comp_glaze_ternary_Ware.png", width = 3600, height = 2400, res=300)
plot(Comp_glaze_ternary_ware_type)
dev.off()



### White slip

# renromalising data removing PbO and colorants

subs <- c("PbO", "MnO", "FeO", "CoO", "CuO", "Cr2O3")

elts <- c("SiO2", "Al2O3", "K2O", "CaO")
elts_norm <- c("SiO2_norm", "Al2O3_norm", "K2O_norm", "CaO_norm")

# Normalization process
Comp_PbO_removed <- comparative_normalized_data %>%
  rowwise() %>%
  mutate(
    # Calculate the total of the elements (including elements to be removed)
    total = sum(c_across(all_of(elts)), na.rm = TRUE) + sum(c_across(all_of(subs)), na.rm = TRUE),
    
    # Subtract PbO, MnO, FeO, CoO, CuO from the denominator
    denominator = total - sum(c_across(all_of(subs)), na.rm = TRUE),
    
    # Normalize oxides to 100
    across(all_of(elts), ~ .x / denominator * 100, .names = "{.col}_norm")
  ) %>%
  ungroup() %>%
  select(-total, -denominator)

# Prepare data for Glaze (both raw and normalized values)
Comp_Slips_glaze <- Comp_PbO_removed %>%
  filter(Component == "Glaze", Glaze_or_slip_color != "Not over slip") %>%
  group_by(Sample, Site, Region, Ware, Provenance) %>%
  summarise(
    # Raw values for glaze
    across(all_of(elts), ~ mean(.x, na.rm = TRUE), .names = "Raw_{.col}_glaze"),
    # Normalized values for glaze
    across(all_of(elts_norm), ~ mean(.x, na.rm = TRUE), .names = "{.col}_glaze"),
    Count = sum(Count, na.rm = TRUE),
    .groups = "drop"
  )

# Prepare data for Slip (both raw and normalized values)
Comparative_slip <- Comp_PbO_removed %>%
  filter(Component == "White Slip") %>%
  filter(!Glaze_or_slip_color %in% c('Not under glaze')) %>%
  select(Sample, Site, Region, Ware, Component, Provenance, Glaze_or_slip_color, all_of(elts)) %>%
  group_by(Sample, Site, Region, Ware, Provenance) %>%
  summarise(
    # Raw values for slip
    across(all_of(elts), ~ mean(.x, na.rm = TRUE), .names = "Raw_{.col}_slip"),
    .groups = "drop"
  )

# Prepare the normalized slip data
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
wide3 <- Comp_Slips_glaze %>%
  inner_join(Comparative_slip, by = c("Sample", "Site", "Region", "Ware", "Provenance")) %>%
  inner_join(Comparative_slip_norm, by = c("Sample", "Site", "Region", "Ware", "Provenance"))

# Convert to numeric if necessary
wide3 <- wide3 %>%
  mutate(
    across(starts_with("SiO2"), as.numeric),
    across(starts_with("Al2O3"), as.numeric),
    across(starts_with("K2O"), as.numeric),
    across(starts_with("CaO"), as.numeric)
  )

# Preview the resulting data
head(wide3)


Slip_SiO2 <- ggplot(wide3, aes(x = SiO2_norm_slip, y = SiO2_norm_glaze, color = Region, shape = Site)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "White Slip (SiO2* wt%)",
       y = "Glaze (SiO2* wt%)") +
  theme_minimal()+ 
  scale_shape_manual(name = "Site", values = c(3,8,13,17,5,6,1,2,4,16,11,9,15,7,10)) + 
  scale_color_manual(name = "Region", values = c("#e63946", "#FF7F51",  "#ebaa14", "#6B8F7A", "black", "#9467BD",  "#8C564B", "#3A86FF")) +
  coord_cartesian(xlim = c(50, 100), ylim = c(50, 100))+ 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1)) 

Slip_SiO2

ggsave(filename = "Slip_SiO2_in_context.png", plot = Slip_SiO2, width = 8, height = 6, dpi = 300)

Slip_Al2O3 <- ggplot(wide3, aes(x = Al2O3_norm_slip, y = Al2O3_norm_glaze, color = Region, shape = Site)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "White Slip (Al2O3* wt%)",
       y = "Glaze (Al2O3* wt%)") +
  theme_minimal()+ 
  scale_shape_manual(name = "Site", values = c(3,8,13,17,5,6,1,2,4,16,11,9,15,7,10)) + 
  scale_color_manual(name = "Region", values = c("#e63946", "#FF7F51",  "#ebaa14", "#6B8F7A", "black", "#9467BD",  "#8C564B", "#3A86FF")) +
  coord_cartesian(xlim = c(0, 42), ylim = c(0, 42))+ 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1)) 

Slip_Al2O3 

ggsave(filename = "Slip_Al2O3_in_context.png", plot = Slip_Al2O3, width = 8, height = 6, dpi = 300)



Slip_CaO <- ggplot(wide3, aes(x = CaO_norm_slip, y = CaO_norm_glaze, color = Region, shape = Site)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "White Slip (CaO* wt%)",
       y = "Glaze (CaO* wt%)") +
  theme_minimal()+ 
  scale_shape_manual(name = "Site", values = c(3,8,13,17,5,6,1,2,4,16,11,9,15,7,10)) + 
  scale_color_manual(name = "Region", values = c("#e63946", "#FF7F51",  "#ebaa14", "#6B8F7A", "black", "#9467BD",  "#8C564B", "#3A86FF")) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 20))+ 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1)) 

Slip_CaO 

ggsave(filename = "Slip_CaO_in_context.png", plot = Slip_CaO, width = 8, height = 6, dpi = 300)



Slip_K2O <- ggplot(wide3, aes(x = K2O_norm_slip, y = K2O_norm_glaze, color = Region, shape = Site)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "White Slip (K2O* wt%)",
       y = "Glaze (K2O* wt%)") +
  theme_minimal()+ 
  scale_shape_manual(name = "Site", values = c(3,8,13,17,5,6,1,2,4,16,11,9,15,7,10)) + 
  scale_color_manual(name = "Region", values = c("#e63946", "#FF7F51",  "#ebaa14", "#6B8F7A", "black", "#9467BD",  "#8C564B", "#3A86FF")) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))+ 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1)) 

Slip_K2O 




### provenance groups

wide4 <- wide3

wide4$Provenance <- as.factor(wide4$Provenance)
summary(wide4$Provenance)

wide4 <- wide4 %>% filter(Provenance %in% c("BUK", "TASH", "TAZ - Group 3", "SAMK"))


Slip_SiO2 <- ggplot(wide4, aes(x = SiO2_norm_slip, y = SiO2_norm_glaze, color = Ware, shape = Provenance)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "White Slip (SiO2* wt%)",
       y = "Glaze (SiO2* wt%)") +
  theme_minimal()+ 
  scale_shape_manual(name = "Group", values = c(17,3, 15,8,16,15,1)) + 
  scale_color_manual(name = "Ware", values = c("#e63946", "#3A86FF", "#ebaa14", "#9467BD")) +
  coord_cartesian(xlim = c(60, 100), ylim = c(60, 100))+ 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1)) 

Slip_SiO2

ggsave(filename = "Slip_SiO2.png", plot = Slip_SiO2, width = 8, height = 6, dpi = 300)

Slip_Al2O3 <- ggplot(wide4, aes(x = Al2O3_norm_slip, y = Al2O3_norm_glaze, color = Ware, shape = Provenance)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "White Slip (Al2O3* wt%)",
       y = "Glaze (Al2O3* wt%)") +
  theme_minimal()+ 
  scale_shape_manual(name = "Group", values = c(17,3, 15,8,16,15,1)) + 
  scale_color_manual(name = "Ware", values = c("#e63946", "#3A86FF", "#ebaa14", "#9467BD"))  +
  coord_cartesian(xlim = c(0, 30), ylim = c(0, 30))+ 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1)) 

Slip_Al2O3 

ggsave(filename = "Slip_Al2O3.png", plot = Slip_Al2O3, width = 8, height = 6, dpi = 300)



Slip_CaO <- ggplot(wide4, aes(x = CaO_norm_slip, y = CaO_norm_glaze, color = Ware, shape = Provenance)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "White Slip (CaO* wt%)",
       y = "Glaze (CaO* wt%)") +
  theme_minimal()+ 
  scale_shape_manual(name = "Group", values = c(17,3, 15,8,16,15,1)) + 
  scale_color_manual(name = "Ware", values = c("#e63946", "#3A86FF", "#ebaa14", "#9467BD"))  +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))+ 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1)) 

Slip_CaO 

ggsave(filename = "Slip_CaO.png", plot = Slip_CaO, width = 8, height = 6, dpi = 300)



Slip_K2O <- ggplot(wide4, aes(x = K2O_norm_slip, y = K2O_norm_glaze, color = Ware, shape = Provenance)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "White Slip (K2O* wt%)",
       y = "Glaze (K2O* wt%)") +
  theme_minimal()+ 
  scale_shape_manual(name = "Group", values = c(17,3, 15,8,16,15,1)) + 
  scale_color_manual(name = "Ware", values =  c("#e63946", "#3A86FF", "#ebaa14", "#9467BD"))  +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))+ 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1)) 

Slip_K2O 



# comparative slips only

wide5 <- wide3

wide5$Site <- as.factor(wide5$Site)
summary(wide5$Site)

wide5 <- wide5 %>% filter(Site %in% c("Akhsiket", "Dandanakan", "Kuva", "Termez", "Aktobe", "Bektobe",
                                      "Kulan", "Lower Barskhan", "Tamdy", "Taraz", "Laskhar-i Bazar",
                                      "Bust")
                          | Provenance == "N/A")


wide4_ellipse <- wide4 %>% filter(Provenance %in% c("BUK", "TASH"))


combined_plot_Al2O3 <- ggplot() +
  # Data from wide5 (slightly transparent points with Site-based shapes)
  geom_point(data = wide5, aes(x = Al2O3_norm_slip, y = Al2O3_norm_glaze, color = Site, shape = Site),
             size = 2) + 
  geom_point(data = wide4, aes(x = Al2O3_norm_slip, y = Al2O3_norm_glaze, color = Provenance, shape = Provenance),
             size = 4) +  # Larger solid points
 # Slight transparency and smaller points
  # Data from wide4 (larger solid points with Provenance-based shapes)
  # Add dashed line (y = x line)
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  stat_ellipse(data = wide4_ellipse, 
               aes(x = Al2O3_norm_slip, y = Al2O3_norm_glaze, group = Provenance, color = Provenance), 
               level = 0.90, geom = "path") +
  # Axis labels and title
  labs(x = "White Slip (Al2O3* wt%)",
       y = "Glaze (Al2O3* wt%)") +
  # Customize color and shape scales
  scale_shape_manual(name = "Provenance", values = c(8, 4, 11,1,13,9,3,10,5,6,7,2,12,14, 17, 18, 15, 16)) +
  scale_color_manual(name = "Provenance", values = c("black", "black", "black","black", "black", "black", "black",
                                                     "black","black", "black", "black", "black","black", "black",
                                                     "#CC6677", "#882255", "#44AA99", "#117733" )) +
  coord_cartesian(xlim = c(0, 42), ylim = c(0, 42)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1)
  )

# Display the combined plot
print(combined_plot_Al2O3)


ggsave(filename = "combined_plot_Al2O3.png", plot = combined_plot_Al2O3, width = 8, height = 6, dpi = 300)



combined_plot_SiO2 <- ggplot() +
  # Data from wide5 (slightly transparent points with Site-based shapes)
  geom_point(data = wide5, aes(x = SiO2_norm_slip, y = SiO2_norm_glaze, color = Site, shape = Site),
             size = 2) +  # Slight transparency and smaller points
   geom_point(data = wide4, aes(x = SiO2_norm_slip, y = SiO2_norm_glaze, color = Provenance, shape = Provenance),
             size = 4) +  # Larger solid points
  # Data from wide4 (larger solid points with Provenance-based shapes)
  # Add dashed line (y = x line)
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  stat_ellipse(data = wide4_ellipse, 
               aes(x = SiO2_norm_slip, y = SiO2_norm_glaze, group = Provenance, color = Provenance), 
               level = 0.90, geom = "path") +
  # Axis labels and title
  labs(x = "White Slip (SiO2* wt%)",
       y = "Glaze (SiO2* wt%)") +
  # Customize color and shape scales
  scale_shape_manual(name = "Provenance", values = c(8, 4, 11,1,13,9,3,10,5,6,7,2,12,14, 17, 18, 15, 16)) +
  scale_color_manual(name = "Provenance", values = c("black", "black", "black","black", "black", "black", "black",
                                                     "black","black", "black", "black", "black","black", "black",
                                                     "#CC6677", "#882255", "#44AA99", "#117733" )) +
  coord_cartesian(xlim = c(50, 100), ylim = c(50, 100)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1)
  )

# Display the combined plot
print(combined_plot_SiO2)


ggsave(filename = "combined_plot_SiO2.png", plot = combined_plot_SiO2, width = 8, height = 6, dpi = 300)



combined_plot_CaO <- ggplot() +
  # Data from wide5 (slightly transparent points with Site-based shapes)
  geom_point(data = wide5, aes(x = CaO_norm_slip, y = CaO_norm_glaze, color = Site, shape = Site),
             size = 2) + 
  geom_point(data = wide4, aes(x = CaO_norm_slip, y = CaO_norm_glaze, color = Provenance, shape = Provenance),
             size = 4) +  # Larger solid points
 # Slight transparency and smaller points
  # Data from wide4 (larger solid points with Provenance-based shapes)
  # Add dashed line (y = x line)
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  stat_ellipse(data = wide4_ellipse, 
               aes(x = CaO_norm_slip, y = CaO_norm_glaze, group = Provenance, color = Provenance), 
               level = 0.90, geom = "path") +
  # Axis labels and title
  labs(x = "White Slip (CaO* wt%)",
       y = "Glaze (CaO* wt%)") +
  # Customize color and shape scales
  scale_shape_manual(name = "Provenance", values = c(8, 4, 11,1,13,9,3,10,5,6,7,2,12,14, 17, 18, 15, 16)) +
  scale_color_manual(name = "Provenance", values = c("black", "black", "black","black", "black", "black", "black",
                                                     "black","black", "black", "black", "black","black", "black",
                                                     "#CC6677", "#882255", "#44AA99", "#117733" )) +
  coord_cartesian(xlim = c(0,20), ylim = c(0, 20)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1)
  )

# Display the combined plot
print(combined_plot_CaO)

ggsave(filename = "combined_plot_CaO.png", plot = combined_plot_CaO, width = 8, height = 6, dpi = 300)



combined_plot_SiO2_Al2O3 <- ggplot() +
  # Data from wide5 (slightly transparent points with Site-based shapes)
  geom_point(data = wide5, aes(x = Raw_SiO2_slip, y = Raw_Al2O3_slip, color = Ware, shape = Site),
             size = 2) + 
  geom_point(data = wide4, aes(x = Raw_SiO2_slip, y = Raw_Al2O3_slip, color = Ware, shape = Provenance),
             size = 4) +  # Larger solid points
 # Slight transparency and smaller points
  # Axis labels and title
  labs(x = "SiO2 slip (wt%)",
       y = "Al2O3 slip (wt%)") +
  # Customize color and shape scales
  scale_shape_manual(name = "Provenance", values = c(8, 4, 11,1,13,9,3,10,5,6,7,2,12,14, 17, 18, 15, 16)) +
  scale_color_manual(name = "Ware", values =   c("#1F77B4", "#FF7F0E",  "#2CA02C",  "#D62728", "#9467BD",
                                                 "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#000000", "#AEC7E8", "#FFBB78")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1)
  )

# Display the combined plot
print(combined_plot_SiO2_Al2O3)

ggsave(filename = "combined_plot_SiO2_Al2O3.png", plot = combined_plot_SiO2_Al2O3, width = 8, height = 6, dpi = 300)


combined_plot_SiO2_Al2O3 <- ggplot() +
  # Sites: all black, shown in legend as "Other site"
  geom_point(
    data = wide5,
    aes(x = Raw_SiO2_slip, y = Raw_Al2O3_slip, color = "Other site", shape = Ware),
    size = 3
  ) +
  # Highlighted provenances (colored)
  geom_point(
    data = wide4,
    aes(x = Raw_SiO2_slip, y = Raw_Al2O3_slip, color = Provenance, shape = Ware),
    size = 4
  ) +
  stat_ellipse(
    data = wide4_ellipse,
    aes(x = Raw_SiO2_slip, y = Raw_Al2O3_slip, group = Provenance, color = Provenance),
    level = 0.90, geom = "path"
  ) +
  labs(x = "SiO2 slip (wt%)", y = "Al2O3 slip (wt%)") +
  scale_shape_manual(name = "Ware", values = c(4, 3, 18, 17, 16, 8, 15, 9)) +
  scale_color_manual(
    name   = "Provenance",
    values = c(
      "Other site" = "black",
      "BUK"  = "#CC6677",
      "TASH" = "#44AA99",
      "TAZ - Group 3"  = "#117733",
      "SAMK" = "#882255"
    ),
    breaks = c("BUK","TASH","TAZ - Group 3","SAMK","Other site"),
    labels = c("BUK","TASH","TAZ - Group 3","SAMK","Other site")
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1)
  )

print(combined_plot_SiO2_Al2O3)





combined_plot_CaO_Al2O3 <- ggplot() +
  # Data from wide5 (slightly transparent points with Site-based shapes)
  geom_point(data = wide4, aes(x = Raw_CaO_slip, y = Raw_Al2O3_slip, color = Provenance, shape = Ware),
             size = 4) +  # Larger solid points
  geom_point(data = wide5, aes(x = Raw_CaO_slip, y = Raw_Al2O3_slip, color = Site, shape = Ware),
             size = 3) +  # Slight transparency and smaller points
  # Data from wide4 (larger solid points with Provenance-based shapes)
  stat_ellipse(data = wide4_ellipse, 
               aes(x = Raw_CaO_slip, y = Raw_Al2O3_slip, group = Provenance, color = Provenance), 
               level = 0.90, geom = "path") +
  # Axis labels and title
  labs(x = "CaO slip (wt%)",
       y = "Al2O3 slip (wt%)") +
  # Customize color and shape scales
  scale_shape_manual(name = "Provenance", values = c(17, 18, 15, 16, 8, 4, 11, 1,13,9)) +
  scale_color_manual(name = "Provenance", values = c("#CC6677", "#882255", "#44AA99", "#117733","black", "black", "black","black", "black", 
                                                     "black","black", "black", "black","black", "black","black"
                                                     )) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1)
  )

# Display the combined plot
print(combined_plot_CaO_Al2O3)


combined_plot_CaO_K2O <- ggplot() +
  # Data from wide5 (slightly transparent points with Site-based shapes)
  geom_point(data = wide4, aes(x = Raw_CaO_slip, y = Raw_K2O_slip, color = Provenance, shape = Ware),
             size = 4) +  # Larger solid points
  geom_point(data = wide5, aes(x = Raw_CaO_slip, y = Raw_K2O_slip, color = Site, shape = Ware),
             size = 3) +  # Slight transparency and smaller points
  # Data from wide4 (larger solid points with Provenance-based shapes)
  stat_ellipse(data = wide4_ellipse, 
               aes(x = Raw_CaO_slip, y = Raw_K2O_slip, group = Provenance, color = Provenance), 
               level = 0.90, geom = "path") +
  # Axis labels and title
  labs(x = "CaO slip (wt%)",
       y = "K2O slip (wt%)") +
  # Customize color and shape scales
  scale_shape_manual(name = "Provenance", values = c(17, 18, 15, 16, 8, 4, 11, 1,13,9)) +
  scale_color_manual(name = "Provenance", values = c("black", "black", "black","black", "black", 
                                                     "black","black", "black", "black","black", "black","black",
                                                     "#CC6677", "#882255", "#44AA99", "#117733")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1)
  )

# Display the combined plot
print(combined_plot_CaO_K2O)

















### Colored slips

comparative_normalized_data$Component <- as.factor(comparative_normalized_data$Component)

summary(comparative_normalized_data$Component)

Comp_Colored_Slip <- filter(comparative_normalized_data, `Component` %in% 
                              c('Black Slip', 'Brown Slip', 'Red Slip', 'Olive Slip', 'White Slip'))

Comp_Colored_Slip <- Comp_Colored_Slip %>%
  filter(!Glaze_or_slip_color == 'Not under glaze') %>%
  filter(!Glaze_or_slip_color == 'Upper') %>%
  filter(!Glaze_or_slip_color == 'Lower') 


Comp_Colored_Slip$MnO[is.na(Comp_Colored_Slip$MnO)] <- 0
Comp_Colored_Slip$FeO[is.na(Comp_Colored_Slip$FeO)] <- 0
Comp_Colored_Slip$Cr2O3[is.na(Comp_Colored_Slip$Cr2O3)] <- 0

# Split glaze and white slip, keep only needed columns
Comp_White <- Comp_Colored_Slip %>%
  filter(Component == "White Slip")

Comp_Brown_slip <- Comp_Colored_Slip %>%
  filter(Component == "Brown Slip")

Comp_Red_slip <- Comp_Colored_Slip %>%
  filter(Component == "Red Slip")

Comp_Olive_slip <- Comp_Colored_Slip %>%
  filter(Component == "Olive Slip")

Comp_Black_slip <- Comp_Colored_Slip %>%
  filter(Component == "Black Slip")


# Join on identifiers
comp_wide_brown <- Comp_White %>%
  inner_join(Comp_Brown_slip, by = c("Sample","Site"), suffix = c("_white","_brown"))

comp_wide_red <- Comp_White %>%
  inner_join(Comp_Red_slip, by = c("Sample","Site"), suffix = c("_white","_red"))

comp_wide_black <- Comp_White %>%
  inner_join(Comp_Black_slip, by = c("Sample","Site"), suffix = c("_white","_black"))

comp_wide_olive <- Comp_White %>%
  inner_join(Comp_Olive_slip, by = c("Sample","Site"), suffix = c("_white","_olive"))



overlay_df <- bind_rows(
  comp_wide_brown %>%
    transmute(Sample, Site, 
              FeO_white = FeO_white, FeO_colored = FeO_brown, 
              MnO_white = MnO_white, MnO_colored = MnO_brown,
              Cr2O3_white = Cr2O3_white, Cr2O3_colored = Cr2O3_brown,
              Slip = "Brown slip"),
  comp_wide_red %>%
    transmute(Sample, Site, 
              FeO_white = FeO_white, FeO_colored = FeO_red, 
              MnO_white = MnO_white, MnO_colored = MnO_red,
              Cr2O3_white = Cr2O3_white, Cr2O3_colored = Cr2O3_red,
              Slip = "Red slip"), 
  comp_wide_black %>%
    transmute(Sample, Site, 
              FeO_white = FeO_white, FeO_colored = FeO_black, 
              MnO_white = MnO_white, MnO_colored = MnO_black,
              Cr2O3_white = Cr2O3_white, Cr2O3_colored = Cr2O3_black,
              Slip = "Black slip"),
  comp_wide_olive %>%
    transmute(Sample, Site, 
              FeO_white = FeO_white, FeO_colored = FeO_olive, 
              MnO_white = MnO_white, MnO_colored = MnO_olive,
              Cr2O3_white = Cr2O3_white, Cr2O3_colored = Cr2O3_olive,
              Slip = "Olive slip")
) %>% drop_na(FeO_white, FeO_colored)



Comp_Colored_slips_plot <- ggplot(overlay_df, aes(x = MnO_white, y = MnO_colored, color = Slip, shape = Site)) +
  geom_point(size = 2.5, alpha = 0.9, stroke = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 20)) +
  scale_color_manual(values = c("Red slip" = "#e63946", "Brown slip" = "#8B5E34", "Olive slip" = "#ada010", "Black slip" = "#000000")) +
  scale_shape_manual(name = "Site", values = c(3,17,13,8,16,15,1, 5,9)) + 
  labs(
    x = "White Slip (MnO* wt%)",
    y = "Colored Slip (MnO* wt%)",
    color = "Slip color", shape = "Slip color"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1)
  )

Comp_Colored_slips_plot 

ggsave(filename = "Colored_Slip_MnO_comp_2.png", plot = Comp_Colored_slips_plot, width = 8, height = 6, dpi = 300)



Comp_Colored_slips_plot <- ggplot(overlay_df, aes(x = FeO_white, y = FeO_colored, color = Slip, shape = Site)) +
  geom_point(size = 2.5, alpha = 0.9, stroke = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_cartesian(xlim = c(0, 16), ylim = c(0, 16)) +
  scale_color_manual(values = c("Red slip" = "#e63946", "Brown slip" = "#8B5E34", "Olive slip" = "#ada010", "Black slip" = "#000000")) +
  scale_shape_manual(name = "Site", values = c(3,17,13,8,16,15,1, 5,9)) + 
  labs(
    x = "White Slip (FeO* wt%)",
    y = "Colored Slip (FeO* wt%)",
    color = "Slip color", shape = "Slip color"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1)
  )

Comp_Colored_slips_plot 

ggsave(filename = "Colored_Slip_MnO_comp_2.png", plot = Comp_Colored_slips_plot, width = 8, height = 6, dpi = 300)



Comp_Colored_slips_plot <- ggplot(overlay_df, aes(x = Cr2O3_white, y = Cr2O3_colored, color = Slip, shape = Site)) +
  geom_point(size = 2.5, alpha = 0.9, stroke = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  scale_color_manual(values = c("Red slip" = "#e63946", "Brown slip" = "#8B5E34", "Olive slip" = "#ada010", "Black slip" = "#000000")) +
  scale_shape_manual(name = "Site", values = c(3,17,13,8,16,15,1, 5,9)) + 
  labs(
    x = "White Slip (Cr2O3O* wt%)",
    y = "Colored Slip (Cr2O3* wt%)",
    color = "Slip color", shape = "Slip color"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1)
  )

Comp_Colored_slips_plot 

