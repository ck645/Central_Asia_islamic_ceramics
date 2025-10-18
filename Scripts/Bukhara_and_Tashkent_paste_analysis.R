##Uzbekistan glazed ceramics - Paste analysis
# C. Klesner
# 2025

install.packages(c("rio", "dplyr", "ggplot2", "tidyr", "GGally", "stats"))

library(rio)
library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)
library(stats)

shape_values <- c(18, 15, 1, 16, 2, 0, 3, 17, 4, 5, 6)

Group_colors <- c(
  "BUK" = "#CC6677", "BUK A" = "#332288", "Outlier" = "#000000", 
  "NISH" = "#DAA520", "PAY 1" = "#999933", "PAY 2" = "#88CCEE", 
  "PAY 3" = "#AA4499", "SAMK" = "#882255", "TASH" = "#44AA99", 
  "TAZ - Group 3" = "#117733", "unassigned" = "#bbbbbb"
)

#---------------------------Paste compositional analysis------------------------

## Importing the dataset
paste_database <- rio::import("./Data/paste_compositional_data.csv")

## Cleaning the dataset
paste_database$ANID <- as.factor(paste_database$ANID)
paste_database$'Site Name' <- as.factor(paste_database$'Site Name')
paste_database$'NAA Group' <- as.factor(paste_database$'NAA Group')
paste_database$Ware <- as.factor(paste_database$Ware)
paste_database$Date <- as.factor(paste_database$Date)


## Paste Group summary statistics
database_group <- paste_database %>% select(ANID, 'Site Name', 'NAA Group', 'Ware', 
                                      Ce, Co, Cr, Cs, Eu, Fe, Hf, Ni, Rb, Sb, Sc,
                                      Sr, Ta, Tb, Th, Zn, As, La, Lu, Nd, Sm, U, Yb,
                                      Al, Ba, Ca, Dy, K, Mn, Na, Ti, V)

elements_Group <- c("Ce", "Co", "Cr", "Cs", "Eu", "Fe", "Hf", "Ni", "Rb", "Sb", "Sc",
                    "Sr", "Ta", "Tb", "Th", "Zn", "As", "La", "Lu", "Nd", "Sm", "U", 
                    "Yb", "Al", "Ba", "Ca", "Dy", "K", "Mn", "Na", "Ti", "V")

# convert major elements to oxides
oxide_factors <- c(
  Na = 1.347970769,
  Al = 1.8894415,
  K  = 1.204601258,
  Ca = 1.399196567,
  Fe = 1.429734085,
  Ti = 1.668477239
)

database_group <- database_group %>%
  mutate(
    Na2O  = Na * oxide_factors["Na"] * 1e-4,
    Al2O3 = Al * oxide_factors["Al"] * 1e-4,
    K2O   = K  * oxide_factors["K"]  * 1e-4,
    CaO   = Ca * oxide_factors["Ca"] * 1e-4,
    Fe2O3 = Fe * oxide_factors["Fe"] * 1e-4,
    TiO2  = Ti * oxide_factors["Ti"] * 1e-4
  )

oxide_columns <- c("Na2O", "Al2O3", "K2O", "CaO", "Fe2O3", "TiO2")

all_columns_to_summarise <- c(elements_Group, oxide_columns)

# Compute stats
stats_comp <- database_group %>%
  select(ANID, `NAA Group`, all_of(all_columns_to_summarise)) %>%
  group_by(`NAA Group`) %>%
  summarise(across(
    all_of(all_columns_to_summarise),
    list(
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      rsd = ~ifelse(mean(.x, na.rm = TRUE) == 0, NA, sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100)
    ),
    .names = "{.col}_{.fn}"
  ))

stats_comp

write.csv(stats_comp, "./Data/NAA_group_stats.csv", row.names = FALSE)




## Paste Group statistics (will receive warnings regarding NAs, but can proceed)

database_group_subset <- database_group %>%
  filter(database_group$'NAA Group' %in% c("BUK", "BUK A", "NISH", "SAMK", "TASH",  
                                           "TAZ - Group 3"))
stats <- database_group_subset %>%
  select(ANID, 'NAA Group', all_of(elements_Group))
stats_clean <- stats[stats$`NAA Group` != "Outlier" & 
                       stats$`NAA Group` != "PAY 1" & 
                       stats$`NAA Group` != "PAY 2" & 
                       stats$`NAA Group` != "PAY 3" & 
                       stats$`NAA Group` != "unassigned", ]
stats_clean$`NAA Group` <- droplevels(stats_clean$`NAA Group`)
summary(stats_clean$`NAA Group`)

response_vars_clean <- subset(stats_clean, select = c(Ce, Co, Cr, Cs, Eu, Fe, Hf, Rb, Sb, Sc,
                                                      Sr, Ta, Tb, Th, Zn, As, La, Lu, Nd, Sm, U, Yb,
                                                      Al, Ba, Ca, Dy, K, Mn, Na, Ti, V))

response_vars_matrix_clean <- as.matrix(response_vars_clean)



# MANOVA model
manova_model_clean <- manova(response_vars_matrix_clean ~ `NAA Group`, data = stats_clean)

summary(manova_model_clean)

summary(manova_model_clean, test = "Pillai")
summary(manova_model_clean, test = "Wilks")
summary(manova_model_clean, test = "Hotelling-Lawley")
summary(manova_model_clean, test = "Roy")

summary.aov(manova_model_clean)





# pairwise t-tests (will receive warnings regarding NAs, but can proceed)

pairwise_t_test_results <- list()
for (element in elements_Group) {
  t_test_result <- pairwise.t.test(stats[[element]], stats$`NAA Group`, 
                                   p.adjust.method = "bonferroni")
  pairwise_t_test_results[[element]] <- t_test_result
}
pairwise_t_test_results


fileConn <- file("./Data/pairwise_t_test_results.txt", "w")
for (element in elements_Group) {
  t_test_result <- pairwise.t.test(stats_clean[[element]], stats_clean$`NAA Group`, 
                                   p.adjust.method = "bonferroni")
  capture.output(cat("\n", element, "\n\n"), file = fileConn)
  capture.output(print(t_test_result), file = fileConn)
}
close(fileConn)



# Summary of elements with statistically significantly different means
significant_elements_by_pair <- list()

for (element in names(pairwise_t_test_results)) {
  p_matrix <- as.matrix(pairwise_t_test_results[[element]]$p.value)
  row_names <- rownames(p_matrix)
  col_names <- colnames(p_matrix)
    for (i in seq_len(nrow(p_matrix))) {
    for (j in seq_len(ncol(p_matrix))) {
      if (!is.na(p_matrix[i, j]) && p_matrix[i, j] < 0.05) {
        pair <- paste(row_names[i], col_names[j], sep = " vs ")
        significant_elements_by_pair[[pair]] <- c(significant_elements_by_pair[[pair]], element)
      }
    }
  }
}
significant_df <- tibble(
  Pair = names(significant_elements_by_pair),
  Elements = sapply(significant_elements_by_pair, function(x) paste(sort(x), collapse = ", "))
)
print(significant_df)

write.csv(significant_df, "./Data/significant_df.csv", row.names = FALSE)




## Visualising Paste Group compositional difference through Element biplots

# selecting samples analysed in this study (UZB001-UZB150)
database_group_sampled <- database_group %>%
  filter(
    `Site Name` %in% c("Bukhara", "Paykand", "Tashkent", "Paykend") |
      (`Site Name` == "Taraz" & Ware == "slipware") | (`Site Name` == "Taraz" & Ware == "monochrome")
  )

ellipse_database_group <- database_group_sampled %>%
  filter(database_group_sampled$'NAA Group' %in% c("BUK", "BUK A", "TASH"))

La_vs_Fe_ellipse <- ggplot(database_group_sampled, aes(x = Fe, y = La, color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size = 2) +
  stat_ellipse(data = ellipse_database_group, 
               aes(x = Fe, y = La, group = `NAA Group`, color = `NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Fe (ppm)", y = "La (ppm)") +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(name = "Compositional Group", values = Group_colors) +
  scale_shape_manual(name = "Compositional Group", values = shape_values) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )

La_vs_Fe_ellipse

png(filename = "./Figures/Figure3.png", width = 3600, height = 2400, res=300)
plot(La_vs_Fe_ellipse)
dev.off()


jpeg(filename = "./Figures/Figure3.jpg", width = 3600, height = 2400, res = 300)
plot(La_vs_Fe_ellipse)
dev.off()

# ---------------------------Pairwise analysis ---------------------------------

out_dir <- "Supplementary_materials/supplementary_materials_pairwise_comparisons"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Loop through each row in significant_df
for (i in seq_len(nrow(significant_df))) {
  pair <- significant_df$Pair[i]
  elements <- strsplit(significant_df$Elements[i], ",\\s*")[[1]]
  
  # Split the pair into site names
  sites <- strsplit(pair, " vs ")[[1]]
  site1 <- sites[1]
  site2 <- sites[2]
  
  # Filter the dataset for the two NAA groups
  database_group_site_comp <- database_group %>%
    filter(`NAA Group` %in% c(site1, site2))
  
  valid_elements <- elements[elements %in% colnames(database_group_site_comp)]
  if (length(valid_elements) == 0) {
    message("No valid elements for pair: ", pair)
    next
  }
  
  # Prepare plot data
  plot_data <- database_group_site_comp %>%
    select(all_of(valid_elements <- elements[elements %in% colnames(database_group_site_comp)]), `NAA Group`) %>%
    mutate(`NAA Group` = factor(`NAA Group`, levels = c(site1, site2)))
  if (length(valid_elements) == 0) next
  
  # Plot matrix
  scatterplot_matrix <- ggpairs(
    plot_data,
    aes(color = `NAA Group`, alpha = 0.5),
    title = paste("Scatterplot Matrix for", site1, "and", site2),
    progress = FALSE,
    cardinality_threshold = 32
  ) +
    scale_color_manual(name = "NAA Group", values = Group_colors) +
    scale_fill_manual(name = "NAA Group", values = Group_colors)
  
  # Save directly to the folder
  png(file.path(out_dir, paste0("Scatterplot_Matrix_", gsub(" vs ", "_", pair), ".png")),
      width = 4800, height = 3600, res = 300)
  print(scatterplot_matrix)
  dev.off()
}

# Define the three groups to compare
Paykand_groups <- c("PAY 1", "PAY 2", "PAY 3")

# Define all unique pairwise combinations of the three groups
pairwise_comparisons <- combn(Paykand_groups, 2, simplify = FALSE)

# Loop through each pairwise comparison
for (pair in pairwise_comparisons) {
  site1 <- pair[1]
  site2 <- pair[2]
  pair_name <- paste(site1, "vs", site2)
  
  # Filter the dataset for the two NAA groups
  database_group_site_comp <- database_group %>%
    filter(`NAA Group` %in% c(site1, site2))
  
  # Check for valid elements (columns that exist in the data)
  valid_elements <- elements_Group[elements_Group %in% colnames(database_group_site_comp)]
  if (length(valid_elements) == 0) {
    message("No valid elements found for pair: ", pair_name)
    next
  }
  
  # Prepare plot data
  plot_data <- database_group_site_comp %>%
    select(all_of(valid_elements), `NAA Group`) %>%
    mutate(`NAA Group` = factor(`NAA Group`, levels = c(site1, site2)))
  
  # Create scatterplot matrix
  scatterplot_matrix <- ggpairs(
    plot_data,
    aes(color = `NAA Group`, alpha = 0.5),
    title = paste("Scatterplot Matrix for", site1, "and", site2),
    progress = FALSE,
    cardinality_threshold = 32
  ) +
    scale_color_manual(name = "NAA Group", values = Group_colors) +
    scale_fill_manual(name = "NAA Group", values = Group_colors)
  
  # Print and save the plot
  print(scatterplot_matrix)
  png(file.path(out_dir, paste0("Scatterplot_Matrix_", gsub(" vs ", "_", pair), ".png")),
      width = 4800, height = 3600, res = 300)
  print(scatterplot_matrix)
  dev.off()
}


# -----------------------select biplots from pairwise comp----------------------

database_group_BUK_BUKA <- database_group %>%
  filter(`NAA Group` %in% c("BUK", "BUK A"))

BUK_BUKA_site_comp <- ggplot(database_group_BUK_BUKA, aes(x = Sr, y = Fe, 
                                                color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_BUK_BUKA, aes(group = `NAA Group`, color = `NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Sr (ppm)", y = "Fe (ppm)") +
  scale_color_manual(values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )
BUK_BUKA_site_comp

png(filename = "./Figures/Table_4/BUK_BUKA_site_comp.png", width = 1200, height = 800, res=300)
plot(BUK_BUKA_site_comp)
dev.off()


database_group_BUK_TASH <- database_group %>%
  filter(`NAA Group` %in% c("BUK", "TASH"))

BUK_TASH_site_comp <- ggplot(database_group_BUK_TASH, aes(x = La, y = Sc, 
                                                          color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_BUK_TASH, aes(group = `NAA Group`, color = `NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "La (ppm)", y = "Sc (ppm)") +
  scale_color_manual(values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )
BUK_TASH_site_comp

png(filename = "./Figures/Table_4/BUK_TASH_site_comp.png", width = 1200, height = 800, res=300)
plot(BUK_TASH_site_comp)
dev.off()


database_group_BUK_TAZ <- database_group %>%
  filter(`NAA Group` %in% c("BUK", "TAZ - Group 3"))

BUK_TAZ_site_comp <- ggplot(database_group_BUK_TAZ, aes(x = Sb, y = Ce, 
                                                          color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_BUK_TAZ, aes(group = `NAA Group`, color = `NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Sb (ppm)", y = "Ce (ppm)") +
  scale_color_manual(values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )
BUK_TAZ_site_comp

png(filename = "./Figures/Table_4/BUK_TAZ_site_comp.png", width = 1200, height = 800, res=300)
plot(BUK_TAZ_site_comp)
dev.off()



database_group_BUK_SAMK <- database_group %>%
  filter(`NAA Group` %in% c("BUK", "SAMK"))

BUK_SAMK_site_comp <- ggplot(database_group_BUK_SAMK, aes(x = Cr, y = Sc, 
                                                        color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_BUK_SAMK, aes(group = `NAA Group`, color = `NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Cr (ppm)", y = "Sc (ppm)") +
  scale_color_manual(values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )
BUK_SAMK_site_comp

png(filename = "./Figures/Table_4/BUK_SAMK_site_comp.png", width = 1200, height = 800, res=300)
plot(BUK_SAMK_site_comp)
dev.off()



database_group_BUK_NISH <- database_group %>%
  filter(`NAA Group` %in% c("BUK", "NISH"))

BUK_NISH_site_comp <- ggplot(database_group_BUK_NISH, aes(x = Th, y = Sb, 
                                                          color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_BUK_NISH, aes(group = `NAA Group`, color =`NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Th (ppm)", y = "Sb (ppm)") +
  scale_color_manual(values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )
BUK_NISH_site_comp

png(filename = "./Figures/Table_4/BUK_NISH_site_comp.png", width = 1200, height = 800, res=300)
plot(BUK_NISH_site_comp)
dev.off()



database_group_BUKA_TASH <- database_group %>%
  filter(`NAA Group` %in% c("BUK A", "TASH"))

BUKA_TASH_site_comp <- ggplot(database_group_BUKA_TASH, aes(x = La, y = Th, 
                                                          color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_BUKA_TASH, aes(group = `NAA Group`, color =`NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "La (ppm)", y = "Th (ppm)") +
  scale_color_manual(values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )
BUKA_TASH_site_comp

png(filename = "./Figures/Table_4/BUKA_TASH_site_comp.png", width = 1200, height = 800, res=300)
plot(BUKA_TASH_site_comp)
dev.off()


database_group_BUKA_TAZ <- database_group %>%
  filter(`NAA Group` %in% c("BUK A", "TAZ - Group 3"))

BUKA_TAZ_site_comp <- ggplot(database_group_BUKA_TAZ, aes(x = Sr, y = Sm, 
                                                            color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_BUKA_TAZ, aes(group = `NAA Group`, color =`NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Sr (ppm)", y = "Sm (ppm)") +
  scale_color_manual(values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )
BUKA_TAZ_site_comp

png(filename = "./Figures/Table_4/BUKA_TAZ_site_comp.png", width = 1200, height = 800, res=300)
plot(BUKA_TAZ_site_comp)
dev.off()



database_group_BUKA_SAMK <- database_group %>%
  filter(`NAA Group` %in% c("BUK A", "SAMK"))

BUKA_SAMK_site_comp <- ggplot(database_group_BUKA_SAMK, aes(x = Cr, y = Co, 
                                                          color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_BUKA_SAMK, aes(group = `NAA Group`, color =`NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Cr (ppm)", y = "Co (ppm)") +
  scale_color_manual(values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )
BUKA_SAMK_site_comp

png(filename = "./Figures/Table_4/BUKA_SAMK_site_comp.png", width = 1200, height = 800, res=300)
plot(BUKA_SAMK_site_comp)
dev.off()




database_group_BUKA_NISH <- database_group %>%
  filter(`NAA Group` %in% c("BUK A", "NISH"))

BUKA_NISH_site_comp <- ggplot(database_group_BUKA_NISH, aes(x = Cr, y = Sb, 
                                                            color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_BUKA_NISH, aes(group = `NAA Group`, color =`NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Cr (ppm)", y = "Sb (ppm)") +
  scale_color_manual(values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )
BUKA_NISH_site_comp

png(filename = "./Figures/Table_4/BUKA_NISH_site_comp.png", width = 1200, height = 800, res=300)
plot(BUKA_NISH_site_comp)
dev.off()



database_group_TASH_TAZ <- database_group %>%
  filter(`NAA Group` %in% c("TASH", "TAZ - Group 3"))

TASH_TAZ_site_comp <- ggplot(database_group_TASH_TAZ, aes(x = Sb, y = As, 
                                                            color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_TASH_TAZ, aes(group = `NAA Group`, color =`NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Sb (ppm)", y = "As (ppm)") +
  scale_color_manual(values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )
TASH_TAZ_site_comp

png(filename = "./Figures/Table_4/TASH_TAZ_site_comp.png", width = 1200, height = 800, res=300)
plot(TASH_TAZ_site_comp)
dev.off()




database_group_TASH_SAMK <- database_group %>%
  filter(`NAA Group` %in% c("TASH", "SAMK"))

TASH_SAMK_site_comp <- ggplot(database_group_TASH_SAMK, aes(x = Th, y = Cr, 
                                                          color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_TASH_SAMK, aes(group = `NAA Group`, color =`NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Th (ppm)", y = "Cr (ppm)") +
  scale_color_manual(values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )
TASH_SAMK_site_comp

png(filename = "./Figures/Table_4/TASH_SAMK_site_comp.png", width = 1200, height = 800, res=300)
plot(TASH_SAMK_site_comp)
dev.off()



database_group_TASH_NISH <- database_group %>%
  filter(`NAA Group` %in% c("TASH", "NISH"))

TASH_NISH_site_comp <- ggplot(database_group_TASH_NISH, aes(x = Th, y = Cr, 
                                                            color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_TASH_NISH, aes(group = `NAA Group`, color =`NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Th (ppm)", y = "Cr (ppm)") +
  scale_color_manual(values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )
TASH_NISH_site_comp

png(filename = "./Figures/Table_4/TASH_NISH_site_comp.png", width = 1200, height = 800, res=300)
plot(TASH_NISH_site_comp)
dev.off()





database_group_SAMK_NISH <- database_group %>%
  filter(`NAA Group` %in% c("SAMK", "NISH"))

SAMK_NISH_site_comp <- ggplot(database_group_SAMK_NISH, aes(x = Co, y = Sc, 
                                                            color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_SAMK_NISH, aes(group = `NAA Group`, color =`NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Co (ppm)", y = "Sc (ppm)") +
  scale_color_manual(values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )
SAMK_NISH_site_comp

png(filename = "./Figures/Table_4/SAMK_NISH_site_comp.png", width = 1200, height = 800, res=300)
plot(SAMK_NISH_site_comp)
dev.off()



database_group_TAZ_NISH <- database_group %>%
  filter(`NAA Group` %in% c("TAZ - Group 3", "NISH"))

TAZ_NISH_site_comp <- ggplot(database_group_TAZ_NISH, aes(x = Sb, y = Mn, 
                                                            color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_TAZ_NISH, aes(group = `NAA Group`, color =`NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Sb (ppm)", y = "Mn (ppm)") +
  scale_color_manual(values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )
TAZ_NISH_site_comp

png(filename = "./Figures/Table_4/TAZ_NISH_site_comp.png", width = 1200, height = 800, res=300)
plot(TAZ_NISH_site_comp)
dev.off()



database_group_TAZ_SAMK <- database_group %>%
  filter(`NAA Group` %in% c("TAZ - Group 3", "SAMK"))

TAZ_SAMK_site_comp <- ggplot(database_group_TAZ_SAMK, aes(x = Hf, y = Mn, 
                                                          color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_TAZ_SAMK, aes(group = `NAA Group`, color =`NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Hf (ppm)", y = "Mn (ppm)") +
  scale_color_manual(values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )
TAZ_SAMK_site_comp

png(filename = "./Figures/Table_4/TAZ_SAMK_site_comp.png", width = 1200, height = 800, res=300)
plot(TAZ_SAMK_site_comp)
dev.off()


# ------------------------------ citations -------------------------------------

citation("rio")
citation("dplyr")
citation("ggplot2")
citation("GGally")


