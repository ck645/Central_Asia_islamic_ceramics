##Uzbekistan glazed ceramics
# C. Klesner
# 2025

library(rio)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
library(GGally)
library(stats)
library(dendextend)
library(ggtern)
library(RColorBrewer)

setwd("~/GitHub/Central_Asia_islamic_ceramics/Scripts")

shape_values <- c(18, 15, 1, 16, 2, 0, 3, 17, 4, 5, 6)

Group_colors <- c(
  "BUK" = "#CC6677", "BUK A" = "#332288", "Outlier" = "#000000", 
  "NISH" = "#DAA520", "PAY 1" = "#999933", "PAY 2" = "#88CCEE", 
  "PAY 3" = "#AA4499", "SAMK" = "#882255", "TASH" = "#44AA99", 
  "TAZ" = "#117733", "unassigned" = "#bbbbbb"
)

#---------------------------Paste compositional analysis------------------------

## Importing the dataset
paste_database <- rio::import("paste_compositional_data.csv")

## Cleaning the dataset

head(paste_database)

paste_database$ANID <- as.factor(paste_database$ANID)
paste_database$'Site Name' <- as.factor(paste_database$'Site Name')
paste_database$'NAA Group' <- as.factor(paste_database$'NAA Group')
paste_database$Ware <- as.factor(paste_database$Ware)
paste_database$Date <- as.factor(paste_database$Date)

## statistics

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

write.csv(stats_comp, "NAA_group_stats.csv", row.names = FALSE)

# subset the data into statistically significant groups

database_group_subset <- database_group %>%
  filter(database_group$'NAA Group' %in% c("BUK", "BUK A", "NISH", "SAMK", "TASH",  
                                           "TAZ"))


#Pairwise t-tests

pairwise_t_test_results <- list()
for (element in elements_Group) {
  t_test_result <- pairwise.t.test(stats[[element]], stats$`NAA Group`, 
                                   p.adjust.method = "bonferroni")
  pairwise_t_test_results[[element]] <- t_test_result
}

pairwise_t_test_results

significant_elements_by_pair <- list()

# Loop through each element's pairwise comparison result
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

# Convert to data frame
library(tibble)
significant_df <- tibble(
  Pair = names(significant_elements_by_pair),
  Elements = sapply(significant_elements_by_pair, function(x) paste(sort(x), collapse = ", "))
)

# Show the result
print(significant_df)


## Element biplots

database_group_sampled <- database_group %>%
  filter(database_group$'Site Name' %in% c("Bukhara", "Paykand", "Taraz", "Tashkent"))

ellipse_database_group <- database_group %>%
  filter(database_group$'NAA Group' %in% c("BUK", "BUK A", "TASH", "TAZ"))

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

# Save PNG file
png(filename = "~/GitHub/Central_Asia_islamic_ceramics/Figures/Figure3.png", width = 3600, height = 2400, res=300)
plot(La_vs_Fe_ellipse)
dev.off()

# ---------------------------Pairwise analysis ---------------------------------

setwd("~/GitHub/Central_Asia_islamic_ceramics/Supplementary_materials/supplementary_materials_pairwise_comparisons")

# Loop through each row in significant_df
for (i in seq_len(nrow(significant_df))) {
  pair <- significant_df$Pair[i]
  elements <- strsplit(significant_df$Elements[i], ",\\s*")[[1]]  # split element string into vector
  sites <- strsplit(pair, " vs ")[[1]]
  site1 <- sites[1]
  site2 <- sites[2]
  database_group_site_comp <- database_group %>%
    filter(`NAA Group` %in% c(site1, site2))
  print("Processing pair:")
  print(paste("Sites:", site1, "and", site2))
  valid_elements <- elements[elements %in% colnames(database_group_site_comp)]
  if (length(valid_elements) == 0) {
    print(paste("No valid elements for pair:", pair))
    next
  }
  plot_data <- database_group_site_comp %>%
    select(all_of(valid_elements), `NAA Group`) %>%
    mutate(`NAA Group` = factor(`NAA Group`, levels = c(site1, site2)))
  scatterplot_matrix <- ggpairs(
    plot_data,
    aes(color = `NAA Group`, alpha = 0.5),
    title = paste("Scatterplot Matrix for", site1, "and", site2),
    progress = FALSE,
    cardinality_threshold = 32
  ) +
    scale_color_manual(name = "NAA Group", values = Group_colors) +
    scale_fill_manual(name = "NAA Group", values = Group_colors)
  print(scatterplot_matrix)
  png(filename = paste0("Scatterplot_Matrix_", gsub(" vs ", "_", pair), ".png"),
      width = 4800, height = 3600, res = 300)
  print(scatterplot_matrix)
  dev.off()
}

# Compare Paykand Groups
Paykand_groups <- c("PAY 1", "PAY 2", "PAY 3")

pairwise_comparisons <- combn(Paykand_groups, 2, simplify = FALSE)

# Loop through each pairwise comparison
for (pair in pairwise_comparisons) {
  site1 <- pair[1]
  site2 <- pair[2]
  pair_name <- paste(site1, "vs", site2)
  database_group_site_comp <- database_group %>%
    filter(`NAA Group` %in% c(site1, site2))
  valid_elements <- elements_Group[elements_Group %in% colnames(database_group_site_comp)]
  if (length(valid_elements) == 0) {
    message("No valid elements found for pair: ", pair_name)
    next
  }
  plot_data <- database_group_site_comp %>%
    select(all_of(valid_elements), `NAA Group`) %>%
    mutate(`NAA Group` = factor(`NAA Group`, levels = c(site1, site2)))
  scatterplot_matrix <- ggpairs(
    plot_data,
    aes(color = `NAA Group`, alpha = 0.5),
    title = paste("Scatterplot Matrix for", site1, "and", site2),
    progress = FALSE,
    cardinality_threshold = 32
  ) +
    scale_color_manual(name = "NAA Group", values = Group_colors) +
    scale_fill_manual(name = "NAA Group", values = Group_colors)
  print(scatterplot_matrix)
  png(filename = paste0("Scatterplot_Matrix_", gsub(" ", "_", site1), "_vs_", gsub(" ", "_", site2), ".png"),
      width = 4800, height = 3600, res = 300)
  print(scatterplot_matrix)
  dev.off()
}


# -----------------------select biplots from pairwise comp----------------------

setwd("~/GitHub/Central_Asia_islamic_ceramics/Figures/Table_4")

database_group_BUK_BUKA <- database_group %>%
  filter(`NAA Group` %in% c("BUK", "BUK A"))

BUK_BUKA_site_comp <- ggplot(database_group_BUK_BUKA, aes(x = Sr, y = Fe, 
                                                color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_BUK_BUKA, aes(group = `NAA Group`, color = `NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Sr (ppm)", y = "Fe (ppm)") +
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )

png(filename = "BUK_BUKA_site_comp.png", width = 1200, height = 800, res=300)
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
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )

png(filename = "BUK_TASH_site_comp.png", width = 1200, height = 800, res=300)
plot(BUK_TASH_site_comp)
dev.off()


database_group_BUK_TAZ <- database_group %>%
  filter(`NAA Group` %in% c("BUK", "TAZ"))

BUK_TAZ_site_comp <- ggplot(database_group_BUK_TAZ, aes(x = Sb, y = Ce, 
                                                          color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_BUK_TAZ, aes(group = `NAA Group`, color = `NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Sb (ppm)", y = "Ce (ppm)") +
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )

png(filename = "BUK_TAZ_site_comp.png", width = 1200, height = 800, res=300)
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
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )

png(filename = "BUK_SAMK_site_comp.png", width = 1200, height = 800, res=300)
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
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )

png(filename = "BUK_NISH_site_comp.png", width = 1200, height = 800, res=300)
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
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )

png(filename = "BUKA_TASH_site_comp.png", width = 1200, height = 800, res=300)
plot(BUKA_TASH_site_comp)
dev.off()


database_group_BUKA_TAZ <- database_group %>%
  filter(`NAA Group` %in% c("BUK A", "TAZ"))

BUKA_TAZ_site_comp <- ggplot(database_group_BUKA_TAZ, aes(x = Sr, y = Sm, 
                                                            color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_BUKA_TAZ, aes(group = `NAA Group`, color =`NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Sr (ppm)", y = "Sm (ppm)") +
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )

png(filename = "BUKA_TAZ_site_comp.png", width = 1200, height = 800, res=300)
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
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )

png(filename = "BUKA_SAMK_site_comp.png", width = 1200, height = 800, res=300)
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
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )

png(filename = "BUKA_NISH_site_comp.png", width = 1200, height = 800, res=300)
plot(BUKA_NISH_site_comp)
dev.off()



database_group_TASH_TAZ <- database_group %>%
  filter(`NAA Group` %in% c("TASH", "TAZ"))

TASH_TAZ_site_comp <- ggplot(database_group_TASH_TAZ, aes(x = Sb, y = As, 
                                                            color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_TASH_TAZ, aes(group = `NAA Group`, color =`NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Sb (ppm)", y = "As (ppm)") +
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )

png(filename = "TASH_TAZ_site_comp.png", width = 1200, height = 800, res=300)
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
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )

png(filename = "TASH_SAMK_site_comp.png", width = 1200, height = 800, res=300)
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
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )

png(filename = "TASH_NISH_site_comp.png", width = 1200, height = 800, res=300)
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
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )

png(filename = "SAMK_NISH_site_comp.png", width = 1200, height = 800, res=300)
plot(SAMK_NISH_site_comp)
dev.off()



database_group_TAZ_NISH <- database_group %>%
  filter(`NAA Group` %in% c("TAZ", "NISH"))

TAZ_NISH_site_comp <- ggplot(database_group_TAZ_NISH, aes(x = Sb, y = Mn, 
                                                            color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_TAZ_NISH, aes(group = `NAA Group`, color =`NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Sb (ppm)", y = "Mn (ppm)") +
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )

png(filename = "TAZ_NISH_site_comp.png", width = 1200, height = 800, res=300)
plot(TAZ_NISH_site_comp)
dev.off()



database_group_TAZ_SAMK <- database_group %>%
  filter(`NAA Group` %in% c("TAZ", "SAMK"))

TAZ_SAMK_site_comp <- ggplot(database_group_TAZ_SAMK, aes(x = Hf, y = Mn, 
                                                          color = `NAA Group`, shape = `NAA Group`)) +
  geom_point(size=2) +
  stat_ellipse(data = database_group_TAZ_SAMK, aes(group = `NAA Group`, color =`NAA Group`), 
               level = 0.90, geom = "path") +
  labs(x = "Hf (ppm)", y = "Mn (ppm)") +
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
    legend.position = "right"
  )

png(filename = "TAZ_SAMK_site_comp.png", width = 1200, height = 800, res=300)
plot(TAZ_SAMK_site_comp)
dev.off()

#---------------------------Glaze compositional analysis------------------------

setwd("~/GitHub/Central_Asia_islamic_ceramics/Scripts")

# Importing the dataset
T_database <- rio::import("transparent_glaze_composition.csv")
O_database <- rio::import("opaque_glaze_ternary.csv")

#cleaning the dataset
T_database$`Catalogue number`<- as.factor(T_database$`Catalogue number`)
T_database$Ware <- as.factor(T_database$Ware)
T_database$`NAA Group` <- as.factor(T_database$`NAA Group`)


O_database$ID<- as.factor(O_database$ID)
O_database$Color <- as.factor(O_database$Color)
O_database$Group <- as.factor(O_database$Group)



# Subsetting the Opaque database into groups
O_db <- filter(O_database, `Group` %in% c('BUK A', 'N/A', 'SAMK', 'TASH'))
O_comp <- filter(O_database, `Group` %in% c('Central Asia','Egypt and Levant', 'Mesopotamian'))



# Ternary

combined_ternary <- ggtern(data=O_db, aes(x=PbO, y=SiO2, z=alkali, color=Color, shape=Group)) +
  geom_point(size=2.5) +
  labs(title = "Opaque Glaze ternary", x = "PbO (%)", y = "SiO2 (%)", z= "alkali (%)") +
  scale_color_manual(name = "Opaque Glaze Color",
                     values = c("Yellow" = "red", "White" = "black", "Ishkor" = "blue")
  ) +
  scale_shape_discrete(name = "Group") +
  geom_point(data=O_comp, aes(x=PbO, y=SiO2, z=alkali, fill=Color),
             shape=21, color="black", size=3, stroke=0.5, alpha=0.3) +
  scale_fill_manual(name = "Comparative Opaque Glaze Color",
                    values = c("Yellow" = "red", "White" = "darkgrey", "Ishkor" = "blue")
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

png(filename = "~/GitHub/Central_Asia_islamic_ceramics/Figures/Figure4.png", width = 3600, height = 2400, res=300)
plot(combined_ternary)
dev.off()



#-----------------------# Transparent Glaze PCA ---------------------------------

#ensure no N/A values in major elements

detection_limit_Mg <- 0.01
replacement_value_Mg <- (2/3) * detection_limit_Mg

T_database <- T_database %>%
  mutate(Mg = ifelse(is.na(Mg), replacement_value_Mg, Mg))

detection_limit_Na <- 0.01
replacement_value_Na <- (2/3) * detection_limit_Na

T_database <- T_database %>%
  mutate(Na = ifelse(is.na(Na), replacement_value_Na, Na))

#select major elements (Pb, Si, Al, Na, Mg, Ca, Fe, K)
pc <- prcomp(T_database[,c("Pb", "Si", "Al", "Na", "Mg", "Ca", "Fe", "K")],
             center = TRUE,
             scale. = TRUE) 
pc.dataframe <- as.data.frame(pc$x)
pc.dataframe <- data.frame(pc.dataframe, Ware = T_database$Ware, NAA = T_database$`NAA Group`, Sample = T_database$`Catalogue number`)


# Loadings scaled for visualization
loadings <- as.data.frame(pc$rotation)
loadings_scaled <- loadings*8
loadings_scaled$Variable <- rownames(loadings)

# Calculate percentage contribution of each PC
total_variance <- sum(pc$sdev^2)  # Total variance (sum of eigenvalues)
pc_contributions <- (pc$sdev^2 / total_variance) * 100
print(pc_contributions)


## plot by Paste compositional group

ellipse_NAA_pc.dataframe <- pc.dataframe %>% 
  filter(NAA %in% c("BUK", "TASH")) 

g4 <- ggplot(pc.dataframe, aes(x = PC1, y = PC2, color = NAA)) +
  geom_point(size=2) +
  theme_minimal() +
  stat_ellipse(data = ellipse_NAA_pc.dataframe, aes(group = NAA, color = NAA), level = 0.90, geom = "path") +
  geom_segment(data = loadings_scaled, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_text(data = loadings_scaled, aes(x = PC1, y = PC2, label = Variable), hjust = 0.3, vjust = 1, size = 5, color = "black") +
  labs(title = "PCA Biplot for select elements - 90% confidence ellipses", x = "PC1 (47.54%)", y = "PC2 (15.57%)") +
  xlim(-6, 6.5) +
  ylim(-6, 6) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  scale_color_manual(name = "NAA Group", values = Group_colors) +
  theme(legend.position = "right")


png(filename = "~/GitHub/Central_Asia_islamic_ceramics/Figures/Figure6a.png", width = 3600, height = 2400, res=300)
print(g4) + theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1))
dev.off()


## plot by Ware

ellipse_pc.dataframe <- pc.dataframe %>% 
  filter(Ware %in% c("Samanid - Slipware", "Slipware - Qarakhanid", "splashed ware", "monochrome green")) 

g2 <- ggplot(pc.dataframe, aes(x = PC1, y = PC2, color = Ware, shape = NAA)) +
  geom_point(size=2) +
  theme_minimal() +
  stat_ellipse(data = ellipse_pc.dataframe, aes(group = Ware, color = Ware), level = 0.90, geom = "path") +
  labs(title = "PCA Biplot for select elements - 90% confidence ellipses", x = "PC1 (47.54%)", y = "PC2 (15.57%)") +
  xlim(-6, 6.5) +
  ylim(-6, 6) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  scale_color_brewer(name = "Ware", palette = "Set1") +
  scale_shape_manual(name = "NAA Group", values = shape_values) +
  theme(legend.position = "right")

png(filename = "~/GitHub/Central_Asia_islamic_ceramics/Figures/Figure6b.png", width = 3600, height = 2400, res=300)
print(g2) + theme(axis.line = element_line(color = "black", linewidth = 1, linetype = 1))
dev.off()

#-------------------------------citation----------------------------------------

citation("rio")
citation("dplyr")
citation("ggplot2")
citation("GGally")
citation("dendextend")
citation("ggtern")
citation("ggbiplot")


