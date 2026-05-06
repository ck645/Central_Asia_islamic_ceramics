##Uzbekistan glazed ceramics - Paste analysis
# C. Klesner
# 2026

install.packages(c("rio", "dplyr", "ggplot2", "tidyr", "GGally", "stats"))

library(rio)
library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)
library(stats)
library(tidyverse)
library(aqp)
library(tibble)
library(purrr)
library(scales)

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





# pairwise t-tests (will receive warnings regarding NAs due to missing values, however
# you can proceed as this behavior is expected and does not affect the validity of the results.

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



#---------------------------Trace element patterns ------------------------

elements_Group <- c("Ce", "Co", "Cr", "Cs", "Eu", "Fe", "Hf", "Rb", "Sb", "Sc",
                    "Sr", "Ta", "Tb", "Th", "Zn", "As", "La", "Lu", "Nd", "Sm", "U", 
                    "Yb", "Al", "Ba", "Ca", "Dy", "K", "Mn", "Na", "Ti", "V")

ree_elements <- c("Sc", "La", "Ce", "Nd", "Sm", "Eu", "Tb", "Dy", "Yb", "Lu")

all_elements <- c("V", "Co", "As", "Rb", "Sr", "Sb", "Cs", 
                  "La", "Ce", "Nd", "Sm", "Eu", "Tb", "Dy", "Yb", "Lu",
                  "Hf", "Ta", "Th", "U", "Sc", "Cr", "Mn", "Zn")
all_elements <- all_elements[all_elements %in% elements_Group]

# --- Remove zero/NA samples ---
database_group_subset <- database_group_subset %>%
  filter(if_all(all_of(elements_Group), ~ . != 0 & !is.na(.)))

# --- Output directory ---
dir.create("./Supplementary_Materials/Spidergrams", recursive = TRUE, showWarnings = FALSE)

# --- Color scales ---
unassigned_site_colors <- c(
  "Bukhara"  = "#C1440E",
  "Paykend"  = "#E8A95C",
  "Tashkent" = "#2E6EA6",
  "Taraz"    = "#7BB8D4",
  "BUK"      = "#CC6677",
  "TASH"     = "#44AA99"
)
buk_site_colors  <- unassigned_site_colors[c("Bukhara", "Paykend", "BUK", "TASH")]
tash_site_colors <- unassigned_site_colors[c("Tashkent", "Taraz", "BUK", "TASH")]

save_plot <- function(plot, filename, width = 3600, height = 2400, res = 300) {
  png(filename = file.path("./Supplementary_Materials/Spidergrams", filename),
      width = width, height = height, res = res)
  print(plot)
  dev.off()
}

spidergram_theme <- function() {
  list(
    theme_minimal(),
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 1, linetype = 1),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
  )
}

### REE SPIDERGRAMS (raw ppm)
ree_long <- database_group_subset %>%
  filter(`NAA Group` %in% c("BUK", "TASH")) %>%
  select(ANID, `NAA Group`, all_of(ree_elements)) %>%
  pivot_longer(cols = all_of(ree_elements), names_to = "Element", values_to = "Concentration") %>%
  mutate(Element = factor(Element, levels = ree_elements))

ree_long_2 <- database_group_subset %>%
  select(ANID, `NAA Group`, all_of(ree_elements)) %>%
  pivot_longer(cols = all_of(ree_elements), names_to = "Element", values_to = "Concentration") %>%
  mutate(Element = factor(Element, levels = ree_elements))

ree_avg   <- ree_long   %>% group_by(`NAA Group`, Element) %>% summarise(Concentration = mean(Concentration, na.rm = TRUE), .groups = "drop")
ree_avg_2 <- ree_long_2 %>% group_by(`NAA Group`, Element) %>% summarise(Concentration = mean(Concentration, na.rm = TRUE), .groups = "drop")

build_ree_plot <- function(data, avg_data, facet = FALSE) {
  p <- ggplot(data, aes(x = Element, y = Concentration, color = `NAA Group`, group = ANID)) +
    geom_line(alpha = 0.15, linewidth = 0.5) +
    geom_point(size = 1.0, alpha = 0.15) +
    geom_line(data = avg_data, aes(group = `NAA Group`), linewidth = 1.2, alpha = 1) +
    geom_point(data = avg_data, aes(group = `NAA Group`), size = 2.5, alpha = 1) +
    scale_y_log10() +
    scale_color_manual(name = "Compositional Group", values = Group_colors) +
    labs(x = "REE Element", y = "Concentration (ppm)") +
    spidergram_theme()
  if (facet) p <- p + facet_wrap(~ `NAA Group`, nrow = 6)
  p
}

ree_spidergram   <- build_ree_plot(ree_long,   ree_avg,   facet = FALSE)
ree_spidergram_2 <- build_ree_plot(ree_long_2, ree_avg_2, facet = FALSE)
ree_spidergram_3 <- build_ree_plot(ree_long_2, ree_avg_2, facet = TRUE)

save_plot(ree_spidergram,   "REE_Spidergram.png")
save_plot(ree_spidergram_2, "REE_Spidergram_2.png")
save_plot(ree_spidergram_3, "REE_Spidergram_3.png")



### TRACE ELEMENT SPIDERGRAMS (raw ppm)
trace_long <- database_group_subset %>%
  filter(`NAA Group` %in% c("BUK", "TASH")) %>%
  select(ANID, `NAA Group`, all_of(all_elements)) %>%
  pivot_longer(cols = all_of(all_elements), names_to = "Element", values_to = "Concentration") %>%
  mutate(Element = factor(Element, levels = all_elements))

trace_long_2 <- database_group_subset %>%
  select(ANID, `NAA Group`, all_of(all_elements)) %>%
  pivot_longer(cols = all_of(all_elements), names_to = "Element", values_to = "Concentration") %>%
  mutate(Element = factor(Element, levels = all_elements))

trace_avg   <- trace_long   %>% group_by(`NAA Group`, Element) %>% summarise(Concentration = mean(Concentration, na.rm = TRUE), .groups = "drop")
trace_avg_2 <- trace_long_2 %>% group_by(`NAA Group`, Element) %>% summarise(Concentration = mean(Concentration, na.rm = TRUE), .groups = "drop")

build_trace_plot <- function(data, avg_data, facet = FALSE) {
  p <- ggplot(data, aes(x = Element, y = Concentration, color = `NAA Group`, group = ANID)) +
    geom_line(alpha = 0.15, linewidth = 0.5) +
    geom_point(size = 1.0, alpha = 0.15) +
    geom_line(data = avg_data, aes(group = `NAA Group`), linewidth = 1.2, alpha = 1) +
    geom_point(data = avg_data, aes(group = `NAA Group`), size = 2.5, alpha = 1) +
    scale_y_log10() +
    scale_color_manual(name = "Compositional Group", values = Group_colors) +
    labs(x = "Element", y = "Concentration (ppm)") +
    spidergram_theme()
  if (facet) p <- p + facet_wrap(~ `NAA Group`, nrow = 6)
  p
}

trace_spidergram   <- build_trace_plot(trace_long,   trace_avg,   facet = FALSE)
trace_spidergram_2 <- build_trace_plot(trace_long_2, trace_avg_2, facet = FALSE)
trace_spidergram_3 <- build_trace_plot(trace_long_2, trace_avg_2, facet = TRUE)

save_plot(trace_spidergram,   "Trace_Spidergram.png")
save_plot(trace_spidergram_2, "Trace_Spidergram_2.png")
save_plot(trace_spidergram_3, "Trace_Spidergram_3.png")


### NORMALISED SPIDERGRAMS (dataset mean)

norm_long_2 <- trace_long_2  # reuse - same data

element_means <- norm_long_2 %>%
  group_by(Element) %>%
  summarise(mean_conc = mean(Concentration, na.rm = TRUE), .groups = "drop")

normalise <- function(df) {
  df %>%
    left_join(element_means, by = "Element") %>%
    mutate(Concentration_norm = Concentration / mean_conc)
}

norm_long   <- normalise(trace_long)
norm_long_2 <- normalise(trace_long_2)

norm_avg   <- norm_long   %>% group_by(`NAA Group`, Element) %>% summarise(Concentration_norm = mean(Concentration_norm, na.rm = TRUE), .groups = "drop")
norm_avg_2 <- norm_long_2 %>% group_by(`NAA Group`, Element) %>% summarise(Concentration_norm = mean(Concentration_norm, na.rm = TRUE), .groups = "drop")

build_norm_plot <- function(data, avg_data, colors = Group_colors, 
                            color_var = "`NAA Group`", facet = FALSE, ylim_max = NA) {
  p <- ggplot(data, aes(x = Element, y = Concentration_norm,
                        color = .data[[gsub("`", "", color_var)]], group = ANID)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    geom_line(alpha = 0.15, linewidth = 0.5) +
    geom_point(size = 1.0, alpha = 0.15) +
    geom_line(data = avg_data, aes(group = `NAA Group`, color = `NAA Group`), linewidth = 1.2, alpha = 1) +
    geom_point(data = avg_data, aes(group = `NAA Group`, color = `NAA Group`), size = 2.5, alpha = 1) +
    scale_color_manual(name = "Compositional Group", values = colors) +
    labs(x = "Element", y = "Concentration / Dataset Mean") +
    coord_cartesian(ylim = c(NA, ylim_max)) +
    spidergram_theme()
  if (facet) p <- p + facet_wrap(~ `NAA Group`, nrow = 6)
  p
}

norm_spidergram   <- build_norm_plot(norm_long,   norm_avg,   ylim_max = NA)
norm_spidergram_2 <- build_norm_plot(norm_long_2, norm_avg_2, ylim_max = NA)
norm_spidergram_3 <- build_norm_plot(norm_long_2, norm_avg_2, ylim_max = NA, facet = TRUE)

save_plot(norm_spidergram,   "Norm_Spidergram.png")
save_plot(norm_spidergram_2, "Norm_Spidergram_2.png")
save_plot(norm_spidergram_3, "Norm_Spidergram_3.png")


### OUTLIER & UNASSIGNED SPIDERGRAMS
ou_long <- database_group %>%
  filter(`NAA Group` %in% c("Outlier", "unassigned")) %>%
  select(ANID, `NAA Group`, `Site Name`, all_of(all_elements)) %>%
  pivot_longer(cols = all_of(all_elements), names_to = "Element", values_to = "Concentration") %>%
  mutate(Element = factor(Element, levels = all_elements)) %>%
  left_join(element_means, by = "Element") %>%
  mutate(Concentration_norm = Concentration / mean_conc)

outlier_long        <- ou_long %>% filter(`NAA Group` == "Outlier")
unassigned_long     <- ou_long %>% filter(`NAA Group` == "unassigned")
unassigned_buk_long <- unassigned_long %>% filter(`Site Name` %in% c("Bukhara", "Paykend"))
unassigned_tash_long <- unassigned_long %>% filter(`Site Name` %in% c("Tashkent", "Taraz"))

bt_avg <- database_group %>%
  filter(`NAA Group` %in% c("BUK", "TASH")) %>%
  select(ANID, `NAA Group`, all_of(all_elements)) %>%
  pivot_longer(cols = all_of(all_elements), names_to = "Element", values_to = "Concentration") %>%
  mutate(Element = factor(Element, levels = all_elements)) %>%
  left_join(element_means, by = "Element") %>%
  mutate(Concentration_norm = Concentration / mean_conc) %>%
  group_by(`NAA Group`, Element) %>%
  summarise(Concentration_norm = mean(Concentration_norm, na.rm = TRUE), .groups = "drop")

build_ou_plot <- function(data, colors, ylim_max = 2, color_col = "Site Name") {
  ggplot(data, aes(x = Element, y = Concentration_norm,
                   color = .data[[color_col]], group = ANID)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    geom_line(alpha = 0.3, linewidth = 0.5) +
    geom_point(size = 1.0, alpha = 0.5) +
    geom_line(data = bt_avg, aes(group = `NAA Group`, color = `NAA Group`), linewidth = 1.2, alpha = 1) +
    geom_point(data = bt_avg, aes(group = `NAA Group`, color = `NAA Group`), size = 2.5, alpha = 1) +
    scale_color_manual(name = "Group / Site", values = colors) +
    labs(x = "Element", y = "Concentration / Dataset Mean") +
    coord_cartesian(ylim = c(NA, ylim_max)) +
    spidergram_theme()
}

outlier_spidergram        <- build_ou_plot(outlier_long,        Group_colors,      ylim_max = 5, color_col = "NAA Group")
unassigned_spidergram     <- build_ou_plot(unassigned_long,     unassigned_site_colors, ylim_max = 2.5)
unassigned_buk_spidergram <- build_ou_plot(unassigned_buk_long, buk_site_colors,   ylim_max = 2.5)
unassigned_tash_spidergram <- build_ou_plot(unassigned_tash_long, tash_site_colors, ylim_max = 2)

save_plot(outlier_spidergram,         "Outlier_Spidergram.png")
save_plot(unassigned_spidergram,      "Unassigned_Spidergram.png")
save_plot(unassigned_buk_spidergram,  "Unassigned_BUK_Spidergram.png")
save_plot(unassigned_tash_spidergram, "Unassigned_TASH_Spidergram.png")


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

png(filename = "./Figures/Table_3/BUK_BUKA_site_comp.png", width = 1200, height = 800, res=300)
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

png(filename = "./Figures/Table_3/BUK_TASH_site_comp.png", width = 1200, height = 800, res=300)
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

png(filename = "./Figures/Table_3/BUK_TAZ_site_comp.png", width = 1200, height = 800, res=300)
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

png(filename = "./Figures/Table_3/BUK_SAMK_site_comp.png", width = 1200, height = 800, res=300)
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

png(filename = "./Figures/Table_3/BUK_NISH_site_comp.png", width = 1200, height = 800, res=300)
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

png(filename = "./Figures/Table_3/BUKA_TASH_site_comp.png", width = 1200, height = 800, res=300)
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

png(filename = "./Figures/Table_3/BUKA_TAZ_site_comp.png", width = 1200, height = 800, res=300)
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

png(filename = "./Figures/Table_3/BUKA_SAMK_site_comp.png", width = 1200, height = 800, res=300)
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

png(filename = "./Figures/Table_3/BUKA_NISH_site_comp.png", width = 1200, height = 800, res=300)
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

png(filename = "./Figures/Table_3/TASH_TAZ_site_comp.png", width = 1200, height = 800, res=300)
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

png(filename = "./Figures/Table_3/TASH_SAMK_site_comp.png", width = 1200, height = 800, res=300)
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

png(filename = "./Figures/Table_3/TASH_NISH_site_comp.png", width = 1200, height = 800, res=300)
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

png(filename = "./Figures/Table_3/SAMK_NISH_site_comp.png", width = 1200, height = 800, res=300)
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

png(filename = "./Figures/Table_3/TAZ_NISH_site_comp.png", width = 1200, height = 800, res=300)
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

png(filename = "./Figures/Table_3/TAZ_SAMK_site_comp.png", width = 1200, height = 800, res=300)
plot(TAZ_SAMK_site_comp)
dev.off()






# ------------------------- MUNSELL COLOUR ANALYSIS ----------------------------

library(tidyverse)
library(aqp)
library(tibble)
library(purrr)
library(scales)

# --- Load data ---
munsell_data <- rio::import("./Data/munsell_values.csv")

# --- Clean column names ---
munsell_data <- munsell_data %>%
  rename(
    ANID        = ANID,
    NAA_Group   = `NAA Group`,
    Munsell     = `Munsell value`,
    Color_desc  = `Color description`
  ) %>%
  mutate(
    Munsell   = trimws(Munsell),
    NAA_Group = trimws(NAA_Group)
  )


# Get unique valid Munsell codes
munsell_values <- munsell_data %>%
  filter(!is.na(Munsell), Munsell != "") %>%
  pull(Munsell) %>%
  unique()

# Clean: replace "/" with " " for aqp parsing
munsell_values_clean <- gsub("/", " ", munsell_values)

# Split into hue, value, chroma
munsell_split <- str_split(munsell_values_clean, " ")

hues   <- sapply(munsell_split, function(x) x[1])
values <- sapply(munsell_split, function(x) as.numeric(x[2]))
chroma <- sapply(munsell_split, function(x) ifelse(length(x) > 2, as.numeric(x[3]), NA_real_))

# Convert to hex
munsell_hex_codes <- munsell2rgb(the_hue = hues, the_value = values, the_chroma = chroma)

# Build lookup table
munsell_lookup <- tibble(
  Munsell       = munsell_values,
  Munsell_clean = munsell_values_clean,
  munsell_hex   = munsell_hex_codes
)

# Join hex codes onto main data
munsell_data <- munsell_data %>%
  left_join(munsell_lookup, by = "Munsell")

# Sanity check - any missing hex?
missing_hex <- munsell_data %>%
  filter(is.na(munsell_hex), !is.na(Munsell), Munsell != "") %>%
  distinct(Munsell)
if (nrow(missing_hex) > 0) message("Missing hex for: ", paste(missing_hex$Munsell, collapse = ", "))

# Count Munsell values per NAA Group
munsell_long <- munsell_data %>%
  filter(
    !is.na(Munsell),    Munsell != "",
    !is.na(munsell_hex),
    !is.na(NAA_Group),  NAA_Group != ""
  ) %>%
  count(NAA_Group, Munsell, munsell_hex, name = "Freq") %>%
  group_by(NAA_Group) %>%
  mutate(
    n_total = sum(Freq),
    prop    = Freq / n_total
  ) %>%
  ungroup()

# Named palette: Munsell code -> hex
munsell_palette <- munsell_long %>%
  distinct(Munsell, munsell_hex) %>%
  deframe()

# Facet labels with n totals
facet_labs <- munsell_long %>%
  distinct(NAA_Group, n_total) %>%
  mutate(label = paste0(NAA_Group, " (n=", n_total, ")")) %>%
  select(NAA_Group, label) %>%
  deframe()

munsell_pie_facets <- ggplot(munsell_long, aes(x = 1, y = prop, fill = Munsell)) +
  geom_col(width = 1, color = NA) +
  coord_polar(theta = "y") +
  facet_wrap(~ NAA_Group, labeller = labeller(NAA_Group = facet_labs)) +
  scale_fill_manual(values = munsell_palette) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Munsell Value Distribution by NAA Compositional Group")

munsell_pie_facets

munsell_pie_plots <- munsell_long %>%
  split(.$NAA_Group) %>%
  imap(~ {
    df <- .x %>%
      arrange(desc(Munsell)) %>%
      mutate(
        lab  = if_else(
          prop >= 0.02,
          paste0(Munsell, "\n", scales::percent(prop, accuracy = 1)),
          NA_character_
        ),
        ypos = cumsum(prop) - 0.5 * prop
      )
    
    n_tot <- unique(df$n_total)[1]
    
    ggplot(df, aes(x = 1, y = prop, fill = Munsell)) +
      geom_col(width = 1, color = NA) +
      coord_polar(theta = "y", clip = "off") +
      scale_fill_manual(values = munsell_palette) +
      theme_void() +
      theme(
        legend.position = "none",
        plot.margin = margin(10, 60, 10, 10)
      ) +
      geom_text(
        aes(y = ypos, label = lab),
        x = 1.7,
        hjust = 0.5,
        size = 3, lineheight = 0.9, na.rm = TRUE
      ) +
      labs(title = paste0(.y, " (n=", n_tot, ")"))
  })

dir.create("./Supplementary_Materials/Munsell", recursive = TRUE, showWarnings = FALSE)

# Save faceted overview
png(filename = "./Supplementary_Materials/Munsell/Munsell_by_group_facets.png",
    width = 4800, height = 4800, res = 300)
print(munsell_pie_facets)
dev.off()

# Save individual group pies
walk2(munsell_pie_plots, names(munsell_pie_plots), ~ {
  filename <- paste0("./Supplementary_Materials/Munsell/Munsell_",
                     gsub(" ", "_", .y), ".png")
  png(filename = filename, width = 1600, height = 1600, res = 300)
  print(.x)
  dev.off()
})

munsell_summary_wide <- munsell_long %>%
  select(NAA_Group, Munsell, Freq) %>%
  pivot_wider(names_from = Munsell, values_from = Freq, values_fill = 0) %>%
  arrange(NAA_Group)

write.csv(munsell_summary_wide,
          "./Supplementary_Materials/Munsell/munsell_by_group_table.csv",
          row.names = FALSE)



# ------------------------------ citations -------------------------------------

citation("rio")
citation("dplyr")
citation("ggplot2")
citation("GGally")
citation("tidyr")
citation("aqp")
citation("purrr")
citation("tibble")
citation("scales")
citation("ggtern")
