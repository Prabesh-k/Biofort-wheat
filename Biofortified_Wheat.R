######################################################
#PCA + DENDROGRAM + HEATMAP FOR WHEAT TRAITS##########
library(tidyverse)
library(factoextra)
library(pheatmap)

# Convert all trait columns to numeric
dat <- wheat %>%
  mutate(
    `Emergence/m2`          = as.numeric(`Emergence/m2`),
    `Tillers/plant`         = as.numeric(`Tillers/plant`),
    `Flag Leaf area`        = as.numeric(`Flag Leaf area`),
    `Days to heading`       = as.numeric(`Days to heading`),
    `Days to maturity`      = as.numeric(`Days to maturity`),
    `Grain filling period`  = as.numeric(`Grain filling period`),
    `Effective Tillers/m2`  = as.numeric(`Effective Tillers/m2`),
    `Plant height`          = as.numeric(`Plant height`),
    `Spike length`          = as.numeric(`Spike length`),
    `Peduncle length`       = as.numeric(`Peduncle length`),
    `Grains/spike`          = as.numeric(`Grains/spike`),
    `Grain weight/spike`    = as.numeric(`Grain weight/spike`),
    `Thoudand grain weight` = as.numeric(`Thoudand grain weight`),
    `Total biomass`         = as.numeric(`Total biomass`),
    `Grain yield`           = as.numeric(`Grain yield`),
    `Harvest index`         = as.numeric(`Harvest index`),
    `Protein Content`       = as.numeric(`Protein Content`),
    `Zn Content`            = as.numeric(`Zn Content`),
    `Iron Content`          = as.numeric(`Iron Content`)
  )

## 2. Clean column names ----------------------------------------------------

dat <- wheat %>%
  rename(
    rep                = Rep,
    genotype           = Genotypes,
    emergence_m2       = `Emergence/m2`,
    tillers_plant      = `Tillers/plant`,
    flag_leaf_area     = `Flag Leaf area`,
    days_to_heading    = `Days to heading`,
    days_to_maturity   = `Days to maturity`,
    grain_filling_per  = `Grain filling period`,
    eff_tillers_m2     = `Effective Tillers/m2`,
    plant_height       = `Plant height`,
    spike_length       = `Spike length`,
    peduncle_length    = `Peduncle length`,
    grains_spike       = `Grains/spike`,
    grain_wt_spike     = `Grain weight/spike`,
    thousand_grain_wt  = `Thoudand grain weight`,
    total_biomass      = `Total biomass`,
    grain_yield        = `Grain yield`,
    harvest_index      = `Harvest index`,
    protein_content    = `Protein Content`,
    zn_content         = `Zn Content`,
    iron_content       = `Iron Content`
  )

## 3. Create genotype_type (NL series vs Check) -----------------------------

dat <- dat %>%
  mutate(
    genotype_type = case_when(
      grepl("^NL", genotype) ~ "NL series",
      genotype %in% c("Gautam", "Zinc Ghaun-1") ~ "Check",
      TRUE ~ "Other"
    )
  )

# Optional: check counts
print(table(dat$genotype_type))

## 4. Select trait columns & convert to numeric -----------------------------

trait_cols <- c(
  "emergence_m2", "tillers_plant", "flag_leaf_area",
  "days_to_heading", "days_to_maturity", "grain_filling_per",
  "eff_tillers_m2","plant_height", "spike_length", "peduncle_length",
  "grains_spike", "grain_wt_spike", "thousand_grain_wt",
  "total_biomass", "grain_yield", "harvest_index",
  "protein_content", "zn_content", "iron_content"
)

dat[trait_cols] <- lapply(dat[trait_cols], function(x) as.numeric(as.character(x)))

## 5. Compute genotype means (across reps) ---------------------------------

dat_mean <- dat %>%
  group_by(genotype, genotype_type) %>%
  summarise(across(all_of(trait_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# Trait matrix for multivariate analyses
trait_mat <- dat_mean %>%
  select(all_of(trait_cols)) %>%
  as.data.frame()

rownames(trait_mat) <- dat_mean$genotype

## 6. PCA -------------------------------------------------------------------

pca_res <- prcomp(trait_mat, center = TRUE, scale. = TRUE)

# Eigenvalues / variance explained
eig_vals <- get_eigenvalue(pca_res)
print(eig_vals)

# Scree plot
fviz_eig(pca_res,
         addlabels = TRUE,
         ylim = c(0, 40)) +         # adjust Y-axis range here
  labs(
    title = NULL,                  # remove title
    x = "Principal Components",    # custom X-axis label
    y = "Percentage of Variance"   # custom Y-axis label
  ) +
  theme_minimal(base_size = 14) +  # clean minimal theme
  theme(
    panel.grid.major = element_blank(),   # remove major grid
    panel.grid.minor = element_blank(),   # remove minor grid
    panel.border = element_blank(),       # remove border
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# PCA individuals colored by genotype_type
fviz_pca_ind(
  pca_res,
  geom = "point",
  label = "ind",    
  habillage = dat_mean$genotype_type,   # NL series vs Check
  addEllipses = TRUE,
  ellipse.level = 0.95,
  pointsize = 4,
  repel = TRUE
)

fviz_pca_ind(
  pca_res,
  geom = "point",
  label = "ind",                         # <<< ADD GENOTYPE NAMES
  habillage = dat_mean$genotype_type,    # NL series vs Check
  addEllipses = TRUE,
  ellipse.level = 0.95,
  pointsize = 4,
  repel = TRUE                           # avoids text overlap
) +
  labs(
    title = NULL,
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "right"
  )

# TRAITS ONLY (Variable factor map)
fviz_pca_var(
  pca_res,
  col.var = "contrib",                 # color by contribution
  gradient.cols = c("grey80", "grey50", "black"),
  repel = TRUE
) +
  labs(
    title = NULL,
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "right"
  )

# GENOTYPES ONLY (Individual factor map)

fviz_pca_biplot(
  pca_res,
  habillage = dat_mean$genotype_type,    # NL series vs Check
  addEllipses = TRUE,
  ellipse.level = 0.95,
  pointsize = 3,
  label = "ind",                         # <<< SHOW GENOTYPE NAMES
  repel = TRUE                           # avoid overlapping labels
) +
  labs(
    title = NULL,
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_blank(),
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12)
  )


## 7. Dendrogram -----------------------------------------------------------

# Scale traits before distance
trait_scaled <- scale(trait_mat)

# Distance matrix (Euclidean) and hierarchical clustering (Ward)
dist_mat <- dist(trait_scaled, method = "euclidean")
hc <- hclust(dist_mat, method = "ward.D2")

# Dendrogram
plot(
  hc,
  labels = rownames(trait_scaled),
  main = "Dendrogram of Genotypes (Ward.D2, Euclidean distance)",
  xlab = "Genotypes",
  sub  = ""
)

## 8. Heat map with clustering ---------------------------------------------

# Row annotation: genotype type (NL vs Check)
row_anno <- data.frame(
  Genotype_type = dat_mean$genotype_type
)
rownames(row_anno) <- dat_mean$genotype  # must match rownames of trait_scaled

# Heatmap (pheatmap does its own clustering)
pheatmap(
  trait_scaled,
  annotation_row = row_anno,
  clustering_distance_rows = dist_mat,
  clustering_method = "ward.D2",
  main = "Heatmap of Traits (Genotype Means)"
)

# CORRELATION HEATMAP WITH SIGNIFICANCE LABELS + AXIS NAMES

library(tidyverse)
library(reshape2)

# 1. Read dataset ------
dat_raw <- read.csv("wheat_data.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE,
                    check.names = FALSE)

# 2. Trait columns ----------------------------------------------------------
trait_cols <- c(
  "Emergence/m2","Tillers/plant","Flag Leaf area","Days to heading",
  "Days to maturity","Grain filling period","Effective Tillers/m2","Plant height","Spike length","Peduncle length",
  "Grains/spike","Grain weight/spike","Thoudand grain weight",
  "Total biomass","Grain yield","Harvest index","Protein Content",
  "Zn Content","Iron Content"
)

trait_data <- wheat[trait_cols] %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

# Impute NA if needed
for (i in seq_along(trait_data)) {
  trait_data[, i] <- ifelse(is.na(trait_data[, i]),
                            mean(trait_data[, i], na.rm = TRUE),
                            trait_data[, i])
}

# 3. Correlation + p-values -------------------------------------------------
cor_with_p <- function(df){
  df <- as.data.frame(df)
  n <- ncol(df)
  cor_mat <- matrix(NA, n, n)
  p_mat  <- matrix(NA, n, n)
  
  for(i in 1:n){
    for(j in 1:n){
      test <- cor.test(df[[i]], df[[j]], method = "pearson")
      cor_mat[i,j] <- test$estimate
      p_mat[i,j] <- test$p.value
    }
  }
  list(cor = cor_mat, p = p_mat)
}

res <- cor_with_p(trait_data)
cor_mat <- res$cor
p_mat  <- res$p

# Assign row & column names -----------------------------------------------
rownames(cor_mat) <- trait_cols
colnames(cor_mat) <- trait_cols
rownames(p_mat)   <- trait_cols
colnames(p_mat)   <- trait_cols

# 4. Convert to long format ------------------------------------------------
cor_long <- melt(cor_mat, varnames = c("Trait1", "Trait2"), value.name = "Correlation")
p_long  <- melt(p_mat, varnames = c("Trait1", "Trait2"), value.name = "pvalue")

df_plot <- left_join(cor_long, p_long, by = c("Trait1", "Trait2"))

# 5. Add significance labels ----------------------------------------------
df_plot <- df_plot %>%
  mutate(sig_label = case_when(
    pvalue < 0.001 ~ "***",
    pvalue < 0.01  ~ "**",
    pvalue < 0.05  ~ "*",
    TRUE ~ ""
  ))

df_plot <- left_join(cor_long, p_long, by = c("Trait1", "Trait2")) %>%
  mutate(
    sig_label = case_when(
      pvalue < 0.001 ~ "***",
      pvalue < 0.01  ~ "**",
      pvalue < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Trait1 = factor(Trait1, levels = trait_cols),
    Trait2 = factor(Trait2, levels = trait_cols),
    i = as.integer(Trait1),
    j = as.integer(Trait2)
  )

# Keep only ONE side of the diagonal (upper triangle: i < j)
df_tri <- df_plot %>% filter(i < j)

# 5. Triangular heatmap (rectangle, not forced square) -------------------
ggplot(df_tri, aes(x = Trait1, y = Trait2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sig_label),
            size = 4, fontface = "bold", color = "black") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, limits = c(-1, 1), name = "r"
  ) +
  labs(
    title = "Triangular Correlation Heatmap of Wheat Traits",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )
                          
############################################################

cor_long <- cor_long %>%
  mutate(
    Trait1 = factor(Trait1, levels = trait_cols),
    Trait2 = factor(Trait2, levels = trait_cols),
    i = as.integer(Trait1),
    j = as.integer(Trait2)
  ) %>%
  filter(i >= j)  # lower triangle (incl. diagonal)

# 5. Plot: lower-triangular heatmap -------------------------------------
ggplot(cor_long, aes(x = Trait1, y = Trait2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "red",       # negative
    mid = "white",
    high = "blue",     # positive
    midpoint = 0,
    limits = c(-1, 1),
    name = ""
  ) +
  coord_fixed() +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 45, hjust = 1, vjust = 1, size = 10
    ),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank(),
    plot.title = element_blank(),
    legend.position = "right"
  )

#####Path_Analysis####
library(variability)
genopath<-geno.path(data1[20],data1[5:19],data1$Geno,data1$Rep)
phenopath<-pheno.path(data1[20],data1[5:19],data1$Geno,data1$Rep)
class(genopath)
genopath<-as.data.frame(genopath)
install.packages("writexl")
library(writexl)
write_xlsx(genopath, "Genopath.xlsx")
phenopath<-as.data.frame(phenopath)
class(phenopath)
write_xlsx(phenopath, "phenopath1.xlsx")
####ANOVA#######
library(agricolae)
attach(data)
ec<-PBIB.test(Block, Geno, Rep, HI, 5, method = "REML", test = "lsd", alpha = 0.05, console = TRUE, group=TRUE)
ec$ANOVA
ec$statistics
LSD.test($modl, "Geno", console = TRUE)
?LSD.test
mod1<- aov(HI~Rep+Geno+Rep:Block, data=data)
summary(mod1)
Out<-LSD.test(mod1,"Geno", console = TRUE)
Out$statistics$LSD
                          
