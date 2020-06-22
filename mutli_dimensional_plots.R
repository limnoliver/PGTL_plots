library(dplyr)
library(ggplot2)
# read in lake metadata
source_meta <- read.csv('data/source_metadata.csv') %>%
  select(site_id, fullname, max_depth, surface_area, SDF, latitude, longitude, zero_temp_doy, lathrop_strat, glm_strat_perc) %>%
  mutate(type = 'source')

target_meta <- read.csv('data/305_lakes_results.csv') %>%
  select(site_id = target_id, fullname, max_depth, surface_area, SDF, latitude, longitude, zero_temp_doy, lathrop_strat, glm_strat_perc) %>%
  mutate(type = 'target')

all_meta <- bind_rows(source_meta, target_meta) %>%
  mutate(area_depth = log10(surface_area)/max_depth)
all_meta_pca <- select(all_meta, -site_id, -fullname, -type)
# collapse to PCA space
meta_pca <- prcomp(all_meta_pca, center = T, scale = T)
perc_cutoff <- 10

pca_out <- summary(meta_pca)$importance
# how many axes should I consider?
n_pca <- length(which(pca_out[2,] >= (perc_cutoff/100)))

pca_plot_dat <- data.frame(meta_pca$x[,1:n_pca])
pca_plot_dat$site_id <- all_meta$site_id
pca_plot_dat$type <- all_meta$type

# now calculate euclidean distances
structure1 <- select(pca_plot_dat, site_id, type) %>%
  filter(type == 'source') %>%
  select(site_id)

structure2 <- select(pca_plot_dat, site_id, type) %>%
  filter(type == 'target') %>%
  select(site_id)

structure <- expand.grid(structure1[[1]], structure2[[1]])

pca_plot_dat_sub <- select(pca_plot_dat, -type)

# left join by sample ids and rename columns
# left join by sample ids and rename columns
comp <- rename(structure, source = Var1, target = Var2) %>%
  left_join(pca_plot_dat_sub, by = c('source' = 'site_id'))

source_colnames <- paste0('PC', 1:n_pca, '_source')
names(comp)[3:(2+n_pca)] <- source_colnames

#left join by source ids and rename columns
comp <- comp %>%
  left_join(pca_plot_dat_sub, by = c('target' = 'site_id'))

target_colnames <- paste0('PC', 1:n_pca, '_target')
names(comp)[(3+n_pca):ncol(comp)] <- target_colnames

# calculate square differences between each pca axis between source and target combos
# square difference between two points in n-dimensional space is equal to the 
# sum of squared distances between each dimension
comp_diff <- comp %>%
  mutate(PC1_sqdiff = (PC1_source - PC1_target)^2,
         PC2_sqdiff = (PC2_source - PC2_target)^2,
         PC3_sqdiff = (PC3_source - PC3_target)^2,
         PC4_sqdiff = (PC4_source - PC4_target)^2) %>%
  rowwise() %>%
  mutate(euc_dist = sqrt(sum(PC1_sqdiff, PC2_sqdiff, PC3_sqdiff, PC4_sqdiff))) %>%
  rename(source_id = source, target_id = target)

# plot distance of source-target combo and RMSE
names(comp_diff)
names(top_source)  

top_source <- readr::read_csv('data/PG-MTL_result_matrix_test_lakes_single_sources.csv') %>%
  select(target_id, source_id, pgmtl_rmse = rmse) %>%
  left_join(select(readr::read_csv('data/305_lakes_results.csv'), target_id, pgmtl9_rmse, pb0_rmse, rmse_pred_mean, rmse_pred_lower)) %>%
  left_join(select(comp_diff, source_id, target_id, euc_dist)) %>%
  mutate(rmse_diff = rmse_pred_lower - pgmtl_rmse) %>%
  left_join(select(all_meta, ))
head(top_source)

source_dat <- filter(all_meta, type == 'source')
target_dat <- filter(all_meta, type == 'target')
hist(source_dat$area_depth)
hist(target_dat$area_depth, add = TRUE)

# should add expanded dataset too
p <- ggplot(all_meta, aes(x = area_depth)) +
  geom_density(aes(fill = type), alpha = 0.5) +
  theme_bw() +
  labs(x = "log10(Area) : Max Depth", y = "Density") +
  scale_fill_discrete(guide = FALSE) +
  theme(panel.grid = element_blank())

p2 <- ggplot(all_meta, aes(x = surface_area, y = max_depth)) +
  geom_point(aes(color = type), alpha = 0.5) +
  scale_x_log10() +
  theme_bw() +
  labs(x = "Surface Area (m2)", y = 'Maximum Depth (m)')
library(cowplot)

p_write <- ggdraw() +
  draw_plot(p2) +
  draw_plot(p, x = 0.08, y = 0.68, width = 0.35, height = 0.3)

ggsave('figures/depth_area_comparison.png', p_write, height = 5, width = 6.5)  

ggplot(top_source, aes(x = rmse_pred_lower, y = pgmtl_rmse)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  coord_cartesian(xlim = c(0,6), ylim = c(0,6)) +
  labs(x = 'Predicted min PGTML RMSE', y = 'Actual PGTML RMSE')

ggplot(top_source, aes(x = euc_dist, y = rmse_diff)) +
  geom_point()

pc1 <- bind_rows(select(comp_diff, site_id = source_id, PC1 = PC1_source) %>% mutate(type = 'source'), 
                 select(comp_diff, site_id = target_id, PC1 = PC1_target) %>% mutate(type = 'target')) %>%
  distinct()


pc2 <- bind_rows(select(comp_diff, site_id = source_id, PC2 = PC2_source) %>% mutate(type = 'source'), 
                 select(comp_diff, site_id = target_id, PC2 = PC2_target) %>% mutate(type = 'target')) %>%
  distinct()

pc12 <- left_join(pc1, pc2)

pca_plot_dat
ggplot(pc12, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = type), alpha = 0.5) +
  scale_color_manual(values = c('black', 'red'))
