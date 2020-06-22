# get model results

mod_results <- readr::read_csv('data/PG-MTL_result_matrix_test_lakes_all_sources.csv')
ensemble <- readr::read_csv('data/PG-MTL_result_matrix_test_lakes_ensemble_sources.csv')
top_source <- readr::read_csv('data/PG-MTL_result_matrix_test_lakes_single_sources.csv')
glm <- readr::read_csv('data/RMSE_transfer_test_extended_glm.csv')

library(googledrive)
library(readxl)
library(ggplot2)
library(dplyr)
library(cowplot)

drive_download(as_id('1_Bzy6aI-wZLb_zeUPMLj0GCQPSpJMx3Evrf3CXcMrEU'),
                           path = 'data/all_mods.xlsx')
all_mods <- readxl::read_xlsx('data/all_mods.xlsx')

meta <- mod_results %>%
  select(target_id, source_observations) %>% distinct()

# difference between model performance
pb0_diff <- all_mods %>%
  tidyr::gather(key = 'transfer_models', value = 'other_rmse', -target_id, -`PB0_rmse`) %>%
  mutate(transfer_improvement = other_rmse - PB0_rmse) %>%
  group_by(transfer_models) %>%
  mutate(median_improvement = median(transfer_improvement),
         median_model = median(other_rmse)) %>% ungroup()

pb0_diff$transfer_models <- as.factor(pb0_diff$transfer_models)
levels(pb0_diff$transfer_models) <- c('PB-MTL', 'PG-MTL', 'PG-MTL9')

# model improvement (y) versus PB0 RMSE (x)
p0 <- ggplot(pb0_diff, aes(x = `PB0_rmse`, y = `transfer_improvement`)) +
  geom_point(aes(color = transfer_models), alpha = 0.8) +
  geom_smooth(method = 'lm', aes(group = transfer_models, color = transfer_models), se = FALSE) +
  #stat_density_2d(aes(fill = transfer_models, alpha = as.factor(..level..)), geom = "polygon") +
  scale_alpha_manual(values = c(.2, 0,0,0,0,0,0,0), guide = FALSE) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, 'Dark2'), guide = FALSE) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, 'Dark2'), guide = FALSE) +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = seq(-4, 6, 2)) +
  facet_wrap(~transfer_models, nrow=3) +
  geom_hline(aes(yintercept = median_improvement, color = transfer_models), size = 2, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_cartesian(xlim = c(1, 7)) +
  theme_bw() +
  theme(strip.background = element_blank(), strip.text = element_blank(),
        plot.margin = margin(0,0,0,0,'cm'), axis.title = element_text(size = 14)) +
  labs(x = 'PB0 RMSE', y = 'Change in RMSE (Transfer - PB0)')

# model RMSE (y) versus PB0 RMSE (x)

p0raw <- ggplot(pb0_diff, aes(x = `PB0_rmse`, y = `other_rmse`)) +
  geom_point(aes(color = transfer_models), alpha = 0.8) +
  #geom_smooth(method = 'lm', aes(group = transfer_models, color = transfer_models), se = FALSE) +
  #stat_density_2d(aes(fill = transfer_models, alpha = as.factor(..level..)), geom = "polygon") +
  #scale_alpha_manual(values = c(.2, 0,0,0,0,0,0,0,0,0,0), guide = FALSE) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, 'Dark2'), guide = FALSE) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, 'Dark2'), guide = FALSE) +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = 1:7) +
  facet_wrap(~transfer_models, ncol=1) +
  #geom_hline(aes(yintercept = median_model, color = transfer_models), size = 2, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  coord_cartesian(xlim = c(1, 7), ylim = c(1,7)) +
  theme_bw() +
  theme(strip.background = element_blank(), strip.text = element_blank(),
        plot.margin = margin(0,0,0,0,'cm'), 
        axis.title = element_text(size = 14)) +
  labs(x = 'PB0 RMSE', y = 'Transfer RMSE')

# density plot with no points
p_density <- ggplot(pb0_diff, aes(x = `PB0_rmse`, y = `other_rmse`)) +
  #geom_point(aes(color = transfer_models), alpha = 0.8) +
  #geom_smooth(method = 'lm', aes(group = transfer_models, color = transfer_models), se = FALSE) +
  stat_density_2d(aes(fill = transfer_models, alpha = as.factor(..level..)), geom = "polygon") +
  scale_alpha_manual(values = c(0,0, 0,0,0.6,0,0,0,0,0,0,0), guide = FALSE) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, 'Dark2'), guide = FALSE) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, 'Dark2')) +
  #facet_wrap(~transfer_models, nrow=1) +
  #geom_hline(aes(yintercept = median_model, color = transfer_models), size = 2, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  coord_cartesian(xlim = c(1, 5), ylim = c(1,5)) +
  theme_bw() +
  theme(strip.background = element_blank(), strip.text = element_blank(),
        plot.margin = margin(0,0,0,0,'cm')) +
  labs(x = 'PB0 RMSE', y = 'Transfer RMSE', fill = "Transfer Model")

ggsave('figures/density_all_mods_density10.png', p_density, height = 3.5, width = 5)

# long version of data
mod_long <- all_mods %>%
  tidyr::gather(key = 'model', value = 'RMSE', -target_id) %>%
  mutate(model = gsub('_rmse', '', model)) %>%
  group_by(model) %>%
  mutate(median_rmse = median(RMSE)) %>% ungroup()

mod_long$model <- factor(mod_long$model, levels = c('PB0', 'PB-MTL', 'PG-MTL', 'PG-MTL9'))

mod_pb0 <- filter(mod_long, model %in% 'PB0') %>% select(-model) %>% rename(pb0_rmse = RMSE, pb0_median_rmse = median_rmse)
mod_plot<- filter(mod_long, !model %in% 'PB0') %>% left_join(mod_pb0)

# overlapping density plots of focal model + PB0 RMSE
p4 <- ggplot(mod_plot, aes(x = RMSE)) +
  geom_density(aes(fill = model, group = model), color = NA, alpha = 0.7) +
  facet_wrap(~model, ncol = 1) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, 'Dark2'), guide = FALSE) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, 'Dark2'), guide = FALSE) +
  geom_density(data = mod_plot, aes(x = pb0_rmse), fill = 'lightgray', color = NA, alpha = 0.7) +
  geom_vline(aes(xintercept = median_rmse, color = model), size = 1.5) +
  geom_vline(aes(xintercept = pb0_median_rmse), color = 'lightgray', size = 1.5) +
  theme_bw() +
  theme(strip.background = element_blank(), strip.text = element_text(size = 14)) +
  theme_bw() +
  scale_x_continuous(breaks = 1:7) +
  coord_cartesian(xlim = c(1, 7), ylim = c(0, 0.65)) +
  theme(panel.grid = element_blank(), 
        strip.text = element_blank(), 
        plot.margin = margin(0,0.6,0,0, 'cm'),
        axis.title = element_text(size = 14)) +
  labs(x = 'RMSE', y = 'Density')

p5 <- ggplot(mod_plot, aes(x = RMSE)) +
  theme(margin(0,0,0,0, 'cm'))

# plot multiple plots together
psave <- cowplot::plot_grid(p0raw, p0, p4, nrow = 1, align = 'hv')

pwrite <- ggdraw(psave) +
  draw_label('PB-MTL', 0.99, 0.86, color = RColorBrewer::brewer.pal(3, 'Dark2')[1], angle = -90) +
  draw_label('PG-MTL', 0.99, 0.54, color = RColorBrewer::brewer.pal(3, 'Dark2')[2], angle = -90) +
  draw_label('PG-MTL9', 0.99, 0.21, color = RColorBrewer::brewer.pal(3, 'Dark2')[3], angle = -90) +
  draw_label('PB0', 0.89, 0.82, color = 'darkgray') +
  draw_label('PB0', 0.89, 0.50, color = 'darkgray') +
  draw_label('PB0', 0.89, 0.18, color = 'darkgray')

ggsave('figures/transfer_plots_9_panel.png', pwrite, height = 7.5, width = 10.5)

psave <- cowplot::plot_grid(p4, p0raw, nrow = 2, rel_heights = c(.6, 1), align = 'hv')

pwrite <- ggdraw(psave) +
  draw_label('PB-MTL', 0.05, 0.93, color = RColorBrewer::brewer.pal(3, 'Dark2')[1]) +
  draw_label('PG-MTL', 0.35, 0.93, color = RColorBrewer::brewer.pal(3, 'Dark2')[2]) +
  draw_label('PG-MTL9', 0.65, 0.93, color = RColorBrewer::brewer.pal(3, 'Dark2')[3]) +
  draw_label('PB0', 0.27, 0.80, color = 'darkgray') +
  draw_label('PB0', 0.59, 0.80, color = 'darkgray') +
  draw_label('PB0', 0.91, 0.80, color = 'darkgray')

ggsave('figures/transfers_density_rmse_labels.png', height = 5, width = 8)
  
p1 <- ggplot(filter(mod_long, model %in% c('PB-MTL', 'PB0')), aes(x = RMSE)) +
  geom_vline(aes(xintercept = median_rmse, color = model), size = 2, linetype = 2) +
  geom_density(aes(fill = model), color = NA, alpha = 0.7) +
  scale_fill_manual(values = c('lightgray', RColorBrewer::brewer.pal(3, 'Dark2')[1]), guide = FALSE) +
  scale_color_manual(values = c('lightgray', RColorBrewer::brewer.pal(3, 'Dark2')[1]), guide = FALSE) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:7) +
  coord_cartesian(xlim = c(1, 7), ylim = c(0, 0.7)) +
  theme(axis.text = element_blank(), panel.grid = element_blank()) +
  labs(x = '', y = '')

p2 <- ggplot(filter(mod_long, model %in% c('PG-MTL', 'PB0')), aes(x = RMSE)) +
  geom_vline(aes(xintercept = median_rmse, color = model), size = 2, linetype = 2) +
  geom_density(aes(fill = model), color = NA, alpha = 0.7) +
  scale_fill_manual(values = c('lightgray', RColorBrewer::brewer.pal(3, 'Dark2')[2]), guide = FALSE) +
  scale_color_manual(values = c('lightgray', RColorBrewer::brewer.pal(3, 'Dark2')[2]), guide = FALSE) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:7) +
  coord_cartesian(xlim = c(1, 7), ylim = c(0, 0.7)) +
  theme(axis.text = element_blank(), panel.grid = element_blank()) +
  labs(x = '', y = '') +
  theme(plot.margin = unit(c(0,0,0,0), 'cm'))

p3 <- ggplot(filter(mod_long, model %in% c('PG-MTL9', 'PB0')), aes(x = RMSE)) +
  geom_vline(aes(xintercept = median_rmse, color = model), size = 2, linetype = 2) +
  geom_density(aes(fill = model), color = NA, alpha = 0.7) +
  scale_fill_manual(values = c('lightgray', RColorBrewer::brewer.pal(3, 'Dark2')[3]), guide = FALSE) +
  scale_color_manual(values = c('lightgray', RColorBrewer::brewer.pal(3, 'Dark2')[3]), guide = FALSE) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:7) +
  coord_cartesian(xlim = c(1, 7), ylim = c(0, 0.7)) +
  theme(axis.text = element_blank(), panel.grid = element_blank()) +
  labs(x = '', y = '') +
  theme(plot.margin = unit(c(0,0,0,0), 'cm'))

cowplot::plot_grid(p4, p0, nrow = 2, axis = 'r', rel_heights = c(0.7,1))
ggsave('figures/mtl_with_densities.png', 
       cowplot::plot_grid(p4, p0, nrow = 2, rel_heights = c(.6, 1), align = 'hv'),
       height = 5, width = 8)



ggsave('figures/mtl_with_densities.png')

# facet_wrap histograms
mod_plots_mtl <- filter(mod_long, !model %in% 'PB0')
mod_plots()
ggsave('figures/pb0_versus_transfers.png', height = 4, width = 8)
ggplot(pb0_diff, aes(x = `PB0_rmse`, y = `other_rmse`)) +
  #geom_point(aes(color = transfer_models)) +
  stat_density_2d(aes(fill = transfer_models, alpha = as.factor(..level..)), geom = "polygon") +
  scale_alpha_manual(values = c(0, 0,0,0,0.2,0,0,0,0,0,0)) +
  #scale_color_manual(values = RColorBrewer::brewer.pal(3, 'Dark2'), guide = FALSE) +
  #scale_fill_manual(values = RColorBrewer::brewer.pal(3, 'Dark2'), guide = FALSE) +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = seq(-4, 6, 2)) +
  #facet_wrap(~transfer_models, nrow=1) +
  geom_hline(aes(yintercept = median_model, color = transfer_models), size = 2) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()
