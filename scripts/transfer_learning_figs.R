library(googledrive)
library(readxl)
library(ggplot2)
library(dplyr)
library(cowplot)

# get model results
# first run Alison's script data_getting.R

files <- list.files('data')
eval_files <- files[grepl('evaluation.csv', files)]
dat <- data.frame()
for (i in 1:length(eval_files)) {
  temp_dat <- read.csv(file.path('data',  eval_files[i])) %>%
    mutate(model = gsub('_evaluation.csv', '', eval_files[i]))
  
  dat <- bind_rows(dat, temp_dat)
}

# difference between model performance
pb0_diff <- dat %>%
  tidyr::spread(key = model, value = rmse) %>%
  # filter to just the 305 lakes
  filter(!is.na(pbmtl)) %>%
  tidyr::gather(key = 'transfer_models', value = 'other_rmse', -site_id, -pb0) %>%
  filter(!is.na(other_rmse)) %>%
  mutate(transfer_improvement = other_rmse - pb0) %>%
  group_by(transfer_models) %>%
  mutate(median_improvement = median(transfer_improvement),
         median_model = median(other_rmse)) %>% ungroup() %>%
  filter(!transfer_models %in% 'pball')

pb0_diff$transfer_models <- as.factor(pb0_diff$transfer_models)
levels(pb0_diff$transfer_models) <- c('PB-MTL', 'PG-MTL', 'PG-MTL9')

# model improvement (y) versus PB0 RMSE (x)
p0 <- ggplot(pb0_diff, aes(x = pb0, y = transfer_improvement)) +
  geom_point(aes(color = transfer_models), alpha = 0.5, size = 1.2) +
  geom_smooth(method = 'lm', aes(group = transfer_models, color = transfer_models), se = FALSE) +
  #stat_density_2d(aes(fill = transfer_models, alpha = as.factor(..level..)), geom = "polygon") +
  scale_alpha_manual(values = c(.2, 0,0,0,0,0,0,0), guide = FALSE) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, 'Dark2'), guide = FALSE) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, 'Dark2'), guide = FALSE) +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = seq(-4, 4, 2)) +
  facet_wrap(~transfer_models, nrow=3) +
  geom_hline(aes(yintercept = median_improvement, color = transfer_models), size = 2, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_cartesian(xlim = c(1, 7)) +
  theme_bw() +
  theme(strip.background = element_blank(), strip.text = element_blank(),
        plot.margin = margin(0,0,0,0,'cm'), axis.title = element_text(size = 14)) +
  labs(x = 'PB0 RMSE', y = 'Change in RMSE (Transfer - PB0)')

# model RMSE (y) versus PB0 RMSE (x)

p0raw <- ggplot(pb0_diff, aes(x = pb0, y = other_rmse)) +
  geom_point(aes(color = transfer_models), alpha = 0.5, size = 1.2) +
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


# long version of data

# 305 lake IDs
lakes305 <- unique(pb0_diff$site_id)
mod_long <- dat %>%
  filter(site_id %in% lakes305) %>%
  group_by(model) %>%
  mutate(median_rmse = median(rmse),
         first_q = quantile(rmse, 0.25),
         third_q = quantile(rmse, 0.75)) %>% ungroup() %>%
  filter(!model %in% 'pball')

summary(as.factor(mod_long$model))
mod_long %>% select(model, median_rmse, first_q, third_q) %>% distinct()

mod_long$model <- as.factor(mod_long$model)
levels(mod_long$model) <- c('PB0', 'PB-MTL', 'PG-MTL', 'PG-MTL9')

mod_pb0 <- filter(mod_long, model %in% 'PB0') %>% 
  select(-model) %>% 
  rename(pb0_rmse = rmse, pb0_median_rmse = median_rmse) %>%
  select(-first_q, -third_q)
mod_plot<- filter(mod_long, !model %in% 'PB0') %>% 
  left_join(mod_pb0)

# overlapping density plots of focal model + PB0 RMSE
p4 <- ggplot(mod_plot, aes(x = rmse)) +
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
  coord_cartesian(xlim = c(1, 7), ylim = c(0, 0.72)) +
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
  draw_label('PB0', 0.82, 0.9, color = 'darkgray') +
  draw_label('PB0', 0.82, 0.58, color = 'darkgray') +
  draw_label('PB0', 0.82, 0.26, color = 'darkgray')

ggsave('figures/transfer_plots_9_panel_sep11.png', pwrite, height = 7.5, width = 10.5)

#psave <- cowplot::plot_grid(p4, p0raw, nrow = 2, rel_heights = c(.6, 1), align = 'hv')

##############################
# do a 2x3 panel figure, same as one row above, but for expanded lake dataset
# difference between model performance
pb0_diff_all <- dat %>%
  tidyr::spread(key = model, value = rmse) %>%
  # filter to just the 305 lakes
  filter(is.na(pbmtl) & !is.na(pgmtl)) %>%
  select(-pball, -pbmtl) %>%
  tidyr::gather(key = 'transfer_models', value = 'other_rmse', -site_id, -pb0) %>%
  filter(!is.na(other_rmse)) %>%
  mutate(transfer_improvement = other_rmse - pb0) %>%
  group_by(transfer_models) %>%
  mutate(median_improvement = median(transfer_improvement),
         median_model = median(other_rmse)) %>% ungroup()

pb0_diff_all$transfer_models <- as.factor(pb0_diff_all$transfer_models)
levels(pb0_diff_all$transfer_models) <- c('PG-MTL', 'PG-MTL9')

# model improvement (y) versus PB0 RMSE (x)
p0 <- ggplot(pb0_diff_all, aes(x = pb0, y = transfer_improvement)) +
  geom_point(aes(color = transfer_models), alpha = 0.2) +
  geom_smooth(method = 'lm', aes(color = transfer_models), se = FALSE) +
  #stat_density_2d(aes(fill = transfer_models, alpha = as.factor(..level..)), geom = "polygon") +
  #scale_alpha_manual(values = c(.2, 0,0,0,0,0,0,0), guide = FALSE) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, 'Dark2')[c(1,3)], guide = FALSE) +
  #scale_fill_manual(values = RColorBrewer::brewer.pal(3, 'Dark2'), guide = FALSE) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = seq(-7, 5, 2)) +
  facet_wrap(~transfer_models, nrow=3) +
  geom_hline(aes(yintercept = median_improvement, color = transfer_models), size = 2, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_cartesian(xlim = c(0, 18)) +
  theme_bw() +
  theme(strip.background = element_blank(), strip.text = element_blank(),
        plot.margin = margin(0,0,0,0,'cm'), axis.title = element_text(size = 14)) +
  labs(x = 'PB0 RMSE', y = 'Change in RMSE (Transfer - PB0)')

# model RMSE (y) versus PB0 RMSE (x)

p0raw <- ggplot(pb0_diff_all, aes(x = pb0, y = other_rmse)) +
  geom_point(aes(color = transfer_models), alpha = 0.2) +
  #geom_smooth(method = 'lm', aes(group = transfer_models, color = transfer_models), se = FALSE) +
  #stat_density_2d(aes(fill = transfer_models, alpha = as.factor(..level..)), geom = "polygon") +
  #scale_alpha_manual(values = c(.2, 0,0,0,0,0,0,0,0,0,0), guide = FALSE) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, 'Dark2')[c(1,3)], guide = FALSE) +
  #scale_fill_manual(values = RColorBrewer::brewer.pal(3, 'Dark2'), guide = FALSE) +
  scale_x_continuous(breaks = 0:19) +
  scale_y_continuous(breaks = 0:19) +
  facet_wrap(~transfer_models, ncol=1) +
  #geom_hline(aes(yintercept = median_model, color = transfer_models), size = 2, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  coord_cartesian(xlim = c(0.5, 19), ylim = c(0.5,19)) +
  theme_bw() +
  theme(strip.background = element_blank(), strip.text = element_blank(),
        plot.margin = margin(0,0,0,0,'cm'), 
        axis.title = element_text(size = 14)) +
  labs(x = 'PB0 RMSE', y = 'Transfer RMSE')


# long version of data

# 305 lake IDs
lakes_plus <- unique(pb0_diff_all$site_id)
mod_long <- dat %>%
  filter(site_id %in% lakes_plus) %>%
  group_by(model) %>%
  mutate(median_rmse = median(rmse),
         first_q = quantile(rmse, 0.25),
         third_q = quantile(rmse, 0.75)) %>% ungroup()

mod_long %>% select(model, median_rmse, first_q, third_q) %>% distinct()

mod_long$model <- as.factor(mod_long$model)
levels(mod_long$model) <- c('PB0', 'PG-MTL', 'PG-MTL9')

mod_pb0 <- filter(mod_long, model %in% 'PB0') %>% select(-model, -first_q, -third_q) %>% rename(pb0_rmse = rmse, pb0_median_rmse = median_rmse)
mod_plot<- filter(mod_long, !model %in% 'PB0') %>% left_join(mod_pb0)

# overlapping density plots of focal model + PB0 RMSE
p4 <- ggplot(mod_plot, aes(x = rmse)) +
  geom_density(aes(fill = model, group = model), color = NA, alpha = 0.7) +
  facet_wrap(~model, ncol = 1) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, 'Dark2')[c(1,3)], guide = FALSE) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, 'Dark2')[c(1,3)], guide = FALSE) +
  geom_density(data = mod_plot, aes(x = pb0_rmse), fill = 'lightgray', color = NA, alpha = 0.7) +
  geom_vline(aes(xintercept = median_rmse, color = model), size = 1.5) +
  geom_vline(aes(xintercept = pb0_median_rmse), color = 'lightgray', size = 1.5) +
  theme_bw() +
  theme(strip.background = element_blank(), strip.text = element_text(size = 14)) +
  theme_bw() +
  scale_x_continuous(breaks = 1:7) +
  coord_cartesian(xlim = c(0.5, 7), ylim = c(0, 0.6)) +
  theme(panel.grid = element_blank(), 
        strip.text = element_blank(), 
        plot.margin = margin(0,0.6,0,0, 'cm'),
        axis.title = element_text(size = 14)) +
  labs(x = 'RMSE', y = 'Density')

p5 <- ggplot(mod_plot, aes(x = RMSE)) +
  theme(margin(0,0,0,0, 'cm'))

# plot multiple plots together
psave <- cowplot::plot_grid(p0raw, p0, p4, nrow = 1, align = 'hv')

# add labels
pwrite <- ggdraw(psave) +
  draw_label('PG-MTL', 0.99, 0.8, color = RColorBrewer::brewer.pal(3, 'Dark2')[1], angle = -90) +
  draw_label('PG-MTL9', 0.99, 0.33, color = RColorBrewer::brewer.pal(3, 'Dark2')[3], angle = -90) +
  draw_label('PB0', 0.89, 0.82, color = 'darkgray') +
  draw_label('PB0', 0.89, 0.18, color = 'darkgray')

ggsave('figures/transfer_plots_9_panel_expanded_lakes_sep11.png', pwrite, height = 5, width = 10.5)

##############################
# do a 1x3 panel figure, same as one row above, but for expanded lake dataset
# difference between model performance

# model improvement (y) versus PB0 RMSE (x)
p0 <- ggplot(filter(pb0_diff_all, transfer_models %in%  'PG-MTL9'), aes(x = pb0, y = transfer_improvement)) +
  geom_point(aes(color = transfer_models), alpha = 0.2) +
  geom_smooth(method = 'lm', aes(group = transfer_models, color = transfer_models), se = FALSE) +
  #stat_density_2d(aes(fill = transfer_models, alpha = as.factor(..level..)), geom = "polygon") +
  #scale_alpha_manual(values = c(.2, 0,0,0,0,0,0,0), guide = FALSE) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, 'Dark2')[3], guide = FALSE) +
  #scale_fill_manual(values = RColorBrewer::brewer.pal(3, 'Dark2'), guide = FALSE) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = seq(-6, 4, 2)) +
  facet_wrap(~transfer_models, nrow=3) +
  geom_hline(aes(yintercept = median_improvement, color = transfer_models), size = 2, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_cartesian(xlim = c(1, 10)) +
  theme_bw() +
  theme(strip.background = element_blank(), strip.text = element_blank(),
        plot.margin = margin(0,0,0,0,'cm'), axis.title = element_text(size = 10)) +
  labs(x = 'PB0 RMSE', y = 'Change in RMSE (Transfer - PB0)')

# model RMSE (y) versus PB0 RMSE (x)

p0raw <- ggplot(filter(pb0_diff_all, transfer_models %in%  'PG-MTL9'), aes(x = pb0, y = other_rmse)) +
  geom_point(aes(color = transfer_models), alpha = 0.2) +
  #geom_smooth(method = 'lm', aes(group = transfer_models, color = transfer_models), se = FALSE) +
  #stat_density_2d(aes(fill = transfer_models, alpha = as.factor(..level..)), geom = "polygon") +
  #scale_alpha_manual(values = c(.2, 0,0,0,0,0,0,0,0,0,0), guide = FALSE) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, 'Dark2')[3], guide = FALSE) +
  #scale_fill_manual(values = RColorBrewer::brewer.pal(3, 'Dark2'), guide = FALSE) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = 1:10) +
  facet_wrap(~transfer_models, ncol=1) +
  #geom_hline(aes(yintercept = median_model, color = transfer_models), size = 2, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  coord_cartesian(xlim = c(0.5, 10), ylim = c(0.5,10)) +
  theme_bw() +
  theme(strip.background = element_blank(), strip.text = element_blank(),
        plot.margin = margin(0,0,0,0,'cm'), 
        axis.title = element_text(size = 10)) +
  labs(x = 'PB0 RMSE', y = 'Transfer RMSE')

# overlapping density plots of focal model + PB0 RMSE
p4 <- ggplot(filter(mod_plot, model %in% 'PG-MTL9'), aes(x = rmse)) +
  geom_density(aes(fill = model, group = model), color = NA, alpha = 0.7) +
  #facet_wrap(~model, ncol = 1) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, 'Dark2')[3], guide = FALSE) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, 'Dark2')[3], guide = FALSE) +
  geom_density(data = mod_plot, aes(x = pb0_rmse), fill = 'lightgray', color = NA, alpha = 0.7) +
  geom_vline(aes(xintercept = median_rmse, color = model), size = 1.5) +
  geom_vline(aes(xintercept = pb0_median_rmse), color = 'lightgray', size = 1.5) +
  theme_bw() +
  theme(strip.background = element_blank(), strip.text = element_text(size = 14)) +
  theme_bw() +
  scale_x_continuous(breaks = 1:7) +
  coord_cartesian(xlim = c(0.5, 7), ylim = c(0, 0.6)) +
  theme(panel.grid = element_blank(), 
        strip.text = element_blank(), 
        plot.margin = margin(0,0.6,0,0, 'cm'),
        axis.title = element_text(size = 10)) +
  labs(x = 'RMSE', y = 'Density')

p5 <- ggplot(mod_plot, aes(x = RMSE)) +
  theme(margin(0,0,0,0, 'cm'))

# plot multiple plots together
psave <- cowplot::plot_grid(p0raw, p0, p4, nrow = 1, align = 'hv')

ggsave('figures/transfer_plots_3_panel_expanded_lakes_pgmtl9.png', psave, height = 2.5, width = 10.5)

###############################
# leftovers
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

###############################
# density plot with no points
# no longer using this fig
p_density <- ggplot(pb0_diff, aes(x = pb0, y = other_rmse)) +
  #geom_point(aes(color = transfer_models), alpha = 0.8) +
  #geom_smooth(method = 'lm', aes(group = transfer_models, color = transfer_models), se = FALSE) +
  stat_density_2d(aes(fill = transfer_models, alpha = as.factor(..level..)), geom = "polygon") +
  scale_alpha_manual(values = c(0,0, 0,0,0.6,0,0,0,0,0,0,0,0), guide = FALSE) +
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

