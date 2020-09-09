library(tidyverse)
source('scripts/style.R')
source('scripts/data_prep.R')

#### Select Lakes ####

# PG-MTL feature importances:
# meta-feature	importance
# Max Depth Difference	0.28276644
# Sqrt Surf Area Percent Difference	0.19808009
# Number of Source Obs	0.15777728
# Max Depth Percent Difference 	0.14737974
# Surface Area Difference	0.12941339
# Mean Obs Temp	0.08458307

# Choose which sites to plot. Original idea was these three panels:
# * good performance and good improvement over PB0
# * good performance but worse performance than PB0
# * bad performance for both models
# all_eval_305 %>%
  # ggplot(aes(x=pb0_rmse, y=pgmtl_rmse)) + geom_abline() + geom_point()
# filter(all_eval_305, pgmtl_rmse > pb0_rmse + 0.5, pb0_rmse > 3, pb0_rmse < 4, pgmtl_rmse < 4.5) %>%
#   arrange(pb0_rmse)
example_sites <- dir('data/examples/mtl_outputs_for_fig')
filter(all_eval_2366, site_id %in% example_sites) %>%
  select(site_id, pgmtl_rmse, pb0_rmse, pgmtl9_rmse, pball_rmse) %>%
  arrange(pb0_rmse)
targets <- tribble(
  ~mtl_rmse_class, ~pb0_rmse_class, ~target_id,
  #'good', 'good', 'nhdhr_120020636', # weird reason to exclude, but it has too many observations so it's really hard to see the predictions
  'bad', 'good', 'nhdhr_120018510', # as of aug 20 they were about equal in performance and both pretty good. as of sept 5 it's pgmtl=2.84, pb0=1.88
  'good', 'bad', 'nhdhr_91688597', # good performance and good improvement over PB0. this is a good site choice
  'bad', 'bad', 'nhdhr_82815984' # bad performance for both models
) %>% 
  left_join(all_eval_2366, by=c('target_id'='site_id')) %>%
  select(target_id, mtl_rmse_class, pgmtl_rmse, pgmtl9_rmse, pb0_rmse_class, pb0_rmse, pball_rmse)

# visualize selection and revise above as needed
targets_in_context <- all_eval_2366 %>%
  filter(site_id %in% target_lakes) %>%
  mutate(
    status = ifelse(
      site_id %in% targets$target_id, 'selected',
      ifelse(site_id %in% example_sites, 'available', 'unavailable')),
    marker_text = sprintf('%s\npgmtl9: %f\npgmtl: %f\npball: %f\npb0: %f', site_id, pgmtl9_rmse, pgmtl_rmse, pball_rmse, pb0_rmse)) %>%
  left_join(lake_metadata_full, by='site_id') %>%
  mutate(lathrop_is_strat = lathrop_recalc > 3.8, # this may be our best bet for now for actual estimates of stratification
         lathrop = sprintf('%s to %s', lathrop_strat, lathrop_is_strat))

# figure out the stratification metrics
# targets_in_context %>%
#   ggplot(aes(y=glm_strat_perc, x=lathrop_strat, fill=lathrop_strat)) +
#   geom_violin(color=NA) +
#   theme_bw()
# targets_in_context %>% tidyr::pivot_longer(cols=pb0_rmse:pgmtl9_rmse, names_to='model', values_to='rmse') %>%
#   ggplot(aes(x=glm_strat_perc, y=rmse, color=lathrop)) +
#   geom_point() +
#   facet_grid(~ model) + theme_bw()
# targets_in_context %>% tidyr::pivot_longer(cols=pb0_rmse:pgmtl9_rmse, names_to='model', values_to='rmse') %>%
#   ggplot(aes(x=glm_strat_perc, y=rmse)) +
#   geom_point() + geom_smooth() +
#   facet_grid(~ model) + theme_bw()
# targets_in_context %>% ggplot(aes(x=glm_strat_perc, y=lathrop)) + geom_violin() + geom_point(position=position_jitter(width=0, height=0.1)) + theme_bw()
# targets_in_context %>% tidyr::pivot_longer(cols=pb0_rmse:pgmtl9_rmse, names_to='model', values_to='rmse') %>%
#   ggplot(aes(x=lathrop, y=rmse, color=lathrop)) + geom_boxplot() + facet_grid(~ model) + theme_bw() + theme(axis.text.x = element_blank())
# targets_in_context %>% tidyr::pivot_longer(cols=pb0_rmse:pgmtl9_rmse, names_to='model', values_to='rmse') %>% ggplot(aes(x=lathrop_recalc > 3.8, y=rmse)) + geom_point() + facet_grid(~ model)
# targets_in_context %>% ggplot(aes(x=glm_strat_perc, y=pball_rmse)) + geom_point()
# targets_in_context %>% ggplot(aes(x=glm_strat_perc, y=pbmtl_rmse)) + geom_point()
# targets_in_context %>% ggplot(aes(x=glm_strat_perc, y=pgmtl_rmse)) + geom_point()
# targets_in_context %>% ggplot(aes(x=lathrop_strat, y=pb0_rmse)) + geom_point() # 1 = stratified, 0 = not (but calculated incorrectly)
# targets_in_context %>% ggplot(aes(x=lathrop_strat, y=pgmtl_rmse)) + geom_point() # 1 = stratified, 0 = not (but calculated incorrectly)
# targets_in_context %>% ggplot(aes(x=lathrop_recalc, y=pb0_rmse, color=lathrop_recalc > 3.8)) + geom_point() # >3.8 = stratified, <=3.8 = not (calculated correctly)
# targets_in_context %>% ggplot(aes(x=lathrop_recalc, y=pgmtl_rmse, color=lathrop_recalc > 3.8)) + geom_point() # >3.8 = stratified, <=3.8 = not (calculated correctly)
# targets_in_context %>% mutate(lathrop_recalc = (max_depth-0.1)/log10(surface_area/10000)) %>%
#   ggplot(aes(x=lathrop_recalc, y=lathrop_strat)) + geom_point() + theme_minimal()
# targets_in_context %>% mutate(lathrop_recalc = (max_depth-0.1)/log10(surface_area)) %>%
#   ggplot(aes(x=lathrop_recalc, y=lathrop_strat)) + geom_point() + theme_minimal()

# interactive plot of the selected and non-selected targets
library(plotly)
g <- ggplot(targets_in_context, aes(x=pb0_rmse, y=pgmtl_rmse, shape=lathrop_recalc <= 3.8)) +
  geom_point(data=filter(targets_in_context, status=='unavailable', lathrop_recalc <= 3.8), color='lightgray') + 
  geom_point(data=filter(targets_in_context, status=='unavailable', lathrop_recalc > 3.8), color='darkgray') + 
  geom_point(data=filter(targets_in_context, status=='available'), color='black') +
  geom_point(data=filter(targets_in_context, status=='selected'), aes(color=site_id)) +
  scale_shape_manual('', values=c(4, 20)) +
  theme_bw()
ggplotly(g)
# here's a fully plotly version, but ggplotly(g) does the trick
# plot_ly(data = targets_in_context, x = ~pb0_rmse, y = ~pgmtl_rmse, text = ~marker_text) %>%
#   add_trace(data=dplyr::filter(targets_in_context, status=='unavailable', lathrop_recalc <= 3.8), # unstratified
#             name = 'all', type = 'scatter', mode = 'markers', marker = list(color='darkgray', symbol=5)) %>%
#   add_trace(data=dplyr::filter(targets_in_context, status=='unavailable', lathrop_recalc > 3.8), # stratified
#             name = 'all', type = 'scatter', mode = 'markers', marker = list(color='lightgray')) %>%
#   add_trace(data=dplyr::filter(targets_in_context, status=='available'),
#             name = 'available', type = 'scatter', mode = 'markers', marker = list(color='seagreen')) %>%
#   add_trace(data=dplyr::filter(targets_in_context, status=='selected'),
#             name = 'selected', type = 'scatter', mode = 'markers', marker = list(color='purple'))

#### Subset Data ####

eg_targets_info <- lake_metadata_full %>%
  filter(site_id %in% targets$target_id) %>%
  mutate(
    site_num = gsub('nhdhr_', '', site_id),
    target_lake_name = case_when(is.na(lake_name) ~ 'Unnamed', TRUE ~ lake_name),
    target_name = sprintf('%s (%s)', target_lake_name, site_num)) %>%
  left_join(all_eval_2366, by='site_id') %>%
  select(target_id=site_id, target_name, target_lake_name, site_num, max_depth, surface_area, n_obs, lathrop_recalc, ends_with('rmse')) %>%
  arrange(target_name)
eg_sources_info <- filter(pgmtl_info$sources_info, target_id %in% targets$target_id) %>%
  left_join(select(lake_metadata_full, site_id, source_lathrop_recalc=lathrop_recalc), by=c('source_id'='site_id')) %>%
  left_join(select(lake_metadata_full, site_id, target_name=lake_name), by=c('target_id'='site_id')) %>%
  mutate(
    source_lathrop_strat = source_lathrop_recalc > 3.8,
    site_num = gsub('nhdhr_', '', target_id),
    target_lake_name = case_when(is.na(target_name) | target_name == 'None' ~ 'Unnamed', TRUE ~ target_name),
    target_name = sprintf('%s (%s)', target_lake_name, site_num))
# note that some sources are used 2-3 times (dots will overlap)
#eg_sources_info %>% filter(top_9) %>% group_by(source_id) %>% tally() %>% arrange(desc(n))

#### Timeseries Figure ####

read_timeseries_data <- function(targets, target_num=3, pgmtl_train_44225) {
  target <- targets[[target_num,'target_id']]
  source_files <- dir(sprintf('data/examples/mtl_outputs_for_fig/%s', target), pattern='source.*_nhdhr.*_output', full.names=TRUE)
  source_ranks <- pgmtl_train_44225 %>%
    filter(target_id == target) %>%
    arrange(predicted_rmse) %>%
    mutate(rank = 1:n()) %>%
    select(source_id, actual_rmse, rank)
  sources <- purrr::map(source_files, function(source_file) {
    source_site <- tibble(filename=source_file) %>%
      tidyr::extract(filename, into=c('source_rank', 'source_id'), regex='source([[:digit:]]+)_(nhdhr.*)_output', convert=TRUE) %>%
      mutate(source_id = gsub('nhdhr', 'nhdhr_', source_id),
             source_rank = source_rank + 1)
    read_preds_mtl_outputs_for_fig(source_file) %>% 
      mutate(
        #source_rank = source_site$source_rank, # covered in source_ranks, which is computed above and joined below
        source_id = source_site$source_id) %>%
      left_join(source_ranks, by=c('source_id'))
  }) %>% bind_rows()
  labels <- read_preds_mtl_outputs_for_fig(sprintf('data/examples/mtl_outputs_for_fig/%s/labels.feather', target)) %>%
    filter(!is.na(temp_c)) %>%
    mutate(source_id='Observed')
  pb0_preds <- read_preds_csv(sprintf('data/predictions/pb0_%s_temperatures.csv', target)) %>%
    mutate(
      source_id = 'PB0',
      actual_rmse = filter(all_eval_2366, site_id == target) %>% pull(pb0_rmse),
      rank = NA)
  
  bind_rows(sources, labels, pb0_preds) %>%
    mutate(target_id=target) %>%
    return()
}
all_target_data <- lapply(seq_len(nrow(targets)), function(target_num) {
  read_timeseries_data(targets, target_num, pgmtl_train_44225)
}) %>%
  bind_rows()

plot_all_target_data <- function(all_target_data, min_date, max_date, lake_xdate, rmse_xdate) {
  common_depths <- all_target_data %>%
    filter(source_id == 'Observed') %>%
    filter(!is.na(temp_c), date >= as.Date(min_date), date <= as.Date(max_date)) %>%
    group_by(target_id, depth) %>% 
    tally() %>%
    mutate(depth_class = ordered(case_when(rank(depth) <= n()/2 ~ 'shallow', TRUE ~ 'deep'), levels=c('shallow','deep'))) %>%
    group_by(target_id, depth_class) %>%
    filter(n > 0.8*max(n) | (rank(-n) == 1)) %>%
    arrange(depth * case_when(depth_class == 'shallow' ~ 1, TRUE ~ -1)) %>%
    slice(1) %>%
    distinct() %>%
    arrange(target_id, depth_class) %>%
    select(target_id, depth, depth_class)
  plot_data <- all_target_data %>%
    filter(!is.na(temp_c), date >= as.Date(min_date), date <= as.Date(max_date)) %>%
    right_join(common_depths, by=c('target_id','depth')) %>%
    filter(is.na(rank) | rank %in% c(1,9,99)) %>%
    mutate(Model = ifelse(grepl('nhdhr', source_id), sprintf('Source %d', rank), source_id)) %>%
    left_join(eg_targets_info, by=c('target_id'))
  lake_labels <- eg_targets_info %>%
    mutate(
      date=as.Date(lake_xdate), temp_c=22, depth_class='shallow', # for positioning on the plot
      lab=sprintf('%s\n(%s)', target_lake_name, site_num))
  rmse_labels <- eg_targets_info %>%
    mutate(
      date=as.Date(rmse_xdate), temp_c=22, depth_class='shallow', # for positioning on the plot
      lab=sprintf('PGDL-MTL: %0.1f °C\nPB0: %0.1f °C', pgmtl_rmse, pb0_rmse))
  panel_letters <- eg_targets_info %>%
    mutate(
      date=as.Date(min_date)+0.02*(as.Date(max_date)-as.Date(min_date)),
      temp_c=36, depth_class='shallow', # for positioning on the plot
      lab=letters[1:n()])
  ggplot(plot_data, aes(x=date, y=temp_c, color=Model, linetype=depth_class, shape=depth_class, fill=depth_class)) +
    geom_line(data=filter(plot_data, source_id != 'Observed'), alpha=0.8) +
    geom_point(data=filter(plot_data, source_id == 'Observed'), color=model_colors['Obs']) +
    geom_text(data=lake_labels[1,], aes(label=lab), color=example_colors[1], size=3) +
    geom_text(data=lake_labels[2,], aes(label=lab), color=example_colors[2], size=3) +
    geom_text(data=lake_labels[3,], aes(label=lab), color=example_colors[3], size=3) +
    geom_text(data=rmse_labels, aes(label=lab), color='black', size=3) +
    geom_text(data=panel_letters, aes(label=lab), color='black', size=5) +
    scale_color_manual('', values=c('PB0'=model_colors[['PB0']], 'Source 1'=pgmtl_colors[['central']], 'Source 9'=pgmtl9_colors[['dark']], 'Source 99'=map_colors[['extended_targets']])) + #https://www.color-hex.com/color-palette/67553
    scale_shape_manual('', values=setNames(c(25, 24), nm=levels(plot_data$depth_class))) +
    scale_fill_manual('', values=setNames(c(model_colors['Obs'],NA), nm=levels(plot_data$depth_class))) +
    scale_linetype_discrete('') +
    xlab('') +
    ylab(expression(paste("Temperature (",degree,"C)"))) +
    theme_minimal() +
    facet_grid(target_name ~ .) +
    theme(
      axis.title.x=element_blank(), 
      strip.text.y=element_blank(),
      legend.position='bottom', legend.box='vertical', legend.spacing.y=unit(-0.3, 'lines'))
}
egplot_timeseries <- plot_all_target_data(all_target_data, min_date='2013-02-01', max_date='2014-01-31', lake_xdate='2013-03-15', rmse_xdate='2013-12-15')
egplot_timeseries

egplot_depth_area <- ggplot(eg_sources_info, aes(x=surface_area/1000000, y=max_depth)) +
  geom_point(data=filter(eg_sources_info, source_lathrop_strat == 0), aes(size=n_obs), color=neutral_colors[['dark']], shape=20) +
  geom_point(data=filter(eg_sources_info, source_lathrop_strat == 1), aes(size=n_obs), color=neutral_colors[['light']], shape=20) +
  geom_point(data=eg_targets_info, aes(fill=target_name, shape=target_name), size=3, color='black') + # color=pgmtl_colors[['central']]
  geom_point(data=filter(eg_sources_info, top_9), aes(fill=target_name, shape=target_name), size=2, color='none') + # color=pgmtl9_colors[['dark']]
  annotate('text', x = min(eg_sources_info$surface_area/1000000), y=0.99*max(eg_sources_info$max_depth), label='d', size=5) +
  scale_shape_manual('Target Lake', values=c(24,23,22,21)[1:nrow(targets)]) + #21
  scale_color_manual('Target Lake', values=example_colors[1:nrow(targets)]) +
  scale_fill_manual('Target Lake', values=example_colors[1:nrow(targets)]) +
  guides(size = guide_legend(override.aes = list(color = neutral_colors[['light']]))) +
  scale_size_continuous('# Obs. Dates', breaks=c(5000,50000)) +
  scale_x_log10(labels=function(x) sprintf('%s', as.character(signif(x, 1)))) +
  xlab('Surface Area (km'^2~')') + ylab('Maximum Depth (m)') +
  theme_minimal() +
  theme(
    legend.position=c(0.32, 0.7),
    legend.background=element_blank(),
    legend.key.size=unit(10, units='points'),
    legend.title=element_text(size=unit(10, units='points')),
    legend.spacing.y=unit(5, units='points'))
egplot_depth_area
# ggsave('figures/examples_depth_area.png', plot=egplot_depth_area, width=6, height=4)

egplot_rmse_predobs <- eg_sources_info %>%
  ggplot(aes(y=predicted_rmse, x=actual_rmse, shape=target_name, fill=target_name,
             color=rank_category, size=rank_category, alpha=rank_category)) +
  geom_abline(color='lightgray') +
  geom_point(data=filter(eg_sources_info, rank_predicted > 9)) +
  geom_point(data=filter(eg_sources_info, rank_predicted <= 9, rank_predicted > 1)) +
  geom_point(data=filter(eg_sources_info, rank_predicted == 1)) +
  annotate('text', x = 1.1, y = 23, label='e', size=5) +
  scale_alpha_manual('Metamodel\nPrediction', values=c(1, 1, 0.5), breaks=levels(pgmtl_info$sources_info$rank_category)) +
  scale_size_manual('Metamodel\nPrediction', values=c(3, 2, 1), breaks=levels(pgmtl_info$sources_info$rank_category)) +
  scale_color_manual('Metamodel\nPrediction', values=c('black','none','white'), breaks=levels(pgmtl_info$sources_info$rank_category)) +
  #scale_color_manual('Metamodel\nPrediction', values=c(pgmtl_colors[['central']],pgmtl9_colors[['dark']],'none'), breaks=levels(pgmtl_info$sources_info$rank_category)) +
  scale_shape_manual('Target Lake', values=c(24,23,22,21)[1:nrow(targets)], guide='none') +
  scale_fill_manual('Target Lake', values=example_colors[1:nrow(targets)], guide='none') +
  scale_x_log10() + scale_y_log10() + coord_cartesian(xlim = c(1, 21), ylim = c(1, 21)) +
  guides(size = guide_legend(override.aes = list(shape=21, fill = neutral_colors[['light']]))) +
  xlab('Actual RMSE (°C)') + ylab('Predicted RMSE (°C)') +
  theme_minimal() + 
  theme(
    legend.position=c(0.24,0.82),
    legend.background=element_blank(),
    legend.title=element_text(size=unit(10, units='points')),
    legend.spacing.y=unit(3, units='points'))
egplot_rmse_predobs
# ggsave('figures/examples_rmse_predobs.png', plot=egplot_rmse_predobs, width=6, height=4)

library(gridExtra)
examples_figure <- grid.arrange(
  grobs = list(
    egplot_timeseries,
    egplot_depth_area,
    egplot_rmse_predobs
  ),
  widths = c(1, 1),
  layout_matrix = rbind(c(1, 2),
                        c(1, 3))
)
cowplot::save_plot('figures/examples_multipanel.png', examples_figure, base_height=8, base_width=8)
