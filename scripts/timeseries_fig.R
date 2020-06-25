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
provided <- list(
  good = c('120020376', '91593573', '91686475', '91688597', '91689681'),
  bad = c('120018495', '120020398', '45730856', '60087894', '82815984'))
filter(all_eval_305, target_id %in% sprintf('nhdhr_%s', unlist(provided))) %>%
  select(target_id, pgmtl_rmse, pb0_rmse, pgmtl9_rmse, pball_rmse) %>%
  arrange(pb0_rmse)
targets <- tribble(
  ~mtl_rmse_class, ~pb0_rmse_class, ~target_id,
  'good', 'bad', 'nhdhr_91688597', # good performance and good improvement over PB0. this is a good site choice
  'good', 'good', 'nhdhr_120020398', # good performance but not much better than PB0. many sites have better performance for both models but are not yet available
  'good', 'good', 'nhdhr_120018495', # good performance but worse performance than PB0
  'bad', 'bad', 'nhdhr_82815984' # bad performance for both models
) %>% 
  left_join(all_eval_305, by='target_id') %>%
  select(target_id, mtl_rmse_class, pgmtl_rmse, pgmtl9_rmse, pball_rmse, pb0_rmse_class, pb0_rmse)

# visualize selection and revise above as needed
targets_in_context <- all_eval_305 %>%
  mutate(
    status = ifelse(
      target_id %in% targets$target_id, 'selected',
      ifelse(target_id %in% sprintf('nhdhr_%s', unlist(provided)), 'available', 'unavailable')),
    marker_text = sprintf('%s\npgmtl9: %f\npgmtl: %f\npball: %f\npb0: %f', target_id, pgmtl9_rmse, pgmtl_rmse, pball_rmse, pb0_rmse))
ggplot(targets_in_context, aes(x=pb0_rmse, y=pgmtl_rmse)) +
  geom_point(data=filter(targets_in_context, status=='unavailable'), color='lightgray') + 
  #geom_point(data=filter(targets_in_context, status=='available')) +
  geom_point(data=filter(targets_in_context, status=='selected'), aes(color=target_id)) +
  # scale_color_manual(values=c(selected='purple', available='seagreen', unavailable='lightgray')) +
  theme_bw()
library(plotly)
plot_ly(data = targets_in_context, x = ~pb0_rmse, y = ~pgmtl_rmse, text = ~marker_text) %>%
  add_trace(data=dplyr::filter(targets_in_context, status=='unavailable'),
            name = 'all', type = 'scatter', mode = 'markers', marker = list(color='lightgray')) %>%
  add_trace(data=dplyr::filter(targets_in_context, status=='available'),
            name = 'available', type = 'scatter', mode = 'markers', marker = list(color='seagreen')) %>%
  add_trace(data=dplyr::filter(targets_in_context, status=='selected'),
            name = 'selected', type = 'scatter', mode = 'markers', marker = list(color='purple'))

#### Subset Data ####

eg_targets_info <- jw_all_eval_305 %>%
  filter(target_id %in% targets$target_id) %>%
  select(target_id, max_depth, surface_area, n_obs) #%>% using info from jw_all_eval_305 because lake_metadata lacks max_depth, etc., and lake_metadata_full lacks the target lakes
#left_join(select(lake_metadata_full, site_id, fullname, max_depth, surface_area, n_obs), by=c('target_id'='site_id'))
eg_sources_info <- filter(sources_info_partial, target_id %in% targets$target_id)
# note that some sources are used 2-3 times (dots will overlap)
eg_sources_info %>% filter(top_9) %>% group_by(source_id) %>% tally() %>% arrange(desc(n))

#### Timeseries Figure ####

read_timeseries_data <- function(targets, target_num=3) {
  target <- targets[[target_num,'target_id']]
  target_class <- names(which(sapply(provided, function(.) any(grepl(gsub('nhdhr_', '', target), .)))))
  source_files <- dir(sprintf('data/%s_outputs/%s_outputs', target_class, target_class), pattern=sprintf('target_%s_.*source', target), full.names=TRUE)
  source_sites <- stringr::str_match(source_files, 'source_(nhdhr_[[:digit:]]+)_outputs.feather')[,2]
  source_ranks <- pgmtl_train_305 %>%
    filter(target_id == target) %>%
    arrange(rmse_predicted) %>%
    slice(1:9) %>%
    mutate(rank = 1:n()) %>%
    select(source_id, rmse, rank)
  sources <- purrr::map(setNames(source_files, nm=source_sites), function(source_file) {
    read_preds_feather(source_file) %>% 
      mutate(source=stringr::str_match(source_file, 'source_(nhdhr_[[:digit:]]+)_outputs.feather')[,2]) %>%
      left_join(source_ranks, by=c('source'='source_id'))
  }) %>% bind_rows()
  # ensemble <- read_preds_feather(sprintf('data/%s_outputs/%s_outputs/target_%s_ENSEMBLE_outputs.feather', target_class, target_class, target)) %>%
  #   mutate(source='Ensemble', rank=0)
  # pb0 <- read_preds_csv(sprintf('data/predictions/pb0_%s_temperatures.csv', target)) %>%
  #   mutate(source='PB0')
  labels <- read_preds_feather(sprintf('data/%s_outputs/%s_outputs/target_%s_labels.feather', target_class, target_class, target)) %>%
    mutate(source='Observed')
  
  return(list(sources=sources, labels=labels))
}

# date range for nhdhr_91688597: "2010-10-05" "2014-10-08" # should it be so limited?
plot_target_timeseries <- function(targets, target_num=3, target_data, min_date='2012-01-01', max_date='2013-12-31') {
  
  common_depths <- target_data$labels %>%
    filter(!is.na(temp_c), date >= as.Date(min_date), date <= as.Date(max_date)) %>%
    group_by(depth) %>% 
    tally() %>%
    filter(n > 0.6*max(n)) %>%
    arrange(depth) %>%
    slice(c(1,n())) %>%
    pull(depth)
  plot_data <- bind_rows(target_data$sources, target_data$labels) %>% # pb0, ensemble
    filter(!is.na(temp_c), date >= as.Date(min_date), date <= as.Date(max_date)) %>%
    filter(depth %in% common_depths, is.na(rank) | rank %in% c(1,2,9)) %>%
    mutate(Model = ifelse(grepl('nhdhr', source), sprintf('Source %d', rank), source),
           Depth = factor(depth))
  
  ggplot(plot_data, aes(x=date, y=temp_c, color=Model, linetype=Depth, shape=Depth, fill=Depth)) +
    geom_line(data=filter(plot_data, source != 'Observed'), alpha=0.8) +
    geom_point(data=filter(plot_data, source == 'Observed'), color=model_colors['Obs']) +
    scale_color_manual(values=c('Source 1'=pgmtl_colors[['central']], 'Source 2'=pgmtl9_colors[['dark']], 'Source 9'=pgmtl9_colors[['neutral']])) + #https://www.color-hex.com/color-palette/67553
    scale_shape_manual(values=setNames(c(25, 24), nm=factor(common_depths))) +
    scale_fill_manual(values=setNames(c(model_colors['Obs'],NA), nm=factor(common_depths))) +
    theme_minimal() +
    ggtitle(targets[target_num,] %>% mutate(label=sprintf('%s: PGMTL9 = %0.2f, PGMTL = %0.2f, PBall = %0.2f, PB0 = %0.2f', target_id, pgmtl9_rmse, pgmtl_rmse, pball_rmse, pb0_rmse)) %>% pull(label))
}
target_num=3; plot_target_timeseries(targets, target_num, target_data=read_timeseries_data(targets, target_num), min_date='2012-01-01', max_date='2013-12-31')

library(cowplot)
ts_plots <- lapply(seq_len(nrow(targets)), plot_target_timeseries, min_date='2012-01-01', max_date='2013-12-31')
ts_grid <- cowplot::plot_grid(
  plotlist=c(ts_plots),
  nrow=4, ncol=1
)
cowplot::save_plot('figures/examples_timeseries.png', ts_grid, base_height=8, base_width=8)


ggplot(eg_sources_info, aes(x=surface_area, y=max_depth, size=n_obs)) +
  geom_point(color='gray90') + 
  geom_point(data=filter(eg_sources_info, top_9), aes(color=target_id, shape=target_id)) +
  geom_point(data=eg_targets_info, aes(color=target_id), shape=19, size=3) +
  scale_shape_manual('Lake', values=c(4,3,2,1)) +
  scale_color_manual('Lake', values=example_colors) +
  scale_size_continuous('Number of\nObservation Dates') +
  scale_x_log10(labels=function(x) sprintf('%.0f', x)) +
  # scale_y_log10() +
  xlab('Surface Area (units?)') + ylab('Maximum Depth (m)') +
  theme_bw()
ggsave('figures/examples_depth_area.png', width=6, height=4)

eg_sources_info %>%
  ggplot(aes(y=rmse_predicted, x=rmse, color=target_id, shape=target_id, fill=target_id,
             size=rank_category, alpha=rank_category)) +
  geom_abline(color='lightgray') +
  geom_point(data=filter(eg_sources_info, rank_predicted > 9)) +
  geom_point(data=filter(eg_sources_info, rank_predicted <= 9, rank_predicted > 1)) +
  geom_point(data=filter(eg_sources_info, rank_predicted == 1)) +
  scale_alpha_manual('Metamodel\nprediction', values=c(1, 0.7, 0.2), breaks=levels(sources_info$rank_category)) +
  scale_size_manual('Metamodel\nprediction', values=c(3, 1.8, 1), breaks=levels(sources_info$rank_category)) +
  scale_shape_manual('Lake', values=c(24,23,22,21)) +
  scale_color_manual('Lake', values=example_colors) +
  scale_fill_manual('Lake', values=example_colors) +
  xlab('Actual RMSE') + ylab('Predicted RMSE') +
  theme_bw()
ggsave('figures/examples_rmse_predobs.png', width=6, height=4)

