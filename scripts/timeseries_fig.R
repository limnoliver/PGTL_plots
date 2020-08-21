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
example_sites <- dir('data/examples/mtl_outputs_for_fig') # use these as a guide for what to download from SB
filter(all_eval_2366, site_id %in% example_sites) %>%
  select(site_id, pgmtl_rmse, pb0_rmse, pgmtl9_rmse, pball_rmse) %>%
  arrange(pb0_rmse)
targets <- tribble(
  ~mtl_rmse_class, ~pb0_rmse_class, ~target_id,
  'good', 'good', 'nhdhr_120020636',
  'bad', 'good', 'nhdhr_120018510', # PROBLEMATIC? they're about equal in performance now, actually, and both pretty good
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
    marker_text = sprintf('%s\npgmtl9: %f\npgmtl: %f\npball: %f\npb0: %f', site_id, pgmtl9_rmse, pgmtl_rmse, pball_rmse, pb0_rmse))
ggplot(targets_in_context, aes(x=pb0_rmse, y=pgmtl_rmse)) +
  geom_point(data=filter(targets_in_context, status=='unavailable'), color='lightgray') + 
  #geom_point(data=filter(targets_in_context, status=='available')) +
  geom_point(data=filter(targets_in_context, status=='selected'), aes(color=site_id)) +
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

eg_targets_info <- lake_metadata_full %>%
  filter(site_id %in% targets$target_id) %>%
  mutate(target_name = ifelse(fullname != 'None', fullname, gsub('nhdhr_', '', site_id))) %>%
  select(target_id=site_id, target_name, max_depth, surface_area, n_obs)
eg_sources_info <- filter(sources_info_partial, target_id %in% targets$target_id) %>%
  left_join(select(filter(lake_metadata, site_id %in% targets$target_id), site_id, target_name=lake_name), by=c('target_id'='site_id')) %>%
  mutate(target_name = ifelse(!is.na(target_name), target_name, gsub('nhdhr_', '', target_id)))
# note that some sources are used 2-3 times (dots will overlap)
eg_sources_info %>% filter(top_9) %>% group_by(source_id) %>% tally() %>% arrange(desc(n))

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
      tidyr::extract(filename, into=c('source_rank', 'source_id'), regex='source([[:digit:]]+)_(nhdhr[[:digit:]]+)_output', convert=TRUE) %>%
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
  
  bind_rows(sources, labels) %>%
    mutate(target_id=target) %>%
    return()
}
all_target_data <- lapply(seq_len(nrow(targets)), function(target_num) {
  read_timeseries_data(targets, target_num, pgmtl_train_44225)
}) %>%
  bind_rows()

plot_all_target_data <- function(all_target_data, min_date='2012-01-01', max_date='2013-12-31') {
  common_depths <- all_target_data %>%
    filter(source_id == 'Observed') %>%
    filter(!is.na(temp_c), date >= as.Date(min_date), date <= as.Date(max_date)) %>%
    group_by(target_id, depth) %>% 
    tally() %>%
    mutate(depth_class = ordered(case_when(rank(depth) <= n()/2 ~ 'shallow', TRUE ~ 'deep'), levels=c('shallow','deep'))) %>%
    group_by(target_id, depth_class) %>%
    filter(n > 0.6*max(n) | (rank(-n) == 1)) %>%
    arrange(depth * case_when(depth_class == 'shallow' ~ 1, TRUE ~ -1)) %>%
    slice(1) %>%
    distinct() %>%
    arrange(target_id, depth_class) %>%
    select(target_id, depth, depth_class)
  plot_data <- all_target_data %>%
    filter(!is.na(temp_c), date >= as.Date(min_date), date <= as.Date(max_date)) %>%
    right_join(common_depths, by=c('target_id','depth')) %>%
    filter(is.na(rank) | rank %in% c(1,2,9)) %>%
    mutate(Model = ifelse(grepl('nhdhr', source_id), sprintf('Source %d', rank), source_id),
           DepthClass = depth_class) %>%
    left_join(select(eg_targets_info, target_id, target_name), by=c('target_id'))
  ggplot(plot_data, aes(x=date, y=temp_c, color=Model, linetype=DepthClass, shape=DepthClass, fill=DepthClass)) +
    geom_line(data=filter(plot_data, source_id != 'Observed'), alpha=0.8) +
    geom_point(data=filter(plot_data, source_id == 'Observed'), color=model_colors['Obs']) +
    scale_color_manual(values=c('Source 1'=pgmtl_colors[['central']], 'Source 2'=pgmtl9_colors[['dark']], 'Source 9'=map_colors[['extended_targets']])) + #https://www.color-hex.com/color-palette/67553
    scale_shape_manual(values=setNames(c(25, 24), nm=levels(plot_data$DepthClass))) +
    scale_fill_manual(values=setNames(c(model_colors['Obs'],NA), nm=levels(plot_data$DepthClass))) +
    xlab('Date') +
    ylab(expression(paste("Temperature (",degree,"C)"))) +
    theme_minimal() +
    facet_grid(target_name ~ .) +
    theme(legend.position='bottom')
    #ggtitle(targets[target_num,] %>% mutate(label=sprintf('%s: PGMTL9 = %0.2f, PGMTL = %0.2f, PBall = %0.2f, PB0 = %0.2f', target_id, pgmtl9_rmse, pgmtl_rmse, pball_rmse, pb0_rmse)) %>% pull(label))
}
egplot_timeseries <- plot_all_target_data(all_target_data, min_date='2012-01-01', max_date='2013-12-31')
egplot_timeseries

egplot_depth_area <- ggplot(eg_sources_info, aes(x=surface_area, y=max_depth, size=n_obs)) +
  geom_point(color='gray90') + 
  geom_point(data=filter(eg_sources_info, top_9), aes(color=target_name, shape=target_name)) +
  geom_point(data=eg_targets_info, aes(color=target_name), shape=19, size=3) +
  scale_shape_manual('Target Lake', values=c(4,3,2,1)) +
  scale_color_manual('Target Lake', values=example_colors) +
  scale_size_continuous('Number of\nObservation Dates') +
  scale_x_log10(labels=function(x) sprintf('%.0f', x)) +
  # scale_y_log10() +
  xlab('Surface Area (units?)') + ylab('Maximum Depth (m)') +
  theme_bw() +
  theme(
    legend.position=c(0.15, 0.65),
    legend.background=element_blank())
    #legend.key.size=unit(10, units='points'),
    #legend.title=element_text(size=unit(10, units='points')),
    #legend.spacing.y=unit(5, units='points'))
egplot_depth_area
# ggsave('figures/examples_depth_area.png', plot=egplot_depth_area, width=6, height=4)

egplot_rmse_predobs <- eg_sources_info %>%
  ggplot(aes(y=predicted_rmse, x=actual_rmse, shape=target_name, fill=target_name,
             color=rank_category, size=rank_category, alpha=rank_category)) +
  geom_abline(color='lightgray') +
  geom_point(data=filter(eg_sources_info, rank_predicted > 9)) +
  geom_point(data=filter(eg_sources_info, rank_predicted <= 9, rank_predicted > 1)) +
  geom_point(data=filter(eg_sources_info, rank_predicted == 1)) +
  scale_alpha_manual('Metamodel\nPrediction', values=c(1, 1, 0.5), breaks=levels(sources_info$rank_category)) +
  scale_size_manual('Metamodel\nPrediction', values=c(3, 2, 1), breaks=levels(sources_info$rank_category)) +
  scale_color_manual(guide='none', values=c('black','white','white'), breaks=levels(sources_info$rank_category)) +
  scale_shape_manual('Target Lake', values=c(24,23,22,21), guide='none') +
  scale_fill_manual('Target Lake', values=example_colors, guide='none') +
  scale_x_log10() + scale_y_log10() + coord_cartesian(xlim = c(1, 21), ylim = c(1, 21)) +
  xlab('Actual RMSE') + ylab('Predicted RMSE') +
  theme_bw() + theme(legend.position=c(0.15,0.8), legend.background=element_blank())
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
cowplot::save_plot('figures/examples_multipanel.png', examples_figure, base_height=10, base_width=12)
