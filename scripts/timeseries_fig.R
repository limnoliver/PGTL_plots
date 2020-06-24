library(feather)
library(tidyverse)

#### Get data ####

# single links shared by Jared:
# data/source_metadata.csv from https://docs.google.com/spreadsheets/d/1DnPHR1f7YZnsZXusF9MWCiHdgRh6BmhXlJfwq2zm8u8/edit#gid=19987048
# data/305_lakes_results.csv from https://docs.google.com/spreadsheets/d/1SK_0i8Qkpydn9ACb4AIfy7qWU6xUipWqOpzrFFpmWmM/edit?usp=sharing

# from Jordan via Teams 6/19
# data/GLM_metamodel_predicted_sources_glm_transfer_pball_test_lakes.csv
# data/RMSE_transfer_test_extended_glm.csv

# from Drive (https://drive.google.com/drive/u/0/folders/101SMKO-sngbpF6au9DZebfJdLh8xBXr0)
# data/PG-MTL_result_matrix_test_lakes_all_sources.csv
# data/PG-MTL_result_matrix_test_lakes_single_sources.csv
# data/PG-MTL_result_matrix_test_lakes_ensemble_sources.csv
provided <- list(
  good = c('120020376', '91593573', '91686475', '91688597', '91689681'), # good_outputs.zip
  bad = c('120018495', '120020398', '45730856', '60087894', '82815984')) # bad_outputs.zip

# From SB
library(sbtools)
sbtools::authenticate_sb('aappling@usgs.gov')
eval_files <- sprintf('%s_evaluation.csv', c('pb0','pball','pbmtl','pgmtl','pgmtl9'))
sbtools::item_file_download(
  sb_id='5ebe577782ce476925e44b32',
  names=eval_files,
  destinations=sprintf('data/%s', eval_files),
  overwrite_file = TRUE)
sbtools::item_file_download(sb_id='5ebe564782ce476925e44b26', names='lake_metadata.csv', destinations='data/lake_metadata.csv', overwrite_file=TRUE)
# also available: data/01_spatial/study_lakes.shp # don't need it [yet]
lapply(unlist(provided), function(site_num) {
  group <- lake_metadata %>%
    filter(site_id == sprintf('nhdhr_%s', site_num)) %>%
    pull(group_id)
  for(model in c('pb0','pball','pgmtl','pgmtl9')) {
    zipfile <- sprintf('data/%s_predictions_%s.zip', model, group)
    if(!file.exists(zipfile)) {
      message(sprintf('downloading %s', basename(zipfile)))
      sbtools::item_file_download(
        sb_id='5ebe569582ce476925e44b2f',
        names=basename(zipfile), destinations=zipfile)
      if(!dir.exists('data/predictions')) dir.create('data/predictions')
      unzip(zipfile, exdir='data/predictions')
    }
  }
})

#### Read ####

# Define reader functions
read_preds_feather <- function(preds_file) {
  feather::read_feather(preds_file) %>%
    pivot_longer(cols=starts_with('Depth_'), names_to='depth', names_prefix='Depth_', 
                 names_transform=list(depth=as.numeric), values_to='temp_c') %>%
    mutate(date=as.Date(index)) %>%
    select(-index)
}
read_preds_csv <- function(preds_file) {
  read_csv(preds_file, col_types=cols(.default=col_double(), date=col_date())) %>%
    pivot_longer(cols=starts_with('temp_'), names_to='depth', names_prefix='temp_', 
                 names_transform=list(depth=as.numeric), values_to='temp_c') 
}

# Read lake metadata
lake_metadata_full <- readr::read_csv('data/source_metadata.csv')
lake_metadata <- readr::read_csv('data/lake_metadata.csv')
#lake_spatial <- sf::read_sf('data/01_spatial/study_lakes.shp') # don't need it [yet]

# Read PB results from Jordan via Teams
pball_mtl_305 <- read_csv('data/GLM_metamodel_predicted_sources_glm_transfer_pball_test_lakes.csv') %>%
  transmute(target_id=sprintf('nhdhr_%s', `target_id(nhdhr)`),
            source_id=sprintf('nhdhr_%s', `best_predicted_site_id (nhdhr)`),
            predicted_rmse=predicted_rmse)
pball_eval_44225 <- read_csv('data/RMSE_transfer_test_extended_glm.csv') %>%
  mutate(target_id = gsub('test_', '', target_id))
pball_eval_305 <- pball_eval_44225 %>%
  right_join(pball_mtl_305, by=c('target_id','source_id'))

# Read PGMTL results from Jared
pgmtl_train_305 <- readr::read_csv('data/PG-MTL_result_matrix_test_lakes_all_sources.csv', na='N/A')
pgmtl_eval_305 <- readr::read_csv('data/PG-MTL_result_matrix_test_lakes_single_sources.csv', na='N/A')
pgmtl9_eval_305 <- readr::read_csv('data/PG-MTL_result_matrix_test_lakes_ensemble_sources.csv', na='N/A')
jw_all_eval_305 <- readr::read_csv('data/305_lakes_results.csv') # includes predicted MTL and MLT9 RMSEs and lake metadata for those 305 lakes

# Read results from ScienceBase
pb0_eval_2364 <- read_csv('data/pb0_evaluation.csv')
# pball_eval_145 <- read_csv('data/pball_evaluation.csv') # would rather have the 305 test lakes (best is from Teams for now)
pbmtl_eval_305 <- read_csv('data/pbmtl_evaluation.csv')
pgmtl_eval_2216 <- read_csv('data/pgmtl_evaluation.csv')
pgmtl9_eval_2216 <- read_csv('data/pgmtl9_evaluation.csv')

# Combine model eval results
all_eval_305 <-  transmute(pb0_eval_2364, target_id=site_id, pb0_rmse=rmse) %>%
  left_join(select(pgmtl_eval_2216, target_id=site_id, pgmtl_rmse=rmse), by='target_id') %>% #  pgmtl_source_id=source_id
  left_join(select(pball_eval_305, target_id, pball_source_id=source_id, pball_rmse=rmse), by='target_id') %>% # Teams file for now
  left_join(select(pgmtl9_eval_2216, target_id=site_id, pgmtl9_rmse=rmse), by='target_id') %>%
  filter(!is.na(pball_rmse), !is.na(pgmtl_rmse))

#### Identify data issues ####

# Mismatch between 'rmse' columns in pgmtl_train_305 and pgmtl_eval_305. Jared
# says he only used 1/3 of the test data for the train RMSEs and is fixing as of
# 6/22
pgmtl_train_305 %>%
  select(target_id, source_id, all_sources_rmse = rmse) %>% 
  left_join(select(pgmtl_eval_305, target_id, source_id, pgmtl_rmse = rmse)) %>%
  filter(!is.na(pgmtl_rmse)) %>%
  ggplot(aes(x=all_sources_rmse, y=pgmtl_rmse)) + geom_point() + theme_bw() +
  xlab('all_sources rmse') + ylab('single_sources rmse')
# fixed as of 6/23

#### Select lakes ####

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

#### Plot

# date range for nhdhr_91688597: "2010-10-05" "2014-10-08" # should it be so limited?
plot_target_timeseries <- function(target_num=3, min_date='2012-01-01', max_date='2013-12-31') {
  target <- targets[[target_num,'target_id']]
  target_class <- names(which(sapply(provided, function(.) any(grepl(gsub('nhdhr_', '', target), .)))))
  source_files <- dir(sprintf('data/%s_outputs/%s_outputs', target_class, target_class), pattern=sprintf('target_%s_.*source', target), full.names=TRUE)
  source_sites <- stringr::str_match(source_files, 'source_(nhdhr_[[:digit:]]+)_outputs.feather')[,2]
  source_ranks <- filter(pgmtl9_eval_305, target_id == target, source_id %in% source_sites) %>% 
    arrange(rmse) %>% # question for Jared: is this MTL-predicted or observed RMSE? what I'd really like right here is MTL-predicted
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
  
  common_depths <- labels %>%
    filter(!is.na(temp_c), date >= as.Date(min_date), date <= as.Date(max_date)) %>%
    group_by(depth) %>% 
    tally() %>%
    filter(n > 0.6*max(n)) %>%
    arrange(depth) %>%
    slice(c(1,n())) %>%
    pull(depth)
  plot_data <- bind_rows(sources, labels) %>% # pb0, ensemble
    filter(!is.na(temp_c), date >= as.Date(min_date), date <= as.Date(max_date)) %>%
    filter(depth %in% common_depths, is.na(rank) | rank %in% c(1,2,9)) %>%
    mutate(Model = ifelse(grepl('nhdhr', source), sprintf('Source %d', rank), source),
           Depth = factor(depth))
  
  obs_color <- 'gray30'
  ggplot(plot_data, aes(x=date, y=temp_c, color=Model, linetype=Depth, shape=Depth, fill=Depth)) +
    geom_line(data=filter(plot_data, source != 'Observed'), alpha=0.8) +
    geom_point(data=filter(plot_data, source == 'Observed'), color=obs_color) +
    scale_color_manual(values=c('Source 1'='#3014ea', 'Source 2'='#0086d6', 'Source 9'='#ffa22f')) + #https://www.color-hex.com/color-palette/67553
    scale_shape_manual(values=setNames(c(25, 24), nm=factor(common_depths))) +
    scale_fill_manual(values=setNames(c(obs_color,NA), nm=factor(common_depths))) +
    theme_minimal() +
    ggtitle(targets[target_num,] %>% mutate(label=sprintf('%s: PGMTL9 = %0.2f, PGMTL = %0.2f, PBall = %0.2f, PB0 = %0.2f', target_id, pgmtl9_rmse, pgmtl_rmse, pball_rmse, pb0_rmse)) %>% pull(label))
}

library(cowplot)
ts_plots <- lapply(seq_len(nrow(targets)), plot_target_timeseries, min_date='2012-01-01', max_date='2013-12-31')
ts_grid <- cowplot::plot_grid(
  plotlist=c(ts_plots),
  nrow=4, ncol=1
)
cowplot::save_plot('figures/examples_timeseries.png', ts_grid, base_height=8, base_width=8)

sources_info_partial <- select(pgmtl_train_305, target_id, source_id, rmse, rmse_predicted) %>%
  left_join(select(lake_metadata_full, site_id, fullname, max_depth, surface_area, n_obs), by=c('source_id'='site_id')) %>%
  group_by(target_id) %>%
  mutate(
    rank=rank(rmse),
    rank_predicted=rank(rmse_predicted),
    top_9 = rank_predicted <= 9) %>%
  ungroup()
target_summary <- sources_info %>%
  group_by(target_id) %>%
  summarize(
    rmse_mean=mean(rmse),
    rmse_median=median(rmse),
    min_rank_top_9 = min(rank[top_9]),
    max_rank_top_9 = max(rank[top_9]),
    mean_rank_top_9 = mean(rank[top_9]),
    median_rank_top_9 = median(rank[top_9])) %>%
  ungroup() %>%
  mutate(
    site_rank_rmse_mean=rank(rmse_mean),
    site_rank_rmse_median=rank(rmse_median))
sources_info <- sources_info_partial %>% left_join(target_summary, by='target_id')
targets_info <- jw_all_eval_305 %>%
  filter(target_id %in% targets$target_id) %>%
  select(target_id, max_depth, surface_area, n_obs) #%>% using info from jw_all_eval_305 because lake_metadata lacks max_depth, etc., and lake_metadata_full lacks the target lakes
  #left_join(select(lake_metadata_full, site_id, fullname, max_depth, surface_area, n_obs), by=c('target_id'='site_id'))

selected_sources_info <- filter(sources_info, target_id %in% targets$target_id)
# note that some sources are used 2-3 times (dots will overlap)
selected_sources_info %>% filter(top_9) %>% group_by(source_id) %>% tally() %>% arrange(desc(n))

ggplot(selected_sources_info, aes(x=surface_area, y=max_depth, size=n_obs)) +
  geom_point(color='gray90') + 
  geom_point(data=filter(selected_sources_info, top_9), aes(color=target_id, shape=target_id)) +
  geom_point(data=targets_info, aes(color=target_id), shape=19, size=3) +
  scale_shape_manual(values=c(4,3,2,1)) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()
ggsave('figures/examples_depth_area.png', width=6, height=4)

selected_sources_info %>%
  ggplot(aes(y=rmse_predicted, x=rmse, color=target_id, alpha=top_9)) +
  geom_abline(color='lightgray') +
  geom_point() +
  theme_bw()
# selected_sources_info %>%
#   ggplot(aes(y=rank_predicted, x=rank, color=target_id, alpha=top_9)) +
#   geom_hline(yintercept=9.5, color='gray70') +
#   geom_point() +
#   theme_bw()

#### Metamodel figure ####

sources_info %>%
  ggplot(aes(x=site_rank_rmse_median, y=rmse, color=top_9)) +
  geom_point(data=filter(sources_info, !top_9), alpha=0.3) +
  geom_point(data=filter(sources_info, top_9), alpha=0.8) +
  scale_color_manual(values=c(`TRUE`='#3a55b4', `FALSE`='#81de76')) +
  theme_bw()
sources_info %>%
  ggplot(aes(x=site_rank_rmse_median, y=rmse, color=top_9)) +
  geom_point(data=filter(sources_info, rank_predicted > 1), alpha=0.3) +
  geom_point(data=filter(sources_info, rank_predicted == 1), alpha=0.8) +
  scale_color_manual(values=c(`TRUE`='#3a55b4', `FALSE`='#81de76')) +
  theme_bw()
# filter(sources_info, top_9) %>%
#   ggplot(aes(x=site_rank_rmse_median, y=rank)) +
#   geom_point(alpha=0.8, color='#3a55b4') +
#   theme_bw()
# filter(sources_info, rank==1) %>% 
#   ggplot(aes(x=rmse_predicted, y=median_rank_top_9, color=max_depth)) +
#   geom_point(alpha=0.8) +
#   theme_bw()
# target_summary %>% 
#   ggplot(aes(x=site_rank_rmse_median, y=min_rank_top_9)) +
#   geom_errorbar(aes(ymin=min_rank_top_9, ymax=max_rank_top_9)) +
#   theme_bw()
target_summary %>% 
  ggplot(aes(x=site_rank_rmse_median, y=min_rank_top_9)) +
  geom_point(aes(y=min_rank_top_9), color='navy') +
  geom_point(aes(y=max_rank_top_9), color='lightblue') +
  geom_point(aes(y=median_rank_top_9), color='blue') +
  theme_bw() +
  ylab('Min, median, and max ranks of 9 selected sources lakes') +
  xlab('Target lakes ranked by median RMSE of all source lakes')

filter(sources_info, rank_predicted==1) %>%
  ggplot(aes(x=site_rank_rmse_median, y=rank)) +
  geom_point() +
  ylab('Actual rank of the source predicted to be rank 1') +
  theme_bw()

lm(median_rank_top_9 ~ max_depth + surface_area + n_obs + rmse_predicted,
   data=filter(sources_info, rank<10)) %>%
  summary()

target_summary %>%
  select(min_rank_top_9, max_rank_top_9, median_rank_top_9) %>%
  pivot_longer(cols=everything(), names_to='rank_statistic', values_to='rank') %>%
  ggplot(aes(x=rank_statistic, fill=rank_statistic)) + geom_violin(aes(y=rank), color=NA) +
  scale_fill_manual(values=c(min_rank_top_9='navy', median_rank_top_9='blue', max_rank_top_9='lightblue')) +
  ylim(0,145) +
  theme_bw()
filter(sources_info, rank_predicted==1) %>%
  ggplot(aes(x='MLT-selected source', y=rank)) +
  geom_violin(color=NA, fill='darkgray') +
  geom_hline(aes(yintercept=median(rank))) +
  ylab('Actual rank of the source predicted to be rank 1') +
  ylim(0,145) +
  theme_bw()
filter(sources_info, rank_predicted==1) %>% summarize(median(rank))
