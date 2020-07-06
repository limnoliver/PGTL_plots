# as of 6/25, this file is only used for the examples and metamodel-source-selection-performance figures

library(feather)
library(tidyverse)

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
# new format from Jared as of 7/6/20
read_preds_mtl_outputs_for_fig <- function(preds_file) {
  feather::read_feather(preds_file) %>%
    pivot_longer(cols=-index, names_to='date', names_transform=list(depth=as.Date), values_to='temp_c') %>%
    mutate(depth=as.numeric(index)) %>%
    select(-index)
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
pbmtl_train_305 <- readr::read_csv('data/PB-MTL_all_sources_with_predictions.csv', na='N/A') # new 7/6/20
# pgmtl_train_305 <- readr::read_csv('data/PG-MTL_result_matrix_test_lakes_all_sources.csv', na='N/A') # NOT updated 7/6/20
pgmtl_eval_305 <- readr::read_csv('data/PG-MTL_result_matrix_test_lakes_single_sources.csv', na='N/A') # updated 7/6/20
pgmtl9_eval_305 <- readr::read_csv('data/PG-MTL_result_matrix_test_lakes_ensemble_sources.csv', na='N/A') # updated 7/6/20
pgmtl9_evalplus_305 <- readr::read_csv('data/PG-MTL9_305Lakes_PredictedRMSEStats_metadata.csv', na='N/A') # new 7/6/20
# jw_all_eval_305 <- readr::read_csv('data/305_lakes_results.csv') # NOT updated 7/6/20 # includes predicted MTL and MLT9 RMSEs and lake metadata for those 305 lakes

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

#### Munge ####

sources_info_partial <- select(pgmtl_train_305, target_id, source_id, rmse, rmse_predicted) %>%
  left_join(select(lake_metadata_full, site_id, fullname, max_depth, surface_area, n_obs), by=c('source_id'='site_id')) %>%
  group_by(target_id) %>%
  mutate(
    rank=rank(rmse),
    rank_predicted=rank(rmse_predicted),
    top_9 = rank_predicted <= 9,
    rank_category = ifelse(rank_predicted == 1, 'Top 1', ifelse(rank_predicted <= 9, 'Top 9', 'Not Top 9')) %>%
      ordered(levels=c('Top 1', 'Top 9', 'Not Top 9'))) %>%
  ungroup()
target_summary <- sources_info_partial %>%
  group_by(target_id) %>%
  summarize(
    rmse_min=min(rmse),
    rmse_mean=mean(rmse),
    rmse_median=median(rmse),
    rank_top_1 = rank[rank_predicted == 1],
    min_rank_top_9 = min(rank[top_9]),
    max_rank_top_9 = max(rank[top_9]),
    mean_rank_top_9 = mean(rank[top_9]),
    median_rank_top_9 = median(rank[top_9])) %>%
  ungroup() %>%
  mutate(
    site_rank_rmse_mean=rank(rmse_mean),
    site_rank_rmse_median=rank(rmse_median),
    site_rank_rank_top_1 = rank(rank_top_1, ties.method='first'),
    site_rank_min_rank_top_9 = rank(min_rank_top_9, ties.method='first'),
    site_rank_median_rank_top_9 = rank(median_rank_top_9, ties.method='first'),
    site_rank_mean_rank_top_9 = rank(mean_rank_top_9, ties.method='first'),
    site_rank_max_rank_top_9 = rank(max_rank_top_9, ties.method='first'))
sources_info <- sources_info_partial %>% left_join(target_summary, by='target_id')
