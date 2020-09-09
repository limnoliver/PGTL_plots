# as of 6/25, this file is only used for the examples and metamodel-source-selection-performance figures

library(feather)
library(tidyverse)

#### Utils ####

# Define reader functions for files from SB
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
# reader for example files from Jared as of 8/6/20
read_preds_mtl_outputs_for_fig <- function(preds_file) {
  feather::read_feather(preds_file) %>%
    pivot_longer(cols=-index, names_to='depth', names_transform=list(depth=as.numeric), values_to='temp_c') %>%
    mutate(date=as.Date(index, '%Y-%m-%d')) %>%
    select(date, depth, temp_c)
}

#### Read ####

# Read lake metadata
lake_metadata <- readr::read_csv('data/lake_metadata.csv', col_types=cols())
lake_spatial <- sf::read_sf('data/spatial/study_lakes.shp')
# temporarily supplement with data from Drive; these columns should be in lake_metadata.csv in future. we also need the properties for source lakes, which are omitted from the 2221
# source_metadata <- read_csv('data/old/source_metadata.csv')
# exp2221_metadata <- read_csv('data/PG-MTL9_Expanded_2221Lakes_wMetadata.csv') %>%
#   slice(-n()) %>% # some trailing line in the Sheet
#   mutate(site_id = sprintf('nhdhr_%s', target_id))
# lake_metadata_full <- bind_rows(exp2221_metadata, source_metadata)
lake_metadata_full <- lake_metadata %>%
  select(site_id, lake_name, max_depth, surface_area, n_obs, lathrop_strat, glm_strat_perc) %>%
  mutate(
    lathrop_strat=as.factor(lathrop_strat),
    lathrop_recalc = (max_depth-0.1)/log10(surface_area/10000))
# surface_area appears to be in m2, based on Wikipedia's statement that Lake Wingra is 1.3 km2 (which is 1300000 m2 == 130 ha)

# Read PB results from Jordan via Teams
# pball_mtl_305 <- read_csv('data/old/GLM_metamodel_predicted_sources_glm_transfer_pball_test_lakes.csv') %>%
#   transmute(target_id=sprintf('nhdhr_%s', `target_id(nhdhr)`),
#             source_id=sprintf('nhdhr_%s', `best_predicted_site_id (nhdhr)`),
#             predicted_rmse=predicted_rmse)
# pball_eval_44225 <- read_csv('data/old/RMSE_transfer_test_extended_glm.csv') %>%
#   mutate(target_id = gsub('test_', '', target_id))
# pball_eval_305 <- pball_eval_44225 %>%
#   right_join(pball_mtl_305, by=c('target_id','source_id')) # target_id, source_id, [actual]rmse, predicted_rmse for the 305 targets and their top source (only)
# can now get this in pbmtl_train_44225

# Read P[B/G]MTL[9] results from Jared
# predicted and actual RMSEs for all 44k source-target pairs
# pbmtl_train_44225 <- readr::read_csv('data/old/PB-MTL_all_sources_with_predictions_200819.csv', na='N/A') %>% # 44k source-target pairs, predicted and actual rmses
#   rename(actual_rmse = transfer_rmse)
pbmtl_train_44225 <- readr::read_csv('data/PB-MTL_all_sources_with_predictions.csv', na='N/A') %>% # 44k source-target pairs, predicted and actual rmses
  rename(actual_rmse = `pb-mtl_rmse`)
pgmtl_train_44225 <- readr::read_csv('data/PG-MTL_result_matrix_test_lakes_all_sources.csv', na='N/A') %>% # junk (NAs) in columns 13 and 14
  select(target_id, source_id, actual_rmse = `pg-mtl_rmse`, predicted_rmse) # includes metafeatures, but I'm taking them out because they should be computable from lake_metadata_full and eventually lake_metadata

# More results from Jared
# I think we don't actually need these files because pred+obs RMSEs are in the pxmtl_train_44225 files and metafeatures are in lake_metadata_full.
# The following files contain the selected source[s] for each target, predicted and observed RMSEs, and metafeatures used in the model (but only metafeatures for the pg models, not pb).
# spearman / median spearman is the correlation coefficient, and meta_rmse is
# the RMSE, for the 145 predictions vs observations of RMSE for the source lakes
# applied to a given target.
# pbmtl_eval_305 <- readr::read_csv('data/PB-MTL_results.csv', na='N/A') # assessments of PBMTL split up by 305 target_ids: spearman, meta_rmse, and others
# pgmtl_eval_305 <- readr::read_csv('data/PG-MTL_result_matrix_test_lakes_single_sources.csv', na='N/A') %>% # assessments of PGMTL split up by 305 target_ids: meta_rmse, median spearman (this is actually just spearman); also 
#   slice(-c(n()-1, n())) # These dangling RMSE values are the Sheets-computed stats of all values above them
# pgmtl9_eval_3050 <- readr::read_csv('data/PG-MTL_result_matrix_test_lakes_ensemble_sources.csv', na='N/A') # 3050 rows, one for each of the top 9 source-target pairs and one for the ensemble for each of 305 targets
# pgmtl9_evalplus_305 <- readr::read_csv('data/PG-MTL9_305Lakes_PredictedRMSEStats_metadata.csv', na='N/A') %>% # 305+ rows, including many lake properties
#   slice(-(n() + (-2:0))) %>% # This dangling RMSE value is the Sheets-computed median of all values above it

# Read evaluation results from ScienceBase
pb0_eval_2366 <- read_csv('data/pb0_evaluation.csv', col_types=cols()) %>% mutate(model = 'pb0') # 1916 + 305 + 145
pball_eval_450 <- read_csv('data/pball_evaluation.csv', col_types=cols()) %>% mutate(model = 'pball') # 305 + 145
pbmtl_eval_305 <- read_csv('data/pbmtl_evaluation.csv', col_types=cols()) %>% mutate(model = 'pbmtl') # 305
pgmtl_eval_2221 <- read_csv('data/pgmtl_evaluation.csv', col_types=cols()) %>% mutate(model = 'pgmtl') # 1916 + 305
pgmtl9_eval_2221 <- read_csv('data/pgmtl9_evaluation.csv', col_types=cols()) %>% mutate(model = 'pgmtl9') # 1916 + 305


#### Munge ####

# site IDs for the 3 classes of lake
target_lakes <- pbmtl_eval_305$site_id
source_lakes <- setdiff(pball_eval_450$site_id, target_lakes)
expand_lakes <- setdiff(pgmtl9_eval_2221$site_id, target_lakes)

# Combine model eval results
all_eval_2366 <- bind_rows(pb0_eval_2366, pball_eval_450, pbmtl_eval_305, pgmtl_eval_2221, pgmtl9_eval_2221) %>%
  mutate(model=sprintf('%s_rmse', model)) %>%
  tidyr::pivot_wider(names_from='model', values_from='rmse')

# prepare info about the sources
prep_source_target_info <- function(mtl_train_44225, lake_metadata_full) {
  sources_info_partial <- select(mtl_train_44225, target_id, source_id, actual_rmse, predicted_rmse) %>%
    left_join(select(lake_metadata_full, site_id, lake_name, max_depth, surface_area, n_obs), by=c('source_id'='site_id')) %>%
    group_by(target_id) %>%
    mutate(
      rank_actual = rank(actual_rmse),
      rank_predicted = rank(predicted_rmse),
      top_9 = rank_predicted <= 9,
      rank_category = ifelse(rank_predicted == 1, 'Top 1', ifelse(rank_predicted <= 9, 'Top 9', 'Not Top 9')) %>%
        ordered(levels=c('Top 1', 'Top 9', 'Not Top 9'))) %>%
    ungroup()
  # prepare info about each target's set of sources
  target_summary <- sources_info_partial %>%
    group_by(target_id) %>%
    summarize(
      rmse_min = min(actual_rmse),
      rmse_mean = mean(actual_rmse),
      rmse_median = median(actual_rmse),
      rank_top_1 = rank_actual[rank_predicted == 1],
      min_rank_top_9 = min(rank_actual[top_9]),
      max_rank_top_9 = max(rank_actual[top_9]),
      mean_rank_top_9 = mean(rank_actual[top_9]),
      median_rank_top_9 = median(rank_actual[top_9]),
      .groups='drop') %>%
    mutate(
      site_rank_rmse_mean = rank(rmse_mean),
      site_rank_rmse_median = rank(rmse_median),
      site_rank_rank_top_1 = rank(rank_top_1, ties.method='first'),
      site_rank_min_rank_top_9 = rank(min_rank_top_9, ties.method='first'),
      site_rank_median_rank_top_9 = rank(median_rank_top_9, ties.method='first'),
      site_rank_mean_rank_top_9 = rank(mean_rank_top_9, ties.method='first'),
      site_rank_max_rank_top_9 = rank(max_rank_top_9, ties.method='first'))
  # parepare a single table with info about each target and each source-target pair (is this merger really needed?)
  sources_info <- sources_info_partial %>% left_join(target_summary, by='target_id')
  
  return(list(target_summary=target_summary, sources_info=sources_info))
}
pgmtl_info <- prep_source_target_info(pgmtl_train_44225, lake_metadata_full)
pbmtl_info <- prep_source_target_info(pbmtl_train_44225, lake_metadata_full)

