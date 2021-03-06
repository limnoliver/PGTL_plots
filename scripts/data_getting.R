# as of 6/25, this file is only used for the examples and metamodel-source-selection-performance figures

library(feather)
library(tidyverse)

#### Get data ####

# Manual download from Jared, new/updated on 8/6/20 on Drive (https://drive.google.com/drive/u/0/folders/101SMKO-sngbpF6au9DZebfJdLh8xBXr0),
# to be replaced by ScienceBase release items soon:
# data/PB-MTL_all_sources_with_predictions.csv
# data/PB-MTL_results.csv
# data/PG-MTL_result_matrix_test_lakes_all_sources.csv
# data/PG-MTL_result_matrix_test_lakes_ensemble_sources.csv
# data/PG-MTL_result_matrix_test_lakes_single_sources.csv
# data/PG-MTL9_305Lakes_PredictedRMSEStats_metadata.csv
# data/PG-MTL9_Expanded_2221Lakes_wMetadata.csv

# Manual download from Jared, new/updated on 8/3/20 on Drive (https://drive.google.com/drive/u/0/folders/101SMKO-sngbpF6au9DZebfJdLh8xBXr0),
# to be used for examples plot (won't ever be on ScienceBase):
# labels.zip -> data/labels/* # 305 feather files with observations in each target lake
# mtl_outputs_for_fig_Aug.zip -> data/mtl_outputs_for_fig/* # 4 folders, one per example lake, with labels, ensemble output, and sources 0-8 and 95-99
# mtl_outputs1_Aug.zip -> data/mtl_outputs1/* # (renamed) 305 feather files, one per target lake, with predictions from single top source
# mtl_outputs9_Aug.zip -> data/mtl_outputs9/* # 305 feather files, one per target lake, with predictions from ensemble
# data/mtl_outputs9exp_Aug.zip -> data/mtl_outputs_expanded/* # 2221 folders, each containing 3 feather files: labels, ensemble output, and top source output
# source_pgdl_outputs.zip -> data/source_pgdl_outputs/* # 145 feather files, one per source lake

#### Scripted downloads from SB ####

library(sbtools)
sbtools::authenticate_sb('aappling@usgs.gov')

# lake metadata / spatial data
lake_files <- c('lake_metadata.csv', '01_spatial.zip')
if(!dir.exists('data/zips')) dir.create('data/zips')
sbtools::item_file_download(
  sb_id='5ebe564782ce476925e44b26',
  names=lake_files,
  destinations=c('data/lake_metadata.csv', 'data/zips/01_spatial.zip'),
  overwrite_file=TRUE)
lake_metadata <- readr::read_csv('data/lake_metadata.csv', col_types=cols())
unzip('data/zips/01_spatial.zip', exdir='data/spatial')

# observations
temp_obs_file <- 'temperature_observations.zip'
sbtools::item_file_download(
  sb_id='5ebe566a82ce476925e44b29',
  names=temp_obs_file,
  destinations=sprintf('data/%s', temp_obs_file),
  overwrite_file=TRUE)
unzip('data/temperature_observations.zip', exdir='data')

# predictions for example sites...they take ~10 minutes per model type to download
# sites_needing_preds <- grep('^nhdhr_.*', dir('data/examples/mtl_outputs_for_fig'), value=TRUE) %>% # use these as a guide for what to download from SB
#   c('nhdhr_141288146', 'nhdhr_155636695', 'nhdhr_105231881', 'nhdhr_114542575')
sites_needing_preds <- read_csv('data/pbmtl_evaluation.csv', col_types=cols()) %>% pull(site_id)
if(!dir.exists('data/zips')) dir.create('data/zips')
if(!dir.exists('data/predictions')) dir.create('data/predictions')
purrr::walk(sites_needing_preds, function(site_id) {
  group <- lake_metadata %>%
    filter(site_id == !!site_id) %>%
    pull(group_id)
  for(model in c('pb0', 'pgmtl')) { #c('pb0','pball','pgmtl','pgmtl9')
    zipfile <- sprintf('data/zips/%s_predictions_%s.zip', model, group)
    if(!file.exists(zipfile)) {
      message(sprintf('downloading %s', basename(zipfile)))
      sbtools::item_file_download(
        sb_id='5ebe569582ce476925e44b2f',
        names=basename(zipfile), destinations=zipfile,
        overwrite_file=TRUE)
    }
    predfile <- sprintf('%s_%s_temperatures.csv', model, site_id)
    if(!file.exists(predfile)) {
      unzip(zipfile, files=predfile, exdir='data/predictions')
    }
  }
})

# source model info - predicted and observed RMSEs
sbtools::item_file_download(
  sb_id='5ebe577782ce476925e44b32',
  names='all_MTL_RMSE_predictions.csv',
  destinations='data/all_MTL_RMSE_predictions.csv',
  overwrite_file = TRUE)

# evaluation files
eval_files <- sprintf('%s_evaluation.csv', c('pb0','pball','pbmtl','pgmtl','pgmtl9'))
sbtools::item_file_download(
  sb_id='5ebe577782ce476925e44b32',
  names=eval_files,
  destinations=sprintf('data/%s', eval_files),
  overwrite_file = TRUE)
