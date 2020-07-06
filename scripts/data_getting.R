# as of 6/25, this file is only used for the examples and metamodel-source-selection-performance figures

library(feather)
library(tidyverse)

#### Get data ####

# Manual download of single links shared by Jared:
# data/source_metadata.csv from https://docs.google.com/spreadsheets/d/1DnPHR1f7YZnsZXusF9MWCiHdgRh6BmhXlJfwq2zm8u8/edit#gid=19987048
# data/305_lakes_results.csv from https://docs.google.com/spreadsheets/d/1SK_0i8Qkpydn9ACb4AIfy7qWU6xUipWqOpzrFFpmWmM/edit?usp=sharing

# Manual downloaded from Jared from Drive (https://drive.google.com/drive/u/0/folders/101SMKO-sngbpF6au9DZebfJdLh8xBXr0):
# data/PG-MTL_result_matrix_test_lakes_all_sources.csv
# data/PG-MTL_result_matrix_test_lakes_single_sources.csv
# data/PG-MTL_result_matrix_test_lakes_ensemble_sources.csv
# data/good_outputs.zip
# data/bad_outputs.zip

# Manual download from Jared, new/updated on 7/6/20
# data/PB-MTL_all_sources_with_predictions.csv
# data/PG-MTL_result_matrix_test_lakes_single_sources.csv
# data/PG-MTL_result_matrix_test_lakes_ensemble_sources.csv
# data/PG-MTL9_305Lakes_PredictedRMSEStats_metadata.csv
# data/mtl_outputs_for_fig.zip

# Manual download from Jordan via Teams 6/19
# data/GLM_metamodel_predicted_sources_glm_transfer_pball_test_lakes.csv
# data/RMSE_transfer_test_extended_glm.csv

# Scripted downloads from SB

lake_metadata <- readr::read_csv('data/lake_metadata.csv')
provided <- list(
  good = c('120020376', '91593573', '91686475', '91688597', '91689681'), # good_outputs.zip
  bad = c('120018495', '120020398', '45730856', '60087894', '82815984')) # bad_outputs.zip
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