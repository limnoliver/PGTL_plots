# as of 6/25, this file is only used for the examples and metamodel-source-selection-performance figures

library(feather)
library(tidyverse)

#### Get data ####

# Manual download from Jared, new/updated on 8/6/20 on Drive (https://drive.google.com/drive/u/0/folders/101SMKO-sngbpF6au9DZebfJdLh8xBXr0),
# to be replaced by ScienceBase release items soon:
# data/PB-MTL_all_sources_with_predictions.csv
# data/PB-MTL_results.csv
# data/PG-MTL_result_matrix_test_lakes_allsources.csv
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

# Scripted downloads from SB
lake_metadata <- readr::read_csv('data/lake_metadata.csv')
provided <- c('82815984', '91688597', '120018510', '120020636') # updated 8/6/20
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
    zipfile <- sprintf('data/zips/%s_predictions_%s.zip', model, group)
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