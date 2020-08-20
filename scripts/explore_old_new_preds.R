#inventory old and new predictions

library(tidyverse)
new_st_pairs <- purrr::map(dir('data/mtl_outputs_for_fig', full.names=TRUE), function(target_dir) {
  tibble(
    target_id = basename(target_dir),
    new_file = dir(target_dir, pattern='source[[:digit:]]+_nhdhr[[:digit:]]+_output', full.names=TRUE)) %>%
    extract(new_file, into=c('new_source_rank','source_id'), regex='source([[:digit:]]+)_nhdhr([[:digit:]]+)_output', remove=FALSE) %>%
    mutate(source_id = sprintf('nhdhr_%s', source_id))
}) %>% bind_rows()
new_st_pairs

old_st_pairs <- 
  tibble(old_file = dir(c('data/good_outputs/good_outputs', 'data/bad_outputs/bad_outputs'), pattern='[[:digit:]]_outputs.feather', full.names=TRUE)) %>%
  extract(old_file, into=c('target_id', 'source_id'), regex='target_(nhdhr_[[:digit:]]+)_.*source_(nhdhr_[[:digit:]]+)_outputs.feather', remove=FALSE)

matched_st_pairs <- inner_join(new_st_pairs, old_st_pairs) %>%
  select(target_id, source_id, everything())

source('scripts/data_prep.R')
lapply(seq_len(nrow(matched_st_pairs)), function(row) {
  matched_pair <- matched_st_pairs[row,]
  old_preds <- read_preds_feather(matched_pair$old_file) %>% mutate(source='Old')
  new_preds <- read_preds_mtl_outputs_for_fig(matched_pair$new_file) %>% mutate(source='New')
  labels <- read_preds_mtl_outputs_for_fig(sprintf('%s/labels', dirname(matched_pair$new_file))) %>%
    filter(!is.na(temp_c)) %>%
    mutate(source='Observed')
  paired_preds <- bind_rows(old_preds, new_preds, labels)
  depths <- if(matched_pair$target_id=='nhdhr_82815984') c(0.5, 12.0, 23.0) else c(0,0.5,1)
  plot_data <- filter(paired_preds, depth %in% depths)
  
  
  
  ggplot(plot_data, aes(x=date, y=temp_c, color=source)) +
    geom_line(data=filter(plot_data, source!='Observed')) +
    geom_point(data=filter(plot_data, source=='Observed'))+
    facet_grid(depth ~ .) + theme_bw()
  ggsave(sprintf('figures/old_new_target_%s_source_%s.png', matched_pair$target_id, matched_pair$source_id))
})

