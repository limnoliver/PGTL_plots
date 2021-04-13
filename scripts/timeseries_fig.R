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

# Note the sites we already have complete predictions for  
example_sites <- dir('data/examples/mtl_outputs_for_fig')

## Choose which sites to plot ##

# set basic parameters
core_sites <- pbmtl_eval_305$site_id
plot_year <- 2012
plot_min_date <- as.Date(sprintf('%d-01-01', plot_year))
plot_max_date <- as.Date(sprintf('%d-12-31', plot_year))

# read PGMTL and PB0 predictions (just to pick out max depths; we'll subset later). These commands take about 60 seconds apiece.
pgmtl_temp_preds <- purrr::map_df(set_names(core_sites), function(site_id) {
  out <- tryCatch(readr::read_csv(sprintf('data/predictions/pgmtl_%s_temperatures.csv', site_id), col_types=cols()), error=function(e) {NULL})
}, .id='site_id') %>%
  select(site_id, date, everything(), -index)
pb0_temp_preds <- purrr::map_df(set_names(core_sites), function(site_id) {
  out <- tryCatch(readr::read_csv(sprintf('data/predictions/pb0_%s_temperatures.csv', site_id), col_types=cols()), error=function(e) {NULL})
}, .id='site_id') %>%
  select(site_id, date, everything())

# pick out max depths for which PGMTL and PB0 have complete timeseries
find_max_depth <- function(temp_preds) {
  temp_preds %>%
    filter(between(date, plot_min_date, plot_max_date), site_id %in% core_sites) %>%
    tidyr::pivot_longer(cols=!any_of(c('site_id','date')), names_to='depth', names_prefix='temp_', names_transform=list(depth = as.numeric), values_to='temp') %>%
    group_by(site_id, depth) %>%
    summarize(depth_complete = !any(is.na(temp)), .groups='drop_last') %>%
    filter(depth_complete) %>%
    summarize(max_depth = max(depth), .groups='drop')
}
max_depths_pred <- left_join(
  find_max_depth(pgmtl_temp_preds),
  find_max_depth(pb0_temp_preds),
  by='site_id', suffix=c('_pgmtl','_pb0')) %>%
  mutate(max_pred_depth = pmin(max_depth_pgmtl, max_depth_pb0))

# subset observation sites and depths based on data availability
plot_temp_obs <- temp_obs %>%
  filter(site_id %in% core_sites, between(date, plot_min_date, plot_max_date)) %>%
  select(-source)
common_depths <- plot_temp_obs %>%
  left_join(max_depths_pred, by='site_id') %>%
  filter(depth <= max_pred_depth) %>% # don't pick an observed depth that's deeper than the completely modeled depths
  group_by(site_id, depth) %>% 
  tally(name='num_obs') %>%
  mutate(num_depths = n(), depth_class = ordered(case_when(depth <= max(depth)/2 ~ 'shallow', TRUE ~ 'deep'), levels=c('shallow','deep'))) %>%
  group_by(site_id, depth_class) %>%
  filter(num_obs > 0.8*max(num_obs)) %>% # pick the well-observed depths
  arrange(depth * case_when(depth_class == 'shallow' ~ 1, TRUE ~ -1)) %>% # get the shallowest well-observed shallow, deepest well-observed deep
  slice(1) %>% # get the best choice in each case
  arrange(site_id, depth_class) %>%
  select(site_id, depth, depth_class, num_obs) %>%
  ungroup()
well_observed <- common_depths %>%
  filter(num_obs > 5) %>% # at least x observations at each key/common depth
  group_by(site_id) %>%
  filter(n() == 2) # both shallow and deep are well observed at the site

# subset and format pgmtl predictions
plot_temp_preds <- pgmtl_temp_preds %>%
  filter(between(date, plot_min_date, plot_max_date), site_id %in% well_observed$site_id) %>% # filtering to well_observed now makes it go a bit faster
  tidyr::pivot_longer(cols=!any_of(c('site_id','date')), names_to='depth', names_prefix='temp_', names_transform=list(depth = as.numeric), values_to='temp') %>%
  right_join(well_observed, by=c('site_id','depth')) # only keep the selected 2 depths

# # Exploration: is there something weird about the predictions? seems like a lot
# # of missing timeseries for plot_temp_preds vs plot_temp_obs. But Jordan and I
# # just discussed and decided that while those really deep obs depths should have
# # been filtered out of the data release (we think they were entered wrong when
# # they differ by more than ~3m from glm max depths), they have already been
# # filtered out for the purpose of training and assessing the models (the stuff
# # that goes in the paper).
# # The site counts themselves are fine:
# obs_sites <- well_observed$site_id %>% unique %>% sort
# pred_sites <- plot_temp_preds$site_id %>% unique %>% sort
# all.equal(obs_sites, pred_sites)
# # but the max depths can differ quite a lot between observations and prediction
# max_depths_obs <- temp_obs %>% filter(site_id %in% unique(well_observed$site_id)) %>% group_by(site_id) %>% summarize(max_depth = max(depth), .groups='drop')
# max_depths_pgmtl <- pgmtl_temp_preds %>%
#   filter(between(date, plot_min_date, plot_max_date), site_id %in% unique(well_observed$site_id)) %>%
#   tidyr::pivot_longer(cols=!any_of(c('site_id','date')), names_to='depth', names_prefix='temp_', names_transform=list(depth = as.numeric), values_to='temp') %>%
#   filter(!is.na(temp)) %>%
#   group_by(site_id) %>%
#   summarize(max_depth = max(depth), .groups='drop')
# depth_comparison <- left_join(max_depths_obs, max_depths_pgmtl, suffix=c('_obs','_pred'), by='site_id') %>%
#   left_join(filter(common_depths, depth_class == 'deep') %>% mutate(depth_deep = depth) %>% select(site_id, depth_deep), by='site_id') %>%
#   arrange(depth_deep) %>%
#   mutate(site_id = ordered(site_id, levels=site_id))
# depth_comparison %>%
#   ggplot(aes(x=site_id)) +
#   geom_point(aes(y=max_depth_obs), shape='o') +
#   geom_point(aes(y=max_depth_pred), shape='p') +
#   geom_point(aes(y=depth_deep, color=depth_deep > max_depth_pred), shape='d') +
#   theme_minimal()
# depth_comparison %>% arrange(desc(max_depth_obs - max_depth_pred)) %>%
#   select(-depth_deep) %>%
#   filter(max_depth_obs > max_depth_pred) %>% 
#   mutate(abs_diff = max_depth_obs - max_depth_pred, rel_diff_pct = 100 * (max_depth_obs / max_depth_pred - 1)) %>%
#   print(n=90)

# Decide whether a lake is stratified in this year. Jordan's recommendation: "an
# approach that is published elsewhere would be taking the difference between
# the two and calculating percent of the time when the difference is more than
# 1. I’d suggest the threshold of 75%."
assess_strat <- function(temp_dat, well_observed) {
  temp_dat %>%
    select(site_id, date, depth, temp) %>%
    right_join(well_observed, by=c('site_id', 'depth')) %>%
    select(-num_obs, -depth) %>%
    pivot_wider(names_from = 'depth_class', values_from='temp', id_cols=c('site_id','date')) %>%
    filter(lubridate::month(date) %in% 6:8) %>%
    group_by(site_id) %>%
    summarize(
      frac_days_strat = length(which(shallow - 1 > deep)) / length(shallow),
      stratifies = case_when(frac_days_strat >= 0.75 ~ TRUE, frac_days_strat < 0.75 ~ FALSE, TRUE ~ NA),
      .groups='drop')
}
strat_obs <- assess_strat(plot_temp_obs, well_observed)
strat_obs %>% group_by(stratifies) %>% tally
strat_pred <- assess_strat(plot_temp_preds, well_observed)
strat_pred %>% group_by(stratifies) %>% tally

candidate_sites <- full_join(strat_obs, strat_pred, suffix=c('_obs','_pred'), by='site_id') %>%
  filter(!is.na(stratifies_obs), !is.na(stratifies_pred)) %>%
  left_join(pivot_wider(well_observed, names_from='depth_class', values_from=c('depth','num_obs')), by='site_id') %>%
  left_join(all_eval_2366, by='site_id') %>%
  left_join(lake_metadata_full, by='site_id') %>%
  mutate(
    lathrop_obs_agree = stratifies_obs == lathrop_strat, # motivation: Medicine Lake (120018088 had !stratifies_obs but lathrop_strat and sure looks stratified, just with noisy observations)
    desired_pgdl_pb0_order = case_when(
      stratifies_obs == stratifies_pred ~ pgmtl_rmse < pb0_rmse,
      stratifies_obs != stratifies_pred ~ pgmtl_rmse > pb0_rmse),
    example_quality = case_when(
      stratifies_obs == stratifies_pred ~ -pgmtl_rmse,
      stratifies_obs != stratifies_pred ~ pgmtl_rmse))
candidate_sites %>%
  filter(desired_pgdl_pb0_order) %>%
  group_by(stratifies_obs, stratifies_pred) %>% tally
show_candidates <- function(candidate_sites, strat_obs=c(TRUE, FALSE), strat_pred=c(TRUE, FALSE)) {
  candidate_sites %>%
    filter(stratifies_obs %in% strat_obs, stratifies_pred %in% strat_pred) %>%
    # filter(lathrop_obs_agree) %>% # optional but helps clean up comparisons between col 1 and col 2
    select(desired_pgdl_pb0_order, lathrop_obs_agree, stratifies_obs, stratifies_pred, site_id, frac_days_strat_obs, frac_days_strat_pred, depth_deep) %>%
    group_by(stratifies_obs, stratifies_pred, lathrop_obs_agree, desired_pgdl_pb0_order) %>%
    arrange(frac_days_strat_obs - frac_days_strat_pred)
}
show_candidates(candidate_sites) %>% tally()
show_candidates(candidate_sites) %>% filter(lathrop_obs_agree) %>% group_by(stratifies_obs, stratifies_pred, lathrop_obs_agree) %>% tally() # limits the TRUE/FALSE combo to 1 site
show_candidates(candidate_sites) %>% filter(desired_pgdl_pb0_order) %>% group_by(stratifies_obs, stratifies_pred, desired_pgdl_pb0_order) %>% tally() # limits the FALSE/TRUE combo to 1 site
show_candidates(candidate_sites) %>% filter(lathrop_obs_agree, desired_pgdl_pb0_order) %>% tally()
show_candidates(candidate_sites, strat_obs=TRUE, strat_pred=FALSE) %>% filter(lathrop_obs_agree, desired_pgdl_pb0_order) # there's only one in each disagreement category that meets all requirements
show_candidates(candidate_sites, strat_obs=FALSE, strat_pred=TRUE) %>% filter(lathrop_obs_agree, desired_pgdl_pb0_order) # there's only one in each disagreement category that meets all requirements
targets <- candidate_sites %>%
  # Choose target sites through a combination of filtering, ranking, and picking the top-ranked in each category
  # filter(depth_deep > 2.5) %>% # deepest I can go for 2012 is 2.5, for 2013 is 2, for 2014 is 3.5
  # filter(between(num_obs_deep, 6, 50)) %>% # super-well-observed lakes make it hard to see the predictions on the plot
  # filter(ifelse(stratifies_obs == FALSE & stratifies_pred == TRUE, site_id=='nhdhr_120020213', TRUE)) %>% # custom selection for panel d
  filter(lathrop_obs_agree) %>%
  # filter(desired_pgdl_pb0_order) %>%
  group_by(stratifies_obs, stratifies_pred) %>%
  arrange(desc(example_quality)) %>% # or could arrange by desc(num_obs_deep) instead
  slice(1) %>%
  ungroup() %>%
  filter(stratifies_obs | !stratifies_pred) %>% # omit that 4th category (!stratifies_obs & stratifies_pred) after all
  
  # now add information
  arrange(desc(stratifies_obs), desc(stratifies_obs == stratifies_pred)) %>%
  mutate(
    target_order = seq_len(n()),
    site_num = gsub('nhdhr_', '', site_id),
    target_lake_name = case_when(is.na(lake_name) ~ 'Unnamed', TRUE ~ lake_name),
    target_name = sprintf('%s (%s)', target_lake_name, site_num))
  # select(target_id=site_id, target_name, target_lake_name, site_num, max_depth, surface_area, n_obs, lathrop_strat, ends_with('rmse'), starts_with('stratifies')) %>%
  # arrange(target_name)
t(targets)
paste0("'", targets$site_id, "'", collapse=', ') %>% cat
# on 4/11/21: 'nhdhr_59808243', 'nhdhr_91689611', 'nhdhr_120020759', 'nhdhr_120020213'

#' # Original site-selection idea was these three panels:
#' # * good performance and good improvement over PB0
#' # * good performance but worse performance than PB0
#' # * bad performance for both models
#' # filter(all_eval_305, pgmtl_rmse > pb0_rmse + 0.5, pb0_rmse > 3, pb0_rmse < 4, pgmtl_rmse < 4.5) %>%
#' #   arrange(pb0_rmse)
#' filter(all_eval_2366, site_id %in% example_sites) %>%
#'   select(site_id, pgmtl_rmse, pb0_rmse, pgmtl9_rmse, pball_rmse) %>%
#'   arrange(pb0_rmse)
#' old_targets <- tribble(
#'   ~mtl_rmse_class, ~pb0_rmse_class, ~target_id,
#'   #'good', 'good', 'nhdhr_120020636', # weird reason to exclude, but it has too many observations so it's really hard to see the predictions
#'   'bad', 'good', 'nhdhr_120018510', # as of aug 20 they were about equal in performance and both pretty good. as of sept 5 it's pgmtl=2.84, pb0=1.88
#'   'good', 'bad', 'nhdhr_91688597', # good performance and good improvement over PB0. this is a good site choice
#'   'bad', 'bad', 'nhdhr_82815984' # bad performance for both models
#' ) %>%
#'   left_join(all_eval_2366, by=c('target_id'='site_id')) %>%
#'   select(target_id, mtl_rmse_class, pgmtl_rmse, pgmtl9_rmse, pb0_rmse_class, pb0_rmse, pball_rmse)

targets_in_context <- all_eval_2366 %>%
  filter(site_id %in% target_lakes) %>%
  mutate(
    status = ifelse(
      site_id %in% targets$site_id, 'selected',
      ifelse(site_id %in% example_sites, 'available', 'unavailable')),
    marker_text = sprintf('%s\npgmtl9: %f\npgmtl: %f\npball: %f\npb0: %f', site_id, pgmtl9_rmse, pgmtl_rmse, pball_rmse, pb0_rmse)) %>%
  left_join(lake_metadata_full, by='site_id')

# visualize selection and revise above as needed: here's an
# interactive plot of the selected and non-selected targets
library(plotly)
g <- ggplot(targets_in_context, aes(group=site_id, x=pb0_rmse, y=pgmtl_rmse, shape=lathrop_strat)) +
  geom_abline(color='slateblue', alpha=0.3) +
  geom_point(data=filter(targets_in_context, status=='available'), color='lightgray') + 
  geom_point(data=filter(targets_in_context, status=='unavailable'), color='darkgray') + 
  geom_point(data=filter(targets_in_context, status=='selected'), aes(color=site_id)) +
  scale_shape_manual('', values=c(4, 20), guide='none') +
  scale_color_discrete(guide='none') + # guide='none' seems to be ignored by ggplotly
  theme_bw()
p <- ggplotly(g)
htmlwidgets::saveWidget(p, sprintf("%s/figures/examples_selection.html", getwd()), selfcontained = TRUE)

#### Subset Data ####

# eg_targets_info <- targets %>%
#   rename(site_id = target_id) %>%
#   mutate(
#     site_num = gsub('nhdhr_', '', site_id),
#     target_lake_name = case_when(is.na(lake_name) ~ 'Unnamed', TRUE ~ lake_name),
#     target_name = sprintf('%s (%s)', target_lake_name, site_num)) %>%
#   select(target_id=site_id, target_name, target_lake_name, site_num, max_depth, surface_area, n_obs, lathrop_strat, ends_with('rmse'), starts_with('stratifies')) %>%
#   arrange(target_name)
eg_sources_info <- targets %>%
  select(site_id, site_num, target_lake_name, target_name, target_order) %>%
  left_join(pgmtl_info$sources_info, by=c(site_id = 'target_id')) %>%
  left_join(select(lake_metadata_full, site_id, source_lathrop_strat = lathrop_strat), by=c('source_id'='site_id'))
# note that some sources are used 2-3 times (dots will overlap)
#eg_sources_info %>% filter(top_9) %>% group_by(source_id) %>% tally() %>% arrange(desc(n))

#### Timeseries Figure ####
read_published_timeseries_data <- function(targets, target_num=3, pgmtl_train_44225, plot_min_date, plot_max_date, depths) {
  target <- targets[[target_num,'site_id']]
  
  source_ranks <- pgmtl_train_44225 %>%
    filter(target_id == target) %>%
    arrange(predicted_rmse) %>%
    mutate(rank = 1:n()) %>%
    select(source_id, actual_rmse, rank)
  # replace this section temporarily, until Jared can get me predictions for ranks 1, 9, and 99 all together
  # source_files <- dir(sprintf('data/examples/mtl_outputs_for_fig/%s', target), pattern='source.*_nhdhr.*_output', full.names=TRUE) %>%
  #   grep('source(0|8|98)_', ., value=TRUE)
  # sources <- purrr::map(source_files, function(source_file) {
  #   source_site <- tibble(filename=source_file) %>%
  #     tidyr::extract(filename, into=c('source_rank', 'source_id'), regex='source([[:digit:]]+)_(nhdhr.*)_output', convert=TRUE) %>%
  #     mutate(source_id = gsub('nhdhr', 'nhdhr_', source_id),
  #            source_rank = source_rank + 1)
  #   read_preds_mtl_outputs_for_fig(source_file) %>% 
  #     filter(between(date, plot_min_date, plot_max_date)) %>%
  #     mutate(source_id = source_site$source_id)
  # }) %>% bind_rows() %>%
  #   left_join(source_ranks, by=c('source_id'))
  # temporary replacement will only have rank 1 (pgmtl), columns date, depth, temp_c, source_id, actual_rmse, rank
  source1 <- plot_temp_preds %>%
    filter(site_id == target) %>%
    select(date, depth, temp_c = temp) %>%
    bind_cols(filter(source_ranks, rank == 1))
  source9_dummy <- source1 %>% mutate(source_id = 'nhdhr_source_9', temp_c = 0, rank = 9)
  source99_dummy <- source1 %>% mutate(source_id = 'nhdhr_source_99', temp_c = 0, rank = 99)
  sources <- bind_rows(source1, source9_dummy, source99_dummy)
  
  labels <- temp_obs %>%
    filter(site_id == target) %>%
    select(date, depth, temp_c = temp) %>%
    mutate(source_id='Observed')
  pb0_preds <- read_preds_csv(sprintf('data/predictions/pb0_%s_temperatures.csv', target)) %>%
    mutate(
      source_id = 'PB0',
      actual_rmse = filter(all_eval_2366, site_id == target) %>% pull(pb0_rmse),
      rank = NA)
  
  # return date, depth, temp_c, source_id, actual_rmse, rank, target_id for 31M rows
  bind_rows(sources, labels, pb0_preds) %>%
    filter(between(date, plot_min_date, plot_max_date), depth %in% depths) %>%
    mutate(target_id=target) %>%
    return()
}
published_target_data <- lapply(seq_len(nrow(targets)), function(target_num) {
  read_published_timeseries_data(
    targets, target_num,
    pgmtl_train_44225, 
    plot_min_date, plot_max_date,
    depths = filter(common_depths, site_id == targets[[target_num, 'site_id']]) %>% pull(depth))
}) %>%
  bind_rows() %>%
  filter(!is.na(temp_c))
#published_target_data %>% group_by(target_id, rank) %>% tally()

read_source_n_timeseries_data <- function(targets, target_num=3, pgmtl_train_44225) {
  target <- targets[[target_num,'site_id']]
  
  source_ranks <- pgmtl_train_44225 %>%
    filter(target_id == target) %>%
    arrange(predicted_rmse) %>%
    mutate(rank = 1:n()) %>%
    select(source_id, actual_rmse, rank) %>%
    filter(rank %in% c(2:9, 96:100))
  source_files <- dir(sprintf('data/examples/mtl_outputs_for_fig/%s', target), pattern='source.*_nhdhr.*_output', full.names=TRUE) %>%
    grep(sprintf('nhdhr(%s)_', paste(gsub('nhdhr_', '', source_ranks$source_id), collapse='|')), ., value=TRUE)
  if(length(source_files) == 0) return(tibble())
  sources <- purrr::map(source_files, function(source_file) {
    source_site <- tibble(filename=source_file) %>%
      tidyr::extract(filename, into=c('source_rank', 'source_id'), regex='source([[:digit:]]+)_(nhdhr.*)_output', convert=TRUE) %>%
      mutate(source_id = gsub('nhdhr', 'nhdhr_', source_id),
             source_rank = source_rank + 1)
    read_preds_mtl_outputs_for_fig(source_file) %>%
      filter(between(date, plot_min_date, plot_max_date)) %>%
      filter(depth == targets[[target_num,'depth_shallow']] | depth == targets[[target_num,'depth_deep']]) %>%
      mutate(source_id = source_site$source_id)
  }) %>% bind_rows() %>%
    left_join(source_ranks, by=c('source_id'))
  
  # labels <- read_preds_mtl_outputs_for_fig(sprintf('data/examples/mtl_outputs_for_fig/%s/labels.feather', target)) %>%
  #   filter(!is.na(temp_c)) %>%
  #   mutate(source_id='Observed')
  # pb0_preds <- read_preds_csv(sprintf('data/predictions/pb0_%s_temperatures.csv', target)) %>%
  #   mutate(
  #     source_id = 'PB0',
  #     actual_rmse = filter(all_eval_2366, site_id == target) %>% pull(pb0_rmse),
  #     rank = NA)
  # bind_rows(sources, labels, pb0_preds) %>%

  sources %>%
    mutate(target_id=target) %>%
    return()
}
source_n_target_data <- lapply(seq_len(nrow(targets)), function(target_num) {
  read_source_n_timeseries_data(targets, target_num, pgmtl_train_44225)
}) %>%
  bind_rows()

# combine published and source-n timeseries, removing dummy timeseries for which we now have data
has_source_n_data <- source_n_target_data %>%
  group_by(target_id, rank) %>%
  summarize(has_source_n_data=TRUE, .groups='drop')
dedup_published_target_data <- published_target_data %>%
  left_join(has_source_n_data, by=c('target_id','rank')) %>%
  filter(is.na(has_source_n_data))
all_target_data <- bind_rows(dedup_published_target_data, source_n_target_data)
# all_target_data %>%
#   select(target_id, rank, source_id) %>%
#   distinct() %>%
#   arrange(target_id, rank, source_id) %>%
#   print(., n=nrow(.))

plot_timeseries <- function(all_target_data, targets, common_depths, plot_min_date, plot_max_date, lake_xdate) {
  plot_data <- all_target_data %>%
    left_join(common_depths, by=c(target_id='site_id','depth')) %>%
    filter(depth_class == 'deep') %>%
    filter(is.na(rank) | rank %in% c(1,9,99)) %>%
    mutate(Model = ifelse(grepl('nhdhr', source_id), sprintf('Source %s', rank), source_id)) %>%
    # mutate(Model = ifelse(grepl('nhdhr', source_id), sprintf('Source %s', ifelse(between(rank, 2, 9), '2-9', ifelse(between(rank, 96, 100), '96-100', rank))), source_id)) %>%
    filter(Model != 'PB0') %>%
    left_join(targets, by=c(target_id = 'site_id'))
  lake_labels <- targets %>%
    mutate(
      date=as.Date(lake_xdate), temp_c=29.7, depth_class='deep', # for positioning on the plot
      lab=sprintf(
        '%s\n%s, %s PGDL-MTL\nMax depth %0.1fm\nPreds at %0.1fm',
        target_name,
        # ifelse(lathrop_strat, 'stratified', 'mixed'), # use with \nLathrop-%s
        ifelse(stratifies_obs, 'Stratified', 'Mixed'), ifelse(stratifies_obs==stratifies_pred, 'accurate', 'inaccurate'),
        max_depth, depth_deep))
  panel_letters <- targets %>%
    mutate(
      date=as.Date(plot_min_date),
      temp_c=34, depth_class='deep', # for positioning on the plot
      lab=letters[targets$target_order])
  ggplot(plot_data, aes(x=date, y=temp_c)) + # linetype=depth_class, fill=depth_class
    geom_line(data=filter(plot_data, source_id != 'Observed'), aes(color=Model, group=rank)) +
    geom_point(data=filter(plot_data, source_id == 'Observed'), aes(shape=Model), color=model_colors['Observed'], size=0.8) + #, shape=depth_class, fill=depth_class
    geom_text(data=lake_labels, aes(label=lab), color='black', size=3, hjust=0) +
    geom_text(data=panel_letters, aes(label=lab), color='black', size=5) +
    scale_color_manual('', values=c(
      'Source 1'=pgmtl_colors[['central']],
      'Source 9'=pgmtl9_colors[['dark']],
      'Source 99'=map_colors[['extended_targets']])) +
      # 'Source 2-9'=pgmtl9_colors[['dark']], 
      # 'Source 96-100'=map_colors[['extended_targets']])) +
    scale_shape_manual('', values=c(Observed = 19)) +
    guides(shape = guide_legend(title='', override.aes = list(color=model_colors[['Observed']]))) +
    scale_x_date(date_labels='%b') +
    xlab(sprintf('Date in %d', plot_year)) +
    ylab(expression(paste("Temperature (",degree,"C)"))) +
    theme_bw() +
    facet_grid(target_order ~ .) +
    theme(
      panel.grid = element_blank(),
      strip.text.y=element_blank(),
      legend.position='bottom', legend.direction='horizontal', legend.box='vertical', legend.spacing.y=unit(-0.3, 'lines'), legend.spacing.x=unit(0.1, 'lines'))
}
egplot_timeseries <- plot_timeseries(
  all_target_data,
  targets,
  common_depths,
  plot_min_date=plot_min_date,
  plot_max_date=plot_max_date,
  lake_xdate=sprintf('%d-01-20', plot_year))
egplot_timeseries

plot_depth_area <- function(eg_sources_info, targets) {
  panel_letters <- targets %>%
    mutate(
      x = min(eg_sources_info$surface_area/1000000)*1.05,
      y = 0.985*max(eg_sources_info$max_depth),
      lab = letters[targets$target_order+3])
  strat_labels <- set_names(c('Stratified', 'Mixed'), c(TRUE, FALSE))
  strat_colors <- set_names(c('gray45', 'lightgray'), c(TRUE, FALSE))
  ggplot(eg_sources_info, aes(x=surface_area/1000000, y=max_depth)) +
    # geom_point(data=filter(eg_sources_info, source_lathrop_strat == FALSE), aes(size=n_obs), color=neutral_colors[['dark']], shape=20) +
    # geom_point(data=filter(eg_sources_info, source_lathrop_strat == TRUE), aes(size=n_obs), color=neutral_colors[['light']], shape=20) +
    geom_point(data=eg_sources_info, aes(color=source_lathrop_strat, fill=source_lathrop_strat, shape=source_lathrop_strat), size=0.8) +
    geom_point(data=mutate(targets, category = c('Target','Top 1','Top 9')), aes(alpha = category), shape=3, size=3, color='black') + # introducing alpha here just to get the legend to show up; we'll override everything but the labels shortly
    geom_point(data=filter(eg_sources_info, top_9 & !rank_predicted==1), shape = 21, size = 2, fill='transparent', color=pgmtl9_colors[['central']]) + # color=pgmtl9_colors[['dark']]
    geom_point(data=filter(eg_sources_info, rank_predicted==1), shape = 21, size = 2, fill='transparent', color=pgmtl_colors[['central']]) + # color=pgmtl9_colors[['dark']]
    geom_text(data=panel_letters, aes(x=x, y=y, label=lab), color='black', size=5) +
    scale_shape_manual('', breaks=c(TRUE,FALSE), values=set_names(c(25, 22), c(TRUE, FALSE)), labels=strat_labels) +
    scale_fill_manual('', breaks=c(TRUE,FALSE), values=strat_colors, labels=strat_labels) +
    scale_color_manual('', breaks=c(TRUE,FALSE), values=strat_colors, labels=strat_labels) +
    scale_alpha_manual('', breaks=c('Target','Top 1','Top 9'), values=c('Target'=1, 'Top 1'=1, 'Top 9'=1), labels=c('Target'='Target', 'Top 1'='Source 1', 'Top 9'='Source 2-9')) +
    guides(alpha = guide_legend('', override.aes = list(
      shape=c('Target'=3, 'Top 1'=21, 'Top 9'=21),
      color=c('black', pgmtl_colors[['central']], pgmtl9_colors[['central']]), fill='transparent'))) +
    scale_x_log10(labels=function(x) sprintf('%s', as.character(signif(x, 1)))) +
    facet_grid(target_order ~ .) +
    xlab('Surface Area (km'^2~')') + ylab('Maximum Depth (m)') +
    theme_bw() +
    theme(
      strip.text.y=element_blank(),
      panel.grid = element_blank(),
      legend.position='bottom', legend.direction='horizontal', legend.box='vertical', legend.spacing.y=unit(-0.3, 'lines'), legend.spacing.x=unit(0.1, 'lines'))
}
egplot_depth_area <- plot_depth_area(eg_sources_info, targets)
egplot_depth_area
# ggsave('figures/examples_depth_area.png', plot=egplot_depth_area, width=6, height=4)

plot_rmse_predobs <- function(eg_sources_info, targets) {
  rmse_range <- range(c(1, eg_sources_info$actual_rmse, eg_sources_info$predicted_rmse))
  panel_letters <- targets %>%
    mutate(
      # source_lathrop_strat = TRUE,
      x = rmse_range[1] * 1.05,
      y = rmse_range[2] * 0.98,
      lab = letters[targets$target_order+6])
  rmse_labels <- targets %>%
    mutate(
      # source_lathrop_strat = TRUE, rank_category = # so mapping doesn't cause errors
      y=1.24, x=9.1, # for positioning on the plot, log-log scale
      y=1.4, x=8.5, #9.1, # for positioning on the plot, non-log scale
      # lab=sprintf('PB0: %0.1f °C\nPGDL-MTL: %0.1f °C\nPGDL-MTL9: %0.1f °C', pb0_rmse, pgmtl_rmse, pgmtl9_rmse))
      lab=sprintf('PGDL-MTL: %0.1f °C\nPGDL-MTL9: %0.1f °C', pgmtl_rmse, pgmtl9_rmse))
  eg_sources_info %>%
    ggplot(aes(y=predicted_rmse, x=actual_rmse, color=rank_category)) +
    geom_abline(color='lightgray') +
    geom_point(data=eg_sources_info, aes(color = rank_category), size=0.8) +
    geom_point(data=filter(eg_sources_info, rank_category %in% c('Top 1','Top 9')), aes(color = rank_category), size=0.8) + # overplot with the important ones
    scale_color_manual('', breaks=levels(pgmtl_info$sources_info$rank_category),
                       values=c('Top 1'=pgmtl_colors[['central']], 'Top 9'=pgmtl9_colors[['central']], 'Not Top 9'=neutral_colors[['light']]),
                       labels=c('Top 1'='Source 1', 'Top 9'='Source 2-9', 'Not Top 9'='Source >9')) +
    geom_text(data=panel_letters, aes(x=x, y=y, label=lab), color='black', size=5, inherit.aes = FALSE) +
    geom_text(data=rmse_labels, aes(x=x, y=y, label=lab), color='black', size=3, hjust=1, inherit.aes = FALSE) +
    geom_vline(data=mutate(targets, linetype=c('PGDL-MTL','PGDL-MTL9')[c(1:2,1)]), aes(xintercept=pgmtl_rmse, linetype=linetype), color=model_colors[['PG-MTL']]) + # introducing linetype just to get a line color legend
    geom_vline(data=targets, aes(xintercept=pgmtl_rmse), color=model_colors[['PG-MTL']]) +
    geom_vline(data=targets, aes(xintercept=pgmtl9_rmse), color=model_colors[['PG-MTL9']]) +
    scale_linetype_manual('', breaks=c('PGDL-MTL','PGDL-MTL9'), values=c('PGDL-MTL'=1,'PGDL-MTL9'=1)) +
    guides(linetype = guide_legend('', override.aes = list(color=set_names(model_colors[c('PG-MTL','PG-MTL9')], c('PGDL-MTL','PGDL-MTL9'))))) +
    scale_x_continuous(breaks=c(1:9)) +
    scale_y_continuous(breaks=c(1:9)) +
    # scale_x_log10(breaks=c(1:9)) +
    # scale_y_log10() +
    coord_cartesian(xlim = rmse_range, ylim = rmse_range) +
    facet_grid(target_order ~ .) +
    xlab('Actual RMSE (°C)') + ylab('Predicted RMSE (°C)') +
    theme_bw() + 
    theme(
      strip.text.y=element_blank(),
      panel.grid = element_blank(),
      legend.position='bottom', legend.direction='horizontal', legend.box='vertical', legend.spacing.y=unit(-0.3, 'lines'), legend.spacing.x=unit(0.1, 'lines'))
}
egplot_rmse_predobs <- plot_rmse_predobs(eg_sources_info, targets)
egplot_rmse_predobs
# ggsave('figures/examples_rmse_predobs.png', plot=egplot_rmse_predobs, width=6, height=4)

library(gridExtra)
examples_figure <- grid.arrange(
  grobs = cowplot::align_plots(
    egplot_timeseries,
    egplot_depth_area,
    egplot_rmse_predobs,
    align = 'h', axis = 'bt'
  ),
  ncol=3, widths = c(1, 1, 1))
cowplot::save_plot('figures/examples_multipanel.png', examples_figure, base_height=9, base_width=9)
cowplot::save_plot('figures/examples_multipanel.eps', examples_figure, base_height=9, base_width=9)

## Stats for text

all_eval_2366 %>% filter(site_id %in% core_sites) %>% left_join(lake_metadata_full, by='site_id') %>% group_by(lathrop_strat) %>%
  rename(rmse = pgmtl_rmse) %>%
  summarize(n=n(), min = min(rmse), median = median(rmse), mean = mean(rmse), max = max(rmse), sd = sd(rmse))
all_eval_2366 %>% filter(site_id %in% core_sites) %>% left_join(lake_metadata_full, by='site_id') %>% group_by(lathrop_strat) %>%
  rename(rmse = pgmtl9_rmse) %>%
  summarize(n=n(), min = min(rmse), median = median(rmse), mean = mean(rmse), max = max(rmse), sd = sd(rmse))

