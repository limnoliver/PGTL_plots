library(tidyverse)
source('scripts/style.R')
source('scripts/data_prep.R')

#### Metamodel stats ####

pgmtl_train_305_date
median(sources_info$rmse)
filter(sources_info, rank_predicted==1) %>% summarize(median(rank))
filter(sources_info, rank_predicted<=9) %>% summarize(median(rank))
beanplot_summary

#### Metamodel figure ####

# TODO: plot spearman rank coefficient for each target model in the rank plot, separate y axis?
# one finding is that spearman ranks were worse for PBMTL than for PGMTL
rankplot <- sources_info %>%
  ggplot(aes(x=site_rank_rmse_median, y=rmse, color=rank_category)) +# site_rank_rank_top_1, site_rank_rmse_median, median_rank_top_9, min_rank_top_9
  geom_point(data=filter(sources_info, rank_category == 'Not Top 9'), alpha=0.3, size=1) +
  geom_point(data=filter(sources_info, rank_category == 'Top 9'), alpha=0.5, size=1) +
  geom_point(data=filter(sources_info, rank_category == 'Top 1'), size=1) +
  scale_color_manual('Metamodel\nprediction', breaks=c('Not Top 9', 'Top 9', 'Top 1'), values=c('Top 1'=model_colors[['PG-MTL']], 'Top 9'=model_colors[['PG-MTL9']], 'Not Top 9'=neutral_colors[['light']])) +
  theme_bw() +
  xlab('Target sites ranked by median actual RMSE of top 9 source models') +
  ylab('Actual RMSE of source applied to target')
rankplot
ggsave(sprintf('figures/metamodel_rankplot_%s.png', pgmtl_train_305_date), rankplot, width=7, height=4)
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
# target_summary %>% 
#   ggplot(aes(x=site_rank_rmse_median, y=min_rank_top_9)) + # site_rank_rmse_median, median_rank_top_9, min_rank_top_9
#   geom_point(aes(y=min_rank_top_9), color='navy') +
#   geom_point(aes(y=max_rank_top_9), color='lightblue') +
#   geom_point(aes(y=median_rank_top_9), color='blue') +
#   theme_bw() +
#   ylab('Min, median, and max ranks of 9 selected sources lakes') +
#   xlab('Target lakes ranked by median RMSE of all source lakes')
# filter(sources_info, rank_predicted==1) %>%
#   ggplot(aes(x=site_rank_rmse_median, y=rank)) +
#   geom_point() +
#   ylab('Actual rank of the source predicted to be rank 1') +
#   theme_bw()

beanplot_data <- target_summary %>%
  select(rank_top_1, min_rank_top_9, max_rank_top_9, median_rank_top_9) %>%
  pivot_longer(cols=everything(), names_to='rank_statistic', values_to='rank') %>%
  mutate(rank_statistic_label = c(
    'rank_top_1'='Predicted best',
    'min_rank_top_9'='Actual best\nof 9 predicted best',
    'median_rank_top_9'='Actual median\nof 9 predicted best',
    'max_rank_top_9'='Actual worst\nof 9 predicted best'
  )[rank_statistic] %>% ordered())
beanplot_summary <- beanplot_data %>%
  group_by(rank_statistic_label) %>%
  summarize(
    q25=quantile(rank, 0.25),
    q50=quantile(rank, 0.50),
    q75=quantile(rank, 0.75)) %>%
  pivot_longer(cols=starts_with('q'), names_to='quantile', names_prefix='q', values_to='rank') %>%
  mutate(hline_x=as.numeric(rank_statistic_label))
beanplot <- ggplot(beanplot_data, aes(x=rank_statistic_label)) +
  geom_violin(aes(y=rank, fill=rank_statistic), color=NA, trim=FALSE, scale='area') +
  scale_fill_manual(
    guide='none',
    values=c(rank_top_1=pgmtl_colors[['central']], min_rank_top_9=pgmtl9_colors[['dark']],
             median_rank_top_9=pgmtl9_colors[['central']], max_rank_top_9=pgmtl9_colors[['light']])) +
  geom_errorbarh(
    data=filter(beanplot_summary, quantile == '50'),
    aes(xmin=hline_x-0.1, xmax=hline_x+0.1, y=rank),
    size=0.8, height=0, color='gray80') +
  geom_errorbarh(
    data=filter(beanplot_summary, quantile != '50'),
    aes(xmin=hline_x-0.03, xmax=hline_x+0.03, y=rank),
    size=0.8, height=0, color='gray80') +
  # scale_x_discrete(values=c(
  #   "Predicted best"=1,
  #   "Actual best\nof 9 predicted best"=2,
  #   "Actual median\nof 9 predicted best"=3,
  #   "Actual worst\nof 9 predicted best"=3.5,
  # )) +
  ylim(0,145) +
  xlab('Source model') +
  ylab('Actual rank of source model by RMSE') +
  theme_bw()
beanplot
ggsave(sprintf('figures/metamodel_beanplot_%s.png', pgmtl_train_305_date), beanplot, width=7, height=4)
# filter(sources_info, rank_predicted==1) %>%
#   ggplot(aes(x='MLT-selected source', y=rank)) +
#   geom_violin(color=NA, fill='darkgray') +
#   geom_hline(aes(yintercept=median(rank))) +
#   ylab('Actual rank of the source predicted to be rank 1') +
#   ylim(0,145) +
#   theme_bw()
filter(sources_info, rank_predicted==1) %>% summarize(median(rank))
filter(sources_info, rank_predicted<=9) %>% summarize(median(rank))

# The sites where the MTL does better tend to be those where all source-target pairs do better.
sources_info

# Sam: And now circling back to where I think we wanted to go with these figures
# -- from any of these figures, can we understand for those lakes that had poor
# transfers, which/how many were because the metamodel performed poorly, and
# which/how many were because it was not possible to find a good source lake? I
# guess I would still argue that the metamodel "failed" in these second cases,
# unless it actually predicted that the transfer was going to be bad. Maybe a
# figure like: one dot for each 305 lakes, abs(x axis predicted minus actual
# rank), y axis is actual RMSE of the actual best. Then the four quadrants would
# be: bottom left - "we were right and the transfer was good"; top left - "we
# were right but the transfer never would have been good"; top right - "we were
# wrong, but it doesn't matter because the transfer never would have been good";
# bottom right - "we were wrong, and the transfer could have been good"
sources_info %>%
  filter(rank_predicted <= 1) %>%
  ggplot(aes(x=rank_predicted-rank, y=rmse_min)) +
  geom_point() +
  scale_y_log10() +
  theme_bw()
sources_info %>%
  filter(rank_predicted <= 1) %>%
  ggplot(aes(x=rmse_min, y=rmse)) +
  geom_abline() +
  geom_point() +
  theme_bw()
