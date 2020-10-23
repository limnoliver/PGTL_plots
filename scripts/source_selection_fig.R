library(tidyverse)
source('scripts/style.R')
source('scripts/data_prep.R')

#### Metamodel figures ####

plot_rankplot <- function(sources_info, model_type=c('PB','PGDL')) {
  # c(PB='Process-based', PG='Process-guided')[model_type]
  rankplot <- sources_info %>%
    ggplot(aes(x=site_rank_rmse_median, y=actual_rmse, color=rank_category)) +# site_rank_rank_top_1, site_rank_rmse_median, median_rank_top_9, min_rank_top_9
    geom_point(data=filter(sources_info, rank_category == 'Not Top 9'), alpha=0.3, size=1) +
    geom_point(data=filter(sources_info, rank_category == 'Top 9'), alpha=0.5, size=1) +
    geom_point(data=filter(sources_info, rank_category == 'Top 1'), size=1) +
    scale_color_manual(sprintf('Metamodel\nprediction'), breaks=c('Not Top 9', 'Top 9', 'Top 1'), values=c('Top 1'=model_colors[['PG-MTL']], 'Top 9'=model_colors[['PG-MTL9']], 'Not Top 9'=neutral_colors[['light']])) +
    theme_bw() +
    xlab(sprintf('Target sites ranked by median actual RMSE of top 9 %s source models', model_type)) +
    ylab(sprintf('Actual RMSE of %s source applied to target', model_type))
  rankplot
  ggsave(sprintf('figures/metamodel_rankplot_%s.png', model_type), rankplot, width=7, height=4)
}
plot_rankplot(pgmtl_info$sources_info, model_type='PGDL')
plot_rankplot(pbmtl_info$sources_info, model_type='PB')

# plot_beanplot <- function(target_summary, model_type='PGDL') {
#   beanplot_data <- target_summary %>%
#     select(rank_top_1, min_rank_top_9, max_rank_top_9, median_rank_top_9) %>%
#     pivot_longer(cols=everything(), names_to='rank_statistic', values_to='rank') %>%
#     mutate(rank_statistic_label = c(
#       'rank_top_1'='Predicted best',
#       'min_rank_top_9'='Actual best\nof 9 predicted best',
#       'median_rank_top_9'='Actual median\nof 9 predicted best',
#       'max_rank_top_9'='Actual worst\nof 9 predicted best'
#     )[rank_statistic] %>% ordered())
#   beanplot_summary <- beanplot_data %>%
#     group_by(rank_statistic_label) %>%
#     summarize(
#       q25=quantile(rank, 0.25),
#       q50=quantile(rank, 0.50),
#       q75=quantile(rank, 0.75),
#       .groups='drop') %>%
#     pivot_longer(cols=starts_with('q'), names_to='quantile', names_prefix='q', values_to='rank') %>%
#     mutate(hline_x=as.numeric(rank_statistic_label))
#   beanplot <- ggplot(beanplot_data, aes(x=rank_statistic_label)) +
#     geom_violin(aes(y=rank, fill=rank_statistic), color=NA, trim=FALSE, scale='area') +
#     scale_fill_manual(
#       guide='none',
#       values=c(rank_top_1=pgmtl_colors[['central']], min_rank_top_9=pgmtl9_colors[['dark']],
#                median_rank_top_9=pgmtl9_colors[['central']], max_rank_top_9=pgmtl9_colors[['light']])) +
#     geom_errorbarh(
#       data=filter(beanplot_summary, quantile == '50'),
#       aes(xmin=hline_x-0.1, xmax=hline_x+0.1, y=rank),
#       size=0.8, height=0, color='gray80') +
#     geom_errorbarh(
#       data=filter(beanplot_summary, quantile != '50'),
#       aes(xmin=hline_x-0.03, xmax=hline_x+0.03, y=rank),
#       size=0.8, height=0, color='gray80') +
#     coord_cartesian(ylim=c(0,145)) + ylim(0, NA) +
#     xlab(sprintf('%s source model', model_type)) +
#     ylab(sprintf('Actual rank of %s source model by RMSE', model_type)) +
#     theme_bw()
#   beanplot
#   ggsave(sprintf('figures/metamodel_beanplot_%s.png', model_type), beanplot, width=7, height=4)
# }
# plot_beanplot(pgmtl_info$target_summary, model_type='PGDL')
# plot_beanplot(pbmtl_info$target_summary, model_type='PB')

plot_2model_boxplot <- function(...) {
  target_summaries <- list(...)
  merged_summary <- purrr::imap_dfr(target_summaries, function(.x, .y) {
    mutate(.x, Model=sprintf('%s', .y))
  }) %>% bind_rows()
  beanplot_data <- merged_summary %>%
    select(Model, rank_top_1, min_rank_top_9, max_rank_top_9, median_rank_top_9) %>%
    pivot_longer(cols=-Model, names_to='rank_statistic', values_to='rank') %>%
    mutate(rank_statistic_label = c(
      'rank_top_1'='Predicted best',
      'min_rank_top_9'='Actual best\nof 9 predicted best',
      'median_rank_top_9'='Actual median\nof 9 predicted best',
      'max_rank_top_9'='Actual worst\nof 9 predicted best'
    )[rank_statistic] %>% ordered(levels=c(
      'Predicted best',
      'Actual best\nof 9 predicted best',
      'Actual median\nof 9 predicted best',
      'Actual worst\nof 9 predicted best'
    )))
  beanplot_summary <- beanplot_data %>%
    group_by(Model, rank_statistic_label) %>%
    summarize(
      q25=quantile(rank, 0.25),
      q50=quantile(rank, 0.50),
      q75=quantile(rank, 0.75),
      .groups='drop') %>%
    pivot_longer(cols=starts_with('q'), names_to='quantile', names_prefix='q', values_to='rank') %>%
    mutate(hline_x=as.numeric(rank_statistic_label))
  beanplot <- ggplot(beanplot_data, aes(x=rank_statistic_label, fill=Model)) +
    geom_boxplot(aes(y=rank), color='black', alpha=0.2) +
    # scale_linetype_manual(
    #   'Source model',
    #   values=c(`PB`='dotted', `PGDL`='dashed')) +
    scale_fill_manual(
      'Source model',
      values=c(`PB`='grey70', `PGDL`='white')) +
    xlab(NULL) +
    ylab('Actual rank of source model by RMSE') +
    theme_bw()
  ggsave('figures/metamodel_2model_boxplot.png', beanplot, width=7, height=4)
  return(beanplot)
}
plot_2model_boxplot(PB=pbmtl_info$target_summary, PGDL=pgmtl_info$target_summary)
