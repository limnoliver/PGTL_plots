#individual lake fig

head(all_mods)

head(mod_long)

pb0 <- mod_long %>%
  filter(model %in% 'PB0') %>%
  rename(PB0_RMSE = RMSE)

pgmtl9 <- mod_long %>%
  filter(model %in% 'PG-MTL9') %>%
  rename(PGMTL9_RMSE = RMSE)

mod_long <- left_join(mod_long, select(pb0, target_id, PB0_RMSE)) %>%
  left_join(select(pgmtl9, target_id, PGMTL9_RMSE)) %>%
  filter(!is.na(target_id))

ggplot(mod_long, aes(x = model, y = RMSE)) +
  geom_point(aes(color = PB0_RMSE), size = 2, alpha = 0.5) +
  geom_line(aes(group = target_id, color = PB0_RMSE), alpha = 0.3) +
  scale_color_viridis_c()

order <- mod_long %>% 
  arrange(-PB0_RMSE) %>% select(target_id) %>% distinct() %>% pull(target_id)

mod_long$target_id <- factor(mod_long$target_id, levels = order)

p <- ggplot(mod_long, aes(x = model, y = target_id)) +
  geom_tile(aes(fill = RMSE)) +
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  scale_x_discrete(expand = c(0,0)) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), 
        panel.grid = element_blank()) +
  labs(y = 'Target lakes, ordered by PB0', x = '')

ggsave('figures/model_tile_plot.png', p, height = 6, width = 3.5)

order <- mod_long %>% 
  arrange(-PGMTL9_RMSE) %>% select(target_id) %>% distinct() %>% pull(target_id)

mod_long$target_id <- factor(mod_long$target_id, levels = order)

p <- ggplot(mod_long, aes(x = model, y = target_id)) +
  geom_tile(aes(fill = RMSE)) +
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  scale_x_discrete(expand = c(0,0)) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), 
        panel.grid = element_blank()) +
  labs(y = 'Target lakes, ordered by PG-MTL9', x = '')

ggsave('figures/model_tile_plot_pgmtl9_order.png', p, height = 6, width = 3.5)

# find spearman rank correlation betwee vars
wide_rmse <- select(mod_long, target_id, model, RMSE) %>%
  tidyr::spread(model, RMSE)

pb0_pbmtl <- cor.test(wide_rmse$PB0, wide_rmse$`PB-MTL`, method = 'spearman')
pb0_pgmtl <- cor.test(wide_rmse$PB0, wide_rmse$`PG-MTL`, method = 'spearman')
pbmtl_pgmtl <- cor.test(wide_rmse$`PB-MTL`, wide_rmse$`PG-MTL`, method = 'spearman')
pbmtl_pgmtl9 <- cor.test(wide_rmse$`PB-MTL`, wide_rmse$`PG-MTL9`, method = 'spearman')
pgmtl_pgmtl9 <- cor.test(wide_rmse$`PG-MTL`, wide_rmse$`PG-MTL9`, method = 'spearman')

