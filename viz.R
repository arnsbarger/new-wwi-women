
viz_data <- merge(x = shp66_cd, y = data, by = "ID_STATEDIST", all.x = TRUE)

ggplot(data = viz_data, aes(fill=raw_change_flfp_16plus_Z)) +
  geom_sf() +
  scale_fill_viridis_c() +
  theme_nothing() +
  theme(legend.position = "bottom")
ggsave("figures/map_raw_change_flfp_16plus_Z.pdf", width = 12, height = 6)

ggplot(data = viz_data, aes(fill=raw_change_mlfp_16plus_Z)) +
  geom_sf() +
  scale_fill_viridis_c() +
  theme_nothing() +
  theme(legend.position = "bottom")
ggsave("figures/map_raw_change_mlfp_16plus_Z.pdf", width = 12, height = 6)

ggplot(data = viz_data, aes(fill=raw_change_flfp_npwai)) +
  geom_sf(color = "black") +
  scale_fill_viridis_c() +
  theme_nothing() +
  theme(legend.position = "bottom")
ggsave("figures/map_raw_change_flfp_npwai.pdf", width = 12, height = 6)

ggplot(data = viz_data, aes(fill=raw_change_flfp_npwai_Z)) +
  geom_sf(color = "black") +
  scale_fill_viridis_c() +
  theme_nothing() +
  theme(legend.position = "bottom")
ggsave("figures/map_raw_change_flfp_npwai_Z.pdf", width = 12, height = 6)
