# Harmonize boundaries for 63rd and 66th congressional districts
# July 10, 2025

# Import shapefiles
shp63_cd <- st_read("raw/shapefiles/districtShapes63/districts063.shp") 
shp65_cd <- st_read("raw/shapefiles/districtShapes65/districts065.shp")
shp66_cd <- st_read("raw/shapefiles/districtShapes66/districts066.shp")

# Create district ID to match Voteview data
shp63_cd$ID_STATEDIST <- paste0(shp63_cd$STATENAME, str_pad(shp63_cd$DISTRICT, width = 2, pad = 0))
shp65_cd$ID_STATEDIST <- paste0(shp65_cd$STATENAME, str_pad(shp65_cd$DISTRICT, width = 2, pad = 0))
shp66_cd$ID_STATEDIST <- paste0(shp66_cd$STATENAME, str_pad(shp66_cd$DISTRICT, width = 2, pad = 0))

# Which districts were redistricted?
# 63 to 66
shp66_cd$redist6366 <- as.numeric(shp66_cd$STARTCONG>63)
table(shp66_cd$STATENAME, shp66_cd$redist6366) # AL, CO, FL, GA, ID, MA, MI, MN, MI, MT, NY, OH, OK, SC, TX, UT, WA, WV
# 63 to 65
shp65_cd$redist6365 <- as.numeric(shp65_cd$STARTCONG>63)
table(shp65_cd$STATENAME, shp65_cd$redist6365) # AL, CO, FL, GA, MA, MI, MN, NY, OH, OK, SC, TX, UT, WA, WV
# 65 to 66
shp66_cd$redist6566 <- as.numeric(shp66_cd$STARTCONG>65)
table(shp66_cd$STATENAME, shp66_cd$redist6566) # ID, MI, MT, NY, TX

# Visually compare 63rd and 66th congressional districts
sort(unique(shp66_cd$ID_STATEDIST))
# 
# # Loop through 48 states ####
# states <- sort(unique(shp66_cd$STATENAME))
# for (i in states) {
# 
#   map63 <-
#     ggplot2::ggplot() +
#     geom_sf(
#       data = shp63_cd %>% filter(STATENAME == i),
#       aes(fill=DISTRICT),
#       color="black",
#       size=0.5
#     ) +
#     geom_sf_label(
#       data = shp63_cd %>% filter(STATENAME == i),
#       aes(label=DISTRICT),
#       size=3
#     ) +
#     scale_fill_viridis_d() +
#     guides(fill = "none") + # Remove legend
#     labs(caption = "63") +  # Add bottom-centered title
#     theme_void() +
#     theme(
#       plot.caption = element_text(
#         hjust = 0.5,    # Center horizontally
#         vjust = 1,      # Adjust vertical position (1 = bottom)
#         size = 16,      # Adjust size as desired
#         face = "bold"
#       )
#     )
#   map65 <-
#     ggplot2::ggplot() +
#     geom_sf(
#       data = shp65_cd %>% filter(STATENAME == i),
#       aes(fill=DISTRICT),
#       color="black",
#       size=0.5
#     ) +
#     geom_sf_label(
#       data = shp65_cd %>% filter(STATENAME == i),
#       aes(label=DISTRICT),
#       size=3
#     ) +
#     scale_fill_viridis_d() +
#     guides(fill = "none") + # Remove legend
#     labs(caption = "65") +  # Add bottom-centered title
#     theme_void() +
#     theme(
#       plot.caption = element_text(
#         hjust = 0.5,    # Center horizontally
#         vjust = 1,      # Adjust vertical position (1 = bottom)
#         size = 16,      # Adjust size as desired
#         face = "bold"
#       )
#     )
#   map66 <-
#     ggplot2::ggplot() +
#     geom_sf(
#       data = shp66_cd %>% filter(STATENAME == i),
#       aes(fill=DISTRICT),
#       color="black",
#       size=0.5
#     ) +
#     geom_sf_label(
#       data = shp66_cd %>% filter(STATENAME == i),
#       aes(label=DISTRICT),
#       size=3
#     ) +
#     scale_fill_viridis_d() +
#     guides(fill = "none") + # Remove legend
#     labs(caption = "66") +  # Add bottom-centered title
#     theme_void() +
#     theme(
#       plot.caption = element_text(
#         hjust = 0.5,    # Center horizontally
#         vjust = 1,      # Adjust vertical position (1 = bottom)
#         size = 16,      # Adjust size as desired
#         face = "bold"
#       )
#     )
#   combined <- cowplot::plot_grid(map63, map65, map66, nrow=1)
#   ggsave(paste0("figures/cd_maps/", i, ".pdf"), combined, width = 12, height = 6)
# }
# 
# # Special cases! ####
# map63 <-
#   ggplot2::ggplot() +
#   geom_sf(
#     # data = shp63_cd %>% filter(STATENAME == "Massachusetts" & as.numeric(DISTRICT) > 7 & as.numeric(DISTRICT) <13), # BOSTON
#     # data = shp63_cd %>% filter(STATENAME == "Michigan" & as.numeric(DISTRICT) > 12 & as.numeric(DISTRICT) %in% c(1, 2, 6, 7, 13)), # DETROIT
#     data = shp63_cd %>% filter(STATENAME == "New York" & as.numeric(DISTRICT) %in% 2:24), # NYC
#     aes(fill=DISTRICT),
#     color="black",
#     size=0.5
#   ) +
#   geom_sf_label(
#     # data = shp63_cd %>% filter(STATENAME == "Massachusetts" & as.numeric(DISTRICT) > 7 & as.numeric(DISTRICT) <13), # BOSTON
#     # data = shp63_cd %>% filter(STATENAME == "Michigan" & as.numeric(DISTRICT) > 12 & as.numeric(DISTRICT) %in% c(1, 2, 6, 7, 13)), # DETROIT
#     data = shp63_cd %>% filter(STATENAME == "New York" & as.numeric(DISTRICT) %in% 2:24), # NYC
#     aes(label=DISTRICT),
#     size=2
#   ) +
#   scale_fill_viridis_d() +
#   guides(fill = "none") + # Remove legend
#   labs(caption = "63") +  # Add bottom-centered title
#   theme_void() +
#   theme(
#     plot.caption = element_text(
#       hjust = 0.5,    # Center horizontally
#       vjust = 1,      # Adjust vertical position (1 = bottom)
#       size = 16,      # Adjust size as desired
#       face = "bold"
#     )
#   )
# map65 <-
#   ggplot2::ggplot() +
#   geom_sf(
#     # data = shp65_cd %>% filter(STATENAME == "Massachusetts" & as.numeric(DISTRICT) > 7 & as.numeric(DISTRICT) <13), # BOSTON
#     # data = shp65_cd %>% filter(STATENAME == "Michigan" & as.numeric(DISTRICT) > 12 & as.numeric(DISTRICT) %in% c(1, 2, 6, 7, 13)), # DETROIT
#     data = shp65_cd %>% filter(STATENAME == "New York" & as.numeric(DISTRICT) %in% 2:24), # NYC
#     aes(fill=DISTRICT),
#     color="black",
#     size=0.5
#   ) +
#   geom_sf_label(
#     # data = shp65_cd %>% filter(STATENAME == "Massachusetts" & as.numeric(DISTRICT) > 7 & as.numeric(DISTRICT) <13), # BOSTON
#     # data = shp65_cd %>% filter(STATENAME == "Michigan" & as.numeric(DISTRICT) > 12 & as.numeric(DISTRICT) %in% c(1, 2, 6, 7, 13)), # DETROIT
#     data = shp65_cd %>% filter(STATENAME == "New York" & as.numeric(DISTRICT) %in% 2:24), # NYC
#     aes(label=DISTRICT),
#     size=2
#   ) +
#   scale_fill_viridis_d() +
#   guides(fill = "none") + # Remove legend
#   labs(caption = "65") +  # Add bottom-centered title
#   theme_void() +
#   theme(
#     plot.caption = element_text(
#       hjust = 0.5,    # Center horizontally
#       vjust = 1,      # Adjust vertical position (1 = bottom)
#       size = 16,      # Adjust size as desired
#       face = "bold"
#     )
#   )
# map66 <-
#   ggplot2::ggplot() +
#   geom_sf(
#     # data = shp66_cd %>% filter(STATENAME == "Massachusetts" & as.numeric(DISTRICT) > 7 & as.numeric(DISTRICT) <13), # BOSTON
#     # data = shp66_cd %>% filter(STATENAME == "Michigan" & as.numeric(DISTRICT) > 12 & as.numeric(DISTRICT) %in% c(1, 2, 6, 7, 13)), # DETROIT
#     data = shp66_cd %>% filter(STATENAME == "New York" & as.numeric(DISTRICT) %in% 2:24), # NYC
#     aes(fill=DISTRICT),
#     color="black",
#     size=0.5
#   ) +
#   geom_sf_label(
#     # data = shp66_cd %>% filter(STATENAME == "Massachusetts" & as.numeric(DISTRICT) > 7 & as.numeric(DISTRICT) <13), # BOSTON
#     # data = shp66_cd %>% filter(STATENAME == "Michigan" & as.numeric(DISTRICT) %in% c(1, 2, 6, 7, 13)), # DETROIT
#     data = shp66_cd %>% filter(STATENAME == "New York" & as.numeric(DISTRICT) %in% 2:24), # NYC
#     aes(label=DISTRICT),
#     size=2
#   ) +
#   scale_fill_viridis_d() +
#   guides(fill = "none") + # Remove legend
#   labs(caption = "66") +  # Add bottom-centered title
#   theme_void() +
#   theme(
#     plot.caption = element_text(
#       hjust = 0.5,    # Center horizontally
#       vjust = 1,      # Adjust vertical position (1 = bottom)
#       size = 16,      # Adjust size as desired
#       face = "bold"
#     )
#   )
# combined <- cowplot::plot_grid(map63, map65, map66, nrow=1)
# # ggsave(paste0("figures/cd_maps/Boston.pdf"), combined, width = 12, height = 6) # BOSTON
# # ggsave(paste0("figures/cd_maps/Detroit.pdf"), combined, width = 12, height = 6) # DETROIT
# ggsave(paste0("figures/cd_maps/New York City.pdf"), combined, width = 12, height = 6) # NYC
# 
# 
# 

# Based on vision inspection, manually code the 66th district to which each 63rd district corresponds ####

# Drop 66th CDs if too severely redistricted and unlinkable to 63rd 
# AL, CO, FL, GA, ID, MA, MI, MN, MI, MT, NY, OH, OK, SC, TX, UT, WA, WV
unlinkable <- c(
  "Alabama06","Alabama07","Alabama09","Alabama10",
  "Colorado01","Colorado02","Colorado03","Colorado04",
  "Florida02", # FL 2-4
  # GA ok
  # ID 0-1, 0-2
  # MA ok
  "Michigan08", "Michigan10", "Michigan11", # MI 1-1, 1-13, 
  # MS ok
  "Minnesota04","Minnesota05","Minnesota06","Minnesota08","Minnesota10", 
  # MT 0-1, 0-2
  "New York04","New York06","New York07","New York08","New York09","New York10","New York15","New York16","New York17","New York18","New York21", 
  "Ohio06","Ohio07","Ohio08","Ohio09","Ohio10","Ohio11","Ohio13","Ohio14","Ohio16","Ohio17","Ohio18","Ohio19","Ohio20","Ohio21","Ohio22",
  "Oklahoma01","Oklahoma02","Oklahoma03","Oklahoma04","Oklahoma05","Oklahoma06","Oklahoma07","Oklahoma08",
  # SC ok
  "Texas06","Texas07","Texas08","Texas13","Texas14","Texas15","Texas16","Texas17","Texas18",
  # UT 0-1 and 0-2
  "Washington01", "Washington02", # WA 3-4 and 3-5
  "West Virginia03","West Virginia04","West Virginia05","West Virginia06"
  )

shp66_cd$linked_to63 <- as.numeric(!shp66_cd$ID_STATEDIST %in% unlinkable)

# Manually assign each 66th CD a 63rd CD
cong66$ID_STATEDISTin63 <- cong66$ID_STATEDIST
cong66$ID_STATEDISTin63[cong66$ID_STATEDIST=="Florida04"] <- "Florida02" # FL 2-4
cong66$ID_STATEDISTin63[cong66$ID_STATEDIST=="Idaho01"] <- "Idaho00b" # FRENCH, Burton Lee
cong66$ID_STATEDISTin63[cong66$ID_STATEDIST=="Idaho02"] <- "Idaho00a" # SMITH, Addison Taylor
cong66$ID_STATEDISTin63[cong66$ID_STATEDIST=="Montana01"] <- "Montana00b" # EVANS, John Morgan
cong66$ID_STATEDISTin63[cong66$ID_STATEDIST=="Montana02"] <- "Montana00a"
cong66$ID_STATEDISTin63[cong66$ID_STATEDIST=="Utah01"] <- "Utah00b"
cong66$ID_STATEDISTin63[cong66$ID_STATEDIST=="Utah02"] <- "Utah00a"
cong66$ID_STATEDISTin63[cong66$ID_STATEDIST=="Washington03"] <- "Washington02" # JOHNSON, Albert
cong66$ID_STATEDISTin63[cong66$ID_STATEDIST=="Washington04"] <- "Washington03" # WA 3-4 
cong66$ID_STATEDISTin63[cong66$ID_STATEDIST=="Washington05"] <- "Washington03" # WA 3-5
cong66$ID_STATEDISTin63[cong66$ID_STATEDIST=="Michigan13"] <- "Michigan01" # MI 1-13

cong66$manual_link_to63 <- ifelse(
  test = (cong66$ID_STATEDIST != cong66$ID_STATEDISTin63) | (cong66$ID_STATEDIST %in% unlinkable),
  yes = 1,
  no = 0
)

# Drop 66th CDs if too severely redistricted and unlinkable to 65th 
# ID, MI, MT, NY, TX
unlinkable <- c(
  # ID 0-1, 0-2
  # MI ok
  # MT 0-1, 0-2
  "New York04","New York06","New York07","New York08","New York09","New York10","New York15","New York16","New York17","New York18","New York21", 
  "Texas06","Texas07","Texas08","Texas13","Texas14","Texas15","Texas16","Texas17","Texas18"
)

shp66_cd$linked_to65 <- as.numeric(!shp66_cd$ID_STATEDIST %in% unlinkable)

# Manually assign each 66th CD a 63rd CD
cong66$ID_STATEDISTin65 <- cong66$ID_STATEDIST
cong66$ID_STATEDISTin65[cong66$ID_STATEDIST=="Idaho01"] <- "Idaho00b" # FRENCH, Burton Lee
cong66$ID_STATEDISTin65[cong66$ID_STATEDIST=="Idaho02"] <- "Idaho00a" # SMITH, Addison Taylor
cong66$ID_STATEDISTin65[cong66$ID_STATEDIST=="Montana01"] <- "Montana00b" # EVANS, John Morgan
cong66$ID_STATEDISTin65[cong66$ID_STATEDIST=="Montana02"] <- "Montana00a"

cong66$manual_link_to65 <- ifelse(
  test = (cong66$ID_STATEDIST != cong66$ID_STATEDISTin65) | (cong66$ID_STATEDIST %in% unlinkable),
  yes = 1,
  no = 0
)


