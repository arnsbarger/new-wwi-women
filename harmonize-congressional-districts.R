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

# Loop through 48 states ####
states <- sort(unique(shp66_cd$STATENAME))
for (i in states) {
  
  map63 <- 
    ggplot2::ggplot() + 
    geom_sf(
      data = shp63_cd %>% filter(STATENAME == i), 
      aes(fill=DISTRICT), 
      color="black", 
      size=0.5
    ) +
    geom_sf_label(
      data = shp63_cd %>% filter(STATENAME == i), 
      aes(label=DISTRICT),
      size=3
    ) +
    scale_fill_viridis_d() +
    guides(fill = "none") + # Remove legend
    labs(caption = "63") +  # Add bottom-centered title
    theme_void() +
    theme(
      plot.caption = element_text(
        hjust = 0.5,    # Center horizontally
        vjust = 1,      # Adjust vertical position (1 = bottom)
        size = 16,      # Adjust size as desired
        face = "bold"
      )
    )
  map66 <- 
    ggplot2::ggplot() + 
    geom_sf(
      data = shp66_cd %>% filter(STATENAME == i), 
      aes(fill=DISTRICT), 
      color="black", 
      size=0.5
    ) +
    geom_sf_label(
      data = shp66_cd %>% filter(STATENAME == i), 
      aes(label=DISTRICT),
      size=3
    ) +
    scale_fill_viridis_d() +
    guides(fill = "none") + # Remove legend
    labs(caption = "66") +  # Add bottom-centered title
    theme_void() +
    theme(
      plot.caption = element_text(
        hjust = 0.5,    # Center horizontally
        vjust = 1,      # Adjust vertical position (1 = bottom)
        size = 16,      # Adjust size as desired
        face = "bold"
      )
    )
  combined <- cowplot::plot_grid(map63, map66, nrow=1)
  ggsave(paste0("figures/cd_maps/", i, ".pdf"), combined, width = 12, height = 6)
}

# Special cases! ####
map63 <- 
  ggplot2::ggplot() + 
  geom_sf(
    # data = shp63_cd %>% filter(STATENAME == "Massachusetts" & as.numeric(DISTRICT) > 7 & as.numeric(DISTRICT) <13), # BOSTON
    # data = shp63_cd %>% filter(STATENAME == "Michigan" & as.numeric(DISTRICT) > 12 & as.numeric(DISTRICT) %in% c(1, 2, 6, 7, 13)), # DETROIT
    data = shp63_cd %>% filter(STATENAME == "New York" & as.numeric(DISTRICT) %in% 2:24), # NYC
    aes(fill=DISTRICT), 
    color="black", 
    size=0.5
  ) +
  geom_sf_label(
    # data = shp63_cd %>% filter(STATENAME == "Massachusetts" & as.numeric(DISTRICT) > 7 & as.numeric(DISTRICT) <13), # BOSTON
    # data = shp63_cd %>% filter(STATENAME == "Michigan" & as.numeric(DISTRICT) > 12 & as.numeric(DISTRICT) %in% c(1, 2, 6, 7, 13)), # DETROIT
    data = shp63_cd %>% filter(STATENAME == "New York" & as.numeric(DISTRICT) %in% 2:24), # NYC
    aes(label=DISTRICT),
    size=2
  ) +
  scale_fill_viridis_d() +
  guides(fill = "none") + # Remove legend
  labs(caption = "63") +  # Add bottom-centered title
  theme_void() +
  theme(
    plot.caption = element_text(
      hjust = 0.5,    # Center horizontally
      vjust = 1,      # Adjust vertical position (1 = bottom)
      size = 16,      # Adjust size as desired
      face = "bold"
    )
  )
map66 <- 
  ggplot2::ggplot() + 
  geom_sf(
    # data = shp66_cd %>% filter(STATENAME == "Massachusetts" & as.numeric(DISTRICT) > 7 & as.numeric(DISTRICT) <13), # BOSTON
    # data = shp66_cd %>% filter(STATENAME == "Michigan" & as.numeric(DISTRICT) > 12 & as.numeric(DISTRICT) %in% c(1, 2, 6, 7, 13)), # DETROIT
    data = shp66_cd %>% filter(STATENAME == "New York" & as.numeric(DISTRICT) %in% 2:24), # NYC
    aes(fill=DISTRICT), 
    color="black", 
    size=0.5
  ) +
  geom_sf_label(
    # data = shp66_cd %>% filter(STATENAME == "Massachusetts" & as.numeric(DISTRICT) > 7 & as.numeric(DISTRICT) <13), # BOSTON
    # data = shp66_cd %>% filter(STATENAME == "Michigan" & as.numeric(DISTRICT) %in% c(1, 2, 6, 7, 13)), # DETROIT
    data = shp66_cd %>% filter(STATENAME == "New York" & as.numeric(DISTRICT) %in% 2:24), # NYC
    aes(label=DISTRICT),
    size=2
  ) +
  scale_fill_viridis_d() +
  guides(fill = "none") + # Remove legend
  labs(caption = "66") +  # Add bottom-centered title
  theme_void() +
  theme(
    plot.caption = element_text(
      hjust = 0.5,    # Center horizontally
      vjust = 1,      # Adjust vertical position (1 = bottom)
      size = 16,      # Adjust size as desired
      face = "bold"
    )
  )
combined <- cowplot::plot_grid(map63, map66, nrow=1)
# ggsave(paste0("figures/cd_maps/Boston.pdf"), combined, width = 12, height = 6) # BOSTON
# ggsave(paste0("figures/cd_maps/Detroit.pdf"), combined, width = 12, height = 6) # DETROIT
ggsave(paste0("figures/cd_maps/New York City.pdf"), combined, width = 12, height = 6) # NYC


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

# WHY AM I NOT DOING THE OPPOSITE??? Why not edit cong66 and use ID_STATEDISTin63, so later i can add ID_STATEDISTin65 ? Also I'm using the 66th id's above.
# keep "linked_to63" above, and then do "manual_link_to63" below.

# Manually assign each 63rd CD a 66th CD
cong63$ID_STATEDISTin66 <- cong63$ID_STATEDIST
cong63$ID_STATEDISTin66[cong63$ID_STATEDIST=="Florida02"] <- "Florida04" # FL 2-4
cong63$ID_STATEDISTin66[cong63$ID_STATEDIST=="Idaho00b"] <- "Idaho01" # FRENCH, Burton Lee
cong63$ID_STATEDISTin66[cong63$ID_STATEDIST=="Idaho00a"] <- "Idaho02" # SMITH, Addison Taylor
cong63$ID_STATEDISTin66[cong63$ID_STATEDIST=="Montana00b"] <- "Montana01" # EVANS, John Morgan
cong63$ID_STATEDISTin66[cong63$ID_STATEDIST=="Montana00a"] <- "Montana02"
cong63$ID_STATEDISTin66[cong63$ID_STATEDIST=="Utah00b"] <- "Utah01"
cong63$ID_STATEDISTin66[cong63$ID_STATEDIST=="Utah00a"] <- "Utah02"
cong63$ID_STATEDISTin66[cong63$ID_STATEDIST=="Washington02"] <- "Washington03" # JOHNSON, Albert
cong63$ID_STATEDISTin66[cong63$ID_STATEDIST=="Washington03"] <- "Washington04" # WA 3-4 
# add rows for CDs that were split in two for 66th congress
temp <- cong63[cong63$ID_STATEDIST %in% c("Washington03","Michigan01"),]
temp$ID_STATEDISTin66[temp$ID_STATEDIST=="Washington03"] <- "Washington05" # WA 3-5
temp$ID_STATEDISTin66[temp$ID_STATEDIST=="Michigan01"] <- "Michigan13"
cong63 <- rbind(cong63, temp)

cong63$manual_link_to66 <- ifelse(
  test = (cong63$ID_STATEDIST != cong63$ID_STATEDISTin66),
  yes = 1,
  no = 0
)
