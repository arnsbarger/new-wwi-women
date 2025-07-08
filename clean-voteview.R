# Clean Voteview data (Main dependent variable)
# July 8, 2025

# Import raw files
suff63a <- read_excel("raw/voteview/20211006_0711_voteview_download.xls", sheet = 1)
suff63b <- read_excel("raw/voteview/20211006_0711_voteview_download.xls", sheet = 2)

suff66a <- read_excel("raw/voteview/20210408_0931_voteview_download.xls", sheet = 1)
suff66b <- read_excel("raw/voteview/20210408_0931_voteview_download.xls", sheet = 2)

proh63a <- read_excel("raw/voteview/20211008_0653_voteview_download.xls", sheet = 1)
proh63b <- read_excel("raw/voteview/20211008_0653_voteview_download.xls", sheet = 2)

proh65a <- read_excel("raw/voteview/20211008_0646_voteview_download.xls", sheet = 1)
proh65b <- read_excel("raw/voteview/20211008_0646_voteview_download.xls", sheet = 2)

# For each vote, combine sheets into one data frame
suff63 <- merge(suff63a, suff63b[,c(1,4,5,7)], by = "id", all = TRUE) 
suff66 <- merge(suff66a, suff66b[,c(1,4,5,7)], by = "id", all = TRUE)
proh63 <- merge(proh63a, proh63b[,c(1,4,5,7)], by = "id", all = TRUE)
proh65 <- merge(proh65a, proh65b[,c(1,4,5,7)], by = "id", all = TRUE)

rm(list = ls(pattern = ".*[ab]$"))

#### Clean up 63rd congress ####

# Combine multiple votes; define relevant variables
cong63 <- 
  merge( # combine suffrage and prohibition votes
    x = suff63, 
    y = proh63 %>% select(id, V1), 
    by = "id",
    all = TRUE
  ) %>%
  filter(district_code > 0) %>% # district 0 is Woodrow Wilson
  rename(V238 = V1.x, V228 = V1.y) %>% # keep votes straight
  mutate(
    atlarge = ifelse(test = district_code %in% 98:99, yes = 1, no = 0), # at-large districts == 98 or 99
    district_code = ifelse(test = district_code %in% 98:99, yes = 0, no = district_code), # set at-large district codes==0 for future merge with county/congressional district characteristics data 
    district_code = ifelse(test = state_abbrev %in% c("AZ", "DE", "NM", "NV", "WY"), yes = 0, no = district_code), # set state-sized districts to code==0 for future merge with county/congressional district characteristics data
    congress = 63
  )

# CO, ID, IL, MT, OK, PA, TX, UT, WA have multiple at-large districts
table(cong63$state_abbrev, cong63$district_code)

# Define new id (ID_STATEDIST) for each district (needed to link across congresses accurately, especially at-large districts)
usps <- read.csv("raw/geo/usps_state_abrv_crosswalk.csv")
cong63 <- merge(y = usps, x = cong63, by.x = "state_abbrev", by.y = "Abbreviation", all.x = TRUE)

cong63$ID_STATEDIST <- paste0(cong63$State, str_pad(cong63$district_code, width = 2, pad = 0))

# Who do I need to manually edit?
cong63 %>% filter(state_abbrev %in% c("CO", "ID", "IL", "MT", "OK", "PA", "TX", "UT", "WA") & district_code==0) %>% dplyr::select(ID_STATEDIST, id, name)

cong63$ID_STATEDIST[cong63$id=="MH06305115"] <- "Colorado00a" #  KEATING, Edward
cong63$ID_STATEDIST[cong63$id=="MH06309205"] <- "Colorado00b" # TAYLOR, Edward Thomas
cong63$ID_STATEDIST[cong63$id=="MH06308598"] <- "Idaho00a" # SMITH, Addison Taylor
cong63$ID_STATEDIST[cong63$id=="MH06303367"] <- "Idaho00b" # FRENCH, Burton Lee
cong63$ID_STATEDIST[cong63$id=="MH06309024"] <- "Illinois00a" # STRINGER, Lawrence Beaumont
cong63$ID_STATEDIST[cong63$id=="MH06310190"] <- "Illinois00b" # WILLIAMS, William Elza
cong63$ID_STATEDIST[cong63$id=="MH06309003"] <- "Montana00a" # STOUT, Tom
cong63$ID_STATEDIST[cong63$id=="MH06303013"] <- "Montana00b" #  EVANS, John Morgan
cong63$ID_STATEDIST[cong63$id=="MH06309332"] <- "Oklahoma00a" # THOMPSON, Joseph Bryan
cong63$ID_STATEDIST[cong63$id=="MH06306829"] <- "Oklahoma00b" # MURRAY, William Henry David
cong63$ID_STATEDIST[cong63$id=="MH06309893"] <- "Oklahoma00c" # WEAVER, Claude
cong63$ID_STATEDIST[cong63$id=="MH06308116"] <- "Pennsylvania00a" # RUPLEY, Arthur Ringwalt
cong63$ID_STATEDIST[cong63$id=="MH06309792"] <- "Pennsylvania00b" # WALTERS, Anderson Howell
cong63$ID_STATEDIST[cong63$id=="MH06305640"] <- "Pennsylvania00c" # LEWIS, Fred Ewing
cong63$ID_STATEDIST[cong63$id=="MH06306688"] <- "Pennsylvania00d" # MORIN, John Mary
cong63$ID_STATEDIST[cong63$id=="MH06303489"] <- "Texas00a" #  GARRETT, Daniel Edward
cong63$ID_STATEDIST[cong63$id=="MH06309087"] <- "Texas00b" # SUMNERS, Hatton William
cong63$ID_STATEDIST[cong63$id=="MH06304963"] <- "Utah00a" # JOHNSON, Jacob
cong63$ID_STATEDIST[cong63$id=="MH06304652"] <- "Utah00b" # HOWELL, Joseph
cong63$ID_STATEDIST[cong63$id=="MH06303052"] <- "Washington00a" # FALCONER, Jacob Alexander
cong63$ID_STATEDIST[cong63$id=="MH06301172"] <- "Washington00b" # BRYAN, James Wesley


#### Clean up 66th Congress ####










































