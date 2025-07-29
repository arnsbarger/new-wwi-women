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

usps <- read.csv("raw/geo/usps_state_abrv_crosswalk.csv")

# For each vote, combine sheets into one data frame
suff63 <- merge(suff63a, suff63b[,c(1,4,5,7)], by = "id", all = TRUE) 
suff66 <- merge(suff66a, suff66b[,c(1,4,5,7)], by = "id", all = TRUE)
proh63 <- merge(proh63a, proh63b[,c(1,4,5,7)], by = "id", all = TRUE)
proh65 <- merge(proh65a, proh65b[,c(1,4,5,7)], by = "id", all = TRUE)

rm(list = ls(pattern = ".*[ab]$"))

#### Clean up 63rd congress ####

# Combine multiple votes; define relevant variables
table(suff63$state_abbrev)

cong63 <- 
  merge( # combine suffrage and prohibition votes
    x = suff63, 
    y = proh63 %>% dplyr::select(id, V1), 
    by = "id",
    all = TRUE
  ) %>%
  filter(district_code > 0) %>% # district 0 is the president (no associated district)
  rename( # relabel vote columns
    V238 = V1.x, 
    V228 = V1.y
    ) %>% 
  mutate( # define new vars
    atlarge = ifelse(test = district_code %in% 98:99, yes = 1, no = 0), # at-large districts == 98 or 99
    district_code = ifelse(test = district_code %in% 98:99, yes = 0, no = district_code), # set at-large district codes==0 for future merge with county/congressional district characteristics data 
    district_code = ifelse(test = state_abbrev %in% c("AZ", "DE", "NM", "NV", "WY"), yes = 0, no = district_code), # set state-sized districts to code==0 for future merge with county/congressional district characteristics data
    congress = 63,
    icpsr = as.numeric(icpsr),
    yeaV238 = case_when(
      V238 %in% c(1, 2) ~ 1, # "Yea"
      V238 %in% c(5, 6) ~ 0, # "Nay"
      V238 == 9 ~ 0, # "Absent"
      TRUE ~ NA_real_
    ),
    yeaV228 = case_when(
      V228 %in% c(1, 2) ~ 1, # "Yea"
      V228 %in% c(5, 6) ~ 0, # "Nay"
      V228 == 9 ~ 0, # "Absent"
      TRUE ~ NA_real_
    )
    )

rm(suff63, proh63)

# CO, ID, IL, MT, OK, PA, TX, UT, WA have multiple at-large districts
table(cong63$state_abbrev, cong63$district_code)

# Define new id (ID_STATEDIST) for each district (needed to link across congresses accurately, especially at-large districts)
cong63 <- merge(y = usps, x = cong63, by.x = "state_abbrev", by.y = "Abbreviation", all.x = TRUE)

cong63$ID_STATEDIST <- paste0(cong63$State, str_pad(cong63$district_code, width = 2, pad = 0))

# Manually edit states with multiple at-large districts
cong63 %>% filter(district_code==0) %>% dplyr::select(ID_STATEDIST, id, name)

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

#### Clean up 65th Congress ####
cong65 <- proh65 %>%
  filter(district_code > 0) %>% # district 0 is the president (no associated district)
  rename(V061 = V1) %>%
  mutate(# define new vars
    atlarge = ifelse(test = district_code %in% 98:99, yes = 1, no = 0), # at-large districts == 98 or 99
    district_code = ifelse(test = district_code %in% 98:99, yes = 0, no = district_code), # set at-large district codes==0 for future merge with county/congressional district characteristics data 
    district_code = ifelse(test = state_abbrev %in% c("AZ", "DE", "NM", "NV", "WY"), yes = 0, no = district_code), # set state-sized districts to code==0 for future merge with county/congressional district characteristics data
    congress = 65,
    icpsr = as.numeric(icpsr),
    yeaV061 = case_when(
      V061 %in% c(1, 2) ~ 1, # "Yea"
      V061 %in% c(5, 6) ~ 0, # "Nay"
      V061 == 9 ~ 0, # "Absent"
      TRUE ~ NA_real_
    )
  )

rm(proh65)

# Define new id (ID_STATEDIST) for each district (needed to link across congresses accurately, especially at-large districts)
cong65 <- merge(y = usps, x = cong65, by.x = "state_abbrev", by.y = "Abbreviation", all.x = TRUE)

cong65$ID_STATEDIST <- paste0(cong65$State, str_pad(cong65$district_code, width = 2, pad = 0))

# Manually edit states with multiple at-large districts
cong65 %>% filter(district_code==0) %>% dplyr::select(ID_STATEDIST, id, name)

cong65$ID_STATEDIST[cong65$id=="MH06508598"] <- "Idaho00a" # SMITH, Addison Taylor
cong65$ID_STATEDIST[cong65$id=="MH06503367"] <- "Idaho00b" # FRENCH, Burton Lee
cong65$ID_STATEDIST[cong65$id=="MH06506065"] <- "Illinois00a" # MASON, William Ernest
cong65$ID_STATEDIST[cong65$id=="MH06506181"] <- "Illinois00b" # McCORMICK, Joseph Medill
cong65$ID_STATEDIST[cong65$id=="MH06507730"] <- "Montana00a" # RANKIN, Jeannette
cong65$ID_STATEDIST[cong65$id=="MH06503013"] <- "Montana00b" #  EVANS, John Morgan
cong65$ID_STATEDIST[cong65$id=="MH06309332"] <- "Oklahoma00a" # THOMPSON, Joseph Bryan
cong65$ID_STATEDIST[cong65$id=="MH06506328"] <- "Pennsylvania00a" # McLAUGHLIN, Joseph
cong65$ID_STATEDIST[cong65$id=="MH06508307"] <- "Pennsylvania00b" # SCOTT, John Roger Kirkpatrick
cong65$ID_STATEDIST[cong65$id=="MH06502130"] <- "Pennsylvania00c" # CRAGO, Thomas Spencer
cong65$ID_STATEDIST[cong65$id=="MH06503478"] <- "Pennsylvania00d" # GARLAND, Mahlon Morris
cong65$ID_STATEDIST[cong65$id=="MH06503489"] <- "Texas00a" #  GARRETT, Daniel Edward
cong65$ID_STATEDIST[cong65$id=="MH06506342"] <- "Texas00b" # McLEMORE, Atkins Jefferson

# Missing x congressional districts
print(435 - nrow(cong65)) # 2

cong65 <- rbind( # Add missing rows from original source documents (Congressional Record vol. 58-1, p. 93; House Journal vol. 66-1, p. 42;), which only reports 428 districts. Others filled from Wikipedia.
  cong65,
  c("IL", NA, 6016, "MARTIN, Charles", 9, 4, "(IL-04)", 100, 0, 65, 0, "Illinois", "Illinois04"), # 1 - Charles Martin (D) died
  c("MO", NA, 1769, "CLARK, James Beauchamp", 9, 9, "(MO-09)", 100, 0, 65, 0, "Missouri", "Missouri09") # 2 -  James Beauchamp Clark SPEAKER
)

print(435 - nrow(cong65)) # All 435 districts accounted for!

#### Clean up 66th Congress ####
table(suff66$state_abbrev)

cong66 <- suff66 %>%
  filter( 
    district_code > 0 # district 0 is the president (no associated district)
    & ! id %in% c(
      "MH06606520", # Delete Milligan - doesn’t start until Feb 1920, predecessor already in the data (district double-counted)
      "MH06607352" # Delete Perlman - doesn’t start until Nov 1920, predecessor already in the data (district double-counted)
      )
  ) %>% 
  rename( # relabel vote columns
    V002 = V1
  ) %>% 
  mutate( # define new vars
    atlarge = ifelse(test = district_code %in% 98:99, yes = 1, no = 0), # at-large districts == 98 or 99
    district_code = ifelse(test = district_code %in% 98:99, yes = 0, no = district_code), # set at-large district codes==0 for future merge with county/congressional district characteristics data 
    district_code = ifelse(test = state_abbrev %in% c("AZ", "DE", "NM", "NV", "WY"), yes = 0, no = district_code), # set state-sized districts to code==0 for future merge with county/congressional district characteristics data
    congress = 66,
    icpsr = as.numeric(icpsr),
    yeaV002 = case_when(
      V002 %in% c(1, 2) ~ 1, # "Yea"
      V002 %in% c(5, 6) ~ 0, # "Nay"
      V002 == 9 ~ 0, # "Absent"
      TRUE ~ NA_real_
    )
  )

rm(suff66)

# Missing x congressional districts
print(435 - nrow(cong66)) # 14

cong66 <- rbind( # Add missing rows from original source documents (Congressional Record vol. 58-1, p. 93; House Journal vol. 66-1, p. 42;), which only reports 428 districts. Others filled from Wikipedia.
  cong66,
  c(NA, 4685, "HUDDLESTON, George", "AL", 9, 9, "(AL-09)", 100, 0, 66, 0), # 1 - George Huddleston of AL 9 NOT VOTING
  c(NA, 5090, "KAHN, Julius", "CA", 9, 4, "(CA-0)", 200, 0, 66, 0), # 2 - Julius Kahn of CA 4 NOT VOTING
  c(NA, 3599, "GILLETT, Frederick", "MA", 9, 2, "(MA-02)", 200, 0, 66, 0), # 3 - Frederick H. Gillett of MA 2 SPEAKER
  c(NA, 4887, "JAMES, W. Frank", "MI", 9, 12, "(MI-12)", 200, 0, 66, 0), # 4 - W. Frank James of MI 12 NOT VOTING
  c(NA, 4734, "HUMPHREYS II, Benjamin G.", "MS", 9, 3, "(MS-03)", 100, 0, 66, 0), # 5 - Benjamin G. Humphreys II of MS 3 NOT VOTING
  c(NA, 3980, "HAMILL, James A.", "NJ", 9, 12, "(NJ-12)", 100, 0, 66, 0), # 6 - James A. Hamill of NJ 12 NOT VOTING
  c(NA, 9332, "THOMPSON, Joseph Byran", "OK", 9, 5, "(OK-05)", 100, 0, 66, 0), # 7 - Joseph Bryan Thompson of OK 5 NOT VOTING
  c(NA, 5154, "KELLY, M. Clyde", "PA", 9, 30, "(PA-30)", 200, 0, 66, 0), # 8 - M. Clyde Kelly of PA 30 NOT VOTING
  c(NA, 1291, "BURNETT, L. John", "AL", 9, 7, "(AL-07)", 100, 0, 66, 0), # 9 - John Burnett of AL 7 died 2 days before vote
  c(NA, 2996, "ESTOPINAL, Albert", "LA", 9, 1, "(LA-01)", 100, 0, 66, 0), # 10 - Albert Estopinal of LA 1 died month before the vote
  c(NA, 9610, "VAN DYKE, Carl", "MN", 9, 4, "(MN-04)", 100, 0, 66, 0), # 11 - vacated seat the day before the vote.
  c(NA, 6649, "MOORE, R. Walton", "VA", 9, 8, "(VA-08)", 100, 0, 66, 0), # 12 - filled vacant seat month prior; no record of vote.
  c(NA, 4291, "HELM, Harvey", "KY", 9, 8, "(KY-08)", 100, 0, 66, 0), # 13 - died during previous congress.
  c(NA, 665, "BERGER, L. Victor", "WI", 9, 5, "(WI-05)", 380, 0, 66, 0) # 14 - ousted due to conviction as a socialist 
)

print(435 - nrow(cong66)) # All 435 districts accounted for!

# Correct Voteview data entry error
cong66$V002[cong66$id=="MH06607684"] <- 6 # RADCLIFFE, Amos Henry voted NAY (data error)

# Define new id (ID_STATEDIST) for each district (needed to link across congresses accurately, especially at-large districts)
cong66 <- merge(y = usps, x = cong66, by.x = "state_abbrev", by.y = "Abbreviation", all.x = TRUE)

cong66$ID_STATEDIST <- paste0(cong66$State, str_pad(cong66$district_code, width = 2, pad = 0))

# Manually edit states with multiple at-large districts
cong66 %>% filter(district_code==0) %>% dplyr::select(ID_STATEDIST, id, name)

cong66$ID_STATEDIST[cong66$id=="MH06610420"] <- "Illinois00a" # YATES, Richard
cong66$ID_STATEDIST[cong66$id=="MH06606065"] <- "Illinois00b" # MASON, William Ernest
cong66$ID_STATEDIST[cong66$id=="MH06602130"] <- "Pennsylvania00a" # CRAGO, Thomas Spencer
cong66$ID_STATEDIST[cong66$id=="MH06609792"] <- "Pennsylvania00b" # WALTERS, Anderson Howell <-- only at-large rep consistent across 63/66 congresses
cong66$ID_STATEDIST[cong66$id=="MH06603478"] <- "Pennsylvania00c" # GARLAND, Mahlon Morris
cong66$ID_STATEDIST[cong66$id=="MH06601274"] <- "Pennsylvania00d" # BURKE, William Joseph

rm(usps)
