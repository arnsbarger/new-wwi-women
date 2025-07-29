# Clean the county-to-district crosswalks
# July 16, 2025

# 66th Congressional districts-to-county crosswalks for counties formed by 1910 
cw_cnty_cd_66 <- 
  read_dta("raw/cd-cnty-crosswalks/150101-V3/crosswalks/CountyToCD-DOA/DOA_stata/Crosswalk_1910_66.dta") %>%
  dplyr::select(
    icpsrst,
    icpsrcty,
    district,
    id,
    starts_with("m"),
    gisjoin_1910 # Unique identifier for 1910 counties
  )  %>%
  rename(
    icpsrst1910 = icpsrst,
    icpsrcty1910 = icpsrcty
  )

# All counties present for 1919 vote NOT accounted for (important for matching control data to correct CD):
# 68th Congressional districts-to-county crosswalks for counties formed by 1920.
cw_cnty_cd_68 <- 
  read_dta("raw/cd-cnty-crosswalks/150101-V3/crosswalks/CountyToCD-DOA/DOA_stata/Crosswalk_1920_68.dta") %>%
  dplyr::select(
    icpsrst,
    icpsrcty,
    gisjoin_1920 # Unique identifier for 1920 counties
  ) %>%
  distinct() %>%
  rename(
    icpsrst1920 = icpsrst,
    icpsrcty1920 = icpsrcty
  )

# Crosswalk between 1910 and 1910 counties
cw_cnty10_cnty20 <- 
  read_dta("raw/cd-cnty-crosswalks/150101-V3/crosswalks/CountyToCounty/1910/1910_stata/Crosswalk_1920_1910.dta") %>%
  dplyr::select(
    # starts_with("m"), # Commented out because I don't care about distributing population into the correct county; just putting these new counties into the correct 66th congressional district, so don't need these weights.
    gisjoin_1910, # Unique identifier for 1910 counties
    gisjoin_1920 # Unique identifier for 1920 counties
  ) %>%
  distinct()

# Add 1920 ICPSR state/county codes with 1910-20 county crosswalk
a <- merge(x = cw_cnty10_cnty20, y = cw_cnty_cd_68, by = "gisjoin_1920", all=TRUE) # same length as county-county crosswalk 

# There are counties missing from cw_cnty_cd_66, created after 1910. I want to put these new counties into the correct 66th congressional district...
cw_cnty_cd_66 <- 
  merge(
    x = a, # 1910-20 county crosswalk
    y = cw_cnty_cd_66, # 66th congressional district to 1910 county crosswalk
    by = "gisjoin_1910", 
    all=TRUE
    ) %>%
  filter(
    !is.na(district) # Remove empty rows with no district info
  ) %>% 
  distinct(icpsrst1920, icpsrcty1920, icpsrst1910, icpsrcty1910, district, .keep_all = TRUE) %>% # merging on gisjoin_1910 alone creates many duplicates
  mutate(
    cnty_change_191020 = ifelse(icpsrcty1920==icpsrcty1910 & icpsrst1920==icpsrst1910, 1, 0) # add flag for whether county boundaries changed; can control for this to account for bias introduced by geographic changes
  )

rm(a, cw_cnty10_cnty20, cw_cnty_cd_68)

# Some counties that existed during the Civil War but no longer exist by 1910. If Civil War-related variables start looking important during the analysis, might have to crosswalk the 1860 counties to 1910 too to improve that merge...
