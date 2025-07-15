# Clean county-level data
# July 14, 2025
# icpsrst
# icpsrcty

# Geo crosswalk ####
cw <- 
  read_dta("~/Dropbox/Pitt/Projects/Suffrage WWI/data/raw/cd-cnty-crosswalks/DOA_stata/Crosswalk_1910_63.dta") %>%
  dplyr::select( # Import geographic crosswalk variables only
    # nhgisnam,
    # statenam,
    cd_statefip,
    icpsrst,
    icpsrcty
  ) %>%
  distinct() # Keep only non-duplicate rows

# WWI Casualties ####
cnty_casualties <- 
  read_dta("raw/wwi/WWI casualties Army Navy clean - county.dta") %>% # Import
  filter(
    fips > 0 & # Remove national-level row
    as.integer(fips) == fips # Remove two fips codes with decimal points 
  ) %>%
  mutate(
    fips = str_pad(string = fips, width = 5, pad = 0), # format to split below
    state_fips = as.numeric(substr(x = fips, start = 1, stop = 2)), # isolate state fips
    county_icpsr = as.numeric(str_pad(string = str_sub(fips, start= -3), width = 4, pad = 0, side = "right")) # isolate county icpsr code
  ) 

cnty_casualties <- # Switch state fips code for state icpsr code
  merge(
    x = cnty_casualties,
    y = cw, 
    by.x = c("state_fips", "county_icpsr"), 
    by.y = c("cd_statefip", "icpsrcty"), 
    all.x = TRUE
    ) %>%
  dplyr::select(
    starts_with("dead"),
    icpsrst,
    county_icpsr
    ) %>%
  rename_with(~ paste0("wwi_", .x)) %>%
  rename(
    icpsrst = wwi_icpsrst,
    icpsrcty = wwi_county_icpsr
  ) %>%
  filter(
    !is.na(icpsrst)
  )

# WWI Drafts ####
cnty_draft <- 
  read_dta("raw/wwi/004_Inductions.dta") %>% # Import
  dplyr::select(
    state, 
    county, 
    starts_with("tot_")
    ) %>%
  rename_with(~ paste0("wwi_", .x)) %>%
  rename(
    icpsrst = wwi_state,
    icpsrcty = wwi_county
  ) %>%
  filter(
    icpsrst != 98 # Remove Washington, D.C.
  ) 
# %>%
#   mutate( # These counties were created post-1910 and therefore don't merge with congressionanl district crosswalks; re-coding so they merge to the appropriate district, at least. 
#     icpsrcty = if_else(icpsrst == 73 & icpsrcty == 510, 650, icpsrcty), # Pend Oreille created out of Stevens County on March 1, 1911
#     icpsrcty = if_else(icpsrst == 72 & icpsrcty == 310, 130, icpsrcty), # Jefferson County was created on December 12, 1914, from a portion of Crook County
#     icpsrcty = if_else(icpsrst == 72 & icpsrcty == 170, 130, icpsrcty), # Deschutes County  created in 1916 out of part of Crook County
#     district = if_else(icsprst == 68, 10, icpsrcty), # All of Wyoming is 1 district in 66th congress
#     district = if_else(icsprst == 66, 0, district), # All of New Mexico is 1 district in 66th congress
#     district = if_else(icsprst == 65, 0, district), # All of Nevada is 1 district in 66th congress
#     icpsrcty = if_else(icpsrst == 64 & icpsrcty == 1090, 210, icpsrcty), # Wibaux County was created by the Montana Legislature in 1914 from parts of Dawson, Fallon, and Richland Counties
#     icpsrcty = if_else(icpsrst == 64 & icpsrcty == 1090, 590, icpsrcty), # Wheatland County was established on February 22, 1917, with areas partitioned from Meagher and Sweet Grass counties.
#     icpsrcty = if_else(icpsrst == 64 & icpsrcty == 1010, 990, icpsrcty), # Toole established in 1914 from parts of Hill County and Teton County 
#     icpsrcty = if_else(icpsrst == 64 & icpsrcty == 950, 990, icpsrcty), # Stillwater established in 1914 (assigning Teton b/c same congressional district)
#     icpsrcty = if_else(icpsrst == 64 & icpsrcty == 910, 210, icpsrcty), # established Sheridan County in 1913 from portions of Dawson and Valley Counties
#     icpsrcty = if_else(icpsrst == 64 & icpsrcty == 830, 210, icpsrcty), # Richland County was created by the Montana Legislature in 1914 from part of Dawson County.
#     icpsrcty = if_else(icpsrst == 64 & icpsrcty == 790, 210, icpsrcty), # Prairie County was created by the Montana Legislature in 1915 out of parts of Custer, Dawson, and Fallon Counties
#     icpsrcty = if_else(icpsrst == 64 & icpsrcty == 710, 150, icpsrcty), # Before February 5, 1915, Phillips County was part of Blaine County, and before 1912 both were part of Chouteau County.
#     icpsrcty = if_else(icpsrst == 64 & icpsrcty == 650, 590, icpsrcty), # Musselshell County was created in 1911 by Montana Governor Edwin L. Norris. The area was taken from Fergus, Yellowstone, and Meagher counties.
#     icpsrcty = if_else(icpsrst == 64 & icpsrcty == 610, 10, icpsrcty), # Mineral is in Disrict 1
#     icpsrcty = if_else(icpsrst == 64 & icpsrcty == 410, 510, icpsrcty), # Hill neighbors Liberty within-district
#     icpsrcty = if_else(icpsrst == 64 & icpsrcty == 250, 170, icpsrcty), # Fallon created in 1913 from a portion of Custer County.
#     icpsrcty = if_else(icpsrst == 64 & icpsrcty == 110, 170, icpsrcty), # Prior to settlement the land of Carter County was occupied by the Sioux tribe; neighbors Custer/Fallon.
#     icpsrcty = if_else(icpsrst == 64 & icpsrcty == 50, 510, icpsrcty), # In 1912 Blaine, Phillips and Hill counties were formed from the area of Chouteau County. The original boundary of Blaine County included a portion of land in the west that is now included in Phillips County
#     icpsrcty = if_else(icpsrst == 64 & icpsrcty == 30, 510, icpsrcty), # Big Horn founded in 1913, same district as Liberty.
#     icpsrcty = if_else(icpsrst == 63 & icpsrcty == 30, 150, icpsrcty), # Ada partitioned from Boise County. 
#   )


# ICPSR County Characteristics ####
# print(lapply(cnty_icpsr1910, attr, "label"))
desired_vars <- c( # Set variables I'd like to pull from 1910 and 1860, whenever available.
  "state", # ICPSR state code
  "county", # ICPSR county code
  "region1", # U.S. Census Region (9)
  "region2", # U.S. Census Region (5)
  "totpop", # Total population 1910
  "urb910", # Urban population 1910
  "urb25", # Population cities 25K+ 1910
  "urb1900", # Population places 2500+, 1910
  "urb860", # Population places 2500+, 1900
  "mtot", # Total male population
  "ftot", # Total female population
  "wmtot", # Total white male population
  "wftot", # Total white female population
  "negmtot", # Total Negro male population
  "negftot", # Total Negro female population
  "mvote", # Total males, voting age
  "negmvote", # Negro males, voting age
  "tillit10", # Total illiterate population 10+
  "tsch620", # Population 6-20 in school
  "cropval", # Value of all crops
  "farms", # Total # of farms
  "acimpown", # Improved acres/owner-operated farms
  "acresown", # Acres in owner-operated farms
  "farmten", # tenant farms
  "fbwtot", # Foreign white population
  "nwill10", # Illiterate native white population 10+
  "fbwill10", # Illiterate foreign white population 10+
  "negill10" # Illiterate Negro population 10+
)

cnty_icpsr1910 <- 
  read_dta("raw/icpsr/ICPSR_02896/DS0022/02896-0022-Data.dta") %>% # Import
  dplyr::select(any_of(desired_vars)) %>% 
  rename(
    icpsrst = state,
    icpsrcty = county
  )

cnty_icpsr1860 <- 
  read_dta("raw/icpsr/ICPSR_02896/DS0009/02896-0009-Data.dta") %>% # Import
  dplyr::select(any_of(desired_vars)) %>% 
  rename_with(~ paste0(.x, "1860")) %>%
  rename(
    icpsrst = state1860,
    icpsrcty = county1860
  )

# 1910 Census characteristics (from NBER) ####
cnty_nber <- 
  read_dta("raw/census/1900-1920-aggregatesJUNE2023.dta") %>%
  rename(
    icpsrst = stateicp,
    icpsrcty = county
  )

# Civil War ####
cnty_civil_war <- 
  read_dta("raw/civil war/UA_battle_and_casualty_data_2020-06-15.dta") %>%
  rename_with(~ paste0("cw_", .x)) %>%
  rename(
    icpsrst = cw_state_icpsr,
    icpsrcty = cw_county_icpsr
  ) %>%
  filter(
    !is.na(icpsrst) &
    !is.na(icpsrcty)
  )

# American Federation of Labor (AFL) ####
cnty_unions <- 
  read_dta("~/Dropbox/Pitt/Projects/Suffrage WWI/data/raw/henry/AFL/ca_wi_il_pa.dta") %>% # Import
  dplyr::select(
    st_abbrev,
    countyicp,
    data_year,
    union_members
  ) %>%
  filter(data_year %in% 1910:1920) %>% # Keep years of interest
  pivot_wider( # Reshape wide to long
    id_cols = c(st_abbrev,
                countyicp
                ),
    names_from = data_year,
    values_from = union_members
  ) %>%
  mutate(
    icpsrst = case_when(
      st_abbrev == "CA" ~ 71,
      st_abbrev == "IL" ~ 21,
      st_abbrev == "PA" ~ 14,
      st_abbrev == "WI" ~ 25,
      TRUE ~ NA_real_
      )
    ) %>%
  rename(
    icpsrcty = countyicp,
  ) %>%
  rename_with(~ paste0("AFL", .x), all_of(as.character(1910:1920))) %>%
  dplyr::select(-st_abbrev)

# Clean up Global Environment
rm(cw, desired_vars)