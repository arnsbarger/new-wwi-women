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
  ) %>%
  mutate(
    icpsrcty = if_else(icpsrst == 54 & icpsrcty == 875, 0650, icpsrcty), # James, TN became Hamilton, TN.
    icpsrcty = if_else(icpsrst == 37 & icpsrcty == 10, 1170, icpsrcty), # Armstrong, SD became Stanley, SD.
  ) %>%
  group_by(icpsrst, icpsrcty) %>%
  summarise( # Since I corrected the counties above, now their codes are duplicates; need to sum up by county/state.
    across(starts_with("wwi_"), sum, na.rm = TRUE),
    .groups = "drop"
  ) 

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
  "negill10", # Illiterate Negro population 10+
  "fbwmtot", # Foreign white male population 
  "fbwftot" # Foreign white female population 
)

cnty_icpsr1910 <- 
  read_dta("raw/icpsr/ICPSR_02896/DS0022/02896-0022-Data.dta") %>% # Import
  dplyr::select(any_of(desired_vars)) %>% 
  rename_with(~ paste0(.x, "1910")) %>%
  rename(
    icpsrst = state1910,
    icpsrcty = county1910
  ) %>%
  filter(
    icpsrcty > 0,
    icpsrcty != 1130, # Shannon County unorganized until 1982.
    !icpsrst %in% c(
      82, # Hawaii
      98 # Washington, DC
    )) %>%
  mutate(
    icpsrcty = if_else(icpsrst == 37 & icpsrcty == 1330, 70, icpsrcty), # Washington, SD became Bennett, SD.
  ) %>%
  group_by(icpsrst, icpsrcty) %>%
  summarise( # Since I corrected the counties above, now their codes are duplicates; need to sum up by county/state.
    across(
      .cols = where(is.numeric) & !any_of(c("icpsrst", "icpsrcty")),
      .fns = sum,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

cnty_icpsr1920 <- 
  read_dta("raw/icpsr/ICPSR_02896/DS0024/02896-0024-Data.dta") %>% # Import
  dplyr::select(any_of(desired_vars)) %>% 
  rename_with(~ paste0(.x, "1920")) %>%
  rename(
    icpsrst = state1920,
    icpsrcty = county1920
  ) %>%
  filter(
    icpsrcty > 0,
    icpsrcty != 1130, # Shannon County unorganized until 1982.
    !icpsrst %in% c(
      82, # Hawaii
      98 # Washington, DC
    )) %>%
  mutate(
    icpsrcty = if_else(icpsrst == 37 & icpsrcty == 1330, 70, icpsrcty), # Washington, SD became Bennett, SD.
  ) %>%
  group_by(icpsrst, icpsrcty) %>%
  summarise( # Since I corrected the counties above, now their codes are duplicates; need to sum up by county/state.
    across(
      .cols = where(is.numeric) & !any_of(c("icpsrst", "icpsrcty")),
      .fns = sum,
      na.rm = TRUE
    ),
    .groups = "drop"
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
  ) %>%
  filter(
    !icpsrst == 98 # Washington, DC
  ) %>%
  mutate(
    icpsrcty = if_else(icpsrst == 72 & icpsrcty == 605, 10, icpsrcty), # Union, OR was Baker, OR.
    icpsrcty = if_else(icpsrst == 65 & icpsrcty == 510, 10, icpsrcty) # Put Carson City, NV into literally any other county that exists in cw_cnty_cd_66 (NV is 1 district, so as long as its capital's population gets counted)
  ) %>%
  group_by(icpsrst, icpsrcty) %>%
  summarise( # Since I corrected the counties above, now their codes are duplicates; need to sum up by county/state.
    across(
      .cols = where(is.numeric) & !any_of(c("icpsrst", "icpsrcty")),
      .fns = sum,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  rename_with(~ paste0(.x, "nber"), .cols = c("totpop1910", "ftot1910", "wftot1910", "totpop1920", "ftot1920", "wftot1920"))

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
    !is.na(icpsrcty) &
    icpsrst != 98 # Remove Washington, D.C.
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
  rename(
    icpsrcty = countyicp,
  ) %>%
  mutate(
    icpsrst = case_when(
      st_abbrev == "CA" ~ 71,
      st_abbrev == "IL" ~ 21,
      st_abbrev == "PA" ~ 14,
      st_abbrev == "WI" ~ 25,
      TRUE ~ NA_real_
      ),
    icpsrcty = as.numeric(icpsrcty),
    icpsrcty = if_else(icpsrst == 25 & icpsrcty == 780, 330, icpsrcty) # Menomonee is the county seat of Dunn
    ) %>%
  rename_with(~ paste0("AFL", .x), all_of(as.character(1910:1920))) %>%
  dplyr::select(-st_abbrev) %>%
  group_by(icpsrst, icpsrcty) %>%
  summarise( # Since I corrected the counties above, now their codes are duplicates; need to sum up by county/state.
    across(
      .cols = where(is.numeric) & !any_of(c("icpsrst", "icpsrcty")),
      .fns = sum,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# Clean up Global Environment
rm(cw, desired_vars)