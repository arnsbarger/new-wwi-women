# Clean county-level data
# July 14, 2025
# icpsrst
# icpsrcty

# Geo crosswalk
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
    fips > 0 # Remove national-level row
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
  rename(
    icpsrcty = county_icpsr
  )

# WWI Drafts ####
cnty_draft <- 
  read_dta("raw/wwi/004_Inductions.dta") %>% # Import
  dplyr::select(
    state, 
    county, 
    starts_with("tot_")
    )  %>%
  rename(
    icpsrcty = county,
    icpsrst = state
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