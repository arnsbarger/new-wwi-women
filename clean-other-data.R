# Clean other data
# July 28, 2025

rep_chars <- read_dta("raw/icpsr/ICPSR_03371/DS0003/03371-0003-Data.dta") %>%
  filter(!is.na(MEMBER)) %>%
  mutate(
    rep_born_st = (BORN_ST - 1)^2, # Indicator: 1 if rep born in state he represents, 0 otherwise.
    rep_born_region = (BORN_REG - 1)^2, # Indicator: 1 if rep born in state he represents, 0 otherwise.
    rep_yrs_since_elected = 920 - YR1STCON,
    rep_years_in_congress = ifelse(test = TOT_YRS < 0, yes = 0, no = TOT_YRS),
    rep_college = as.integer(COL_COLL > 0),    # 1 if yes, 0 if no
    rep_military = as.integer(COL_MIL > 0),    # 1 if yes, 0 if no
    rep_occupation = as.factor(COL_OCCU),
    rep_age_first_cong = as.numeric(AGE_FRST),
    rep_new_rep_1915 = as.integer(YR1STCON > 915),   # 1 if yes, 0 if no
    rep_new_rep_1919 = as.integer(YR1STCON > 919),    # 195 == 1915, etc.
    icpsr = MEMBER # Merge to voteview data (cong##) on icpsr
  ) %>%
  dplyr::select(
    icpsr,
    starts_with("rep_")
  )


### Pre-Amendment Suffrage Rights -----

state_suffrage <- data.frame(
  statenam = c(
    "Washington", "Oregon", "California","Nevada","Idaho","Montana","Wyoming","Colorado","South Dakota","Kansas","Oklahoma","Michigan","New York","Arizona Territory","Utah",
    "Texas", "Arkansas",
    "North Dakota","Nebraska","Illinois","Vermont",
    "New Mexico Territory","Louisiana","Mississippi","Kentucky","Iowa","Minnesota","Wisconsin","New Jersey","Delaware","New Hampshire","Massachusetts","Connecticut",
    "Florida","Ohio",
    "Rhode Island",
    "Missouri","Indiana","Tennessee","Alabama","Georgia","South Carolina","North Carolina","Virginia","West Virginia","Pennsylvania","Maryland","Maine"
    ),
  prior_suff = c(
    rep("Full", 15),
    rep("Primary", 2),
    rep("Presidential and Municipal", 4),
    rep("School bond or Tax", 12),
    rep("Municipal", 2),
    "Presidential",
    rep("None", 12)
    ),
  simple_suff = c(
    rep("Full", 15),
    rep("Partial",21),
    rep("None",12)
    )
  )
