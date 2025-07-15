# Aggregate county-level data up to congressional district level
# July 15, 2025

# Congressional districts-to-county crosswalks for counties formed by 1910.
cw_cnty_cd_66 <- 
  read_dta("~/Dropbox/Pitt/Projects/Suffrage WWI/data/raw/cd-cnty-crosswalks/DOA_stata/Crosswalk_1910_66.dta") %>%
  dplyr::select(
    icpsrst,
    icpsrcty,
    district,
    id,
    starts_with("m")
    ) 

sort(unique(cw_cnty_cd_66$icpsrst))

# print(lapply(cw_cnty_cd_66, attr, "label"))
# $m1_weight
# [1] "Weight for count variables (model 1: population assumed homogeneous in counties)"
# 
# $m2_weight
# [1] "Weight for count variables (model 2: pop assigned to urban/rural areas in cntys)"
# 
# $m3_weight
# [1] "Weight for count variables (model 3: m2 with noninhabitable areas given pop=0)"
# 
# $m4_weight
# [1] "Weight for count variables (model 4: m3 adjusting for topographic suitability)"

merged_data_a <- merge(x = cw_cnty_cd_66, y = cnty_casualties, by = c("icpsrst", "icpsrcty"), all = TRUE)   # perfect merge - all equivalent to all.x 
merged_data_b <- merge(x = merged_data_a, y = cnty_civil_war, by = c("icpsrst", "icpsrcty"), all.x = TRUE)  # Executive decision to only keep rows in cw_cnty_cd_66, because i'll need the weights to aggregate up... Can add a robustness later retaining the most data, but the least specific population distribution (e.g. m1)
merged_data_c <- merge(x = merged_data_b, y = cnty_draft, by = c("icpsrst", "icpsrcty"), all.x = TRUE)      # draft data messed up by county changes from 1910-1920; re-coding started in clean_cnty_data.R
merged_data_d <- merge(x = merged_data_c, y = cnty_icpsr1860, by = c("icpsrst", "icpsrcty"), all.x = TRUE)  
merged_data_e <- merge(x = merged_data_d, y = cnty_icpsr1910, by = c("icpsrst", "icpsrcty"), all.x = TRUE)  
merged_data_f <- merge(x = merged_data_e, y = cnty_nber, by = c("icpsrst", "icpsrcty"), all.x = TRUE)  
merged_data_g <- merge(x = merged_data_f, y = cnty_unions, by = c("icpsrst", "icpsrcty"), all.x = TRUE)  

final <- merged_data_g %>%
  group_by(icpsrst, district) %>%
  summarise(
    across(
      where(is.numeric) & !any_of(c("icpsrcty", "m1_weight","m2_weight","m3_weight","m4_weight")), 
      ~ sum(.x * m4_weight, na.rm = TRUE)
    ),
    .groups = "drop"
  )



