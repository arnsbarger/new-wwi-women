# Aggregate county-level data up to congressional district level
# July 15, 2025

merged_data_a <- merge(
  x = cw_cnty_cd_66, 
  y = cnty_casualties, 
  by.x = c("icpsrst1920", "icpsrcty1920"), # WWI Casualty data collected after 1910
  by.y = c("icpsrst", "icpsrcty"), 
  all = TRUE) # perfect merge - all equivalent to all.x, same length as cw_cnty_cd_66

merged_data_b <- merge(
  x = merged_data_a, 
  y = cnty_civil_war, 
  by.x = c("icpsrst1910", "icpsrcty1910"), # Civil War data collected before 1910
  by.y = c("icpsrst", "icpsrcty"), 
  all.x = TRUE) # This technically drops counties that existed during the Civil War, but no longer exist by 1910. If Civil War-related variables start looking important during the analysis, might have to crosswalk the 1860 counties too...
  
merged_data_c <- merge(
  x = merged_data_b, 
  y = cnty_draft, 
  by.x = c("icpsrst1920", "icpsrcty1920"), # WWI Draft data collected after 1910
  by.y = c("icpsrst", "icpsrcty"), 
  all.x = TRUE) 

merged_data_d <- merge(
  x = merged_data_c, 
  y = cnty_icpsr1860, 
  by.x = c("icpsrst1910", "icpsrcty1910"), # 1860 Census data collected before 1910
  by.y = c("icpsrst", "icpsrcty"), 
  all.x = TRUE) # Same caveat as Civil War merge above

merged_data_e <- merge(
  x = merged_data_d, 
  y = cnty_icpsr1910, 
  by.x = c("icpsrst1910", "icpsrcty1910"), # 1910 Census data collected during 1910
  by.y = c("icpsrst", "icpsrcty"), 
  all = TRUE)

merged_data_f <- merge(
  x = merged_data_e, 
  y = cnty_icpsr1920, 
  by.x = c("icpsrst1920", "icpsrcty1920"), # 1910 Census data collected during 1910
  by.y = c("icpsrst", "icpsrcty"), 
  all = TRUE)

merged_data_g <- merge(
  x = merged_data_f, 
  y = cnty_nber, 
  by.x = c("icpsrst1910", "icpsrcty1910"), # 1910 Census data collected during 1910
  by.y = c("icpsrst", "icpsrcty"), 
  all = TRUE)

merged_data_h <- merge(
  x = merged_data_g, 
  y = cnty_unions, 
  by.x = c("icpsrst1920", "icpsrcty1920"), # Data set favors more recent county definitions
  by.y = c("icpsrst", "icpsrcty"), 
  all = TRUE)  

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

vars_to_aggregate <- names(merged_data_h)[14:length(names(merged_data_h))]

final_data <- merged_data_h %>%
  mutate(
    # across(all_of(vars_to_aggregate), ~ .x * m1_weight, .names = "m1_{.col}"),
    # across(all_of(vars_to_aggregate), ~ .x * m2_weight, .names = "m2_{.col}"),
    # across(all_of(vars_to_aggregate), ~ .x * m3_weight, .names = "m3_{.col}"),
    across(all_of(vars_to_aggregate), ~ .x * m4_weight, .names = "m4_{.col}")
  ) %>%
  group_by(icpsrst1920, district) %>%
  summarise(
    cnty_change_191020 = max(cnty_change_191020),
    across(starts_with("m4_"), sum, na.rm = TRUE),
    # across(starts_with("m3_"), sum, na.rm = TRUE),
    # across(starts_with("m2_"), sum, na.rm = TRUE),
    # across(starts_with("m1_"), sum, na.rm = TRUE),
    .groups = "drop"
  )


# Clean up Global Environment
rm(list = ls(pattern = "^(merged_data_|cnty_)"), vars_to_aggregate)
