# Combine congressional district level data
# July 28, 2025

# Goal: 435 congressional districts
nrow(final_data) # 431
which(duplicated(final_data %>% dplyr::select(district, icpsrst1920)) | duplicated(final_data %>% dplyr::select(district, icpsrst1920), fromLast = TRUE)) # no duplicates
table(final_data$icpsrst1920, final_data$district) # Need to duplicate PA-00 x 4, IL-00 x 2

data <- final_data %>% 
  # This step isn't strictly necessary if only keeping m4_ weights from merge-cnty-to-cd.R; written for generalization after exploring robustness to other weighting schemes
  pivot_longer( 
    cols = matches("^m[1-4]_"),  
    names_to = c("weight_scheme", ".value"),
    names_pattern = "(m[1-4])_(.*)"
  ) %>%
  # Define variables based off raw counts
  mutate(
    # Female labor force participation rates
    flfp1900 = (flf1900 / ftot1900) * 100,
    flfp1910 = (flf1910 / ftot1910) * 100,
    flfp1920 = (flf1920 / ftot1920) * 100,
    raw_change_flfp = flfp1920 - flfp1910,
    raw_change_flfp1900_10 = flfp1910 - flfp1900,
    raw_change_flfp1900_20 = flfp1920 - flfp1900,
    raw_change_flfp_Z = as.numeric(scale(raw_change_flfp)),
    
    # Female labor force participation rates, working-age denominator
    flfp_16plus1900 = (flf1900 / totf16plus1900) * 100,
    flfp_16plus1910 = (flf1910 / totf16plus1910) * 100,
    flfp_16plus1920 = (flf1920 / totf16plus1920) * 100,
    raw_change_flfp_16plus = flfp_16plus1920 - flfp_16plus1910,
    raw_change_flfp1900_10_16plus = flfp_16plus1910 - flfp_16plus1900,
    raw_change_flfp_16plus_Z = as.numeric(scale(raw_change_flfp_16plus)),
    
    # Male labor force participation rates, working-age denominator
    mlfp_16plus1900 = (mlf1900 / totm16plus1900) * 100,
    mlfp_16plus1910 = (mlf1910 / totm16plus1910) * 100,
    mlfp_16plus1920 = (mlf1920 / totm16plus1920) * 100,
    raw_change_mlfp1910_20_16plus = mlfp_16plus1920 - mlfp_16plus1910,
    raw_change_mlfp1900_10_16plus = mlfp_16plus1910 - mlfp_16plus1900,
    raw_change_mlfp_16plus_Z = as.numeric(scale(raw_change_mlfp1910_20_16plus)),
    
    # Chiswick & Robinson labor force participation rates
    flfp_cr1910 = ((flf1910 + flf_famfarm_cr1910) / totf16plus1910) * 100,
    flfp_cr1920 = ((flf1920 + flf_famfarm_cr1920) / totf16plus1920) * 100,
    raw_change_flfp_cr = flfp_cr1920 - flfp_cr1910,
    
    # Common denominator for 1910/1920
    raw_change_flfp_commondenom = ((flf1920 - flf_ind1051920 - flf_ind8261920) - (flf1910 - flf_ind1051910 - flf_ind8261910)) * 100 / totf16plus1910,
    
    # Non-agriculture labor force participation changes
    raw_change_flfp_nonag = ((flf1920 - flf_ind1051920) / ftot1920) * 100 - ((flf1910 - flf_ind1051910) / ftot1910) * 100,
    raw_change_flfp_nonag16plus = ((flf1920 - flf_ind1051920) / totf16plus1920) * 100 - ((flf1910 - flf_ind1051910) / totf16plus1910) * 100,
    raw_change_flfp_non830all = ((flf1920 - flf_occ1950_8301920) / ftot1920) * 100 - ((flf1910 - flf_occ1950_8301910) / ftot1910) * 100,
    raw_change_flfp_non83016plus = ((flf1920 - flf_occ1950_8301920) / totf16plus1920) * 100 - ((flf1910 - flf_occ1950_8301910) / totf16plus1910) * 100,
    
    raw_change_mlfp_nonag = ((mlf1920 - mlf_ind1051920) / mtot1920) * 100 - ((mlf1910 - mlf_ind1051910) / mtot1910) * 100,
    raw_change_mlfp_nonag16plus = ((mlf1920 - mlf_ind1051920) / totm16plus1920) * 100 - ((mlf1910 - mlf_ind1051910) / totm16plus1910) * 100,
    # raw_change_mlfp_non830all = ((mlf1920 - mlf_occ1950_8301920) / mtot1920) * 100 - ((mlf1910 - mlf_occ1950_8301910) / mtot1910) * 100,
    # raw_change_mlfp_non83016plus = ((mlf1920 - mlf_occ1950_8301920) / totm16plus1920) * 100 - ((mlf1910 - mlf_occ1950_8301910) / totm16plus1910) * 100,
    
    # White women only
    wflfp1910 = (wflf1910 / wftot1910) * 100,
    wflfp1920 = (wflf1920 / wftot1920) * 100,
    raw_change_wflfp = wflfp1920 - wflfp1910,
    
    # White women, working-age denominator
    wflfp_16plus1900 = (wflf1900 / totwf16plus1900) * 100,
    wflfp_16plus1910 = (wflf1910 / totwf16plus1910) * 100,
    wflfp_16plus1920 = (wflf1920 / totwf16plus1920) * 100,
    raw_change_wflfp_16plus = wflfp_16plus1920 - wflfp_16plus1910,
    raw_change_wflfp_16plus190010 = wflfp_16plus1910 - wflfp_16plus1900,
    
    # NPWAI-designated war-related industries
    flf_npwai1900 = rowSums(dplyr::select(., matches("^flf_ind(336|346|358|337|379|357|338|348|388|347|399|309|308|469|307|456|419|477|467|476|468|488|448|487|489|326|316|319|439|436|437|449|446|416|407|417|409|406|418|429|458|459|356|376|386|367|478|378)1900$"))),
    flf_npwai1910 = rowSums(dplyr::select(., matches("^flf_ind(336|346|358|337|379|357|338|348|388|347|399|309|308|469|307|456|419|477|467|476|468|488|448|487|489|326|316|319|439|436|437|449|446|416|407|417|409|406|418|429|458|459|356|376|386|367|478|378)1910$"))),
    flf_npwai1920 = rowSums(dplyr::select(., matches("^flf_ind(336|346|358|337|379|357|338|348|388|347|399|309|308|469|307|456|419|477|467|476|468|488|448|487|489|326|316|319|439|436|437|449|446|416|407|417|409|406|418|429|458|459|356|376|386|367|478|378)1920$"))),
    
    flfp_npwai1900 = (flf_npwai1900 / ftot1900) * 100,
    flfp_npwai1910 = (flf_npwai1910 / ftot1910) * 100,
    flfp_npwai1920 = (flf_npwai1920 / ftot1920) * 100,
    raw_change_flfp_npwai = flfp_npwai1920 - flfp_npwai1910,
    raw_change_flfp_npwai1900_10 = flfp_npwai1910 - flfp_npwai1900,
    raw_change_flfp_npwai_Z = as.numeric(scale(raw_change_flfp_npwai)),
    
    # Ag 
    pct_ag1910 = (flf_ind1051910 / flf1910) * 100,
    
    # Manufacturing
    flfmfg1910 = rowSums(dplyr::select(., matches("^flf_ind(306|307|308|309|316|319|326|336|337|338|346|347|348|356|357|358|367|376|378|379|386|388|399|406|407|409|416|417|418|419|429|436|437|439|446|448|449|456|458|459|467|468|469|476|477|478|487|488|489)1910$"))),
    flfmfg1920 = rowSums(dplyr::select(., matches("^flf_ind(306|307|308|309|316|319|326|336|337|338|346|347|348|356|357|358|367|376|378|379|386|388|399|406|407|409|416|417|418|419|429|436|437|439|446|448|449|456|458|459|467|468|469|476|477|478|487|488|489)1920$"))),
    raw_change_flfp_mfg = flfmfg1920 - flfmfg1910,
    pct_mfg = (flfmfg1910 / flf1910) * 100,

    # Log transformations
    log_liq1910 = log(totliqlf1910 + 1),
    log_totpop1910 = log(totpop1910),
    log_german1910 = log(totgerman1910),
    log_black1910 = log(negftot1910 + negmtot1910),
    
    # Share/percentage/per capita transformations
    pct_liq1910 = (totliqlf1910 / (flf1910 + mlf1910)) * 100,
    pct_german1910 = (totgerman1910 / totpop1910) * 100,
    pct_drafted = (wwi_tot_inducted_num / mvote1910) * 100,
    pct_dead = (wwi_dead_all / mvote1910) * 100,
    casualty_rate = (wwi_dead_all / wwi_tot_inducted_num) * 100,
    sexratio1910 = ftot1910 / mtot1910,
    sexratio16plus1910 = totf16plus1910 / totm16plus1910,
    pct_fb1910 = (fbwtot1910 * 100) / (ftot1910 + mtot1910),
    fbwtot1920 = fbwmtot1920 + fbwftot1920,
    pct_fb1920 = (fbwtot1920 * 100) / (ftot1920 + mtot1920),
    raw_change_pct_fb = pct_fb1920 - pct_fb1910,
    pct_illiterate1910 = (tillit101910 / totpop1910) * 100,
    farms_pc1910 = farms1910 / totpop1910,
    pct_ownacimp1910 = (acimpown1910 / acresown1910) * 100,
    pct_urb1910 = (urb1910 / totpop1910) * 100,
    pct_urb1920 = (urb1920 / totpop1920) * 100,
    pct_white1910 = (wftot1910 + wmtot1910) * 100 / totpop1910,
    pct_black1910 = (negftot1910 + negmtot1910) * 100 / totpop1910,
    pct_black1920 = (negftot1920 + negmtot1920) * 100 / totpop1920,
    raw_change_pct_black = pct_black1920 - pct_black1910,
    pct_black_f1910 = (negftot1910 / ftot1910) * 100,
    pct_black_m1910 = (negmtot1910 / mtot1910) * 100,
    pct_singlef16plus1910 = (ftotsingle1910 / totf16plus1910) * 100,
    pct_f21plus1910 = (totf21plus1910 / ftot1910) * 100, # Voting-age
    pct_wf21plus1910 = (totwf21plus1910 / wftot1910) * 100,
    pct_f16plus1910 = (totf16plus1910 / ftot1910) * 100,
    pct_wf16plus1910 = (totwf16plus1910 / wftot1910) * 100,
    pct_lf_f1910 = ((flf1910 + flf_famfarm_cr1910) / 
                      (flf1910 + flf_famfarm_cr1910 + mlf1910 + mlf_famfarm_cr1910)) * 100,
    pct_lf_f1920 = ((flf1920 + flf_famfarm_cr1920) /
                      (flf1920 + flf_famfarm_cr1920 + mlf1920 + mlf_famfarm_cr1920)) * 100,
    raw_change_share_lf_f = pct_lf_f1920 - pct_lf_f1910,
    
    # Census divisions: https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
    census_division = as.factor(case_when(
      icpsrst1920 %in% c(01, 02, 03, 04, 05, 06) ~ 1, # New England: CT, ME, MA, NH, RI, VT
      icpsrst1920 %in% c(12, 13, 14) ~ 2, # Middle Atlantic: NJ, NY, PA
      icpsrst1920 %in% c(21, 22, 23, 24, 25) ~ 3, # East North Central: IL, IN, MI, OH, WI
      icpsrst1920 %in% c(31, 32, 33, 34, 35, 36, 37) ~ 4, # West North Central: IA, KS, MN, MO, NE, ND, SD
      icpsrst1920 %in% c(11, 40, 43, 44, 47, 48, 52, 55, 56) ~ 5, # South Atlantic: DE, VA, FL, GA, NC, SC, MD, DC, WV
      icpsrst1920 %in% c(41, 46, 51, 54) ~ 6, # East South Central: AL, MS, KY, TN
      icpsrst1920 %in% c(42, 45, 49, 53) ~ 7, # West South Central: AR, LA, TX, OK
      icpsrst1920 %in% c(61, 62, 63, 64, 65, 66, 67, 68) ~ 8, # Mountain: AZ, CO, ID, MT, NV, NM, UT, WY
      icpsrst1920 %in% c(71, 72, 73, 81, 82) ~ 9, # Pacific: CA, OR, WA, AK, HI
      TRUE ~ NA_real_
    )),
    northeast = if_else(census_division %in% c(1, 2), 1, 0, missing = 0),
    midwest = if_else(census_division %in% c(3, 4), 1, 0, missing = 0),
    south = if_else(census_division %in% c(5, 6, 7), 1, 0, missing = 0),
    west = if_else(census_division %in% c(8, 9), 1, 0, missing = 0),
    pct_black_GM = pct_black1910 * (south == 0)
  )

# Add district identifier (ID_STATEDIST)
cw <- read_excel("raw/geo/icpsrcnt.xls") %>%
  distinct(State, STATEICP) %>%
  filter(!is.na(STATEICP))

data <- 
  merge(x = cw, y = data, by.x = "STATEICP", by.y = "icpsrst1920", all.y = TRUE) %>%
  rename(
    icpsrst1920 = STATEICP
    ) %>%
  mutate(
    ID_STATEDIST = paste0(State, str_pad(district, width = 2, pad = 0)),
    ID_STATEDIST = replace(ID_STATEDIST, row_number() == 113, "Pennsylvania00a"),
    ID_STATEDIST = replace(ID_STATEDIST, row_number() == 143, "Illinois00a"),
    across(everything(), ~ ifelse(is.nan(.x) | is.infinite(.x) | is.na(.x), NA, .x))
  )

# Add rows for multiple at-large districts
# pa_a <- data %>% slice(113) %>% mutate(ID_STATEDIST = replace(ID_STATEDIST, row_number() == 1, "Pennsylvania00a"))
pa_b <- data %>% slice(113) %>% mutate(ID_STATEDIST = replace(ID_STATEDIST, row_number() == 1, "Pennsylvania00b"))
pa_c <- data %>% slice(113) %>% mutate(ID_STATEDIST = replace(ID_STATEDIST, row_number() == 1, "Pennsylvania00c"))
pa_d <- data %>% slice(113) %>% mutate(ID_STATEDIST = replace(ID_STATEDIST, row_number() == 1, "Pennsylvania00d"))
# il_a <- data %>% slice(143) %>% mutate(ID_STATEDIST = replace(ID_STATEDIST, row_number() == 1, "Illinois00a"))
il_b <- data %>% slice(143) %>% mutate(ID_STATEDIST = replace(ID_STATEDIST, row_number() == 1, "Illinois00b"))

data <- bind_rows(data, pa_b, pa_c, pa_d, il_b)
nrow(data) # 435

# # Add rep chars to cong## data
# cong66 <- merge(x = cong66, y = rep_chars, by = "icpsr", all.x = TRUE)
# cong65 <- merge(x = cong65, y = rep_chars, by = "icpsr", all.x = TRUE)
# cong63 <- merge(x = cong63, y = rep_chars, by = "icpsr", all.x = TRUE)

# Merge RHS with LHS:
data <- merge(
  x = data, 
  y = cong66 %>% 
    dplyr::select(-c("State","district_code")), # duplicate State/district code columns
  by.x = "ID_STATEDIST", 
  by.y = "ID_STATEDIST", 
  all= TRUE
)

data <- merge(
  x = data, 
  y = cong65 %>% dplyr::select(ID_STATEDIST, V061, yeaV061),
  by.x = "ID_STATEDISTin65",
  by.y = "ID_STATEDIST",
  all.x=TRUE
  )

data <- merge(
  x = data, 
  y = cong63 %>% dplyr::select(ID_STATEDIST, V238, V228, yeaV238, yeaV228),
  by.x = "ID_STATEDISTin63",
  by.y = "ID_STATEDIST",
  all.x=TRUE
)

# Merge in "other" data
data <- merge(
  x = data,
  y = rep_chars, # House Rep characteristics
  by = "icpsr",
  all.x = TRUE
)

data <- merge(
  x = data,
  y = state_suffrage, # State suffrage laws
  by.x = "State",
  by.y = "statenam",
  all.x = TRUE
)

# Fix variable types
data[c("yeaV002", "yeaV061", "yeaV228", "yeaV238")] <- lapply(data[c("yeaV002", "yeaV061", "yeaV228", "yeaV238")], as.numeric)






rm(list = c(
  "final_data", "cw", "rep_chars", "state_suffrage", 
  "il_b", "pa_b", "pa_c", "pa_d", 
  "cong63", "cong65", "cong66"
))

