# Combine congressional district level data
# July 28, 2025

data <- final_data %>% 
  # This step isn't strictly necessary if only keeping m4_ weights from merge-cnty-to-cd.R; written for eralization after exploring robustness to other weighting schemes
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
    raw_change_flfp_SD = sd(raw_change_flfp1900_20, na.rm = TRUE),
    
    # Female labor force participation rates, working-age denominator
    flfp1900_16plus = (flf1900 / totf16plus1900) * 100,
    flfp1910_16plus = (flf1910 / totf16plus1910) * 100,
    flfp1920_16plus = (flf1920 / totf16plus1920) * 100,
    raw_change_flfp_16plus = flfp1920_16plus - flfp1910_16plus,
    raw_change_flfp1900_10_16plus = flfp1910_16plus - flfp1900_16plus,
    raw_change_flfp_16plus_SD = sd(raw_change_flfp_16plus, na.rm = TRUE),
    
    # Male labor force participation rates, working-age denominator
    mlfp1900_16plus = (mlf1900 / totm16plus1900) * 100,
    mlfp1910_16plus = (mlf1910 / totm16plus1910) * 100,
    mlfp1920_16plus = (mlf1920 / totm16plus1920) * 100,
    raw_change_mlfp1910_20_16plus = mlfp1920_16plus - mlfp1910_16plus,
    raw_change_mlfp1900_10_16plus = mlfp1910_16plus - mlfp1900_16plus,
    raw_change_mlfp_16plus_SD = sd(raw_change_mlfp1910_20_16plus, na.rm = TRUE),
    
    # Chiswick & Robinson labor force participation rates
    flfp1910_cr = ((flf1910 + flf_famfarm_cr1910) / totf16plus1910) * 100,
    flfp1920_cr = ((flf1920 + flf_famfarm_cr1920) / totf16plus1920) * 100,
    raw_change_flfp_cr = flfp1920_cr - flfp1910_cr,
    
    # Common denominator for 1910/1920
    raw_change_flfp_commondenom = ((flf1920 - flf_ind1051920 - flf_ind8261920) - (flf1910 - flf_ind1051910 - flf_ind8261910)) * 100 / totf16plus1910,
    
    # Non-agriculture labor force participation changes
    raw_change_flfp_nonag = ((flf1920 - flf_ind1051920) / ftot1920) * 100 - ((flf1910 - flf_ind1051910) / ftot1910) * 100,
    raw_change_flfp_nonag16plus = ((flf1920 - flf_ind1051920) / totf16plus1920) * 100 - ((flf1910 - flf_ind1051910) / totf16plus1910) * 100,
    raw_change_flfp_non830all = ((flf1920 - flf_occ1950_8301920) / ftot1920) * 100 - ((flf1910 - flf_occ1950_8301910) / ftot1910) * 100,
    raw_change_flfp_non83016plus = ((flf1920 - flf_occ1950_8301920) / totf16plus1920) * 100 - ((flf1910 - flf_occ1950_8301910) / totf16plus1910) * 100,
    
    # White women only
    wflfp1910 = (wflf1910 / wftot1910) * 100,
    wflfp1920 = (wflf1920 / wftot1920) * 100,
    raw_change_wflfp = wflfp1920 - wflfp1910,
    
    # White women, working-age denominator
    wflfp1900_16plus = (wflf1900 / totwf16plus1900) * 100,
    wflfp1910_16plus = (wflf1910 / totwf16plus1910) * 100,
    wflfp1920_16plus = (wflf1920 / totwf16plus1920) * 100,
    raw_change_wflfp_16plus = wflfp1920_16plus - wflfp1910_16plus,
    raw_change_wflfp_16plus190010 = wflfp1910_16plus - wflfp1900_16plus,
    
    # NPWAI-designated war-related industries
    flf_npwai1900 = rowSums(dplyr::select(., matches("^flf_ind(336|346|358|337|379|357|338|348|388|347|399|309|308|469|307|456|419|477|467|476|468|488|448|487|489|326|316|319|439|436|437|449|446|416|407|417|409|406|418|429|458|459|356|376|386|367|478|378)1900$"))),
    flf_npwai1910 = rowSums(dplyr::select(., matches("^flf_ind(336|346|358|337|379|357|338|348|388|347|399|309|308|469|307|456|419|477|467|476|468|488|448|487|489|326|316|319|439|436|437|449|446|416|407|417|409|406|418|429|458|459|356|376|386|367|478|378)1910$"))),
    flf_npwai1920 = rowSums(dplyr::select(., matches("^flf_ind(336|346|358|337|379|357|338|348|388|347|399|309|308|469|307|456|419|477|467|476|468|488|448|487|489|326|316|319|439|436|437|449|446|416|407|417|409|406|418|429|458|459|356|376|386|367|478|378)1920$"))),
    
    flfp_npwai1900 = (flf_npwai1900 / ftot1900) * 100,
    flfp_npwai1910 = (flf_npwai1910 / ftot1910) * 100,
    flfp_npwai1920 = (flf_npwai1920 / ftot1920) * 100,
    raw_change_flfp_npwai = flfp_npwai1920 - flfp_npwai1910,
    raw_change_flfp_npwai1900_10 = flfp_npwai1910 - flfp_npwai1900,
    
    # Ag 
    pct_ag = (flf_ind1051910 / flf1910) * 100,
    
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
    pct_urb1910 = (urb9101910 / totpop1910) * 100,
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
    
    # Census regions
    census_region = as.factor(case_when(
      icpsrst1920 %in% c(6, 4, 2, 3, 5, 1) ~ 1, # "VT", "NH", "ME", "MA", "RI", "CT"
      icpsrst1920 %in% c(12, 14, 13) ~ 2, # "NJ", "PA", "NY"
      icpsrst1920 %in% c(11, 52, 56, 40, 47, 48, 44, 43) ~ 3, # "DE", "MD", "WV", "VA", "NC", "SC", "GA", "FL"
      icpsrst1920 %in% c(51, 54, 46, 41) ~ 4, # "KY", "TN", "MS", "AL"
      icpsrst1920 %in% c(42, 45, 53, 49) ~ 5, # "AR", "LA", "OK", "TX"
      icpsrst1920 %in% c(66, 61, 62, 67, 65, 63, 68, 64) ~ 6, # "NM", "AZ", "CO", "UT", "NV", "ID", "WY", "MT"
      icpsrst1920 %in% c(73, 72, 71) ~ 7, # "WA", "OR", "CA"
      icpsrst1920 %in% c(36, 37, 35, 32, 33, 31, 34) ~ 9, # "ND", "SD", "NE", "KS", "MN", "IA", "MO"
      icpsrst1920 %in% c(25, 21, 22, 23, 24) ~ 10, # "WI", "IL", "IN", "MI", "OH"
      TRUE ~ NA_real_ # for missing or unclassified states
    )),
    northeast = if_else(census_region %in% c(1, 2), 1, 0, missing = 0),
    south = if_else(census_region %in% c(3, 4, 5), 1, 0, missing = 0),
    west = if_else(census_region %in% c(6, 7), 1, 0, missing = 0),
    midwest = if_else(census_region %in% c(9, 10), 1, 0, missing = 0),
    pct_black_GM = pct_black1910 * (south == 0)
  )







