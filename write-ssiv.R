### Compute the Shift-Share IV

# Import data
  # Industry sizes, NPWAI
  npwai <- read_excel("raw/npwai/New Position of Women in American Industry - Digitized.xlsx", sheet = 2)
  # Industry sizes, Census (Created by raw-census-counts.do on NBER Server, 9/22/2022)
  nber1910 <- read_dta("raw/census/1910-raw-census-counts.dta")
  nber1920 <- read_dta("raw/census/1920-raw-census-counts.dta")
  # Pre-treatment county-industry shares (Created by industries.do on NBER Server, 3/19/2022)
  ind_ci1910 <- read_dta("raw/census/1910-industry-shares-ctyJUNE2023.dta")
  ind_ci1900 <- read_dta("raw/census/1900-industry-shares-cty.dta")
  
# Aggregate NPWAI to industry/national level
npwai_i <- npwai %>% 
  group_by(ind1950, label1950) %>%
  summarise(nfr = sum(nfr, na.rm = TRUE), # number of firms reporting
            twe1 = sum(twe1, na.rm = TRUE), # total wage earners after 1st draft
            twe2 = sum(twe2, na.rm = TRUE), # total wage earners after 2nd draft
            we1f = sum(we1f, na.rm = TRUE), # wage earners, 1st draft, female
            we2f = sum(we2f, na.rm = TRUE), # wage earners, 2nd draft, female
            we1m = sum(we1m, na.rm = TRUE), # wage earners, 1st draft, male
            we2m = sum(we2m, na.rm = TRUE)  # wage earners, 2nd draft, male
  ) %>%
  ungroup()

# Aggregate Census employment counts up to industry/national level:
i1910 <- nber1910 %>% 
  filter(occ1950<=970) %>% # Count only those with a real wage-earning occupation
  group_by(ind1950, sex) %>%
  summarise(lf_count = sum(count, na.rm=TRUE))
i1910$sex <- ifelse(test = i1910$sex==1, yes = "mlf1910", no = "flf1910")
i1910 <- spread(data = i1910, key = sex, value = lf_count)
# i1910[is.na(i1910)] <- 0

i1920 <- nber1920 %>% 
  filter(occ1950<=970) %>%
  group_by(ind1950, sex) %>%
  summarise(lf_count = sum(count, na.rm=TRUE))
i1920$sex <- ifelse(test = i1920$sex==1, yes = "mlf1920", no = "flf1920")
i1920 <- spread(data = i1920, key = sex, value = lf_count)
# i1920[is.na(i1920)] <- 0

i <- merge(x = i1910, y = i1920, by = "ind1950", all = TRUE)

# Add NPWAI to i
i <- merge(x = i, y = npwai_i, by = "ind1950", all=TRUE)

# Drop:
# 979 Not yet specified
# 984 Retired
# 997 Nonclassifiable
# 105 Agriculture (Goldin)
# 826 Private Households (Goldin)
i <- i[!(i$ind1950 %in% c(979, 984, 997, 0)), ]

# To prevent NA growth rates, replace NA Census counts with 1
i$flf1910[is.na(i$flf1910)] <- 1
i$flf1920[is.na(i$flf1920)] <- 1

# Industry i growth rate according to Census
i$g_iCensusF <- (i$flf1920 - i$flf1910)/i$flf1910
i$g_iCensusM <- (i$mlf1920 - i$mlf1910)/i$mlf1910

# Industry i growth rate according to NPWAI
i$g_iNpwaiF <- (i$we2f - i$we1f)/i$we1f
i$g_iNpwaiM <- (i$we2m - i$we1m)/i$we1m

# Assume g_iNpwaiF0=0 if i not in NPWAI data
i$g_iNpwaiF0 <- ifelse(test = is.na(i$g_iNpwaiF), yes = 0, no = i$g_iNpwaiF)
i$npwai_flag <- ifelse(test = is.na(i$g_iNpwaiF), yes = 0, no = 1)

# PREDICT g_iCensus using g_iNpwaiF0
fit <- lm(g_iCensusF ~ g_iNpwaiF0 + npwai_flag -1, data = i)
summary(fit)
i$g_iHat <- predict(fit)

# SHIFT
# i$predicted_shiftHat <- (i$g_iHat) * i$flf1910
# i$predicted_shiftCensusF <- (i$g_iCensusF) * i$flf1910
# i$predicted_shiftCensusM <- (i$g_iCensusM) * i$flf1910
# i$predicted_shiftNpwaiF <- (i$g_iNpwaiF) * i$flf1910
# i$predicted_shiftNpwaiF0 <- (i$g_iNpwaiF0) * i$flf1910
# i$predicted_shiftNpwaiM <- (i$g_iNpwaiM) * i$flf1910

# SHARES
# MERGE TO data DF POST-AGGREGATION FROM CNTY TO CD.
ind_data <- data %>% 
  dplyr::select(ID_STATEDIST, starts_with("flf_ind"), starts_with("mlf_ind")) %>%
  pivot_longer(
    cols = -ID_STATEDIST,
    names_to = c("sex", "ind1950", "year"),
    names_pattern = "(flf|mlf)_ind(\\d{3})(19\\d{2})",
    values_to = "value"
  ) %>%
  mutate(sex_year = paste0(sex, "_", year)) %>% 
  dplyr::select(-c(year, sex)) %>%
  filter(!is.na(ind1950)) %>%
  pivot_wider(
    names_from = sex_year,
    values_from = value
  )

ind_data <- merge(x = ind_data, y = i, by = "ind1950", all.x=TRUE)
ind_data$predicted_shiftHat <- (ind_data$g_iHat) * ind_data$flf1910
ind_data$predicted_shiftCensusF <- (ind_data$g_iCensusF) * ind_data$flf1910
ind_data$predicted_shiftCensusM <- (ind_data$g_iCensusM) * ind_data$flf1910
ind_data$predicted_shiftNpwaiF <- (ind_data$g_iNpwaiF) * ind_data$flf1910
ind_data$predicted_shiftNpwaiF0 <- (ind_data$g_iNpwaiF0) * ind_data$flf1910
ind_data$predicted_shiftNpwaiM <- (ind_data$g_iNpwaiM) * ind_data$flf1910

test <- ind_data %>%
  dplyr::select(ID_STATEDIST, starts_with("predicted")) %>%
  group_by(ID_STATEDIST) %>%
  summarise(
    predicted_shiftHat = sum(predicted_shiftHat, na.rm = TRUE),
    predicted_shiftCensusF = sum(predicted_shiftCensusF, na.rm = TRUE),
    predicted_shiftCensusM = sum(predicted_shiftCensusM, na.rm = TRUE),
    predicted_shiftNpwaiF = sum(predicted_shiftNpwaiF, na.rm = TRUE),
    predicted_shiftNpwaiF0 = sum(predicted_shiftNpwaiF0, na.rm = TRUE), # Main spec, with 0 growth assigned to non-NPWAI
    predicted_shiftNpwaiM = sum(predicted_shiftNpwaiM, na.rm = TRUE)
  )

# Merge back into "data" and divide "predicted" by preferred female population count (e.g. totf16plus1910)

# Close script
rm(list = c(
  "npwai_i"
))
