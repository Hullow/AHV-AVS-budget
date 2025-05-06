##########################################################
# IMPUTE FUTURE FOREIGN POPULATION COUNTS AND MEAN RENTS #
################################################################################

# Read in final analysis data.
a_dat <- 
  loadRData("data/a_dat.rdata")

# Read in historical total AHV expenditures from the ZAS Betriebsrechnung.
zas <- 
  loadRData("data/zas.rdata") %>% 
  mutate(exp_tot = exp_tot * 1000000) %>% 
  filter(year <= p_start - 1)

# Cost effect of AHV21 as estimated by Delfin in billions (without polynomial
# distortions and Eckwerte from 2024).
AHV21_cost <- 
  tibble(year = 2024:2040, 
         cost =
  c(   73, - 140, - 386, - 632, - 849, - 770, - 644, - 528, - 414, - 320, - 250,
    - 230, - 150, - 120, - 70, -  40, -  10) / 1000)

# Fix first year whose data is used for extrapolation. Multiple choices lead to
# an averaging of the results over the chosen basis years. The current value has
# been determined via cross-validation on historical data since 1975 (smoothed
# to account for law changes).
range <-
  loadRData("data/range.rdata")

# Set up list to collect results in case of multi-valued 'range' parameter.
rl <- list()

# Imputation over rent type, sex, and domicile.
rl[["au"]] <-
  a_dat %>%
  filter(dom == "au") %>%
  mutate(tn = ifelse(year >= range[1], year - range[1], NA),
         tm = ifelse(year >= range[2], year - range[2], NA)) %>% 
  # Extrapolate rent counts and mean rents with a linear time trend. This is
  # done separately for the Cartesian product of rent type, sex, and domicile.
  impute_lm(n ~ tn | type + sex) %>%
  impute_lm(m ~ tm | type + sex) %>%
  # Prevent eventual negative predictions due to linear extrapolation.
  mutate(m = ifelse(m < 0, 0, m),
         n = ifelse(n < 0, 0, n)) %>% 
  select(- tn, - tm)

rl[["ch"]] <-
  a_dat %>%
  filter(dom == "ch") %>%
  mutate(tm = ifelse(year >= range[3], year - range[3], NA)) %>% 
  # Extrapolate rent counts and mean rents with a linear time trend. This is
  # done separately for the Cartesian product of rent type, sex, and domicile.
  impute_lm(m ~ tm | type + sex) %>%
  mutate(m = ifelse(m < 0, 0, m)) %>% 
  select(- tm)

if (ind_wid) {
  
  # Replace average pension projections for male widows with exogenous
  # vector based on consolidated STATPOP & rent registry data (author:
  # Thomas Borek).
  wid <- 
    loadRData("data/wid.rdata") %>% 
    left_join(mi, by = "year") %>% 
    mutate(mi = 12 * mi) %>% 
    filter(year <= p_end)
    
  p_dat <-
    bind_rows(rl) %>%
    mutate(m = ifelse(type == "alt" & year >= 2026,
                      m * (1 + ind_AHV13 * 1/12), m)) %>% 
    filter(!(type == "wit" & dom == "au" & sex == "m" & year >= p_start)) %>% 
    bind_rows(filter(wid, dom == "au", sex == "m")) %>%
    # Unify pension types into a global synthetic pension by taking a weighted 
    # average of the respective mean payments and summing all counts.
    dplyr::summarise(m = weighted.mean(m, n), n = sum(n), 
                     .by = c("year", "mi")) %>%
    left_join(zas, by = "year") %>%
    mutate(exp_p = exp_tot) %>%
    select(- exp_tot)
  
} else {
  
  p_dat <-
    bind_rows(rl) %>%
    mutate(m = ifelse(type == "alt" & year >= 2026,
                      m * (1 + ind_AHV13 * 1/12), m)) %>%
    # Unify pension types into a global synthetic pension by taking a weighted 
    # average of the respective mean payments and summing all counts.
    dplyr::summarise(m = weighted.mean(m, n), n = sum(n),
                     .by = c("year", "mi")) %>%
    left_join(zas, by = "year") %>%
    select(- exp_tot)
}

# Fit model and produce projections.
fit <-
  lm(exp_p ~ 0 + n : m : mi, filter(p_dat, year %in% 2008:(p_start - 1)))

res <-
  predict(fit, filter(p_dat, year %in% p_start:p_end))

# Consolidate results.
rtab <-
  bind_rows(res) %>%
  pivot_longer(cols = !contains("range"), names_to = "year") %>%
  mutate(year = as.numeric(year) + p_start - 1) %>%
  # Express monetary values in billions.
  mutate(value = value / 10e8) %>%
  dplyr::rename(exp_p = value) %>%
  left_join(inf, by = "year") %>% 
  left_join(AHV21_cost, by = "year") %>% 
  replace_na(list(cost = 0)) %>% 
  filter(year %in% p_start:p_end)

# Adjust predictions by the adapted Delfin AHV21 cost projections and fix prices 
# at the latest observed year.
if (ind_AHV21) 
  rtab %<>% mutate(exp_p = exp_p + cost)

if (ind_real)
  rtab %<>% mutate(exp_p = exp_p * df  )

# Print and save final projections.
(write_delim(select(rtab, year, exp_p), file = "data/proj_base.csv",
             delim = ";"))
