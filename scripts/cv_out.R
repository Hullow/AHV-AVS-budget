##############################################################
# TUNE TREND SAMPLE RANGE VIA SLIDING-WINDOW CROSSVALIDATION #
################################################################################

# Error metric and aggregator for k-step errors during cross-validation.
err <- function(pred, obs) abs(pred / obs - 1)
agg <- function(x) mean(x)

# Set starting year of training data (historical old-age AHV pensions) for 
# cross-validation. Current choice reflects launch of automatic biannual 
# minimal pension adjustments.
sj <- 1980

# Restrict to viable training data.
t_dat <- 
  na.omit(s_dat) %>% 
  filter(year >= sj)

# Number of points to consider for trend estimation.
ran <- 
  expand.grid(nau = 2:5, mau = 2:5, mch = 2:5)

# Size of training window (equal to number of observations used to estimate 
# top-up on pension sum to arrive at total expenditures).
int <- 16

# Maximal forecast horizon in years.
out <- 10

# List to store forecast performances of respective 'range' fits.
el <- list()

# Cross-validation loop over trend ranges, groups and time windows. -------

for (z in 1:nrow(ran)) {
  
  rant <- slice(ran, z)
  eval <- list()
  
  for (w in 1:(length(unique(t_dat$year)) - (int + out) + 1)) {
    
    rl <- list()
    
      for (i in c("m", "f")) {
        for (j in c("ch", "au")) {
          
          temp <- 
            filter(t_dat, sex == i, dom == j) %>% 
            slice(w : (w + (int + out) - 1)) %>%
            mutate(t = 0:(n() - 1),
                   ind = ifelse(year < 2007, 1, 0))
          
          # Impute native average pension level.
          temp$m[(int + 1):(int + out)] <- NA
          indm <- 
            which(is.na(temp$m))
          
          if (j == "ch") {
            
            fitm <- 
              lm(m ~ t, slice(na.omit(temp), (int - rant$mch):int)) %>% 
              predict(slice(temp, indm))
            
            temp %<>%
              mutate(m = coalesce(m, c(na.omit(temp$m), fitm)))
          }
          
          if (j == "au") {
            
            # Impute foreign average pension level.
            fitm <- 
              lm(m ~ t, slice(na.omit(temp), (int - rant$mau):int)) %>% 
              predict(slice(temp, indm))
            
            temp %<>%
              mutate(m = coalesce(m, c(na.omit(temp$m), fitm)),
                     m = ifelse(m < 0, 0, m))

            # Impute foreign pension count.
            temp$n[(int + 1) : (int + out)] <- NA
            indn <- 
              which(is.na(temp$n))
            fitn <- 
              lm(n ~ t, slice(na.omit(temp), (int - rant$nau):int)) %>% 
              predict(slice(temp, indn))
            
            temp %<>%
              mutate(n = coalesce(n, c(na.omit(temp$n), fitn)),
                     n = ifelse(n < 0, 0, n))
          }
  
          rl[[paste0(i, j)]] <- select(temp, - t)
        }
      }
    
    tra <- 
      bind_rows(rl) %>%
      dplyr::summarise(m = weighted.mean(m, n), n = sum(n),
                       .by = c("year", "mi", "ind")) %>% 
      left_join(zas, by = "year") %>% 
      rename(exp_p = exp_tot)
    
    split <- 
      initial_time_split(tra, prop = int / (int + out))
    
    suppressWarnings(
    fit <- 
      lm(exp_p ~ ind * (n : m : mi), data = training(split)))
    
    temp <- 
      tibble(k    = 1:out,
             obs  = testing(split)$exp_p[1:out], 
             pred = predict(fit, testing(split))[1:out])
    eval[[w]] <- 
      mutate(temp,
             err = err(pred, obs),
             ran = rep(paste0(rant, collapse = ""), n()))
  }
  
  # Save run for specific 'range' combination.
  el[[z]] <- eval
}  
  
# Arrange and aggregate k-step errors of different trend estimation procedures, 
# and select the 'range' combination with minimal average error.
(range <- 
  max(s_dat$year) + 1 - 
  slice(ran, 
    el %>%
    bind_rows %>%
    dplyr::summarise(err = agg(err) * 100, .by = c("ran", "k")) %>%
    pivot_wider(names_from = ran, values_from = err) %>%
    summarise(across(- 1, agg)) %>%
    which.min) %>% as.numeric)

save(range, file = "data/range.rdata")
