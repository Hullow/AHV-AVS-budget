###############################
# READ AND PROCESS INPUT DATA #
################################################################################

# Helper for assignment of '.RDATA' files.
loadRData <- function(fileName) {
  load(fileName)
  get(ls()[ls() != "fileName"]) }

# STATPOP. ------------------------------------------------

pop <- 
  loadRData("data/statpop_2022.rdata") %>% 
  rename(year = jahr, age = alt, pop = bevendejahr) %>% 
  filter(year %in% 2001:(p_start - 2))

save(pop, file = "data/pop.rdata")

# Pension counts. ---------------------------------------------------------

pen_n <-
  read_delim("data/rr_cube.csv", show = FALSE) %>%
  filter(tsex    != "Sexe - total"       ,
         twohn   == "Suisse"             ,
         tnation != "Nationalité - total",
         lage    != - 99999) %>%
  select(year   = an_rr, sex = tsex, nat = tnation, age = lage,
         n_alt  = RAV ,
         n_wit  = RAS ,
         n_waiv = RAOp,
         n_waim = RAOm,
         n_waid = RAOd,
         n_ehe  = RAC ,
         n_kinv = RAEp,
         n_kinm = RAEm) %>%
  mutate(sex = recode(sex, "Homme"    = "m" , "Femme"  = "f"),
         nat = recode(nat, "Etranger" = "au", "Suisse" = "ch"),
         age = ifelse(age >= 100, 100, age)) %>%
  pivot_longer(cols = contains("n_"), names_to = "type", names_prefix = "n_",
               values_to = "n") %>%
  mutate(n = ifelse(type == "waid", 2 * n, n),
         type = fct_collapse(type,
                             wai = c("waiv", "waim", "waid"),
                             kin = c("kinv", "kinm"))) %>% 
  dplyr::summarise(n = sum(n), .by = c("year", "sex", "nat", "age", "type")) %>% 
  filter(year <= p_start - 1)

save(pen_n, file = "data/pen_n.rdata")

# Pension counts and levels. ----------------------------------------------

pen_nm <-
  read_delim("data/rr_cube.csv", show = FALSE) %>%
  filter(tsex    != "Sexe - total"             ,
         twohn   != "Pays de résidence - total",
         tnation == "Nationalité - total"      ,
         lage    != - 99999) %>%
  select(year   = an_rr, sex = tsex, dom = twohn, age = lage,
         # 'n' refers to pension count, 'm' to the relevant mean pension.
         n_alt  = RAV , m_alt  = RAV_moy ,
         n_wit  = RAS , m_wit  = RAS_moy ,
         n_waiv = RAOp, m_waiv = RAOp_moy,
         n_waim = RAOm, m_waim = RAOm_moy,
         n_waid = RAOd, m_waid = RAOd_moy,
         n_ehe  = RAC , m_ehe  = RAC_moy ,
         n_kinv = RAEp, m_kinv = RAEp_moy,
         n_kinm = RAEm, m_kinm = RAEm_moy) %>%
  pivot_longer(cols = contains(c("n_", "m_")), names_to = c("metric", "type"),
               names_sep = "_") %>%
  mutate(sex = recode(sex, "Homme"    = "m" , "Femme"  = "f") ,
         dom = recode(dom, "Etranger" = "au", "Suisse" = "ch")) %>%
  pivot_wider(names_from = metric, values_from = value, values_fn = sum) %>%
  select(year, sex, dom, type, n, m) %>%
  # Account for 'doppelte Waisenrente' as two single orphan pensions and
  # dispense with distinction between father and mother.
  mutate(n = ifelse(type == "waid", 2 * n, n),
         type = fct_collapse(type,
                             wai = c("waiv", "waim", "waid"),
                             kin = c("kinv", "kinm"))) %>% 
  dplyr::summarise(m = weighted.mean(m, n), n = sum(n), 
                   .by = c("year", "sex", "dom", "type")) %>% 
  filter(year <= p_start - 1)

# Take-up rates for different rent types. ---------------------------------

sh <-
  left_join(pop, pen_n, by = c("year", "sex", "nat", "age"),
            relationship = "one-to-many") %>%
  filter(year == p_start - 2) %>%
  mutate(share = n / pop) %>%
  select(- year, - pop, - n) %>%
  na.omit %>% 
  complete(age, sex, nat, type) %>% 
  replace_na(list(share = 0))

save(sh, file = "data/sh.rdata")

# BFS population scenario. ------------------------------------------------

scen <-
  loadRData("data/scenario.rdata") %>%
  dplyr::rename(unit = Beobachtungseinheit, year = Jahr, age = Alter,
                sex  = Geschlecht,
                nat  = `Staatsangehörigkeit..Kategorie.`,
                scen = Szenario.Variante,
                val  = value) %>%
  filter(age  != "Alter - Total", scen == "Referenzszenario A-00-2020",
         nat  != "Staatsangehörigkeit - Total", sex != "Geschlecht - Total",
         unit == "Bevölkerungsstand am 31. Dezember")  %>%
  select(- scen, - unit) %>%
  mutate(age = gsub("1 Jahr$", "1 Jahre", age),
         age = gsub("11 Jahr$", "11 Jahre", age)) %>%
  mutate(age  = as.integer(gsub(" Jahre", "", age)),
         year = as.integer(as.character(year)),
         # Dispense with distinction between EUEFTA vs other foreigners.
         nat   = recode(nat, "Schweiz"           = "ch",
                             "Ausland EWR"       = "au",
                             "Ausland Nicht-EWR" = "au"),
         sex   = recode(sex, "Mann" = "m", "Frau" = "f"),
         # Consolidate age 100+ for consistency with ESPOP/STATPOP.
         age = ifelse(age >= 100, 100, age)) %>%
  dplyr::summarise(val = sum(val), .by = c("year", "sex", "nat", "age")) %>% 
  # Only keep latest observed year for adjustment purposes.
  filter(year >= p_start - 1)

scen %<>% 
  left_join(sh, by = c("sex", "nat", "age"), relationship = "many-to-many") %>%
  # Multiply observed pop counts with rent quotas to arrive at pension counts.
  mutate(heads = val * share) %>%
  dplyr::summarise(heads = sum(heads), .by = c("year", "sex", "nat", "type"))

## Adjust scenario to observed pension counts in 2023.
pen_sc <- 
  pen_n %>% 
  filter(year == p_start - 1) %>%
  dplyr::summarise(n = sum(n), .by = c("sex", "nat", "type"))

adj <-
  filter(scen, year == p_start - 1) %>%
  dplyr::summarise(heads = sum(heads), .by = c("sex", "nat", "type")) %>%
  left_join(pen_sc, by = c("sex", "nat", "type")) %>%
  mutate(adj = n / heads) %>%
  select(- heads, -n)

scen %<>%
  left_join(adj, by = c("sex", "nat", "type")) %>%
  mutate(heads = heads * adj) %>%
  select(- adj) %>%
  dplyr::summarise(heads = sum(heads), .by = c("year", "sex", "type")) %>%
  mutate(dom = "ch") %>%
  select(year, sex, dom, type, n = heads) %>%
  mutate(m = NA) %>%
  filter(year >= p_start)

save(scen, file = "data/scen.rdata")

# Widow projections from complementary model. -----------------------------

wid <- 
  loadRData("data/hila_go.rdata") %>% 
  mutate(type = "wit") %>% 
  select(year, sex = csex_hila, dom = cdom, type, m = hila_rel2,
         n = benefs_go) %>% 
  mutate(sex = recode(sex, `1` = "m", `2` = "f"),
         dom = recode(dom, `100` = "ch", `900` = "au"))

save(wid, file = "data/wid.rdata")

# Historic and projected minimal pensions & inflation. --------------------

# Source for minimal pensions pre-2001: 
# https://www.123-pensionierung.ch/ahv/hoehe-ahv/
mi <- 
  tibble(year = 1975:2000,
         mi = c(500, 500, 525 , 525, 525, 550, 550, 620, 620, 690, 690, 
                720, 720, 750 , 750, 800, 800, 900, 940, 940, 970, 970,
                995, 995, 1005, 1005)) %>% 
  bind_rows(read_delim("data/mpen_2024_juni.csv", show = F) %>% 
              select(year = jahr, mi = minimalrente) %>% 
              filter(year >= 2001))

inf <- 
  read_delim("data/discount_2024_juni.csv", show = FALSE) %>%
  select(year = jahr, df = diskontfaktor) %>% 
  filter(year >= p_start)

# Consolidation into final analysis data for regression. ------------------

a_dat <-
  bind_rows(pen_nm, scen) %>%
  complete(year, dom, sex, type) %>%
  left_join(mi, by = "year") %>%
  filter(!(is.na(type))) %>%
  arrange(dom, sex, type, year) %>%
  # Express mean rents as multiples of contemporaneous minimal rents.
  mutate(m = m / mi) %>% 
  mutate(mi = 12 * mi)

save(a_dat, file = "data/a_dat.rdata")

# Historic data for cross-validation. -------------------------------------     

ahv <- 
  read_delim("data/ahv_data.csv", show = FALSE) %>% 
  filter(Art == "Altersrente") %>% 
  pivot_longer(cols = `1948`:`2023`, names_to = 'year') %>%
  mutate(year = as.numeric(year)) %>% 
  filter(year %in% 1975:2000) %>% 
  mutate(value = as.numeric(gsub("'", "", value))) %>% 
  select(year, sex = Geschlecht, dom = Domizil, type = Art, 
         metric = Beobachtungseinheit, value) %>% 
  mutate(dom  = recode(dom , "Schweiz" = "ch", "Ausland" = "au"),
         sex  = recode(sex , "Männer" = "m", "Frauen" = "f"),
         type = recode(type, "Altersrente" = "alt"),
         metric = recode(metric, "Anzahl Renten" = "n", 
                         "Renten-Mittelwert in Franken" = "m")) %>% 
  pivot_wider(names_from = metric) %>% 
  select(everything(), m)

s_dat <- 
  bind_rows(ahv, pen_nm)  %>%
  left_join(mi , by = "year") %>% 
  mutate(m = m / mi) %>% 
  group_by(sex, dom) %>% 
  arrange(sex, dom, year) %>%
  filter(dom %in% c("ch", "au"), type == "alt", year <= p_start - 1) %>% 
  ungroup

# Heuristic smoothing of jumps due to law changes via splines.
s_dat %<>%
  mutate(m = na.spline(ifelse(dom == "au" & sex == "f" & year %in% 1993:2003, 
                              NA, m)),
         m = na.spline(ifelse(dom == "au" & sex == "m" & year %in% 1993:2004, 
                              NA, m)),
         n = na.spline(ifelse(dom == "ch" &  sex == "f" & 
                                year %in% 2000:2010, NA, n)),
         m = na.spline(ifelse(dom == "ch" & year %in% 1991:1994, NA, m)),
         n = na.spline(ifelse(dom == "au" &  sex == "m" & 
                                year %in% 2001:2001, NA, n)),
         m = na.spline(ifelse(dom == "ch" & year %in% 2000:2001, NA, m)))

save(s_dat, file = "data/s_dat.rdata")

# Yearly ZAS expenditure data. --------------------------------------------

zas <- 
  read_excel("data/sv_ahv_fin.xlsx", sheet = "daten", skip = 10) %>%
  select(year = jahr, exp_tot = aus_tot)

save(zas, file = "data/zas.rdata")
