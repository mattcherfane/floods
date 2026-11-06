
library(raster)
library(sf)
#Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE
sf_use_s2(FALSE)
library(lwgeom)
library(stargazer)
library(AER)
library(car)

# (Requires objects created by the earlier scripts: run 01-06 in order in one R session)

# =========================
# State By State Baseline Coefficients & Table (With Panel Updates)
# =========================
library(dplyr)
library(sandwich)
library(lmtest)
library(knitr)

clean_num <- function(x, digits = 2) {x[abs(x) < 0.5 * 10^(-digits)] <- 0 
  formatC(x, format = "f", digits = digits, big.mark = ",")}

clean_r2 <- function(x) {
  ifelse(is.nan(x) | is.na(x), "—", formatC(x, format = "f", digits = 3))}

state_fips_lookup <- data.frame(
  state_fips = c(0, 1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18,
                 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
                 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45,
                 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  state = c("National", "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
            "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
            "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
            "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
            "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
            "WY"))

state_formula <- claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016
state_list <- sort(unique(td0$state_fips))
state_results <- list()

pooled_formula <- claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips)
mod_pool <- lm(pooled_formula, data = td0)
vc_pool <- sandwich::vcovCL(mod_pool, cluster = td0$county_fips, type = "HC1")
ct_pool <- lmtest::coeftest(mod_pool, vcov. = vc_pool)
state_results[["0"]] <- data.frame(
  state_fips = 0, coef = ct_pool["wetland_2001_2016", "Estimate"], se = ct_pool["wetland_2001_2016", "Std. Error"], pval = ct_pool["wetland_2001_2016", "Pr(>|t|)"],
  n = nobs(mod_pool), r2 = summary(mod_pool)$r.squared, mean_claims_000s = mean(td0$claims_2001_2016, na.rm = TRUE) / 1000)

for (s in state_list) {
  df_s <- td0 %>% filter(state_fips == s)
  mod_s <- lm(state_formula, data = df_s)
  if ("county_fips" %in% names(df_s) && length(unique(na.omit(df_s$county_fips))) > 1) {vc_s <- sandwich::vcovCL(mod_s, cluster = df_s$county_fips, type = "HC1")} 
  else {vc_s <- sandwich::vcovHC(mod_s, type = "HC1")}
  
  ct_s <- lmtest::coeftest(mod_s, vcov. = vc_s)
  state_results[[as.character(s)]] <- data.frame(state_fips = s,
    coef = ct_s["wetland_2001_2016", "Estimate"], se = ct_s["wetland_2001_2016", "Std. Error"], pval = ct_s["wetland_2001_2016", "Pr(>|t|)"],
    n = nobs(mod_s), r2 = summary(mod_s)$r.squared, mean_claims_000s = mean(df_s$claims_2001_2016, na.rm = TRUE) / 1000)}

state_coef_table <- do.call(rbind, state_results) %>% left_join(state_fips_lookup, by = "state_fips") %>%
  mutate(stars = case_when(pval < 0.01 ~ "***", pval < 0.05 ~ "**", pval < 0.10 ~ "*", TRUE ~ ""),
         wetland_coef = paste0(clean_num(coef, 2), stars),
         wetland_coef_nostars = clean_num(coef, 2),
         std_error = paste0("(", clean_num(se, 2), ")"),
         r2_clean = clean_r2(r2),
         sort_order = ifelse(state == "National", 0, 1)) %>% arrange(sort_order, state)

state_coef_table_out <- state_coef_table %>% select(State = state, `Wetland coefficient` = wetland_coef, `Std. error` = std_error, Observations = n, `Mean claims change ($000s)` = mean_claims_000s, `R-squared` = r2_clean)
state_coef_latex <- knitr::kable(state_coef_table_out, format = "latex", booktabs = TRUE, digits = 3, align = "lccccc")
writeLines(state_coef_latex, "output/tables/state_by_state_baseline_wetland_coefficients.tex")

# =========================
# State-by-State, No Stars
# =========================

state_coef_table_out_nostars <- state_coef_table %>% mutate(wetland_coef_nostars = formatC(coef, format = "f", digits = 2, big.mark = ",")) %>% select(State = state, `Wetland coefficient` = wetland_coef_nostars, `Std. error` = std_error, Observations = n, `Mean claims change ($000s)` = mean_claims_000s, `R-squared` = r2_clean)
state_coef_latex_nostars <- knitr::kable(state_coef_table_out_nostars, format = "latex", booktabs = TRUE, digits = 3, align = "lccccc")
writeLines(state_coef_latex_nostars,"output/tables/state_by_state_baseline_wetland_coefficients_nostars.tex")

# =========================
# New NFIP Claims Column: Non TX/LA US (For all 3 figure tables)
# =========================

claims_df <- td0 %>% st_drop_geometry() %>% mutate(state_group = case_when(state_fips == 48 ~ "TX", state_fips == 22 ~ "LA", TRUE ~ "US Other"), ind_Harvey = ifelse(is.na(ind_Harvey), 0, ind_Harvey))
make_claims_col <- function(df, group_name) {
  df_g <- df %>% filter(state_group == group_name)
  total_change <- sum(as.numeric(df_g$claims_2001_2016), na.rm = TRUE)
  inside_change <- sum(as.numeric(df_g$claims_2001_2016)[df_g$ind_Harvey == 1], na.rm = TRUE)
  outside_change <- sum(as.numeric(df_g$claims_2001_2016)[df_g$ind_Harvey == 0], na.rm = TRUE)
  c(sum(as.numeric(df_g$claims_2001_5yr), na.rm = TRUE), sum(as.numeric(df_g$claims_2016_5yr), na.rm = TRUE), total_change, outside_change, inside_change, 100 * inside_change / total_change)}
TX_vals <- make_claims_col(claims_df, "TX")
LA_vals <- make_claims_col(claims_df, "LA")
Other_vals <- make_claims_col(claims_df, "US Other")

claims_summary <- data.frame(
  row = c(
    "Flood claims in 2001 (000s)",
    "Flood claims in 2016 (000s)",
    "Change in flood claims, 2001--2016 (000s)",
    "Change in flood claims outside Harvey ZIPs (000s)",
    "Change in flood claims inside Harvey ZIPs (000s)",
    "Share of total change inside Harvey ZIPs (%)"),
  TX = c(round(TX_vals[1:5] / 1000, 2), round(TX_vals[6], 2)), LA = c(round(LA_vals[1:5] / 1000, 2), round(LA_vals[6], 2)), `US Other` = c(round(Other_vals[1:5] / 1000, 2), round(Other_vals[6], 2)), check.names = FALSE)
claims_summary
print(xtable::xtable(claims_summary, align = c("l", "l", "r", "r", "r")), include.rownames = FALSE, include.colnames = TRUE, floating = FALSE, file = "output/tables/claims_summary_tx_la_other.tex")

# =========================
# New Wetlands Change Column: Non TX/LA US (For all 3 figure tables)
# =========================

wetland_df <- td0 %>% st_drop_geometry() %>% mutate(state_group = case_when(state_fips == 48 ~ "TX", state_fips == 22 ~ "LA", TRUE ~ "US Other"), ind_Harvey = ifelse(is.na(ind_Harvey), 0, ind_Harvey))
make_wetland_col <- function(df, group_name) {
  df_g <- df %>% filter(state_group == group_name)
  total_change <- sum(as.numeric(df_g$wetland_2001_2016), na.rm = TRUE)
  inside_change <- sum(as.numeric(df_g$wetland_2001_2016)[df_g$ind_Harvey == 1], na.rm = TRUE)
  outside_change <- sum(as.numeric(df_g$wetland_2001_2016)[df_g$ind_Harvey == 0], na.rm = TRUE)
  c(sum(as.numeric(df_g$wetland_ha_2001), na.rm = TRUE), sum(as.numeric(df_g$wetland_ha_2016), na.rm = TRUE), total_change, outside_change, inside_change, 100 * inside_change / total_change)}
TX_wet <- make_wetland_col(wetland_df, "TX")
LA_wet <- make_wetland_col(wetland_df, "LA")
Other_wet <- make_wetland_col(wetland_df, "US Other")

wetland_summary <- data.frame(
  row = c(
    "Wetland area in 2001 (ha)",
    "Wetland area in 2016 (ha)",
    "Net wetland change, 2001--2016 (ha)",
    "Net wetland change outside Harvey ZIPs (ha)",
    "Net wetland change inside Harvey ZIPs (ha)",
    "Share of total wetland change inside Harvey ZIPs (%)"),
  TX = round(TX_wet, 2), LA = round(LA_wet, 2), `US Other` = round(Other_wet, 2), check.names = FALSE)

wetland_summary_out <- wetland_summary
wetland_summary_out[, c("TX", "LA", "US Other")] <- lapply(wetland_summary_out[, c("TX", "LA", "US Other")], fmt_latex_num, digits = 2)
wetland_summary_out$row <- gsub("%", "\\\\%", wetland_summary_out$row)
print(xtable::xtable(wetland_summary_out, align = c("l", "l", "r", "r", "r")), include.rownames = FALSE, include.colnames = TRUE, floating = FALSE,sanitize.text.function = identity,
  file = "output/tables/wetland_summary_tx_la_other.tex")

# =========================
# WR Table Replications
# =========================

# TABLE 1
td0_wr <- td0 %>% st_drop_geometry()
td0_nfhl_wr <- td0_nfhl %>% st_drop_geometry() %>% mutate(share_A = ifelse(is.na(share_A), 0, share_A), share_V = ifelse(is.na(share_V), 0, share_V), share_nfhl = ifelse(is.na(share_nfhl), 0, share_nfhl))
qz <- quantile(td0_wr$claims_2001_2016, c(.001, .005, .01, .99, .995, .999), na.rm = TRUE)
wr_ld_formula <- claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips)

wr_ld_harvey <- claims_2001_2016 ~ wetland_2001_2016*ind_Harvey + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips)
wr_ld_nfhl <- claims_2001_2016 ~ wetland_2001_2016*share_nfhl + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips)

wr_samples <- list(
  "All" = td0_wr,
  "99pct" = subset(td0_wr, claims_2001_2016 > qz[2] & claims_2001_2016 < qz[5]),
  "98pct" = subset(td0_wr, claims_2001_2016 > qz[3] & claims_2001_2016 < qz[4]),
  "No TX/LA" = subset(td0_wr, !(state_fips %in% c(48, 22, "48", "22"))),
  "Harvey" = td0_wr,
  "NFHL" = td0_nfhl_wr)

wr_formulas <- list(wr_ld_formula, wr_ld_formula, wr_ld_formula, wr_ld_formula, wr_ld_harvey, wr_ld_nfhl)
zip_ld <- list()
zip_ld_se <- list()

for (i in seq_along(wr_samples)) {
  dat <- wr_samples[[i]] %>% st_drop_geometry()
  form <- wr_formulas[[i]]
  needed_vars <- unique(c(all.vars(form), "county_fips"))
  dat_fit <- dat[complete.cases(dat[, needed_vars]), ]
  zip_ld[[i]] <- lm(form, data = dat_fit)
  zip_ld_se[[i]] <- sqrt(diag(sandwich::vcovCL(zip_ld[[i]], cluster = dat_fit$county_fips, type = "HC1")))}

stargazer(zip_ld, se = zip_ld_se, type = "latex", keep = c("wetland_2001_2016", "wetland_2001_2016:ind_Harvey", "wetland_2001_2016:share_nfhl"), covariate.labels = c("Wetland change, 2001--2016", "Wetland change $\\times$ Harvey zip code", "Wetland change $\\times$ NFHL share"), column.labels = names(wr_samples), dep.var.labels = "Change in zip code-level NFIP claims (US\\$)", omit.stat = c("f", "ser"), no.space = TRUE, header = FALSE, float = FALSE, notes = "", notes.label = "", notes.append = FALSE, star.cutoffs = NA, 
          out = "output/tables/wr_zip_ld_trimmed_new.tex")

# Table 2: average wetland value calculations with trimming, No TX/LA, Harvey interaction, and NFHL interaction

td0_wr <- td0 %>% st_drop_geometry() %>% mutate(state_fips_chr = as.character(state_fips), zip = as.character(zip))
td_wr <- td %>% st_drop_geometry() %>% mutate(state_fips_chr = as.character(state_fips), zip = as.character(zip))
td0_nfhl_wr <- td0_nfhl %>% st_drop_geometry() %>% mutate(share_A = ifelse(is.na(share_A), 0, share_A), share_V = ifelse(is.na(share_V), 0, share_V), share_nfhl = ifelse(is.na(share_nfhl), 0, share_nfhl)) %>% mutate(state_fips_chr = as.character(state_fips), zip = as.character(zip))

if (!("ind_Harvey" %in% names(td_wr)) & "zip" %in% names(td_wr)) {
  td_wr <- td_wr %>% left_join(td0_wr %>% select(zip, ind_Harvey), by = "zip")
}

qz <- quantile(td0_wr$claims_2001_2016, c(.001, .005, .01, .99, .995, .999), na.rm = TRUE)

loss_lags <- c("wetland_loss", "wetlands_loss_up_0_10", "wetlands_loss_up_10_20", "wetlands_loss_up_20_30", "wetlands_loss_up_30_40", "wetlands_loss_up_40_50", "wetlands_loss_up_50_60", "wetlands_loss_up_60_Inf")
gain_lags <- c("wetland_gain", "wetlands_gain_up_0_10", "wetlands_gain_up_10_20", "wetlands_gain_up_20_30", "wetlands_gain_up_30_40", "wetlands_gain_up_40_50", "wetlands_gain_up_50_60", "wetlands_gain_up_60_Inf")
loss_did <- c("wetland_loss", "wetlands_loss_up")
gain_did <- c("wetland_gain", "wetlands_gain_up", "wetlands_gain_all", "wetlands_loss_all")
controls <- c("developed_2001_2016", "income_2001_2016", "population_2001_2016", "housing_units_2001_2016", "housing_value_2001_2016", "CRS_2001_2016", "factor(state_fips)")

avg_calc <- function(est_data, avg_data, gain_vars, loss_vars, interact_var = NULL) {
  wet_terms <- c(gain_vars, loss_vars)
  control_vars <- c("developed_2001_2016", "income_2001_2016", "population_2001_2016", "housing_units_2001_2016", "housing_value_2001_2016", "CRS_2001_2016")
  num_vars <- unique(c("claims_2001_2016", wet_terms, control_vars, interact_var))
  est_data <- est_data %>% mutate(across(any_of(num_vars), ~ as.numeric(.)))
  avg_data <- avg_data %>% mutate(across(any_of(unique(c(loss_vars, interact_var))), ~ as.numeric(.)))
  rhs <- if (is.null(interact_var)) {
    paste(c(wet_terms, control_vars, "factor(state_fips)"), collapse = " + ")
  } else {
    paste0("(", paste(wet_terms, collapse = " + "), ") * ", interact_var, " + ", paste(c(control_vars, "factor(state_fips)"), collapse = " + "))
  }
  form <- as.formula(paste("claims_2001_2016 ~", rhs))
  need_est <- unique(all.vars(form))
  need_avg <- unique(c(loss_vars, interact_var))
  est_data <- est_data[complete.cases(est_data[, need_est]), ]
  avg_data <- avg_data[complete.cases(avg_data[, need_avg]), ]
  b <- coef(lm(form, data = est_data))
  total_loss <- 0
  for (v in loss_vars) {
    base <- ifelse(v %in% names(b), b[v], 0)
    if (!is.null(interact_var)) {
      int1 <- paste0(v, ":", interact_var)
      int2 <- paste0(interact_var, ":", v)
      int <- ifelse(int1 %in% names(b), b[int1], ifelse(int2 %in% names(b), b[int2], 0))
      base <- as.numeric(base) + as.numeric(int) * avg_data[[interact_var]]
    }
    total_loss <- total_loss + sum(as.numeric(base) * avg_data[[v]], na.rm = TRUE)
  }
  total_loss / abs(sum(avg_data$wetland_loss, na.rm = TRUE))
}

run_ld <- function(est_data, avg_data, interact_var = NULL) avg_calc(est_data, avg_data, gain_lags, loss_lags, interact_var)
run_did <- function(est_data, avg_data, interact_var = NULL) avg_calc(est_data, avg_data, gain_did, loss_did, interact_var)

s_all <- td0_wr
s_99 <- td0_wr %>% filter(claims_2001_2016 > qz[2], claims_2001_2016 < qz[5])
s_98 <- td0_wr %>% filter(claims_2001_2016 > qz[3], claims_2001_2016 < qz[4])
s_noharvey <- td0_wr %>% filter(!(state_fips_chr %in% c("48", "22")))
s_harvey <- td0_wr
s_nfhl <- td0_nfhl_wr

samples_ld <- list(s_all, s_99, s_98, s_noharvey)
samples_did <- lapply(samples_ld, function(x) x %>% filter(coastal == FALSE))

avg_all <- td_wr
avg_urban <- td_wr %>% filter(developed_percent >= .1)
avg_rural <- td_wr %>% filter(developed_percent < .1)
avg_nfhl <- td0_nfhl_wr
avg_nfhl_urban <- td0_nfhl_wr %>% filter(developed_percent >= .1)
avg_nfhl_rural <- td0_nfhl_wr %>% filter(developed_percent < .1)

ld_lags_avg <- c(sapply(samples_ld, run_ld, avg_data = avg_all), run_ld(s_harvey, avg_all, "ind_Harvey"), run_ld(s_nfhl, avg_nfhl, "share_nfhl"))
did_avg_2 <- c(sapply(lapply(samples_ld, function(x) x %>% filter(coastal == FALSE)), run_did, avg_data = avg_all), run_did(s_harvey %>% filter(coastal == FALSE), avg_all, "ind_Harvey"), run_did(s_nfhl %>% filter(coastal == FALSE), avg_nfhl, "share_nfhl"))
ld_lags_avg_urban <- c(sapply(lapply(samples_ld, function(x) x %>% filter(developed_percent >= .1)), run_ld, avg_data = avg_urban), run_ld(s_harvey %>% filter(developed_percent >= .1), avg_urban, "ind_Harvey"), run_ld(s_nfhl %>% filter(developed_percent >= .1), avg_nfhl_urban, "share_nfhl"))
did_avg_2_urban <- c(sapply(lapply(samples_did, function(x) x %>% filter(developed_percent >= .1)), run_did, avg_data = avg_urban), run_did(s_harvey %>% filter(coastal == FALSE, developed_percent >= .1), avg_urban, "ind_Harvey"), run_did(s_nfhl %>% filter(coastal == FALSE, developed_percent >= .1), avg_nfhl_urban, "share_nfhl"))
ld_lags_avg_rural <- c(sapply(lapply(samples_ld, function(x) x %>% filter(developed_percent < .1)), run_ld, avg_data = avg_rural), run_ld(s_harvey %>% filter(developed_percent < .1), avg_rural, "ind_Harvey"), run_ld(s_nfhl %>% filter(developed_percent < .1), avg_nfhl_rural, "share_nfhl"))
did_avg_2_rural <- c(sapply(lapply(samples_did, function(x) x %>% filter(developed_percent < .1)), run_did, avg_data = avg_rural), run_did(s_harvey %>% filter(coastal == FALSE, developed_percent < .1), avg_rural, "ind_Harvey"), run_did(s_nfhl %>% filter(coastal == FALSE, developed_percent < .1), avg_nfhl_rural, "share_nfhl"))

tab_avg <- rbind(ld_lags_avg, did_avg_2, ld_lags_avg_urban, did_avg_2_urban, ld_lags_avg_rural, did_avg_2_rural)

rownames(tab_avg) <- c("Spillovers, spatial lags model", "Spillovers, DID", "Spillovers, spatial lags model, developed areas only", "Spillovers, DID, developed areas only", "Spillovers, spatial lags model, undeveloped areas only", "Spillovers, DID, undeveloped areas only")
colnames(tab_avg) <- c("All", "99pct", "98pct", "No TX/LA", "Harvey", "NFHL")

avg_tex <- capture.output(stargazer(tab_avg, digits = 0, type = "latex", header = FALSE, summary = FALSE, float = FALSE))
avg_tex <- gsub("cccccc", "lcccccc", avg_tex)
avg_tex <- gsub("Spillovers, spatial lags model ", "Spillovers, spatial lags model (\\\\$/ha/year)", avg_tex)
avg_tex <- gsub("Spillovers, spatial lags model, developed areas", "\\\\\\\\[-1.8ex]\\\\hline Spillovers, spatial lags model, developed areas", avg_tex)
avg_tex <- gsub("\\$-\\$", "-", avg_tex)
avg_tex <- gsub("\\\\end\\{tabular\\}", "\\\\hline \\\\end{tabular}", avg_tex)

writeLines(avg_tex, "output/tables/wr_avg_value_new.tex")
tab_avg

# =========================
# WINSORIZATION
# =========================

#1/99pct claims
td0$claims_winsor_01_99 <- td0$claims_2001_2016
claims_q99 <- with(td0, quantile(claims_2001_2016, .99, na.rm = TRUE))
ind <- with(td0, claims_2001_2016 > claims_q99)
td0$claims_winsor_01_99[ind] <- claims_q99
claims_q01 <- with(td0, quantile(claims_2001_2016, .01, na.rm = TRUE))
ind <- with(td0, claims_2001_2016 < claims_q01)
td0$claims_winsor_01_99[ind] <- claims_q01

#0.5/99.5pct claims
td0$claims_winsor_005_995 <- td0$claims_2001_2016
claims_q995 <- with(td0, quantile(claims_2001_2016, .995, na.rm = TRUE))
ind <- with(td0, claims_2001_2016 > claims_q995)
td0$claims_winsor_005_995[ind] <- claims_q995
claims_q005 <- with(td0, quantile(claims_2001_2016, .005, na.rm = TRUE))
ind <- with(td0, claims_2001_2016 < claims_q005)
td0$claims_winsor_005_995[ind] <- claims_q005

#1/99pct wetlands
td0$wetland_winsor_01_99 <- td0$wetland_2001_2016
wetland_q99 <- with(td0, quantile(wetland_2001_2016, .99, na.rm = TRUE))
ind <- with(td0, wetland_2001_2016 > wetland_q99)
td0$wetland_winsor_01_99[ind] <- wetland_q99
wetland_q01 <- with(td0, quantile(wetland_2001_2016, .01, na.rm = TRUE))
ind <- with(td0, wetland_2001_2016 < wetland_q01)
td0$wetland_winsor_01_99[ind] <- wetland_q01

#0.5/99.5pct wetlands
td0$wetland_winsor_005_995 <- td0$wetland_2001_2016
wetland_q995 <- with(td0, quantile(wetland_2001_2016, .995, na.rm = TRUE))
ind <- with(td0, wetland_2001_2016 > wetland_q995)
td0$wetland_winsor_005_995[ind] <- wetland_q995
wetland_q005 <- with(td0, quantile(wetland_2001_2016, .005, na.rm = TRUE))
ind <- with(td0, wetland_2001_2016 < wetland_q005)
td0$wetland_winsor_005_995[ind] <- wetland_q005

#winsorized regressions
base_controls <- "developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips)"

w_baseline <- lm(as.formula(paste("claims_2001_2016 ~ wetland_2001_2016 +", base_controls)), data = td0)
w_claims_w005 <- lm(as.formula(paste("claims_winsor_005_995 ~ wetland_2001_2016 +", base_controls)), data = td0)
w_claims_w01 <- lm(as.formula(paste("claims_winsor_01_99 ~ wetland_2001_2016 +", base_controls)), data = td0)
w_wetland_w005 <- lm(as.formula(paste("claims_2001_2016 ~ wetland_winsor_005_995 +", base_controls)), data = td0)
w_wetland_w01 <- lm(as.formula(paste("claims_2001_2016 ~ wetland_winsor_01_99 +", base_controls)), data = td0)
w_both_w005 <- lm(as.formula(paste("claims_winsor_005_995 ~ wetland_winsor_005_995 +", base_controls)), data = td0)
w_both_w01 <- lm(as.formula(paste("claims_winsor_01_99 ~ wetland_winsor_01_99 +", base_controls)), data = td0)

winsor_models <- list(w_baseline, w_both_w005, w_both_w01, w_claims_w005, w_claims_w01, w_wetland_w005, w_wetland_w01)
winsor_models_se <- list()
winsor_models_se[[1]] <- sqrt(diag(vcovCL(w_baseline, cluster = td0$county_fips, type = "HC1")))
winsor_models_se[[2]] <- sqrt(diag(vcovCL(w_both_w005, cluster = td0$county_fips, type = "HC1")))
winsor_models_se[[3]] <- sqrt(diag(vcovCL(w_both_w01, cluster = td0$county_fips, type = "HC1")))
winsor_models_se[[4]] <- sqrt(diag(vcovCL(w_claims_w005, cluster = td0$county_fips, type = "HC1")))
winsor_models_se[[5]] <- sqrt(diag(vcovCL(w_claims_w01, cluster = td0$county_fips, type = "HC1")))
winsor_models_se[[6]] <- sqrt(diag(vcovCL(w_wetland_w005, cluster = td0$county_fips, type = "HC1")))
winsor_models_se[[7]] <- sqrt(diag(vcovCL(w_wetland_w01, cluster = td0$county_fips, type = "HC1")))

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
teX(winsor_models, winsor_models_se, "/winsorization_robustness_raw")

raw_file  <- "output/tables/winsorization_robustness_raw.tex"
edit_file <- "output/tables/winsorization_robustness_clean.tex"

x <- readLines(raw_file)
x <- round_table_nums_2(x)

dep_idx <- grep("Dependent variable:", x, fixed = TRUE)[1]
coef_idx <- grep("^\\s*Local wetland change \\(ha\\)\\s*&", x)[1]

if (is.na(dep_idx)) stop("Could not find dependent variable header.")
if (is.na(coef_idx)) stop("Could not find true local wetland coefficient row.")

header_block <- c(
  "& \\multicolumn{7}{c}{\\textit{Dependent variable:}} \\\\",
  "\\cline{2-8}",
  "& \\multicolumn{7}{c}{Change in zip code-level NFIP claims (US\\$)} \\\\",
  "& All & Both & Both & Claims & Claims & Wetlands & Wetlands \\\\",
  "& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\\\",
  "\\hline \\\\[-1.8ex]")

x <- c(x[1:(dep_idx - 1)], header_block, x[coef_idx:length(x)])

x <- gsub("^\\s*Local wetland change \\(ha\\)", "Wetland change, 2001--2016", x)
x <- gsub("^\\s*wetland\\\\_winsor\\\\_005\\\\_995", "Wetland change, 2001--2016", x)
x <- gsub("^\\s*wetland\\\\_winsor\\\\_01\\\\_99", "Wetland change, 2001--2016", x)

winsor_panel_rows <- c(
  "Baseline controls & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\",
  "Claims winsorized &  & 0.5/99.5 & 1/99 & 0.5/99.5 & 1/99 &  &  \\\\",
  "Wetland change winsorized &  & 0.5/99.5 & 1/99 &  &  & 0.5/99.5 & 1/99 \\\\",
  "Full sample & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\")

winsor_means <- sapply(winsor_models, function(m) {round(mean(model.response(model.frame(m)), na.rm = TRUE) / 1000, 2)})
mean_row <- paste0("Mean dep. var. (US\\$000s) & ", paste(winsor_means, collapse = " & "), " \\\\")

obs_idx <- grep("^Observations[[:space:]]*&", x)
if (length(obs_idx) == 0) stop("Could not find Observations row in winsorization table.")

hline_idx <- grep("^\\\\hline", x)
panel_hline_idx <- max(hline_idx[hline_idx < obs_idx[1]])
x_new <- c(x[1:panel_hline_idx], winsor_panel_rows, mean_row, x[obs_idx[1]:length(x)])

writeLines(x_new, edit_file)

# ============================================================
# Symmetric outlier-dropping robustness
# Note: all drop indicators are calculated on original td0
# ============================================================

td0$row_id <- seq_len(nrow(td0))

# 0.5 / 99.5 percentiles
claims_q005 <- quantile(td0$claims_2001_2016, .005, na.rm = TRUE)
claims_q995 <- quantile(td0$claims_2001_2016, .995, na.rm = TRUE)
wetland_q005 <- quantile(td0$wetland_2001_2016, .005, na.rm = TRUE)
wetland_q995 <- quantile(td0$wetland_2001_2016, .995, na.rm = TRUE)
drop_claims_005 <- with(td0, claims_2001_2016 < claims_q005 | claims_2001_2016 > claims_q995)
drop_wetland_005 <- with(td0, wetland_2001_2016 < wetland_q005 | wetland_2001_2016 > wetland_q995)
drop_both_005 <- drop_claims_005 | drop_wetland_005

# 1 / 99 percentiles
claims_q01 <- quantile(td0$claims_2001_2016, .01, na.rm = TRUE)
claims_q99 <- quantile(td0$claims_2001_2016, .99, na.rm = TRUE)
wetland_q01 <- quantile(td0$wetland_2001_2016, .01, na.rm = TRUE)
wetland_q99 <- quantile(td0$wetland_2001_2016, .99, na.rm = TRUE)
drop_claims_01 <- with(td0, claims_2001_2016 < claims_q01 | claims_2001_2016 > claims_q99)
drop_wetland_01 <- with(td0, wetland_2001_2016 < wetland_q01 | wetland_2001_2016 > wetland_q99)
drop_both_01 <- drop_claims_01 | drop_wetland_01

base_controls <- "developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips)"
trim_formula <- as.formula(paste("claims_2001_2016 ~ wetland_2001_2016 +", base_controls))
t_baseline <- lm(trim_formula, data = td0)

# 0.5 / 99.5
t_claims_005 <- lm(trim_formula, data = td0[!drop_claims_005, ])
t_wetland_005 <- lm(trim_formula, data = td0[!drop_wetland_005, ])
t_both_005 <- lm(trim_formula, data = td0[!drop_both_005, ])

# 1 / 99
t_claims_01 <- lm(trim_formula, data = td0[!drop_claims_01, ])
t_wetland_01 <- lm(trim_formula, data = td0[!drop_wetland_01, ])
t_both_01 <- lm(trim_formula, data = td0[!drop_both_01, ])

trim_models <- list(t_baseline, t_both_005, t_both_01, t_claims_005, t_claims_01, t_wetland_005, t_wetland_01)

trim_models_se <- list(cluster_se(t_baseline, td0$county_fips),
  cluster_se(t_both_005, td0$county_fips[!drop_both_005]),
  cluster_se(t_both_01, td0$county_fips[!drop_both_01]),
  cluster_se(t_claims_005, td0$county_fips[!drop_claims_005]),
  cluster_se(t_claims_01, td0$county_fips[!drop_claims_01]),
  cluster_se(t_wetland_005, td0$county_fips[!drop_wetland_005]),
  cluster_se(t_wetland_01, td0$county_fips[!drop_wetland_01]))

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
teX(trim_models, trim_models_se, "/trimming_robustness_symmetric_raw")

raw_file  <- "output/tables/trimming_robustness_symmetric_raw.tex"
edit_file <- "output/tables/trimming_robustness_symmetric_clean.tex"

x <- readLines(raw_file)
x <- round_table_nums_2(x)

dep_idx <- grep("Dependent variable:", x, fixed = TRUE)[1]
coef_idx <- grep("^\\s*Local wetland change \\(ha\\)\\s*&", x)[1]

if (is.na(dep_idx)) stop("Could not find dependent variable header.")
if (is.na(coef_idx)) stop("Could not find true local wetland coefficient row.")

header_block <- c(
  "& \\multicolumn{7}{c}{\\textit{Dependent variable:}} \\\\",
  "\\cline{2-8}",
  "& \\multicolumn{7}{c}{NFIP claims, 2001--2016} \\\\",
  "& All & Both & Both & Claims & Claims & Wetlands & Wetlands \\\\",
  "& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\\\",
  "\\hline \\\\[-1.8ex]")

x <- c(x[1:(dep_idx - 1)], header_block, x[coef_idx:length(x)])
x <- gsub("^\\s*Local wetland change \\(ha\\)", "Wetland change, 2001--2016", x)

trim_panel_rows <- c(
  "Baseline controls & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\",
  "Claims outliers dropped &  & 0.5/99.5 & 1/99 & 0.5/99.5 & 1/99 &  &  \\\\",
  "Wetland-change outliers dropped &  & 0.5/99.5 & 1/99 &  &  & 0.5/99.5 & 1/99 \\\\",
  "Full sample & \\checkmark &  &  &  &  &  &  \\\\")

trim_means <- sapply(trim_models, function(m) {round(mean(model.response(model.frame(m)), na.rm = TRUE) / 1000, 2)})
mean_row <- paste0("Mean dep. var. (\\$000s) & ", paste(trim_means, collapse = " & ")," \\\\")
obs_idx <- grep("^Observations[[:space:]]*&", x)

if (length(obs_idx) == 0) {
  stop("Could not find Observations row in trimming table.")}

hline_idx <- grep("^\\\\hline", x)
panel_hline_idx <- max(hline_idx[hline_idx < obs_idx[1]])
x_new <- c(x[1:panel_hline_idx], trim_panel_rows, mean_row, x[obs_idx[1]:length(x)])
writeLines(x_new, edit_file)

# =========================
# ELASTICITY VERIFICATION
# =========================

gross_wetland_loss <- -sum(td$wetland_loss, na.rm = TRUE)
wetlands_2001_total <- sum(td$wetland_ha_2001, na.rm = TRUE)
wetland_loss_pct <- 100* gross_wetland_loss/wetlands_2001_total
claims_increase_pct <- 23.6 # from TD
wc_elasticity <- claims_increase_pct/wetland_loss_pct
#29.91

# =========================
# CONSOLIDATED TABLES
# =========================

# #IDENTIFICATION TABLE
# table_identification_models <- list(idf1, idf2, idf3, idf8, idf9, idf_nfhl_1, idf_nfhl_2, idf_nfhl_3, idf_nfhl_5)
# table_identification_se <- list(idf1_se, idf2_se, idf3_se, idf8_se, idf9_se, idf_nfhl_1_se, idf_nfhl_2_se, idf_nfhl_3_se, idf_nfhl_5_se)
# 
# dir.create("tex", recursive = TRUE, showWarnings = FALSE)
# teX(table_identification_models, table_identification_se,"/05102026_idf_consolidated")
# 
# idf1_mean <- round(mean(idf1$model$wetland_2001_2016, na.rm = TRUE), 2)
# idf2_mean <- round(mean(idf2$model$wetland_2001_2016, na.rm = TRUE), 2)
# idf3_mean <- round(mean(idf3$model$wetland_2001_2016, na.rm = TRUE), 2)
# idf8_mean <- round(mean(idf8$model$wetland_2001_2016, na.rm = TRUE), 2)
# idf9_mean <- round(mean(idf9$model$wetland_2001_2016, na.rm = TRUE), 2)
# idf_nfhl_1_mean <- round(mean(idf_nfhl_1$model$wetland_2001_2016, na.rm = TRUE), 2)
# idf_nfhl_2_mean <- round(mean(idf_nfhl_2$model$wetland_2001_2016, na.rm = TRUE), 2)
# idf_nfhl_3_mean <- round(mean(idf_nfhl_3$model$wetland_2001_2016, na.rm = TRUE), 2)
# idf_nfhl_5_mean <- round(mean(idf_nfhl_5$model$wetland_2001_2016, na.rm = TRUE), 2)
# 
# raw_file <- "output/tables/05102026_idf_consolidated.tex"
# new_file <- "output/tables/05102026_idf_consolidated_all_NEW.tex"
# 
# x <- readLines(raw_file)
# dep_idx <- grep("Dependent variable:", x, fixed = TRUE)
# if (length(dep_idx) == 1) {
#   x[dep_idx] <- "& \\multicolumn{9}{c}{\\textit{Dependent variable: Local wetland change (ha)}} \\\\"
#   if (dep_idx + 2 <= length(x)) x <- x[-(dep_idx + 2)]}
# 
# idf_panel_rows <- c(
#   "Baseline controls & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\",
#   "Full sample & \\checkmark & \\checkmark & \\checkmark &  &  &  &  &  &  \\\\",
#   "Harvey ZIPs only &  &  &  & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\")
# mean_row <- paste0("Mean dep. var. (ha) & ", fmt_latex_num(idf1_mean), " & ", fmt_latex_num(idf2_mean), " & ", fmt_latex_num(idf3_mean), " & ", fmt_latex_num(idf8_mean), " & ", fmt_latex_num(idf9_mean), " & ", fmt_latex_num(idf_nfhl_1_mean), " & ", fmt_latex_num(idf_nfhl_2_mean), " & ", fmt_latex_num(idf_nfhl_3_mean), " & ", fmt_latex_num(idf_nfhl_5_mean), " \\\\")
# 
# obs_idx <- grep("^Observations[[:space:]]*&", x)
# if (length(obs_idx) == 0) stop("Could not find Observations row in identification table.")
# hline_idx <- grep("^\\\\hline", x)
# panel_hline_idx <- max(hline_idx[hline_idx < obs_idx[1]])
# 
# x_new <- c(x[1:panel_hline_idx], idf_panel_rows, mean_row, x[obs_idx[1]:length(x)])
# x_new <- fix_table_dashes(x_new)
# writeLines(x_new, new_file)

#UPDATE: REPLACE COLUMNS 7/8 with new combined COL 7
table_identification_models <- list(idf1, idf2, idf3, idf8, idf9, idf_nfhl_1, idf_nfhl_av, idf_nfhl_5)
table_identification_se <- list(idf1_se, idf2_se, idf3_se, idf8_se, idf9_se, idf_nfhl_1_se, idf_nfhl_av_se, idf_nfhl_5_se)
dir.create("tex", recursive = TRUE, showWarnings = FALSE)

teX(table_identification_models, table_identification_se, "/05102026_idf_consolidated")

# Mean dependent variable
idf1_mean <- round(mean(idf1$model$wetland_2001_2016, na.rm = TRUE), 2)
idf2_mean <- round(mean(idf2$model$wetland_2001_2016, na.rm = TRUE), 2)
idf3_mean <- round(mean(idf3$model$wetland_2001_2016, na.rm = TRUE), 2)
idf8_mean <- round(mean(idf8$model$wetland_2001_2016, na.rm = TRUE), 2)
idf9_mean <- round(mean(idf9$model$wetland_2001_2016, na.rm = TRUE), 2)
idf_nfhl_1_mean <- round(mean(idf_nfhl_1$model$wetland_2001_2016, na.rm = TRUE), 2)
idf_nfhl_av_mean <- round(mean(idf_nfhl_av$model$wetland_2001_2016, na.rm = TRUE), 2)
idf_nfhl_5_mean <- round(mean(idf_nfhl_5$model$wetland_2001_2016, na.rm = TRUE), 2)

raw_file <- "output/tables/05102026_idf_consolidated.tex"
new_file <- "output/tables/05102026_idf_consolidated_all_NEW.tex"

x <- readLines(raw_file)

dep_idx <- grep("Dependent variable:", x, fixed = TRUE)

if (length(dep_idx) == 1) {
  x[dep_idx] <- "& \\multicolumn{8}{c}{\\textit{Dependent variable: Local wetland change (ha)}} \\\\"
  if (dep_idx + 2 <= length(x)) x <- x[-(dep_idx + 2)]
}

idf_panel_rows <- c(
  "Baseline controls & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\",
  "Full sample & \\checkmark & \\checkmark & \\checkmark &  &  &  &  &  \\\\",
  "Harvey zips only &  &  &  & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\"
)

mean_row <- paste0(
  "Mean dep. var. (ha) & ",
  fmt_latex_num(idf1_mean), " & ",
  fmt_latex_num(idf2_mean), " & ",
  fmt_latex_num(idf3_mean), " & ",
  fmt_latex_num(idf8_mean), " & ",
  fmt_latex_num(idf9_mean), " & ",
  fmt_latex_num(idf_nfhl_1_mean), " & ",
  fmt_latex_num(idf_nfhl_av_mean), " & ",
  fmt_latex_num(idf_nfhl_5_mean), " \\\\")

obs_idx <- grep("^Observations[[:space:]]*&", x)

if (length(obs_idx) == 0) {
  stop("Could not find Observations row in identification table.")
}

hline_idx <- grep("^\\\\hline", x)
panel_hline_idx <- max(hline_idx[hline_idx < obs_idx[1]])
x_new <- c(x[1:panel_hline_idx], idf_panel_rows, mean_row, x[obs_idx[1]:length(x)])
x_new <- fix_table_dashes(x_new)

x_new <- gsub("ind\\_Harvey", "Harvey indicator", x_new, fixed = TRUE)
x_new <- gsub("height\\_above\\_gnd\\_max", "Max Harvey flood height", x_new, fixed = TRUE)
x_new <- gsub("harvey\\_hwm\\_count", "Harvey high-water marks count", x_new, fixed = TRUE)
x_new <- gsub("share\\_nfhl", "Flood zone share", x_new, fixed = TRUE)
x_new <- gsub("share\\_A", "Zone A share", x_new, fixed = TRUE)
x_new <- gsub("share\\_V", "Zone V share", x_new, fixed = TRUE)
x_new <- gsub("elevation\\_m", "Elevation", x_new, fixed = TRUE)

writeLines(x_new, new_file)

#IDENTIFICATION (TX/LA Only) 7b
table_7b_models <- list(idf4, idf5, idf6, idf_nfhl_4)
table_7b_se <- list(idf4_se, idf5_se, idf6_se, idf_nfhl_4_se)
teX(table_7b_models, table_7b_se, "/05102026_idf_7b")

idf4_mean <- round(mean(idf4$model$wetland_2001_2016, na.rm = TRUE), 2)
idf5_mean <- round(mean(idf5$model$wetland_2001_2016, na.rm = TRUE), 2)
idf6_mean <- round(mean(idf6$model$wetland_2001_2016, na.rm = TRUE), 2)
idf_nfhl_4_mean <- round(mean(idf_nfhl_4$model$wetland_2001_2016, na.rm = TRUE), 2)

raw_file <- "output/tables/05102026_idf_7b.tex"
new_file <- "output/tables/05102026_idf_7b_all_NEW.tex"
x <- readLines(raw_file)

dep_idx <- grep("Dependent variable:", x, fixed = TRUE)
if (length(dep_idx) == 1) {
  x[dep_idx] <- "& \\multicolumn{4}{c}{\\textit{Dependent variable: Local wetland change (ha)}} \\\\"
  if (dep_idx + 2 <= length(x)) x <- x[-(dep_idx + 2)]}

idf_7b_panel_rows <- c(
  "Baseline controls & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\",
  "TX/LA only & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\")
mean_row <- paste0("Mean dep. var. (ha) & ", fmt_latex_num(idf4_mean), " & ", fmt_latex_num(idf5_mean), " & ", fmt_latex_num(idf6_mean), " & ", fmt_latex_num(idf_nfhl_4_mean), " \\\\")

obs_idx <- grep("^Observations[[:space:]]*&", x)
if (length(obs_idx) == 0) stop("Could not find Observations row in Table 7b.")
hline_idx <- grep("^\\\\hline", x)
panel_hline_idx <- max(hline_idx[hline_idx < obs_idx[1]])

x_new <- c(x[1:panel_hline_idx], idf_7b_panel_rows, mean_row, x[obs_idx[1]:length(x)])
x_new <- fix_table_dashes(x_new)

x_new <- gsub("ind\\_Harvey", "Harvey indicator", x_new, fixed = TRUE)
x_new <- gsub("height\\_above\\_gnd\\_max", "Max Harvey flood height", x_new, fixed = TRUE)
x_new <- gsub("harvey\\_hwm\\_count", "Harvey high-water marks count", x_new, fixed = TRUE)
x_new <- gsub("share\\_nfhl", "Flood zone share", x_new, fixed = TRUE)
x_new <- gsub("elevation\\_m", "Elevation", x_new, fixed = TRUE)

writeLines(x_new, new_file)

# =========================
# Consolidated Summary Table: Claims, Wetlands, NFHL, Elevation
# =========================

summary_df <- td0 %>% st_drop_geometry() %>% mutate(state_group = case_when(state_fips == 48 ~ "TX", state_fips == 22 ~ "LA", TRUE ~ "US Other"), ind_Harvey = ifelse(is.na(ind_Harvey), 0, ind_Harvey))

make_claims_col <- function(df, group_name) {
  df_g <- df %>% filter(state_group == group_name)
  total_change <- sum(as.numeric(df_g$claims_2001_2016), na.rm = TRUE)
  inside_change <- sum(as.numeric(df_g$claims_2001_2016)[df_g$ind_Harvey == 1], na.rm = TRUE)
  outside_change <- sum(as.numeric(df_g$claims_2001_2016)[df_g$ind_Harvey == 0], na.rm = TRUE)
  c(sum(as.numeric(df_g$claims_2001_5yr), na.rm = TRUE), sum(as.numeric(df_g$claims_2016_5yr), na.rm = TRUE), total_change, outside_change, inside_change, 100 * inside_change / total_change)}

make_wetland_col <- function(df, group_name) {
  df_g <- df %>% filter(state_group == group_name)
  total_change <- sum(as.numeric(df_g$wetland_2001_2016), na.rm = TRUE)
  inside_change <- sum(as.numeric(df_g$wetland_2001_2016)[df_g$ind_Harvey == 1], na.rm = TRUE)
  outside_change <- sum(as.numeric(df_g$wetland_2001_2016)[df_g$ind_Harvey == 0], na.rm = TRUE)
  c(sum(as.numeric(df_g$wetland_ha_2001), na.rm = TRUE), sum(as.numeric(df_g$wetland_ha_2016), na.rm = TRUE), total_change, outside_change, inside_change, 100 * inside_change / total_change)}

make_nfhl_col <- function(df, group_name) {
  df_g <- df %>% filter(state_group == group_name)
  c(mean(as.numeric(df_g$share_nfhl), na.rm = TRUE),
    mean(as.numeric(df_g$share_A), na.rm = TRUE),
    mean(as.numeric(df_g$share_V), na.rm = TRUE),
    mean(as.numeric(df_g$share_nfhl)[df_g$ind_Harvey == 0], na.rm = TRUE),
    mean(as.numeric(df_g$share_nfhl)[df_g$ind_Harvey == 1], na.rm = TRUE),
    mean(as.numeric(df_g$share_nfhl)[df_g$ind_Harvey == 1], na.rm = TRUE) - mean(as.numeric(df_g$share_nfhl)[df_g$ind_Harvey == 0], na.rm = TRUE))}

TX_claims <- make_claims_col(summary_df, "TX")
LA_claims <- make_claims_col(summary_df, "LA")
Other_claims <- make_claims_col(summary_df, "US Other")
TX_wet <- make_wetland_col(summary_df, "TX")
LA_wet <- make_wetland_col(summary_df, "LA")
Other_wet <- make_wetland_col(summary_df, "US Other")

nfhl_df <- td0_nfhl %>% st_drop_geometry() %>% mutate(state_group = case_when(state_fips == 48 ~ "TX", state_fips == 22 ~ "LA", TRUE ~ "US Other"), ind_Harvey = ifelse(is.na(ind_Harvey), 0, ind_Harvey))
TX_nfhl <- make_nfhl_col(nfhl_df, "TX")
LA_nfhl <- make_nfhl_col(nfhl_df, "LA")
Other_nfhl <- make_nfhl_col(nfhl_df, "US Other")

claims_summary <- data.frame(Panel = "A. NFIP claims",
  row = c(
    "Flood claims in 2001 (000s)",
    "Flood claims in 2016 (000s)",
    "Change in flood claims, 2001--2016 (000s)",
    "Change in flood claims outside Harvey zips (000s)",
    "Change in flood claims inside Harvey zips (000s)",
    "Share of total change inside Harvey zips (\\%)"),
  TX = c(round(TX_claims[1:5] / 1000, 2), round(TX_claims[6], 2)), LA = c(round(LA_claims[1:5] / 1000, 2), round(LA_claims[6], 2)), `US Other` = c(round(Other_claims[1:4] / 1000, 2), NA, NA), check.names = FALSE)

wetland_summary <- data.frame(Panel = "B. Wetlands",
  row = c(
    "Wetland area in 2001 (ha)",
    "Wetland area in 2016 (ha)",
    "Net wetland change, 2001--2016 (ha)",
    "Net wetland change outside Harvey zips (ha)",
    "Net wetland change inside Harvey zips (ha)",
    "Share of total wetland change inside Harvey zips (\\%)"),
  TX = round(TX_wet, 2), LA = round(LA_wet, 2), `US Other` = c(round(Other_wet[1:4], 2), NA, NA), check.names = FALSE)

nfhl_summary <- data.frame(Panel = "D. FEMA flood hazard",
  row = c("Mean NFHL exposure (\\%)", "Mean Zone A exposure (\\%)", "Mean Zone V exposure (\\%)", "Mean NFHL exposure outside Harvey zips (\\%)", "Mean NFHL exposure inside Harvey zips (\\%)", "Difference: inside minus outside (pp)"),
  TX = round(TX_nfhl, 2), LA = round(LA_nfhl, 2), `US Other` = c(round(Other_nfhl[1:4], 2), NA, NA), check.names = FALSE)

elevation_summary <- td_txla_elev %>% st_drop_geometry() %>% mutate(state_group = case_when(state_fips == 48 ~ "TX", state_fips == 22 ~ "LA"), ind_Harvey = ifelse(is.na(ind_Harvey), 0, ind_Harvey)) %>%
  summarise(
    TX = c(mean(as.numeric(elevation_m)[state_group == "TX"], na.rm = TRUE),
      mean(as.numeric(elevation_m)[state_group == "TX" & ind_Harvey == 0], na.rm = TRUE),
      mean(as.numeric(elevation_m)[state_group == "TX" & ind_Harvey == 1], na.rm = TRUE),
      mean(as.numeric(elevation_m)[state_group == "TX" & ind_Harvey == 1], na.rm = TRUE) - mean(as.numeric(elevation_m)[state_group == "TX" & ind_Harvey == 0], na.rm = TRUE)),
    LA = c(mean(as.numeric(elevation_m)[state_group == "LA"], na.rm = TRUE),
      mean(as.numeric(elevation_m)[state_group == "LA" & ind_Harvey == 0], na.rm = TRUE),
      mean(as.numeric(elevation_m)[state_group == "LA" & ind_Harvey == 1], na.rm = TRUE),
      mean(as.numeric(elevation_m)[state_group == "LA" & ind_Harvey == 1], na.rm = TRUE) - mean(as.numeric(elevation_m)[state_group == "LA" & ind_Harvey == 0], na.rm = TRUE))) %>% 
  mutate(Panel = "C. Elevation",
    row = c("Mean elevation (m)", "Mean elevation outside Harvey zips (m)", "Mean elevation inside Harvey zips (m)", "Difference: inside minus outside (m)"),
    TX = round(TX, 2), LA = round(LA, 2), `US Other` = NA) %>% select(Panel, row, TX, LA, `US Other`)

summary_tex_file <- "output/tables/summary_claims_wetlands_elevation_nfhl_consolidated.tex"
fmt_row <- function(label, tx, la, other = NA) {paste0(label, " & ", fmt_latex_num(tx, digits = 2), " & ", fmt_latex_num(la, digits = 2), " & ", ifelse(is.na(other), "—", fmt_latex_num(other, digits = 2)), " \\\\")}

claims_rows <- mapply(fmt_row, claims_summary$row, claims_summary$TX, claims_summary$LA, claims_summary$`US Other`)
wetland_rows <- mapply(fmt_row, wetland_summary$row, wetland_summary$TX, wetland_summary$LA, wetland_summary$`US Other`)
elevation_rows <- mapply(fmt_row, elevation_summary$row, elevation_summary$TX, elevation_summary$LA, rep(NA, nrow(elevation_summary)))
nfhl_rows <- mapply(fmt_row, nfhl_summary$row, nfhl_summary$TX, nfhl_summary$LA, nfhl_summary$`US Other`)

summary_tex <- c("\\begin{tabular}{lrrr}", "\\hline", "\\hline", " & TX & LA & US Other \\\\", "\\hline", "\\multicolumn{4}{l}{\\textbf{Panel A. NFIP claims}} \\\\",
                 claims_rows, "\\addlinespace", "\\multicolumn{4}{l}{\\textbf{Panel B. Wetlands}} \\\\",
                 wetland_rows, "\\addlinespace", "\\multicolumn{4}{l}{\\textbf{Panel C. Elevation}} \\\\",
                 elevation_rows, "\\addlinespace", "\\multicolumn{4}{l}{\\textbf{Panel D. FEMA flood hazard}} \\\\",
                 nfhl_rows, "\\hline", "\\hline", "\\end{tabular}")
summary_tex <- fix_table_dashes(summary_tex)
writeLines(summary_tex, summary_tex_file)

# =========================
# NEW NFIP CLAIMS FIGURE: Make sure to run the "Set Up" in the figures script prior to generative zips_txla
# =========================

city_lookup <- data.frame(
  city = c("Amarillo", "Houston", "Galveston"),
  zip4 = c("7910", "7700", "7755"),
  lon = c(-101.8313, -95.3698, -94.7977),
  lat = c(35.2219, 29.7604, 29.3013),
  lab_lon = c(-98.9, -92.2, -93.9),
  lab_lat = c(36.4, 35.3, 27.1),
  stringsAsFactors = FALSE)

td0_nogeo <- td0 %>% st_drop_geometry() %>% mutate(zip = as.character(zip))
td_elev_nogeo <- td_txla_elev %>% st_drop_geometry() %>% mutate(zip = as.character(zip))

city_stats <- city_lookup %>% rowwise() %>% mutate(
    claims_change = sum(td0_nogeo$claims_2001_2016[substr(td0_nogeo$zip, 1, 4) == zip4], na.rm = TRUE),
    wetland_change = sum(td0_nogeo$wetland_2001_2016[substr(td0_nogeo$zip, 1, 4) == zip4], na.rm = TRUE),
    elevation_m = mean(td_elev_nogeo$elevation_m[substr(td_elev_nogeo$zip, 1, 4) == zip4], na.rm = TRUE)) %>% ungroup() %>% mutate(
    claims_change = round(claims_change, 0),
    wetland_change = round(wetland_change, 0),
    elevation_m = round(elevation_m, 1),
    claims_change_lab = format(claims_change, big.mark = ",", scientific = FALSE, trim = TRUE),
    wetland_change_lab = format(wetland_change, big.mark = ",", scientific = FALSE, trim = TRUE),
    elevation_m_lab = format(elevation_m, big.mark = ",", scientific = FALSE, trim = TRUE),
    label = paste0(city, "\n", "Elevation: ", elevation_m_lab, " m\n", "Wetland change: ", wetland_change_lab, " ha\n", "Claims change: +$", claims_change_lab))

p_claims_v3 <- ggplot() +
  geom_sf(data = zips_txla, aes(fill = claims_signed_log), color = NA) +
  geom_sf(data = states_txla, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "black", linewidth = 0.5, linetype = "solid") +
  geom_segment(data = city_stats, aes(x = lon, y = lat, xend = lab_lon, yend = lab_lat), inherit.aes = FALSE, color = "black", linewidth = 0.35) +
  geom_point(data = city_stats, aes(x = lon, y = lat), inherit.aes = FALSE, color = "black", size = 1.5) +
  geom_label(data = city_stats, aes(x = lab_lon, y = lab_lat, label = label), inherit.aes = FALSE, hjust = 0, size = 3, label.size = 0.2, fill = "white", lineheight = 0.95) +
  scale_fill_gradient2(low = "#1a9850", mid = "white", high = "#d73027", midpoint = 0, na.value = "grey80", name = "NFIP claims change\n($ million) 2001-2016", breaks = sign(c(-10, 0, 10) * 1e6) * log(abs(c(-10, 0, 10) * 1e6) + 1), labels = c("-10", "0", "10")) +
  coord_sf(xlim = c(-106.5, -87.5), ylim = c(24.5, 37.5), clip = "off") +
  theme_void() + theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9), plot.margin = margin(5.5, 30, 5.5, 5.5))

ggsave(filename = "output/figures/claims_change_txla_diverging_labeled.pdf", plot = p_claims_v3, width = 8.5, height = 6)

# =========================
# NFIP CLAIMS FIGURE V2: Census ZIP groups + upstream wetland labels
# =========================

library(tigris)
library(readr)

options(tigris_use_cache = TRUE)

places_tx <- places(state = "TX", cb = TRUE, year = 2020) %>% st_transform(4326) %>% filter(NAME %in% c("Amarillo", "Houston", "Galveston")) %>% transmute(city = NAME)
zctas_tx <- zctas(cb = TRUE, starts_with = "7", year = 2020) %>% st_transform(4326)
zip_col <- names(zctas_tx)[grepl("ZCTA5CE|GEOID", names(zctas_tx))][1]
zctas_tx <- zctas_tx %>% mutate(zip = as.character(.data[[zip_col]])) %>% filter(substr(zip, 1, 2) %in% c("77", "79")) %>% select(zip, geometry) %>% mutate(zcta_area_ha = as.numeric(st_area(st_transform(geometry, 5070))) / 10000)
city_zip_list_v2 <- st_intersection(st_transform(zctas_tx, 5070), st_transform(places_tx, 5070)) %>% mutate(overlap_ha = as.numeric(st_area(geometry)) / 10000, overlap_share = overlap_ha / zcta_area_ha) %>% st_drop_geometry() %>% filter(overlap_ha >= 1, overlap_share >= 0.01) %>% select(city, zip, overlap_ha, overlap_share) %>% arrange(city, zip)
write_csv(city_zip_list_v2, "data/derived/city_zip_list_v2.csv")

zips_txla <- zips_txla %>% mutate(claims_signed_log = ifelse(is.na(claims_2001_2016), NA_real_, sign(claims_2001_2016) * log(abs(claims_2001_2016) + 1) ))
td_nogeo <- if ("sf" %in% class(td)) st_drop_geometry(td) else td
td_nogeo <- td_nogeo %>% mutate(zip = sprintf("%05s", as.character(zip)))
td_elev_nogeo <- td_txla_elev %>% st_drop_geometry() %>% mutate(zip = sprintf("%05s", as.character(zip)))

city_wetland_explore_v2 <- td_nogeo %>% inner_join(city_zip_list_v2 %>% select(city, zip), by = "zip") %>% group_by(city) %>%
  summarise(
    n_zips = n_distinct(zip),
    claims_total = sum(claims_2001_2016, na.rm = TRUE),
    claims_mean = mean(claims_2001_2016, na.rm = TRUE),
    local_wetland_total = sum(wetland_2001_2016, na.rm = TRUE),
    local_wetland_mean = mean(wetland_2001_2016, na.rm = TRUE),
    upstream_wetland_total = sum(wetlands_change_up, na.rm = TRUE),
    upstream_wetland_mean = mean(wetlands_change_up, na.rm = TRUE),
    upstream_gain_total = sum(wetlands_gain_up, na.rm = TRUE),
    upstream_gain_mean = mean(wetlands_gain_up, na.rm = TRUE),
    upstream_loss_total = sum(wetlands_loss_up, na.rm = TRUE),
    upstream_loss_mean = mean(wetlands_loss_up, na.rm = TRUE),
    .groups = "drop")
write_csv(city_wetland_explore_v2, "data/derived/city_wetland_explore_v2.csv")

city_lookup_v2 <- data.frame(
  city = c("Amarillo", "Houston", "Galveston"),
  lon = c(-101.8313, -95.3698, -94.7977),
  lat = c(35.2219, 29.7604, 29.3013),
  lab_lon = c(-98.9, -92.2, -93.9),
  lab_lat = c(37.5, 35.0, 27.1),
  stringsAsFactors = FALSE)
city_stats_v2 <- td_nogeo %>% inner_join(city_zip_list_v2 %>% select(city, zip), by = "zip") %>% group_by(city) %>%
  summarise(n_zips = n_distinct(zip), claims_change = mean(claims_2001_2016, na.rm = TRUE), upstream_wetland_change = mean(wetlands_change_up, na.rm = TRUE), .groups = "drop") %>%
  left_join(td_elev_nogeo %>% inner_join(city_zip_list_v2 %>% select(city, zip), by = "zip") %>% group_by(city) %>% summarise(elevation_m = mean(elevation_m, na.rm = TRUE), .groups = "drop"), by = "city") %>%
  left_join(city_lookup_v2, by = "city") %>% mutate(claims_change_lab = format(round(claims_change, 0), big.mark = ",", scientific = FALSE, trim = TRUE), upstream_wetland_change_lab = format(round(upstream_wetland_change, 0), big.mark = ",", scientific = FALSE, trim = TRUE), elevation_m_lab = format(round(elevation_m, 1), big.mark = ",", scientific = FALSE, trim = TRUE),
    label = paste0(city, "\n", "Elevation: ", elevation_m_lab, " m\n", "Avg Upstream Wetland Change: ", upstream_wetland_change_lab, " ha/ZIP\n", "Avg Claims Change: +$", claims_change_lab, "/ZIP"))

p_claims_v3_labeled_v2 <- ggplot() +
  geom_sf(data = zips_txla, aes(fill = claims_signed_log), color = NA) +
  geom_sf(data = states_txla, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "black", linewidth = 0.5, linetype = "solid") +
  geom_segment(data = city_stats_v2, aes(x = lon, y = lat, xend = lab_lon, yend = lab_lat), inherit.aes = FALSE, color = "black", linewidth = 0.35) +
  geom_point(data = city_stats_v2, aes(x = lon, y = lat), inherit.aes = FALSE, color = "black", size = 1.5) +
  geom_label(data = city_stats_v2, aes(x = lab_lon, y = lab_lat, label = label), inherit.aes = FALSE, hjust = 0, size = 3, label.size = 0.2, fill = "white", lineheight = 0.95) +
  scale_fill_gradient2(low = "#1a9850", mid = "white", high = "#d73027", midpoint = 0, na.value = "grey80", name = "NFIP claims change\n($ million) 2001-2016", breaks = sign(c(-10, 0, 10) * 1e6) * log(abs(c(-10, 0, 10) * 1e6) + 1), labels = c("-10", "0", "10"), limits = sign(c(-50, 50) * 1e6) * log(abs(c(-50, 50) * 1e6) + 1), oob = scales::squish) +
  coord_sf(xlim = c(-106.5, -87.5), ylim = c(24.5, 37.5), clip = "off") +
  theme_void() + theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9), plot.margin = margin(5.5, 30, 5.5, 5.5))
ggsave(filename = "output/figures/claims_change_txla_diverging_labeled_v2.pdf", plot = p_claims_v3_labeled_v2, width = 8.5, height = 6)


