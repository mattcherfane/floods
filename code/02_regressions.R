
library(raster)
library(sf)
#Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE
sf_use_s2(FALSE)
library(lwgeom)
library(stargazer)
library(AER)

# (Requires objects created by the earlier scripts: run 01-06 in order in one R session)

td_harvey <- subset(td_txla, ind_Harvey == 1)
base_controls <- "developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips)"

# =========================
# KNew_hur_harv_03022026
# =========================

# NFHL DATA REGRESSIONS #

nfhl <- readRDS("data/derived/nfhl_z_df0.rds")
load("data/derived/nfhl_pct_zip_48.RData")
print(names(z_df))
nfhl_LA <- nfhl[substr(nfhl$zip,1,2) == "70", ]
nfhl_final <- rbind(nfhl_LA, z_df)
td_harvey_nfhl <- merge(td_harvey, nfhl_final, by="zip", all.x=TRUE)
td_harvey_nfhl$pct_nfhl <- td_harvey_nfhl$pct_A + td_harvey_nfhl$pct_V

cor(td_harvey_nfhl$pct_A, td_harvey_nfhl$pct_V)
# 0.007772085

cor_h1 <- lm(wetland_2001_2016 ~ pct_A + pct_V + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_nfhl)
# pct_A                   -1.086e+02  3.155e+01  -3.443 0.000684 ***
# pct_V                    4.829e+01  4.589e+01   1.052 0.293723  
# Multiple R-squared:  0.4209,	Adjusted R-squared:  0.3984 

# ELEVATION DATA INVESTIGATION #

elev <- read.csv("data/derived/txla_zip_elevation.csv", stringsAsFactors = FALSE)
td_txla$zip <- as.character(td_txla$zip)
elev$zip <- as.character(elev$zip)
td_txla_elev <- td_txla %>% dplyr::left_join(elev %>% dplyr::select(zip, elevation_m), by = "zip")
td_harvey_elev <- subset(td_txla_elev, ind_Harvey == TRUE)

e1 <- lm(claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_txla_elev)
# wetland_2001_2016   -755.3475
e2 <- lm(claims_2001_2016 ~ wetland_2001_2016 + elevation_m + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_txla_elev)
# wetland_2001_2016   -854.1759 
e3 <- lm(claims_2001_2016 ~ wetland_2001_2016 + elevation_m + I(elevation_m^2) + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_txla_elev)
# wetland_2001_2016   -729.2933 
e4 <- lm(claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_elev)
# wetland_2001_2016   -18859.97
e5 <- lm(claims_2001_2016 ~ wetland_2001_2016 + elevation_m + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_elev)
# wetland_2001_2016   -16459.33 
e6 <- lm(claims_2001_2016 ~ wetland_2001_2016 + elevation_m + I(elevation_m^2) + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_elev)
# wetland_2001_2016   -16084.82

cor_e1 <- lm(wetland_2001_2016 ~ elevation_m, data = td_txla_elev)
#   -0.04580392 
cor_e1_fe <- lm(wetland_2001_2016 ~ elevation_m + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_txla_elev)
#   -0.02992833 
cor_e2 <- lm(wetland_2001_2016 ~ elevation_m, data = td_harvey_elev)
#   0.3956061 
cor_e2_fe <- lm(wetland_2001_2016 ~ elevation_m + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_elev)
#   0.2903704 

# CORRELATION MATRIX #

elev_sub <- td_txla_elev %>% st_drop_geometry() %>% dplyr::select(zip, elevation_m)
td_harvey_nfhl_elev <- merge(td_harvey_nfhl, elev_sub, by = "zip", all.x = TRUE)
vars <- td_harvey_nfhl_elev[, c("wetland_2001_2016", "harvey_peak_stage_max", "harvey_hwm_count", "pct_A", "pct_V", "elevation_m")]

library(units)
vars_clean <- td_harvey_nfhl_elev %>%
  st_drop_geometry() %>%
  dplyr::select(wetland_2001_2016, harvey_peak_stage_max, harvey_hwm_count, pct_A, pct_V, elevation_m)
vars_clean$pct_A <- drop_units(vars_clean$pct_A)
vars_clean$pct_V <- drop_units(vars_clean$pct_V)

cor_matrix <- cor(vars_clean, use = "pairwise.complete.obs")
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper")
library(stargazer)
stargazer(cor_matrix, summary = FALSE, type = "latex", out = "output/figures/03022026_harvey_cor_matrix.tex")

# =========================
# KNew_wetlands_03112026_MC
# =========================

# Included height_above_gnd variables in 01_build_data.R on 03/05/2026

td_txla_elev$height_above_gnd_max[is.infinite(td_txla_elev$height_above_gnd_max)] <- 0
td_harvey_elev$height_above_gnd_max[is.infinite(td_harvey_elev$height_above_gnd_max)] <- 0
td0$height_above_gnd_max[is.infinite(td0$height_above_gnd_max)] <- 0

# Row 1709 entry was -Inf since it had peak_stage points, but were all NAs, thus decided to replace with 0, which is a conservative approach

# REGRESSIONS WITH PEAK STAGE and HEIGHT ABOVE GROUND#

P1 <- lm(claims_2001_2016 ~ wetland_2001_2016 + harvey_peak_stage_max + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_txla_elev)
P2 <- lm(claims_2001_2016 ~ wetland_2001_2016 + elevation_m + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_txla_elev)
P3 <- lm(claims_2001_2016 ~ wetland_2001_2016 + harvey_peak_stage_max + elevation_m + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_txla_elev)
P4 <- lm(claims_2001_2016 ~ wetland_2001_2016 + height_above_gnd_max + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_txla_elev)
P5 <- lm(claims_2001_2016 ~ wetland_2001_2016 + height_above_gnd_max + elevation_m + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_txla_elev)
P6 <- lm(claims_2001_2016 ~ wetland_2001_2016*height_above_gnd_max + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_txla_elev)

# > vif(P3) both above 10
# harvey_peak_stage_max     11.877055
# elevation_m               11.799182
# population_2001_2016      57.547248
# housing_units_2001_2016   58.303925

# =========================
# COASTALS INVESTIGATION
# =========================

# > table(td_harvey$coastal)
#   FALSE  TRUE 
#   231    11 

# > table(td0$coastal)
#   FALSE  TRUE 
#   24475  1259 

c1 <- lm(claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td0)
c2 <- lm(claims_2001_2016 ~ wetland_2001_2016 + coastal + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td0)
c3 <- lm(claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey)
c4 <- lm(claims_2001_2016 ~ wetland_2001_2016 + coastal + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey)
c5 <- lm(claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td0, coastal==T)
c6 <- lm(claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td0, coastal==F)

# =========================
# NFHL ZONE INVESTIGATION
# =========================

# Convert Units
td_harvey_nfhl$share_A    <- td_harvey_nfhl$pct_A * 100
td_harvey_nfhl$share_V    <- td_harvey_nfhl$pct_V * 100
td_harvey_nfhl$share_nfhl <- td_harvey_nfhl$pct_nfhl * 100

td_harvey_nfhl_elev$share_A    <- td_harvey_nfhl_elev$pct_A * 100
td_harvey_nfhl_elev$share_V    <- td_harvey_nfhl_elev$pct_V * 100
td_harvey_nfhl_elev$share_nfhl <- td_harvey_nfhl_elev$pct_nfhl * 100

nfhl1 <- lm(claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_nfhl) 
# matches normal regression restricted to harvey zips

nfhl2 <- lm(claims_2001_2016 ~ wetland_2001_2016 + share_nfhl + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_nfhl)
nfhl3 <- lm(claims_2001_2016 ~ wetland_2001_2016 + share_A + share_V + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_nfhl)

nfhl4 <- lm(claims_2001_2016 ~ wetland_2001_2016 * share_nfhl + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_nfhl)
nfhl5 <- lm(claims_2001_2016 ~ wetland_2001_2016 * share_A + wetland_2001_2016 * share_V + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_nfhl) 

# =========================
# DIFF TREATMENT VARIABLES
# =========================

t1 <- lm(claims_2001_2016 ~ share_nfhl + wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_nfhl_elev)
t2 <- lm(claims_2001_2016 ~ share_A + wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_nfhl_elev)
t3 <- lm(claims_2001_2016 ~ share_V + wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_nfhl_elev)

t4 <- lm(claims_2001_2016 ~ share_nfhl + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_nfhl_elev)
t5 <- lm(claims_2001_2016 ~ share_A + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_nfhl_elev)
t6 <- lm(claims_2001_2016 ~ share_V + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_nfhl_elev)

t7 <- lm(claims_2001_2016 ~ share_nfhl + elevation_m + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_nfhl_elev)
t8 <- lm(claims_2001_2016 ~ share_nfhl + elevation_m + wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_nfhl_elev)

# =========================
# IDENTIFICATIONS TABLE
# =========================

idf1 <- lm(as.formula(paste("wetland_2001_2016 ~ ind_Harvey +",base_controls)),data = td0)
idf2 <- lm(as.formula(paste("wetland_2001_2016 ~ height_above_gnd_max +",base_controls)),data = td0)
idf3 <- lm(as.formula(paste("wetland_2001_2016 ~ harvey_hwm_count +",base_controls)),data = td0)

idf4 <- lm(as.formula(paste("wetland_2001_2016 ~ ind_Harvey +",base_controls)),data = td_txla_elev)
idf5 <- lm(as.formula(paste("wetland_2001_2016 ~ height_above_gnd_max +",base_controls)),data = td_txla_elev)
idf6 <- lm(as.formula(paste("wetland_2001_2016 ~ harvey_hwm_count +",base_controls)),data = td_txla_elev)

idf7 <- lm(as.formula(paste("wetland_2001_2016 ~ ind_Harvey +",base_controls)),data = td_harvey_elev)
idf8 <- lm(as.formula(paste("wetland_2001_2016 ~ height_above_gnd_max +",base_controls)),data = td_harvey_elev)
idf9 <- lm(as.formula(paste("wetland_2001_2016 ~ harvey_hwm_count +",base_controls)),data = td_harvey_elev)

cluster_se <- function(model, cluster_var) {sqrt(diag(vcovCL(model, cluster = cluster_var)))}

idf1_se <- cluster_se(idf1, td0$county_fips)
idf2_se <- cluster_se(idf2, td0$county_fips)
idf3_se <- cluster_se(idf3, td0$county_fips)

idf4_se <- cluster_se(idf4, td_txla_elev$county_fips)
idf5_se <- cluster_se(idf5, td_txla_elev$county_fips)
idf6_se <- cluster_se(idf6, td_txla_elev$county_fips)

idf7_se <- cluster_se(idf7, td_harvey_elev$county_fips)
idf8_se <- cluster_se(idf8, td_harvey_elev$county_fips)
idf9_se <- cluster_se(idf9, td_harvey_elev$county_fips)

# (NFHL LAYERS & ELEVATION)

idf_nfhl_1 <- lm(as.formula(paste("wetland_2001_2016 ~ share_nfhl +",base_controls)),data = td_harvey_nfhl)
idf_nfhl_2 <- lm(as.formula(paste("wetland_2001_2016 ~ share_A +",base_controls)),data = td_harvey_nfhl)
idf_nfhl_3 <- lm(as.formula(paste("wetland_2001_2016 ~ share_V +",base_controls)),data = td_harvey_nfhl)
idf_nfhl_4 <- lm(as.formula(paste("wetland_2001_2016 ~ elevation_m +",base_controls)),data = td_txla_elev)
idf_nfhl_5 <- lm(as.formula(paste("wetland_2001_2016 ~ elevation_m +",base_controls)),data = td_harvey_elev)

idf_nfhl_av <- lm(as.formula(paste("wetland_2001_2016 ~ share_A + share_V +",base_controls)),data = td_harvey_nfhl)


idf_nfhl_1_se <- cluster_se(idf_nfhl_1, td_harvey_nfhl$county_fips)
idf_nfhl_2_se <- cluster_se(idf_nfhl_2, td_harvey_nfhl$county_fips)
idf_nfhl_3_se <- cluster_se(idf_nfhl_3, td_harvey_nfhl$county_fips)
idf_nfhl_4_se <- cluster_se(idf_nfhl_4, td_txla_elev$county_fips)
idf_nfhl_5_se <- cluster_se(idf_nfhl_5, td_harvey_elev$county_fips)

idf_nfhl_av_se <- cluster_se(idf_nfhl_av, td_harvey_nfhl$county_fips)


# IDENTIFICATION GAINS/LOSSES #

idf_loss_1 <- lm(as.formula(paste("wetland_loss ~ ind_Harvey +",base_controls)),data = td_txla_elev)
idf_loss_2 <- lm(as.formula(paste("wetland_loss ~ height_above_gnd_max +",base_controls)),data = td_txla_elev)
idf_loss_3 <- lm(as.formula(paste("wetland_loss ~ harvey_hwm_count +",base_controls)),data = td_txla_elev)
idf_loss_4 <- lm(as.formula(paste("wetland_loss ~ share_nfhl +",base_controls)),data = td_harvey_nfhl)
idf_loss_5 <- lm(as.formula(paste("wetland_loss ~ elevation_m +",base_controls)),data = td_harvey_elev)

idf_loss_1_se <- cluster_se(idf_loss_1, td_txla_elev$county_fips)
idf_loss_2_se <- cluster_se(idf_loss_2, td_txla_elev$county_fips)
idf_loss_3_se <- cluster_se(idf_loss_3, td_txla_elev$county_fips)
idf_loss_4_se <- cluster_se(idf_loss_4, td_harvey_nfhl$county_fips)
idf_loss_5_se <- cluster_se(idf_loss_5, td_harvey_elev$county_fips)

# NEW OVERVIEW TABLE (1) Full sample baseline (2) TX/LA baseline (3) ind_harvey (4) height_above_gnd (5) harvey_hwm_count (6) elevation_m (7) Harvey baseline (8) share_nfhl (9) elevation_m

ov1 <- lm(claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td0)
ov2 <- lm(claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_txla_elev)
ov3 <- lm(claims_2001_2016 ~ wetland_2001_2016*ind_Harvey + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_txla_elev)
ov4 <- lm(claims_2001_2016 ~ wetland_2001_2016*height_above_gnd_max + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_txla_elev)
ov5 <- lm(claims_2001_2016 ~ wetland_2001_2016*harvey_hwm_count + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_txla_elev)
ov6 <- lm(claims_2001_2016 ~ wetland_2001_2016*elevation_m + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_txla_elev)
ov7 <- lm(claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey)
ov8 <- lm(claims_2001_2016 ~ wetland_2001_2016*share_nfhl + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_nfhl)
ov9 <- lm(claims_2001_2016 ~ wetland_2001_2016*elevation_m + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td_harvey_nfhl_elev)

# 5/11
nfhl <- readRDS("data/derived/nfhl_z_df0.rds")
load("data/derived/nfhl_pct_zip_48.RData")  # loads z_df for Texas
nfhl$zip <- as.character(nfhl$zip)
z_df$zip <- as.character(z_df$zip)
td0$zip <- as.character(td0$zip)
nfhl_no_tx <- nfhl[substr(nfhl$zip, 1, 2) != "77" & substr(nfhl$zip, 1, 2) != "78" & substr(nfhl$zip, 1, 2) != "79" & substr(nfhl$zip, 1, 2) != "75" & substr(nfhl$zip, 1, 2) != "76", ]
nfhl_final_national <- rbind(nfhl_no_tx, z_df)

td0_nfhl <- merge(td0, nfhl_final_national, by = "zip", all.x = TRUE)
td0_nfhl$pct_nfhl <- td0_nfhl$pct_A + td0_nfhl$pct_V
td0_nfhl$share_A <- 100 * td0_nfhl$pct_A
td0_nfhl$share_V <- 100 * td0_nfhl$pct_V
td0_nfhl$share_nfhl <- td0_nfhl$share_A + td0_nfhl$share_V

# lm(formula = claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + pct_nfhl + factor(state_fips), data = td0_nfhl)
# wetland_2001_2016       -2.566e+02  9.924e+01  -2.586 0.009730 ** 

# lm(formula = claims_2001_2016 ~ wetland_2001_2016*share_nfhl + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips), data = td0_nfhl)

# wetland_2001_2016               1.016e+01
# share_nfhl                      1.112e+04
# wetland_2001_2016:share_nfhl   -1.418e+01



