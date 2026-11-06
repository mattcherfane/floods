
# (Requires objects created by the earlier scripts: run 01-06 in order in one R session)

# RECOMMEND RUNNING CHUNKS INDIVIDUALLY TO NOT OVERWRITE ALREADY PRODUCED SCRIPTS

# =========================
# T1: OVERVIEW TABLE
# =========================

ov_models <- list(ov1, ov2, ov3, ov4, ov5, ov6, ov7, ov8, ov9)
ov_models_se <- list()

ov_models_se[[1]] <- sqrt(diag(vcovCL(ov1, cluster = td0$county_fips, type = "HC1")))
ov_models_se[[2]] <- sqrt(diag(vcovCL(ov2, cluster = td_txla_elev$county_fips, type = "HC1")))
ov_models_se[[3]] <- sqrt(diag(vcovCL(ov3, cluster = td_txla_elev$county_fips, type = "HC1")))
ov_models_se[[4]] <- sqrt(diag(vcovCL(ov4, cluster = td_txla_elev$county_fips, type = "HC1")))
ov_models_se[[5]] <- sqrt(diag(vcovCL(ov5, cluster = td_txla_elev$county_fips, type = "HC1")))
ov_models_se[[6]] <- sqrt(diag(vcovCL(ov6, cluster = td_txla_elev$county_fips, type = "HC1")))
ov_models_se[[7]] <- sqrt(diag(vcovCL(ov7, cluster = td_harvey$county_fips, type = "HC1")))
ov_models_se[[8]] <- sqrt(diag(vcovCL(ov8, cluster = td_harvey_nfhl$county_fips, type = "HC1")))
ov_models_se[[9]] <- sqrt(diag(vcovCL(ov9, cluster = td_harvey_nfhl_elev$county_fips, type = "HC1")))

dir.create("tex", recursive = TRUE, showWarnings = FALSE)
teX(ov_models, ov_models_se,"/03112026_ov_models", digits = 1)

# =========================
# T2-4: IDENTIFICATIONS TABLE
# =========================

idf_models <- list(idf1, idf2, idf3, idf4, idf5, idf6, idf7, idf8, idf9)
idf_models_se <- list(idf1_se, idf2_se, idf3_se, idf4_se, idf5_se, idf6_se, idf7_se, idf8_se, idf9_se)
dir.create("tex", recursive = TRUE, showWarnings = FALSE)
teX(idf_models, idf_models_se, "/03112026_idf_models")

idf_nfhl_models <- list(idf_nfhl_1, idf_nfhl_2, idf_nfhl_3, idf_nfhl_4, idf_nfhl_5)
idf_nfhl_models_se <- list(idf_nfhl_1_se, idf_nfhl_2_se, idf_nfhl_3_se, idf_nfhl_4_se, idf_nfhl_5_se)
dir.create("tex", recursive = TRUE, showWarnings = FALSE)
teX(idf_nfhl_models, idf_nfhl_models_se, "/03112026_idf_nfhl_models")

idf_loss_models <- list(idf_loss_1, idf_loss_2, idf_loss_3, idf_loss_4, idf_loss_5)
idf_loss_models_se <- list(idf_loss_1_se, idf_loss_2_se, idf_loss_3_se, idf_loss_4_se, idf_loss_5_se)
dir.create("tex", recursive = TRUE, showWarnings = FALSE)
teX(idf_loss_models, idf_loss_models_se, "/03112026_idf_loss_models")

# =========================
# T5: NFHL LAYER REGRESSIONS
# =========================

harvey_nfhl_new <- list(nfhl1, nfhl2, nfhl3, nfhl4, nfhl5)
harvey_nfhl_new_se <- list()

harvey_nfhl_new_se[[1]] <- sqrt(diag(vcovCL(nfhl1, cluster = td_harvey_nfhl$county_fips, type = "HC1")))
harvey_nfhl_new_se[[2]] <- sqrt(diag(vcovCL(nfhl2, cluster = td_harvey_nfhl$county_fips, type = "HC1")))
harvey_nfhl_new_se[[3]] <- sqrt(diag(vcovCL(nfhl3, cluster = td_harvey_nfhl$county_fips, type = "HC1")))
harvey_nfhl_new_se[[4]] <- sqrt(diag(vcovCL(nfhl4, cluster = td_harvey_nfhl$county_fips, type = "HC1")))
harvey_nfhl_new_se[[5]] <- sqrt(diag(vcovCL(nfhl5, cluster = td_harvey_nfhl$county_fips, type = "HC1")))

dir.create("tex", recursive = TRUE, showWarnings = FALSE)
teX(harvey_nfhl_new, harvey_nfhl_new_se, "/03112026_harvey_nfhl_new_models", digits = 1)

# =========================
# T6: ELEVATION CONTROL REGRESSIONS
# =========================

elev_se <- list()

elev_se[[1]] <- sqrt(diag(vcovCL(e1, cluster = td_txla_elev$county_fips, type = "HC1")))
elev_se[[2]] <- sqrt(diag(vcovCL(e2, cluster = td_txla_elev$county_fips, type = "HC1")))
elev_se[[3]] <- sqrt(diag(vcovCL(e3, cluster = td_txla_elev$county_fips, type = "HC1")))
elev_se[[4]] <- sqrt(diag(vcovCL(e4, cluster = td_harvey_elev$county_fips, type = "HC1")))
elev_se[[5]] <- sqrt(diag(vcovCL(e5, cluster = td_harvey_elev$county_fips, type = "HC1")))
elev_se[[6]] <- sqrt(diag(vcovCL(e6, cluster = td_harvey_elev$county_fips, type = "HC1")))

dir.create("tex", recursive = TRUE, showWarnings = FALSE)
teX(list(e1, e2, e3, e4, e5, e6), elev_se, "/03012026_elevation_models")

# =========================
# T7: DIFF TREATMENT VARS
# =========================

floodplain_models <- list(t1, t2, t3, t4, t5, t6, t7, t8)
floodplain_models_se <- list()

floodplain_models_se[[1]] <- sqrt(diag(vcovCL(t1, cluster = td_harvey_nfhl_elev$county_fips, type = "HC1")))
floodplain_models_se[[2]] <- sqrt(diag(vcovCL(t2, cluster = td_harvey_nfhl_elev$county_fips, type = "HC1")))
floodplain_models_se[[3]] <- sqrt(diag(vcovCL(t3, cluster = td_harvey_nfhl_elev$county_fips, type = "HC1")))
floodplain_models_se[[4]] <- sqrt(diag(vcovCL(t4, cluster = td_harvey_nfhl_elev$county_fips, type = "HC1")))
floodplain_models_se[[5]] <- sqrt(diag(vcovCL(t5, cluster = td_harvey_nfhl_elev$county_fips, type = "HC1")))
floodplain_models_se[[6]] <- sqrt(diag(vcovCL(t6, cluster = td_harvey_nfhl_elev$county_fips, type = "HC1")))
floodplain_models_se[[7]] <- sqrt(diag(vcovCL(t7, cluster = td_harvey_nfhl_elev$county_fips, type = "HC1")))
floodplain_models_se[[8]] <- sqrt(diag(vcovCL(t8, cluster = td_harvey_nfhl_elev$county_fips, type = "HC1")))

dir.create("tex", recursive = TRUE, showWarnings = FALSE)
teX(floodplain_models, floodplain_models_se,"/03112026_floodplain_treatment_models")

# =========================
# T8-9: PEAK MODELS INVESTIGATION + CORR MATRIX
# =========================

peak_sub_models <- list(P1, P2, P3, P4, P5, P6)
peak_sub_models_se <- list()

peak_sub_models_se[[1]] <- sqrt(diag(vcovCL(P1, cluster = td_txla_elev$county_fips, type = "HC1")))
peak_sub_models_se[[2]] <- sqrt(diag(vcovCL(P2, cluster = td_txla_elev$county_fips, type = "HC1")))
peak_sub_models_se[[3]] <- sqrt(diag(vcovCL(P3, cluster = td_txla_elev$county_fips, type = "HC1")))
peak_sub_models_se[[4]] <- sqrt(diag(vcovCL(P4, cluster = td_txla_elev$county_fips, type = "HC1")))
peak_sub_models_se[[5]] <- sqrt(diag(vcovCL(P5, cluster = td_txla_elev$county_fips, type = "HC1")))
peak_sub_models_se[[6]] <- sqrt(diag(vcovCL(P6, cluster = td_txla_elev$county_fips, type = "HC1")))

dir.create("tex", recursive = TRUE, showWarnings = FALSE)
teX(peak_sub_models, peak_sub_models_se,"/03112026_peak_sub_models")

vars_peak <- sf::st_drop_geometry(td_harvey_elev)[, c("wetland_2001_2016", "harvey_peak_stage_max", "elevation_m", "height_above_gnd_max")]
cor_matrix <- cor(vars_peak, use = "pairwise.complete.obs")
library(corrplot)
corrplot(cor_matrix)
stargazer(cor_matrix, summary = FALSE, type = "latex", out = "output/figures/harvey_vars_peak_matrix.tex")

# =========================
# T10: COASTALS
# =========================

coastal_models <- list(c1, c2, c3, c4, c5, c6)
coastal_models_se <- list()

coastal_models_se[[1]] <- sqrt(diag(vcovCL(c1, cluster = td0$county_fips, type = "HC1")))
coastal_models_se[[2]] <- sqrt(diag(vcovCL(c2, cluster = td0$county_fips, type = "HC1")))
coastal_models_se[[3]] <- sqrt(diag(vcovCL(c3, cluster = td_harvey$county_fips, type = "HC1")))
coastal_models_se[[4]] <- sqrt(diag(vcovCL(c4, cluster = td_harvey$county_fips, type = "HC1")))
coastal_models_se[[5]] <- sqrt(diag(vcovCL(c5, cluster = td0$county_fips[td0$coastal == T], type = "HC1")))
coastal_models_se[[6]] <- sqrt(diag(vcovCL(c6, cluster = td0$county_fips[td0$coastal == F], type = "HC1")))

dir.create("tex", recursive = TRUE, showWarnings = FALSE)
teX(coastal_models, coastal_models_se,"/03112026_coastal_models")


