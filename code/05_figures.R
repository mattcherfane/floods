
library(sf)
library(ggplot2)
library(dplyr)
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# =========================
# SETUP: Make sure to run the regressions before generating figures
# =========================

zips <- st_read("replication_files_TD/data/geo/tl_2019_us_zcta510/tl_2019_us_zcta510.shp")
states <- st_read("replication_files_TD/data/geo/tl_2020_us_state/tl_2020_us_state.shp")
td0$zip <- as.character(td0$zip)

# Restrict to Texas and Louisiana
states_txla <- states %>% filter(STUSPS %in% c("TX","LA"))
zips_txla <- st_intersection(zips, states_txla)
zips_txla <- zips_txla %>% left_join(td0, by = c("ZCTA5CE10" = "zip"))

nfhl <- readRDS("data/derived/nfhl_z_df0.rds")
load("data/derived/nfhl_pct_zip_48.RData")
print(names(z_df))
nfhl_LA <- nfhl[substr(nfhl$zip,1,2) == "70", ]
nfhl_final <- rbind(nfhl_LA, z_df)
td_txla_nfhl <- merge(td_txla, nfhl_final, by="zip", all.x=TRUE)
td_txla_nfhl$pct_nfhl <- td_txla_nfhl$pct_A + td_txla_nfhl$pct_V

harvey_zips <- td_harvey %>% st_drop_geometry() %>% distinct(zip) %>% mutate(zip = as.character(zip))
zips_harvey_outline <- zips_txla %>% mutate(ZCTA5CE10 = as.character(ZCTA5CE10)) %>% semi_join(harvey_zips, by = c("ZCTA5CE10" = "zip"))
harvey_outline_union <- zips_harvey_outline %>% summarise()

# NOTE:
# harvey_outline_union is NOT a physical Hurricane Harvey flood boundary. It is the union of TX/LA ZIP polygons included in the td_harvey sample. 
# Any holes inside the red outline therefore reflect sample construction. (ZIPs omitted from td_harvey) rather than proof of no Harvey exposure.

# Harvey ZIP list with intensity var (Height Above Ground)
td_harvey_intensity <- td_harvey %>% st_drop_geometry() %>% transmute(zip = sprintf("%05d", zip), intensity = as.numeric(height_above_gnd_max)) %>% filter(!is.na(intensity), intensity > 0)
harvey_points <- zips_txla %>% mutate(ZCTA5CE10 = as.character(ZCTA5CE10)) %>% left_join(td_harvey_intensity, by = c("ZCTA5CE10" = "zip"))
harvey_points_centroid <- harvey_points %>% filter(!is.na(intensity), intensity > 0) %>% st_centroid()

# =========================
# NFHL ZONE MAP
# =========================

td_txla_nfhl$zip <- as.character(td_txla_nfhl$zip)
zips_txla_nfhl <- zips_txla %>% select(ZCTA5CE10, geometry) %>% left_join(st_drop_geometry(td_txla_nfhl) %>% mutate(pct_nfhl = as.numeric(pct_nfhl)) %>% select(zip, pct_A, pct_V, pct_nfhl), by = c("ZCTA5CE10" = "zip"))

p_nfhl <- ggplot() +
  geom_sf(data = zips_txla_nfhl, aes(fill = pct_nfhl), color = NA) +
  geom_sf(data = states_txla, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "red", linewidth = 0.3) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", na.value = "grey70", name = "PCT of ZIP area\nin Zone A and Zone V (%)") +
  coord_sf(xlim = c(-106, -88), ylim = c(25, 37)) +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))
ggsave(filename = "output/figures/nfhl_txla.pdf", plot = p_nfhl, width = 8, height = 6)

# =========================
# NFHL ZONE A/V MAP
# =========================

td_txla_nfhl$zip <- as.character(td_txla_nfhl$zip)
zips_txla_nfhl <- zips_txla %>% select(ZCTA5CE10, geometry) %>% left_join(st_drop_geometry(td_txla_nfhl) %>% mutate(pct_A = as.numeric(pct_A), pct_V = as.numeric(pct_V)) %>% select(zip, pct_A, pct_V), by = c("ZCTA5CE10" = "zip"))

p_zoneA <- ggplot() +
  geom_sf(data = zips_txla_nfhl, aes(fill = pct_A), color = NA) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "red", linewidth = 0.3) +
  geom_sf(data = states_txla, fill = NA, color = "black", linewidth = 0.6) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", na.value = "grey70", name = "PCT of ZIP area\nin Zone A (0–1)") +
  coord_sf(xlim = c(-106, -88), ylim = c(25, 37)) +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))
ggsave(filename = "output/figures/zoneA_txla.pdf", plot = p_zoneA, width = 8, height = 6)

p_zoneV <- ggplot() +
  geom_sf(data = zips_txla_nfhl, aes(fill = pct_V), color = NA) +
  geom_sf(data = states_txla, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "red", linewidth = 0.3) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", na.value = "grey70", name = "PCT of ZIP area\nin Zone V (0–1)") +
  coord_sf(xlim = c(-106, -88), ylim = c(25, 37)) +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))
ggsave(filename = "output/figures/zoneV_txla.pdf", plot = p_zoneV, width = 8, height = 6)

# =========================
# HARVEY-ONLY NFHL ZONE MAP
# =========================

td_harvey_nfhl$zip <- as.character(td_harvey_nfhl$zip)
harvey_zip_list <- td_harvey_nfhl %>% st_drop_geometry() %>% transmute(zip = as.character(zip)) %>% distinct()
zips_harvey_nfhl <- zips_txla %>% mutate(ZCTA5CE10 = as.character(ZCTA5CE10)) %>% semi_join(harvey_zip_list, by = c("ZCTA5CE10" = "zip")) %>% left_join(td_harvey_nfhl %>% st_drop_geometry() %>% mutate(zip = as.character(zip), pct_nfhl = as.numeric(pct_nfhl)) %>% select(zip, pct_A, pct_V, pct_nfhl), by = c("ZCTA5CE10" = "zip"))
bbox <- st_bbox(zips_harvey_nfhl)

p_nfhl_harvey <- ggplot() +
  geom_sf(data = zips_harvey_nfhl, aes(fill = pct_nfhl), color = NA) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "red", linewidth = 0.3) +
  geom_sf(data = harvey_points_centroid, aes(size = intensity), color = "grey20", fill = "grey70", shape = 21, stroke = 0.25, alpha = 0.9) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", na.value = "grey70", name = "Share of ZIP area\nin SFHA (0–1)") +
  scale_size_continuous(name = "max high water mark\nabove ground, feet", range = c(0.5, 2.5)) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))
ggsave(filename = "output/figures/nfhl_harvey.pdf", plot = p_nfhl_harvey, width = 8, height = 6)

# =========================
# NFIP CLAIMS CHANGE MAP
# =========================

p_claims <- ggplot() +
  geom_sf(data = zips_txla, aes(fill = log(claims_2001_2016 + 1)), color = NA) +
  geom_sf(data = states_txla, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "red", linewidth = 0.3) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", na.value = "grey70", name = "Log NFIP claims\n2001—2016 + 1") +
  coord_sf(xlim = c(-106, -88), ylim = c(25, 37)) +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))
ggsave(filename = "output/figures/claims_txla.pdf", plot = p_claims, width = 8, height = 6)

# =========================
# HARVEY-ONLY NFIP CLAIMS MAP
# =========================

zips_harvey_claims <- zips_txla %>% mutate(ZCTA5CE10 = as.character(ZCTA5CE10)) %>% semi_join(td_harvey %>% st_drop_geometry() %>% mutate(zip = as.character(zip)), by = c("ZCTA5CE10" = "zip"))
bbox <- st_bbox(zips_harvey_claims)
p_claims_harvey <- ggplot() +
  geom_sf(data = zips_harvey_claims, aes(fill = log(claims_2001_2016 + 1)), color = NA) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "red", linewidth = 0.4) +
  geom_sf(data = harvey_points_centroid, aes(size = intensity), color = "grey20", fill = "grey70", shape = 21, stroke = 0.25, alpha = 0.9) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", na.value = "grey70", name = "Log NFIP claims\n2001—2016 + 1") +
  scale_size_continuous(name = "max high water mark\nabove ground, feet", range = c(0.5, 2.5)) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))
ggsave(filename = "output/figures/claims_harvey.pdf", plot = p_claims_harvey, width = 8, height = 6)

# =========================
# WETLANDS CHANGE MAP
# =========================

wetland_limits <- quantile(zips_txla$wetland_2001_2016, probs = c(0.01, 0.99), na.rm = TRUE)
p_wetlandschange <- ggplot() +
  geom_sf(data = zips_txla, aes(fill = wetland_2001_2016), color = NA) +
  geom_sf(data = states_txla, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "red", linewidth = 0.3) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#1a9850", midpoint = 0, limits = wetland_limits, oob = scales::squish, na.value = "grey70", name = "Net wetland change,\nha (2001—2016)") +
  coord_sf(xlim = c(-106, -88), ylim = c(25, 37)) +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))
ggsave(filename = "output/figures/wetlands_change_txla.pdf", plot = p_wetlandschange, width = 8, height = 6)

# =========================
# ELEVATION MAP
# =========================

td_txla_elev$zip <- as.character(td_txla_elev$zip)
zips_txla_elev <- zips_txla %>% select(ZCTA5CE10, geometry) %>% left_join(st_drop_geometry(td_txla_elev) %>% select(zip, elevation_m), by = c("ZCTA5CE10" = "zip"))
p_elevation <- ggplot() +
  geom_sf(data = zips_txla_elev, aes(fill = elevation_m), color = NA) +
  geom_sf(data = states_txla, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "red", linewidth = 0.3) +
  scale_fill_gradient(low = "#f7e5b2", high = "#8c510a", na.value = "grey70", name = "Elevation,\nmeters") +
  coord_sf(xlim = c(-106, -88), ylim = c(25, 37)) +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))
ggsave(filename = "output/figures/elevation_txla.pdf", plot = p_elevation, width = 8, height = 6)

# =========================
# NEW MAPS 03/31
# =========================

# claims_2016_5yr
p_claims_2016 <- ggplot() +
  geom_sf(data = zips_txla, aes(fill = log(claims_2016_5yr + 1)), color = NA) +
  geom_sf(data = states_txla, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "red", linewidth = 0.3) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", na.value = "grey70", name = "Log NFIP claims\n2016–2021 + 1") +
  coord_sf(xlim = c(-106, -88), ylim = c(25, 37)) +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))
ggsave(filename = "output/figures/claims_2016_txla.pdf", plot = p_claims_2016, width = 8, height = 6)

# harvey only claims_2016_5yr
bbox <- st_bbox(zips_harvey_claims)
xpad <- 0.15
ypad <- 0.15
p_claims_2016_harvey <- ggplot() +
  geom_sf(data = zips_harvey_claims, aes(fill = log(claims_2016_5yr + 1)), color = NA) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "red", linewidth = 0.3) +
  geom_sf(data = harvey_points_centroid, aes(size = intensity), color = "grey20", fill = "grey70", shape = 21, stroke = 0.25, alpha = 0.9) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", na.value = "grey70", name = "Log NFIP claims\n2016–2021 + 1") +
  scale_size_continuous(name = "max high-water mark\nabove ground, feet", range = c(0.5, 2.5)) +
  coord_sf(xlim = c(bbox["xmin"] - xpad, bbox["xmax"] + xpad), ylim = c(bbox["ymin"] - ypad, bbox["ymax"] + ypad), expand = FALSE) +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))
ggsave(filename = "output/figures/claims_2016_harvey.pdf", plot = p_claims_2016_harvey, width = 8, height = 6)

# wetland_ha_2001
p_wetland_ha_2001 <- ggplot() +
  geom_sf(data = zips_txla, aes(fill = log(wetland_ha_2001 + 1)), color = NA) +
  geom_sf(data = states_txla, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "red", linewidth = 0.4) +
  scale_fill_gradient(low = "#e5f5e0", high = "#006d2c", na.value = "grey70", name = "Log wetland area,\nha in 2001 + 1") +
  coord_sf(xlim = c(-106, -88), ylim = c(25, 37)) +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))
ggsave(filename = "output/figures/wetland_ha_2001_txla.pdf", plot = p_wetland_ha_2001, width = 8, height = 6)

# wetland_ha_2016
p_wetland_ha_2016 <- ggplot() +
  geom_sf(data = zips_txla, aes(fill = log(wetland_ha_2016 + 1)), color = NA) +
  geom_sf(data = states_txla, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "red", linewidth = 0.4) +
  scale_fill_gradient(low = "#e5f5e0", high = "#006d2c", na.value = "grey70", name = "Log wetland area,\nha in 2016 + 1") +
  coord_sf(xlim = c(-106, -88), ylim = c(25, 37)) +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))
ggsave(filename = "output/figures/wetland_ha_2016_txla.pdf", plot = p_wetland_ha_2016, width = 8, height = 6)

# GRAY OUT NEGATIVE CLAIMS CHANGES
zips_txla <- zips_txla %>% mutate(claims_pos = ifelse(claims_2001_2016 >= 0, log(claims_2001_2016 + 1), NA_real_))
p_claims_v2 <- ggplot() +
  geom_sf(data = zips_txla, aes(fill = claims_pos), color = NA) +
  geom_sf(data = states_txla, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "red", linewidth = 0.4) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", na.value = "grey70", name = "Log positive NFIP\nclaims change + 1") +
  coord_sf(xlim = c(-106, -88), ylim = c(25, 37)) +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))
ggsave(filename = "output/figures/claims_change_positive_txla.pdf", plot = p_claims_v2, width = 8, height = 6)

# FLIP CLAIMS AND INTENSITY
zips_txla <- zips_txla %>% mutate(intensity_fill = ifelse(height_above_gnd_max >= 0, log(height_above_gnd_max + 1), NA_real_), claims_dot = ifelse(claims_2001_2016 >= 0, log(claims_2001_2016 + 1), NA_real_))
zips_harvey_claims <- zips_harvey_claims %>% mutate(intensity_fill = ifelse(height_above_gnd_max >= 0, log(height_above_gnd_max + 1), NA_real_), claims_dot = ifelse(claims_2001_2016 >= 0, log(claims_2001_2016 + 1), NA_real_))
claims_points <- zips_harvey_claims %>% filter(!is.na(claims_dot)) %>% st_centroid()
bbox <- st_bbox(zips_harvey_outline)
p_claims_flip <- ggplot() +
  geom_sf(data = zips_harvey_claims, aes(fill = intensity_fill), color = NA) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "red", linewidth = 0.3) +
  geom_sf(data = claims_points, aes(size = claims_dot), color = "black", fill = "white", shape = 21, alpha = 0.7) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", na.value = "grey70", name = "Log max high-water mark\nabove ground, feet") +
  scale_size_continuous(name = "Log positive NFIP\nclaims change + 1", range = c(0.5, 3)) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))
ggsave(filename = "output/figures/claims_flip_harvey.pdf", plot = p_claims_flip, width = 8, height = 6)

library(tidyr)
library(xtable)

# FLOODCLAIMS TABLE
claims_summary <- td_txla %>% st_drop_geometry() %>% mutate(state = case_when(state_fips == 48 ~ "TX", state_fips == 22 ~ "LA")) %>% summarise(
    TX = c(
      sum(as.numeric(claims_2001_5yr)[state == "TX"], na.rm = TRUE),
      sum(as.numeric(claims_2016_5yr)[state == "TX"], na.rm = TRUE),
      sum(as.numeric(claims_2001_2016)[state == "TX"], na.rm = TRUE),
      sum(as.numeric(claims_2001_2016)[state == "TX" & ind_Harvey == 0], na.rm = TRUE),
      sum(as.numeric(claims_2001_2016)[state == "TX" & ind_Harvey == 1], na.rm = TRUE),
      100 * sum(as.numeric(claims_2001_2016)[state == "TX" & ind_Harvey == 1], na.rm = TRUE) /
        sum(as.numeric(claims_2001_2016)[state == "TX"], na.rm = TRUE)),
    LA = c(
      sum(as.numeric(claims_2001_5yr)[state == "LA"], na.rm = TRUE),
      sum(as.numeric(claims_2016_5yr)[state == "LA"], na.rm = TRUE),
      sum(as.numeric(claims_2001_2016)[state == "LA"], na.rm = TRUE),
      sum(as.numeric(claims_2001_2016)[state == "LA" & ind_Harvey == 0], na.rm = TRUE),
      sum(as.numeric(claims_2001_2016)[state == "LA" & ind_Harvey == 1], na.rm = TRUE),
      100 * sum(as.numeric(claims_2001_2016)[state == "LA" & ind_Harvey == 1], na.rm = TRUE) /
        sum(as.numeric(claims_2001_2016)[state == "LA"], na.rm = TRUE))) %>%
  mutate(
    row = c(
      "Flood claims in 2001 (000s)",
      "Flood claims in 2016 (000s)",
      "Change in flood claims, 2001–2016 (000s)",
      "Change in flood claims outside Harvey ZIPs (000s)",
      "Change in flood claims inside Harvey ZIPs (000s)",
      "Share of total change inside Harvey ZIPs (%)"),
    TX = c(round(TX[1:5] / 1000, 2), round(TX[6], 2)),
    LA = c(round(LA[1:5] / 1000, 2), round(LA[6], 2))) %>% select(row, TX, LA)
claims_summary
print(xtable(claims_summary, align = c("l", "l", "r", "r")), include.rownames = FALSE, include.colnames = TRUE, floating = FALSE, file = "output/tables/claims_summary.tex")

# WETlANDS AREA TABLE
wetland_summary <- td_txla %>% st_drop_geometry() %>% mutate(state = case_when(state_fips == 48 ~ "TX", state_fips == 22 ~ "LA")) %>% summarise(
    TX = c(
      sum(as.numeric(wetland_ha_2001)[state == "TX"], na.rm = TRUE),
      sum(as.numeric(wetland_ha_2016)[state == "TX"], na.rm = TRUE),
      sum(as.numeric(wetland_2001_2016)[state == "TX"], na.rm = TRUE),
      sum(as.numeric(wetland_2001_2016)[state == "TX" & ind_Harvey == 0], na.rm = TRUE),
      sum(as.numeric(wetland_2001_2016)[state == "TX" & ind_Harvey == 1], na.rm = TRUE),
      100 * sum(as.numeric(wetland_2001_2016)[state == "TX" & ind_Harvey == 1], na.rm = TRUE) /
        sum(as.numeric(wetland_2001_2016)[state == "TX"], na.rm = TRUE)),
    LA = c(
      sum(as.numeric(wetland_ha_2001)[state == "LA"], na.rm = TRUE),
      sum(as.numeric(wetland_ha_2016)[state == "LA"], na.rm = TRUE),
      sum(as.numeric(wetland_2001_2016)[state == "LA"], na.rm = TRUE),
      sum(as.numeric(wetland_2001_2016)[state == "LA" & ind_Harvey == 0], na.rm = TRUE),
      sum(as.numeric(wetland_2001_2016)[state == "LA" & ind_Harvey == 1], na.rm = TRUE),
      100 * sum(as.numeric(wetland_2001_2016)[state == "LA" & ind_Harvey == 1], na.rm = TRUE) /
        sum(as.numeric(wetland_2001_2016)[state == "LA"], na.rm = TRUE))) %>%
  mutate(
    row = c(
      "Wetland area in 2001 (ha)",
      "Wetland area in 2016 (ha)",
      "Net wetland change, 2001–2016 (ha)",
      "Net wetland change outside Harvey ZIPs (ha)",
      "Net wetland change inside Harvey ZIPs (ha)",
      "Share of total wetland change inside Harvey ZIPs (%)"),
    TX = round(TX, 2),
    LA = round(LA, 2)) %>% select(row, TX, LA)
wetland_summary
print(xtable(wetland_summary, align = c("l", "l", "r", "r")), include.rownames = FALSE, include.colnames = TRUE, floating = FALSE, file = "output/tables/wetland_summary.tex")

# ELEVATION TABLE
elevation_summary <- td_txla_elev %>% st_drop_geometry() %>% mutate(state = case_when(state_fips == 48 ~ "TX", state_fips == 22 ~ "LA")) %>%
  summarise(
    TX = c(
      mean(as.numeric(elevation_m)[state == "TX"], na.rm = TRUE),
      mean(as.numeric(elevation_m)[state == "TX" & ind_Harvey == 0], na.rm = TRUE),
      mean(as.numeric(elevation_m)[state == "TX" & ind_Harvey == 1], na.rm = TRUE),
      mean(as.numeric(elevation_m)[state == "TX" & ind_Harvey == 1], na.rm = TRUE) -
        mean(as.numeric(elevation_m)[state == "TX" & ind_Harvey == 0], na.rm = TRUE)),
    LA = c(
      mean(as.numeric(elevation_m)[state == "LA"], na.rm = TRUE),
      mean(as.numeric(elevation_m)[state == "LA" & ind_Harvey == 0], na.rm = TRUE),
      mean(as.numeric(elevation_m)[state == "LA" & ind_Harvey == 1], na.rm = TRUE),
      mean(as.numeric(elevation_m)[state == "LA" & ind_Harvey == 1], na.rm = TRUE) -
        mean(as.numeric(elevation_m)[state == "LA" & ind_Harvey == 0], na.rm = TRUE))) %>%
  mutate(
    row = c(
      "Mean elevation (m)",
      "Mean elevation outside Harvey ZIPs (m)",
      "Mean elevation inside Harvey ZIPs (m)",
      "Difference: inside minus outside (m)"),
    TX = round(TX, 2),
    LA = round(LA, 2)) %>% select(row, TX, LA)
elevation_summary_out <- elevation_summary
elevation_summary_out[, c("TX", "LA")] <- lapply(elevation_summary_out[, c("TX", "LA")], fmt_latex_num, digits = 2)
print(xtable(elevation_summary_out, align = c("l", "l", "r", "r")), include.rownames = FALSE, include.colnames = TRUE, floating = FALSE, sanitize.text.function = identity,
  file = "output/tables/elevation_summary.tex")

# =========================
# 4/02/2026 MAP UPDATES: Include Coastal
# =========================

#Coastal highlighted map
zips_txla <- zips_txla %>% mutate(coastal = as.logical(coastal))
zips_coastal_outline <- zips_txla %>% filter(coastal == TRUE)
p_claims_coastal <- ggplot() +
  geom_sf(data = zips_txla, aes(fill = log(claims_2001_2016 + 1)), color = NA) +
  geom_sf(data = zips_coastal_outline, fill = NA, color = "darkgreen", linewidth = 0.35) +
  geom_sf(data = states_txla, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "red", linewidth = 0.3) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", na.value = "grey70", name = "Log NFIP claims\n2001–2016 + 1") +
  coord_sf(xlim = c(-106, -88), ylim = c(25, 37)) +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))
ggsave(filename = "output/figures/claims_coastal_txla.pdf", plot = p_claims_coastal, width = 8, height = 6)

# NORMALIZED WETLANDS CHANGE MAP BASED ON ZIP SIZE
zips_txla$wetland_change_density <- zips_txla$wetland_2001_2016 / zips_txla$area_ha_2016
dens_limits <- quantile(zips_txla$wetland_change_density, probs = c(0.02, 0.98), na.rm = TRUE)
p_wetlandschange_density <- ggplot() +
  geom_sf(data = zips_txla, aes(fill = wetland_change_density), color = NA) +
  geom_sf(data = states_txla, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = harvey_outline_union, fill = NA, color = "red", linewidth = 0.3) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#1a9850", midpoint = 0, limits = dens_limits, oob = scales::squish, na.value = "grey70", name = "Wetland change per\nha ZIP area (2016)") +
  coord_sf(xlim = c(-106, -88), ylim = c(25, 37)) +
  theme_void() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))
ggsave(filename = "output/figures/wetlands_change_density_txla.pdf", plot = p_wetlandschange_density, width = 8, height = 6)



