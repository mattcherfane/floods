#load their data
td <- read.csv('replication_files_TD/data/final/zip_LD_data.csv')

# =========================
# Originally from tidal_A1 load TD data_NEW
# =========================

#actual estimating sample
td0 <- subset(td,!is.na(claims_2001_2016) & !is.na(wetland_2001_2016) & !is.na(developed_2001_2016) & !is.na(income_2001_2016) & !is.na(population_2001_2016) & !is.na(housing_units_2001_2016) & !is.na(housing_value_2001_2016) & !is.na(CRS_2001_2016) & !is.na(state_fips) & !is.na(county_fips))

#add coastal:
coastal <- read.csv('replication_files_TD/data/geo/coastal_zips.csv')
td0 <- merge(td0,coastal,by="zip")

zip_ld <- list()
summary(zip_ld[[1]] <- lm(claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips),data = td0))

#only diff, drop TX+LA
summary(zip_ld[[2]] <- lm(claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips),data = td,state_fips!="48" & state_fips!="22"))

library(lmtest)
library(sandwich)

#drop outliers
qz <- with(td0,quantile(claims_2001_2016,c(.001,.005,.01,.99,.995,.999),na.rm=T))

zip_ld <- list()
zip_ld_se <- list()
for(i in 1:4){
  if(i==1){dat <- td0}
  if(i==2){dat <- subset(td0,claims_2001_2016>qz[2] & claims_2001_2016<qz[5])}
  if(i==3){dat <- subset(td0,claims_2001_2016>qz[3] & claims_2001_2016<qz[4])}
  if(i==4){dat <- subset(td0,state_fips!="48" & state_fips!="22")}
  if(i==5){dat <- td0_no17}
  
  
  zip_ld[[i]] <- lm(claims_2001_2016 ~ wetland_2001_2016 + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips),data = dat)
  zip_ld_se[[i]] <- sqrt(diag(vcovCL(zip_ld[[i]],cluster=dat$county_fips,type="HC1")))
}

##piecewise
zip_ld_2 <- list()
zip_ld_2_se <- list()
for(i in 1:4){
  if(i==1){dat <- td0}
  if(i==2){dat <- subset(td0,claims_2001_2016>qz[2] & claims_2001_2016<qz[5])}
  if(i==3){dat <- subset(td0,claims_2001_2016>qz[3] & claims_2001_2016<qz[4])}
  if(i==4){dat <- subset(td0,state_fips!="48" & state_fips!="22")}
  if(i==5){dat <- td0_no17}
  
  
  zip_ld_2[[i]] <- lm(claims_2001_2016 ~ wetland_gain + wetland_loss + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips),data = dat)
  zip_ld_2_se[[i]] <- sqrt(diag(vcovCL(zip_ld_2[[i]],cluster=dat$county_fips,type="HC1")))
}

# =========================
# tidal_K1_hur_harvey
# =========================

library(raster)
library(sf)
#Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE
sf_use_s2(FALSE)
library(lwgeom)
library(stargazer)
library(AER)

zips <- st_read('replication_files_TD/data/geo/tl_2019_us_zcta510/tl_2019_us_zcta510.shp')
load('replication_files_TD/data/geo/zip_state_crosswalk.Rdata')

#"zip"
z_TX_LA <- subset(zip,state %in% c("22","48"))$zip

crosswalk <- read.table('data/raw/census/zcta_county_rel_10.txt',sep=",",header=T)
z_TX_LA_v2 <- subset(crosswalk,STATE %in% c("22","48"))$ZCTA5
zips_TX_LA <- subset(zips,ZCTA5CE10 %in% z_TX_LA)
zips_TX_LA_v2 <- subset(zips,ZCTA5CE10 %in% z_TX_LA_v2)

hwms <- read.csv('data/raw/usgs_harvey/peak_summary.csv')
hwms_coords <- hwms[,c("longitude_dd","latitude_dd")]
hwms_points <- st_as_sf(hwms_coords,coords=c("longitude_dd","latitude_dd"))
#hwms_points$geometry <- st_point(hwms_points$geometry)
st_crs(hwms_points) <- st_crs(zips_TX_LA)

test <- st_contains(zips_TX_LA,hwms_points,sparse=F)
summary(colSums(test)) #no more than 1 match, some zeros b/c ocean probably
ind <- rowSums(test)>0
#   Mode   FALSE    TRUE
#logical    1934     261
zips_Harvey <- zips_TX_LA$ZCTA5CE10[ind]
#> sum(ind)
#[1] 261

mat <- test %*% hwms[,"peak_stage"]
vec <- mat/rowSums(test)
vec[is.na(vec)] <- 0

library(ggplot2)

z <- ggplot() + geom_sf(data=zips_TX_LA,aes(),color=NA,lwd=0.1)
z <- z + geom_sf(data=subset(zips_TX_LA,ZCTA5CE10 %in% zips_Harvey),aes(),fill="#4A6672",color=NA,lwd=0.1,alpha=1)
#z <- z + geom_sf(data=subset(zips_TX_LA,ZCTA5CE10 %in% zips_Harvey_v2),aes(),fill="red",color=NA,lwd=0.1,alpha=0.2)
z <- z + theme_void() + theme(legend.position=c(0.4,0.4))
z0 <- z
today <- '04022026'
dir <- paste('output/figures/',today,'_Harvey_zips.pdf',sep="")
pdf(dir)
print(z0)
dev.off()

td0$ind_Harvey <- td0$zip %in% zips_Harvey
#note 242 observations i.e. 19 zip codes impacted by harvey not present in td0 data

#VIOLATES EXCLUSION:
summary(lm(wetland_2001_2016 ~ ind_Harvey,td0))
#(Intercept)     -0.2461     0.6750  -0.365           0.715
#ind_HarveyTRUE -47.2741     6.9608  -6.792 0.0000000000113 ***

summary(lm(wetland_loss ~ ind_Harvey,td0))
#(Intercept)    -11.6467     0.4332 -26.883   <2e-16 ***
#ind_HarveyTRUE -38.9073     4.4675  -8.709   <2e-16 ***

summary(lm(wetlands_loss_up ~ ind_Harvey,td0))
#(Intercept)    -19.4860     0.6192  -31.47   <2e-16 ***
#ind_HarveyTRUE -98.1362     6.3853  -15.37   <2e-16 ***

summary(lm(wetland_2001_2016 ~ ind_Harvey + developed_2001_2016 + income_2001_2016 + population_2001_2016 + housing_units_2001_2016 + housing_value_2001_2016 + CRS_2001_2016 + factor(state_fips),td0))
#(Intercept)               8.273212760   4.557696603   1.815       0.069502 .
#ind_HarveyTRUE          -18.819378192   7.336931075  -2.565       0.010323 *

# =========================
# KNew_hur_harv_data_MC
# =========================

# peak stage points
peak <- read.csv("data/raw/usgs_harvey/peak_summary.csv")
peak_sf <- st_as_sf(peak, coords=c("longitude_dd","latitude_dd"), crs=st_crs(zips))

# point-in-zip mapping
M_peak <- st_contains(zips, peak_sf, sparse=FALSE)

# ZIP-level summaries
zips$harvey_any_peak <- as.integer(rowSums(M_peak) > 0)

# mean peak_stage within zip (0 if none)
peak_stage_mat <- M_peak %*% ifelse(is.na(peak$peak_stage), 0, peak$peak_stage)
zips$harvey_peak_stage_mean <- as.numeric(peak_stage_mat / pmax(1, rowSums(M_peak)))
zips$harvey_peak_stage_mean[rowSums(M_peak)==0] <- 0

# max peak_stage within zip
zips$harvey_peak_stage_max <- apply(M_peak, 1, function(idx){
  if(!any(idx)) return(0) 
  max(peak$peak_stage[idx], na.rm=TRUE)})

# high-water marks (data = filtered_hwms.csv)
hwm <- read.csv("data/raw/usgs_harvey/filtered_hwms.csv")
hwm_sf <- st_as_sf(hwm, coords=c("longitude_dd","latitude_dd"), crs=st_crs(zips))

M_hwm <- st_contains(zips, hwm_sf, sparse=FALSE)
zips$harvey_any_hwm <- as.integer(rowSums(M_hwm) > 0)

zips$harvey_hwm_elev_max <- apply(M_hwm, 1, function(idx){
  if(!any(idx)) return(0)
  max(hwm$elev_ft[idx], na.rm=TRUE)})

zips$harvey_hwm_count <- rowSums(M_hwm)

# INCLUDE height_above_gnd INTO WORKING DATASET # 03/05/2026

# mean height_above_gnd within zip (0 if none)
height_mat <- M_peak %*% ifelse(is.na(peak$height_above_gnd), 0, peak$height_above_gnd)

zips$height_above_gnd_mean <- as.numeric(height_mat / pmax(1, rowSums(M_peak)))
zips$height_above_gnd_mean[rowSums(M_peak)==0] <- 0

# max height_above_gnd within zip
zips$height_above_gnd_max <- apply(M_peak, 1, function(idx){
  if(!any(idx)) return(0)
  max(peak$height_above_gnd[idx], na.rm=TRUE)})

# MERGE INTO td0: td0 has zip as character; zips has ZCTA5CE10 # UPDATED 03/05/2026
harvey_zip_controls <- st_drop_geometry(zips)[,c("ZCTA5CE10",
                                                 "harvey_any_peak","harvey_peak_stage_mean","harvey_peak_stage_max",
                                                 "harvey_any_hwm","harvey_hwm_elev_max","harvey_hwm_count", "height_above_gnd_mean","height_above_gnd_max")]
names(harvey_zip_controls)[1] <- "zip"

td0 <- merge(td0, harvey_zip_controls, by="zip", all.x=TRUE)
td0[is.na(td0)] <- 0  # for the new controls only (don’t blanket this if td0 has other NAs)

# SF Zip Distance Data #
library(sf)

td0_sf <- st_as_sf(td0, coords = c("lon", "lat"), crs = 4326)

#checks
st_crs(td0_sf)
st_crs(peak_sf)

#transform
peak_sf <- st_transform(peak_sf, 4326)
st_crs(peak_sf)

#test
td0_proj  <- st_transform(td0_sf, 5070)
peak_proj <- st_transform(peak_sf, 5070)
dist_matrix <- st_distance(td0_proj, peak_proj)
td0_proj$dist_peak_m <- apply(dist_matrix, 1, min)
td0_proj$dist_peak_km <- as.numeric(td0_proj$dist_peak_m) / 1000
summary(td0_proj$dist_peak_km)
#  Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.3283  975.4456 1380.3149 1404.8724 1862.7760 3078.7068 

#construct new 
td_txla <- subset(td0_proj, state_fips %in% c("48","22"))
summary(td_txla$dist_peak_km)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.3283  63.0330 183.5213 213.1890 293.6831 883.4529 

# =========================
# K_harvey_elevdata_03022026
# =========================

# # CSV IS AVAILABLE IN DATA FOLDER IF YOU DON'T WANT TO RUN (IT WILL TAKE A FEW MINUTES)
# library(readr)
# library(dplyr)
# uszips <- read_csv("data/raw/simplemaps_uszips_basicv1.94/uszips.csv")
# head(uszips)
# txla_uszips <- uszips %>% filter(state_id %in% c("TX", "LA")) %>%
#   filter(zcta == TRUE) %>%   # keep real ZCTAs only
#   select(zip, lat, lng)
# 
# nrow(txla_uszips)
# head(txla_uszips)
# 
# #install.packages(c("httr", "jsonlite", "purrr"))
# library(httr)
# library(jsonlite)
# library(purrr)
# library(dplyr)
# 
# get_elevation_batch <- function(df_batch) {
#   coords <- paste0(df_batch$lat, ",", df_batch$lng, collapse="|")
#   response <- GET(url = "https://api.open-elevation.com/api/v1/lookup", query = list(locations = coords))
#   content_json <- fromJSON(content(response, "text"), flatten = TRUE)
#   df_batch$elevation_m <- content_json$results$elevation
#   return(df_batch)
# }
# 
# batch_size <- 100
# split_batches <- split(txla_uszips, ceiling(seq_len(nrow(txla_uszips)) / batch_size))
# elevation_results <- map_dfr(split_batches, function(batch) {
#   Sys.sleep(1)   # avoid rate limit
#   get_elevation_batch(batch)
# })
# 
# head(elevation_results)
# summary(elevation_results$elevation_m)
# write.csv(elevation_results, "data/derived/txla_zip_elevation.csv", row.names = FALSE)

