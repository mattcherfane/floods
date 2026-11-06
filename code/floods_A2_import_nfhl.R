# =============================================================================
# floods_A2_import_nfhl.R
# Computes the share of each zip code's (2010 ZCTA) land area in FEMA NFHL
# Zone A, Zone V, and Zone X, one state at a time.
#
# Inputs:  FEMA NFHL state geodatabases in data/raw/fema_nfhl/each_state/
#          (not included; see README Section 1.6), and the TD ZCTA shapefile
#          and zip-state crosswalk in replication_files_TD/.
# Output:  data/derived/nfhl_pct_zip_<state FIPS>.RData (object z_df)
#
# The outputs used in the paper are included in data/derived/, so this script
# does not need to be run to reproduce the results.
# =============================================================================

library('sf')
sf_use_s2(FALSE)
library(lwgeom)

list.files('data/raw/fema_nfhl/each_state/')
zips <- st_read('replication_files_TD/data/geo/tl_2019_us_zcta510/tl_2019_us_zcta510.shp')

load('replication_files_TD/data/geo/zip_state_crosswalk.Rdata')
fz_nfhls <- list.files('data/raw/fema_nfhl/each_state/')
fz <- fz_nfhls[setdiff(1:length(fz_nfhls),grep("zip",fz_nfhls))]
sz <- sort(unique(zip$state))


#for(s in sz[-1]){
#for(s in sz[17:49]){
#for(s in sz[16:49]){
#	which(sz=="48")
#[1] 42
#for(s in sz[43:49]){
for(s in sz[42]){

clock <- Sys.time()

print(paste("initializing state fips",s))

fz0 <- fz[grep(paste("NFHL_",s,sep=""),fz)]

dir <- paste('data/raw/fema_nfhl/each_state/',fz0,sep="")
fzz <- list.files(dir)
fzz0 <- fzz[grep("gdb",fzz)]
#st_layers(paste(dir,fzz0,sep="/"))
gdb <- st_read(paste(dir,fzz0,sep="/"),layer="S_FLD_HAZ_AR")


zs <- subset(zip,state==s)$zip

zips_state <- subset(zips,ZCTA5CE10 %in% zs)
z_areas <- st_area(st_transform(zips_state,"+proj=eqc"))

z_df <- data.frame(
zip=zips_state$ZCTA5CE10,
area=z_areas,
area_V=NA,
area_A=NA,
area_X=NA,
pct_V=NA,
pct_A=NA,
pct_X=NA
)

gdb_A <- subset(gdb,FLD_ZONE %in% c("A","AE","AH","AO","A99"))

#for Texas!!
#> summary(st_geometry(gdb_A))
# MULTIPOLYGON  MULTISURFACE     epsg:4269 +proj=long...
#       172531             2             0             0
#gdb_A$geom_label <- st_geometry(gdb_A)
gdb_A0 <- st_cast(gdb_A,"MULTIPOLYGON")

#> summary(st_geometry(gdb_A))
# MULTIPOLYGON  MULTISURFACE     epsg:4269 +proj=long...
#       172531             2             0             0
#> summary(st_geometry(gdb_A0))
# MULTIPOLYGON     epsg:4269 +proj=long...
#       172533             0             0

#gdb_A_union <- st_union(gdb_A)
gdb_A_union <- st_union(gdb_A0)

print("done unifying A after")
print(Sys.time()-clock)

poly00 <- st_intersection(zips_state,gdb_A_union)

#z_area_A_union <- st_area(poly00)
z_area_A_union <- st_area(st_transform(poly00,"+proj=eqc"))
#plausible!!
#cbind(z_area_A/z_areas,z_area_A_union/z_areas)
ind <- zips_state$ZCTA5CE10 %in% poly00$ZCTA5CE10
z_df$area_A[ind] <- z_area_A_union

print("done with A after")
print(Sys.time()-clock)

gdb_V <- subset(gdb,FLD_ZONE %in% c("V","VE"))

if(nrow(gdb_V)>0){
gdb_V_union <- st_union(gdb_V)

print("done unifying V after")
print(Sys.time()-clock)

#st_area(gdb_V_union)
#sum(st_area(gdb_V))
poly00 <- st_intersection(zips_state,gdb_V_union)
#z_area_V_union <- st_area(poly00)
z_area_V_union <- st_area(st_transform(poly00,"+proj=eqc"))
ind <- zips_state$ZCTA5CE10 %in% poly00$ZCTA5CE10
z_df$area_V[ind] <- z_area_V_union

print("done with V after")
print(Sys.time()-clock)
}
if(nrow(gdb_V)==0){print("no gdb_V")}
#summary(z_area_V_union/z_areas[zips_state$ZCTA5CE10 %in% poly00$ZCTA5CE10])
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0001533 0.0243959 0.0418998 0.0824063 0.0929278 0.3210929 

##X breaks for Texas...
gdb_X <- subset(gdb,FLD_ZONE %in% "X")
gdb_X0 <- st_cast(gdb_X)
gdb_X_union <- st_union(gdb_X0)
#gdb_X_union <- st_union(gdb_X)
print("done unifying X after")
print(Sys.time()-clock)
#although coordinates are longitude/latitude, st_union assumes that they are planar
#st_area(gdb_X_union)
#sum(st_area(gdb_X))
poly00 <- st_intersection(zips_state,gdb_X_union)
#z_area_X_union <- st_area(poly00)
z_area_X_union <- st_area(st_transform(poly00,"+proj=eqc"))

#summary(z_area_X_union/z_areas[zips_state$ZCTA5CE10 %in% poly00$ZCTA5CE10])
ind <- zips_state$ZCTA5CE10 %in% poly00$ZCTA5CE10
z_df$area_X[ind] <- z_area_X_union

print("done with X after")
print(Sys.time()-clock)

for(v in c("V","A","X")){
vec <- z_df[,paste("area",v,sep="_")]/z_df[,"area"]
vec[is.na(vec)] <- 0
z_df[,paste("pct",v,sep="_")] <- vec
}


dir <- paste('data/derived/nfhl_pct_zip_',s,'.RData',sep="")
gdb_zones <- gdb$FLD_ZONE
save(z_df,gdb_zones,file=dir)
print(summary(z_df))
print(paste("done saving with",s,"after"))
print(Sys.time()-clock)
}