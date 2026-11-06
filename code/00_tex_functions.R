deathstar <- function(star,loc){
#example: deathstar(stargazer(ol,omit.stat=c("f","ser")),"~/Desktop/UtopiaTree/Tables/AA")
hm <- star
hm <- hm[-c(1:6,length(hm),length(hm)-2)]
write(hm,file=loc)
}


library(stargazer)

# line0 <- list(
# c("Fixed effects","State","State","State","State"),    
# c("Original data","Yes","","",""),
# c("Truncate at 0.5pct","","Yes","",""),
# c("Truncate at 1pct","","","Yes",""),
# c("Drop TX, LA","","","","Yes"))

#dir <- 'output/tablesmain_ol_2k23_01_23_all.tex'
#deathstar(stargazer(zip_ld,se=zip_ld_se,omit=c("state_fips","Constant")#,"developed","income","population","housing","CRS","Constant")
#,omit.stat=c("f","res.dev","ser"),add.lines=line0,digits=3,star.cutoffs=NA),dir)

# tx <- readLines(dir)
# tx <- gsub("Yes","\\\\checkmark",tx)
# tx <- gsub("wetland\\\\_2001\\\\_2016","Local wetland change (ha)",tx)
# tx <- gsub("claims\\\\_2001\\\\_2016","Zip code-level NFIP claims (US\\\\$)",tx)
# tx <- gsub("developed\\\\_2001\\\\_2016","\\\\emph\\{Controls\\} \\\\\\\\ Developed area (ha)",tx)
# tx <- gsub("income\\\\_2001\\\\_2016","Median income (US\\\\$)",tx)
# tx <- gsub("population\\\\_2001\\\\_2016","Population",tx)
# tx <- gsub("housing\\\\_units\\\\_2001\\\\_2016","Number of housing units",tx)
# tx <- gsub("housing\\\\_value\\\\_2001\\\\_2016","Median home value (US\\\\$)",tx)
# tx <- gsub("CRS\\\\_2001\\\\_2016","CRS discount (percent)",tx)
# writeLines(tx,con=dir)

# dir <- 'output/tablesmain_ol_2k23_01_23.tex'
# deathstar(stargazer(zip_ld,se=zip_ld_se,omit=c("state_fips","Constant","developed","income","population","housing","CRS")
# ,omit.stat=c("f","res.dev","ser"),add.lines=line0,digits=3,star.cutoffs=NA),dir)

# tx <- readLines(dir)
# tx <- gsub("Yes","\\\\checkmark",tx)
# tx <- gsub("wetland\\\\_2001\\\\_2016","Local wetland change (ha)",tx)
# tx <- gsub("claims\\\\_2001\\\\_2016","Zip code-level NFIP claims (US\\\\$)",tx)
# writeLines(tx,con=dir)


teX <- function(ol,ol_se,dir0,digits=3){

line0 <- list(
c("Fixed effects","State","State","State","State","State"),    
c("Original data","Yes","","",""),
c("Truncate at 0.5pct","","Yes","",""),
c("Truncate at 1pct","","","Yes",""),
c("Drop 2017","","","","Yes"),
c("Drop TX, LA","","","","","Yes"))

dir <- paste('output/tables',dir0,'_all.tex',sep="")
deathstar(stargazer(ol,se=ol_se,omit=c("state_fips","Constant"),omit.stat=c("f","res.dev","ser"),add.lines=line0,digits=digits,star.cutoffs=NA),dir)

tx <- readLines(dir)
tx <- gsub("Yes","\\\\checkmark",tx)
tx <- gsub("wetland\\\\_2001\\\\_2016","Local wetland change (ha)",tx)
tx <- gsub("wetland\\\\_gain","Local wetland gain (ha)",tx)
tx <- gsub("wetland\\\\_loss","Local wetland loss (ha)",tx)
tx <- gsub("wetlands\\\\_change\\\\_up","Upstream wetland change (ha)",tx)
tx <- gsub("wetlands\\\\_change\\\\_all","Watershed wetland change (ha)",tx)
tx <- gsub("wetlands\\\\_gain\\\\_up","Upstream wetland gain (ha)",tx)
tx <- gsub("wetlands\\\\_gain\\\\_all","Watershed wetland gain (ha)",tx)
tx <- gsub("wetlands\\\\_loss\\\\_up","Upstream wetland loss (ha)",tx)
tx <- gsub("wetlands\\\\_loss\\\\_all","Watershed wetland loss (ha)",tx)

tx <- gsub("claims\\\\_2001\\\\_2016","Zip code-level NFIP claims (US\\\\$)",tx)
tx <- gsub("developed\\\\_2001\\\\_2016","\\\\emph\\{Controls\\} \\\\\\\\ Developed area (ha)",tx)
tx <- gsub("income\\\\_2001\\\\_2016","Median income (US\\\\$)",tx)
tx <- gsub("population\\\\_2001\\\\_2016","Population",tx)
tx <- gsub("housing\\\\_units\\\\_2001\\\\_2016","Number of housing units",tx)
tx <- gsub("housing\\\\_value\\\\_2001\\\\_2016","Median home value (US\\\\$)",tx)
tx <- gsub("CRS\\\\_2001\\\\_2016","CRS discount (percent)",tx)
writeLines(tx,con=dir)


dir <- paste('output/tables',dir0,'.tex',sep="")
deathstar(stargazer(ol,se=ol_se,omit=c("state_fips","Constant","developed","income","population","housing","CRS","all")
,omit.stat=c("f","res.dev","ser"),add.lines=line0,digits=digits,star.cutoffs=NA),dir)

tx <- readLines(dir)
tx <- gsub("Yes","\\\\checkmark",tx)
tx <- gsub("wetland\\\\_2001\\\\_2016","Local wetland change (ha)",tx)
tx <- gsub("wetland\\\\_gain","Local wetland gain (ha)",tx)
tx <- gsub("wetland\\\\_loss","Local wetland loss (ha)",tx)

tx <- gsub("wetlands\\\\_change\\\\_up","Upstream wetland change (ha)",tx)
tx <- gsub("wetlands\\\\_change\\\\_all","Watershed wetland change (ha)",tx)
tx <- gsub("wetlands\\\\_gain\\\\_up","Upstream wetland gain (ha)",tx)
tx <- gsub("wetlands\\\\_gain\\\\_all","Watershed wetland gain (ha)",tx)
tx <- gsub("wetlands\\\\_loss\\\\_up","Upstream wetland loss (ha)",tx)
tx <- gsub("wetlands\\\\_loss\\\\_all","Watershed wetland loss (ha)",tx)

tx <- gsub("claims\\\\_2001\\\\_2016","Zip code-level NFIP claims (US\\\\$)",tx)
writeLines(tx,con=dir)

}


##stitch together!!
stitch <- function(){
dir0 <- "2k23_01_27_zip_ld"
dir <- paste('output/tables',dir0,'.tex',sep="")

tx <- readLines(dir)
tx <- gsub("ex] \\& \\(1","ex] \\\\emph{1. Long difference} \\& \\(1",tx)


dir0 <- "2k23_01_27_zip_ld_2"
dir <- paste('output/tables',dir0,'.tex',sep="")
#tx2 <- readLines(dir)[c(7:15,22)] #R2
tx2 <- readLines(dir)[7:14]
tx2 <- gsub("ex] \\& \\(1","ex] \\\\emph{2. Long difference (piecewise)} \\& \\(1",tx2)

dir0 <- "2k23_01_27_zip_did"
dir <- paste('output/tables',dir0,'.tex',sep="")
#tx3 <- readLines(dir)[c(7:15,22)] #R2
tx3 <- readLines(dir)[7:14]
tx3 <- gsub("ex] \\& \\(1","ex] \\\\emph{3. Upstream DID} \\& \\(1",tx3)

dir0 <- "2k23_01_27_zip_did_2"
dir <- paste('output/tables',dir0,'.tex',sep="")
#tx4 <- readLines(dir)[c(7:21,28)] #R2
tx4 <- readLines(dir)[7:21]
tx4 <- gsub("ex] \\& \\(1","ex] \\\\emph{4. Upstream DID (piecewise)} \\& \\(1",tx4)


dir0 <- "2k23_01_27_zip_all"
dir <- paste('output/tables',dir0,'.tex',sep="")
#writeLines(c(tx[c(1:12,19)],tx2,tx3,tx4,tx[c(13:18,21:23)]),con=dir)#R2
writeLines(c(tx[1:11],tx2,tx3,tx4,tx[c(13:18,21:24)]),con=dir)
}