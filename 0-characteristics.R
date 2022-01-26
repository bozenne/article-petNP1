## * package
library(data.table)

## * set working directory and path to dataset
if(Sys.info()["login"] == "hpl802"){
    path <- "c:/Users/hpl802/Documents/Consult/Consult-NRU/C45-WP1-Kristin/"
}else if(Sys.info()["nodename"] == "brice-Latitude-E5540"){
    path <- "~/Dropbox/C45-WP1-Kristin/"    
}else{
    path <- "C:/Users/localadmin/Dropbox/C45 - WP1 - Kristin"
}
path.code <- file.path(path,"code")
path.data <- file.path(path,"source")

## * Load data
source(file.path(path.code,"data-management-baseline.R"))
source(file.path(path.code,"data-management-longitudinal-prediction.R"))
source(file.path(path.code,"data-management-rescan.R"))

## * Number of patients

## ** baseline
table(df.baseline$group)
## Healthy Control            Case 
##              91              91 

## ** follow-up
table(df.longi$group)
## Case Healthy Control 
##   78              91 

table(df.longi$response.w8, useNA = "ifany")
## Non-responder         Other      Remitter          <NA> 
##            13            43            22            91 

table(df.longi$response.w4, useNA = "ifany")
## Early non-responder     Early responder                  HC               Other 
##                  14                  34                  91                  30 

## sanity check
all(df.longi$id %in% df.baseline$id)
## [1] TRUE

## ** rescan
NROW(df.rescan)
## [1] 80

table(df.rescan[!duplicated(df.rescan$id),"response.w8"], useNA = "ifany")
## Non-responder         Other      Remitter 
##             5            23            12

unique(df.rescan[df.rescan$response.w8=="Remitter","id"])

## sanity check
all(df.rescan$id %in% df.baseline$id)
## [1] TRUE


