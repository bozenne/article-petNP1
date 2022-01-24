## * Load packages
library(lava)
library(lavaSearch2) ## installation: devtools::install_github("bozenne/lavaSearch2", ref = "development")
library(butils) ## installation: devtools::install_github("bozenne/butils")

library(multcomp)
library(qqtest)
library(nlme)

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

## * Import data
source(file.path(path.code,"data-management-rescan.R"))

## * Main analysis: change in binding from baseline to rescan

dim(dfW.rescan)
## 40 38
table(dfW.rescan$response.w8)
## Non-responder         Other      Remitter 
##             5            23            12 

## ** fit LVM
## 0- fit simplify model to initialize the model of interest

lvmRescan0.change <- lvm(dif_neocortex.log ~ dif_sb.per.kg,
                         dif_hippocampus.log ~ dif_sb.per.kg,
                         dif_neostriatum.log ~ dif_sb.per.kg
                         )
lvmfitRescan0.change <-  estimate(lvmRescan0.change, data = dfW.rescan)
## summary(lvmfitRescan0.change)

lvmRescanF.change <- lvm(dif_neocortex.log ~ eta + dif_sb.per.kg,
                        dif_hippocampus.log ~ eta + dif_sb.per.kg,
                        dif_neostriatum.log ~ eta + dif_sb.per.kg,
                        eta[0:1] ~ 1)

latent(lvmRescanF.change) <- ~eta
## manifest(lvmRescan.change) %in% names(dfW.rescan)


lvmfitRescanF.change <- estimate(lvmRescanF.change, data = dfW.rescan, 
                                 control = list(start = coef(lvmfitRescan0.change), constrain = TRUE))
logLik(lvmfitRescanF.change)
## 'log Lik.' 104.6843 (df=12)
summary(lvmfitRescanF.change)

                           
## ** diagnostics
## score tests. We already have a saturate model so we can not do modelsearch...

#normality
##butils::qqplot2(lvmfitRescanF.change)


## ** inference
## global , including global p-value
lvmfitRescanF.changeC <- estimate2(lvmfitRescanF.change, ssc = NA, df = NA)
glhtRescan.change <- compare2(lvmfitRescanF.changeC,
                              linfct = c("neocortex" = "dif_neocortex.log = 0",
                                         "hippocampus" = "dif_hippocampus.log = 0",
                                         "neostriatum" = "dif_neostriatum.log = 0"),
                              as.lava = FALSE)
glhtRescan.change <- summary(glhtRescan.change,
                             test = adjusted("single-step"),
                             transform = function(x){100*(exp(x)-1)},
                             seed = 10)
glhtRescan.change
## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmRescanF.change, data = dfW.rescan, control = list(start = coef(lvmfitRescan0.change), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                  estimate std.error       df ci.lower ci.upper statistic p.value    
## neocortex == 0    -1.3871    2.1554      Inf  -6.1607   3.6294   -0.6391  0.7894    
## hippocampus == 0  -1.6753    2.6313      Inf  -7.4709   4.4832   -0.6313  0.7947    
## neostriatum == 0  -8.9905    1.7223      Inf -12.8176  -4.9953   -4.9780  <1e-05 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 5.5641e-05 (seed 10) 

## Global test: p.value=< 1e-05 (statistic=21.77148, df=Inf)

## * Main analysis: change in binding from baseline to rescan as a function of treatment response
## ** fit LVM
## 0- fit simplify model to initialize the model of interest
lvmRescan0.changeGroup <- lvm(dif_neocortex.log ~ dif_sb.per.kg,
                              dif_hippocampus.log ~ dif_sb.per.kg,
                              dif_neostriatum.log ~ dif_sb.per.kg)
lvmfitRescan0.changeGroup <-  estimate(lvmRescan0.changeGroup, data = dfW.rescan.binary)
## summary(lvmfitRescan0.changeGroup)

## fit model of interest
lvmRescanF.changeGroup <- lvm(dif_neocortex.log ~ eta + dif_sb.per.kg,
                              dif_hippocampus.log ~ eta + dif_sb.per.kg,
                              dif_neostriatum.log ~ eta + dif_sb.per.kg,
                              eta ~ response.w8)

latent(lvmRescanF.changeGroup) <- ~eta
## manifest(lvmRescanF.changeGroup) %in% names(dfW.rescan.binary)

lvmfitRescanF.changeGroup <- estimate(lvmRescanF.changeGroup, data = dfW.rescan.binary, 
                                      control = list(start = coef(lvmfitRescan0.changeGroup), constrain = TRUE))
logLik(lvmfitRescanF.changeGroup)
#' 'log Lik.' 58.09543 (df=13)
## summary(lvmfitRescanF.changeGroup)

## ** diagnostics
#normality
## butils::qqplot2(lvmfitRescanF.changeGroup)

## ** inference
lvmfitRescanF.changeGroupC <- estimate2(lvmfitRescanF.changeGroup, df = NA, ssc = NA)

summary2(lvmfitRescanF.changeGroupC)$table2["eta~response.w8Remitter",]
##                           estimate  std.error  df    ci.lower  ci.upper statistic   p.value
## eta~response.w8Remitter 0.02695708 0.05185537 Inf -0.07467758 0.1285917 0.5198512 0.6031673

lvm.regionEffect.changeGroup <- effects2(lvmfitRescanF.changeGroupC,
                                         linfct = c("dif_neocortex.log~response.w8Remitter",
                                                    "dif_hippocampus.log~response.w8Remitter",
                                                    "dif_neostriatum.log~response.w8Remitter")
                                         )
lvm.regionEffect.changeGroup <- summary(lvm.regionEffect.changeGroup,
                                        test = adjusted("single-step"),
                                        transform = function(x){100*(exp(x)-1)},
                                        seed = 10)
lvm.regionEffect.changeGroup

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmRescanF.changeGroup, data = dfW.rescan.binary, 
##     control = list(start = coef(lvmfitRescan0.changeGroup), constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                                              estimate std.error      df ci.lower ci.upper statistic p.value
## dif_neocortex.log~response.w8Remitter == 0     2.7324    5.3272     Inf  -7.6642  14.2995    0.5199  0.6785
## dif_hippocampus.log~response.w8Remitter == 0   2.4479    4.7961     Inf  -6.9599  12.8069    0.5166  0.6809
## dif_neostriatum.log~response.w8Remitter == 0   2.0240    3.9623     Inf  -5.8115  10.5112    0.5159  0.6815
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00039762 (seed 10) 

## tapply(dfW.rescan.binary$neocortex_baseline,dfW.rescan.binary$response.w8, mean)
## tapply(dfW.rescan.binary$neocortex_rescan,dfW.rescan.binary$response.w8, mean)

## * Main analysis: change in binding from baseline to rescan as a function of change in HAMD

## ** fit LVM
## 0- fit simplify model to initialize the model of interest
lvmRescan0.changeHAMD <- lvm(dif_neocortex.log ~ dif_sb.per.kg,
                             dif_hippocampus.log ~ dif_sb.per.kg,
                             dif_neostriatum.log ~ dif_sb.per.kg)

lvmfitRescan0.changeHAMD <-  estimate(lvmRescan0.changeHAMD, data = dfW.rescan)


## fit model of interest
lvmRescanF.changeHAMD <- lvm(dif_neocortex.log ~ eta + dif_sb.per.kg,
                             dif_hippocampus.log ~ eta + dif_sb.per.kg,
                             dif_neostriatum.log ~ eta + dif_sb.per.kg,
                             eta ~ change.hamd6.w8)

latent(lvmRescanF.changeHAMD) <- ~eta


## Check that all variables are there?
## manifest(lvmRescanF.changeHAMD) %in% names(dfW.rescan)

lvmfitRescanF.changeHAMD <- estimate(lvmRescanF.changeHAMD, data = dfW.rescan, 
                                     control = list(start = coef(lvmfitRescan0.changeHAMD), constrain = TRUE))
logLik(lvmfitRescanF.changeHAMD)
## 'log Lik.' 104.744 (df=13)

## ** diagnostics

## butils::residplot(lvmfitRescanF.changeHAMD, obs.variables = "change.hamd6.w8")
## butils::qqplot2(lvmfitRescanF.changeHAMD)

## ** inference
lvmfitRescanF.changeHAMDC <- estimate2(lvmfitRescanF.changeHAMD, df = NA, ssc = NA)
summary2(lvmfitRescanF.changeHAMDC)$table2["eta~change.hamd6.w8",]
##                         estimate    std.error  df     ci.lower    ci.upper statistic   p.value
## eta~change.hamd6.w8 0.0002253198 0.0006519909 Inf -0.001052559 0.001503199 0.3455874 0.7296528

lvm.regionEffect.changeHAMD <- effects2(lvmfitRescanF.changeHAMDC,
                                        linfct = c("dif_neocortex.log~change.hamd6.w8",
                                                   "dif_hippocampus.log~change.hamd6.w8",
                                                   "dif_neostriatum.log~change.hamd6.w8")
                                        )
lvm.regionEffect.changeHAMD <- summary(lvm.regionEffect.changeHAMD,
                                       test = adjusted("single-step"),
                                       transform = function(x){100*(exp(x)-1)},
                                       seed = 10)
lvm.regionEffect.changeHAMD

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmRescanF.changeHAMD, data = dfW.rescan, control = list(start = coef(lvmfitRescan0.changeHAMD), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                                           estimate std.error        df  ci.lower  ci.upper statistic p.value
## dif_neocortex.log~change.hamd6.w8 == 0    0.022535  0.065214       Inf -0.106791  0.152027    0.3456  0.7521
## dif_hippocampus.log~change.hamd6.w8 == 0  0.027544  0.079711       Inf -0.130509  0.185847    0.3456  0.7524
## dif_neostriatum.log~change.hamd6.w8 == 0  0.020709  0.059912       Inf -0.098109  0.139667    0.3457  0.7522
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00032591 (seed 10) 

