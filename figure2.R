## * set working directory and path to dataset
if(Sys.info()["login"] == "hpl802"){
    path <- "c:/Users/hpl802/Documents/Consult/Consult-NRU/C45-WP1-Kristin/"
    path.code <- file.path(path,"code")
    path.data <- file.path(path,"source")
}else{
    path <- "C:/Users/localadmin/Dropbox/C45 - WP1 - Kristin"
    path.code <- file.path(path,"code")
    path.data <- file.path(path,"source")
}


## * Load data
source(file.path(path.code,"0-analysis-baseline.R"))

## * figure 2
lvm.etaEffect.baseline
##                     estimate std.error        df  ci.lower  ci.upper statistic   p.value    
## eta~groupCase == 0 -0.077965  0.023486       Inf -0.123997 -0.031933   -3.3196 0.0009014 ***

## loading
summary2(lvmfitF.baseline)$coef[c("neocortex.log~eta","hippocampus.log~eta","caudate.log~eta","putamen.log~eta"),]
##                      Estimate Std. Error  t-value  df P-value
## neocortex.log~eta   1.0000000         NA       NA  NA      NA
## hippocampus.log~eta 1.0984671 0.08202974 13.39108 Inf       0
## caudate.log~eta     0.9293435 0.07845749 11.84519 Inf       0
## putamen.log~eta     0.8964760 0.06660053 13.46049 Inf       0

## region specific effect
lvm.regionEffect.baseline
## Linear Hypotheses:
##                                estimate std.error       df ci.lower ci.upper statistic   p.value    
## neocortex.log~groupCase == 0    -7.5003    2.1724      Inf -12.0198  -2.7487   -3.3196 0.0011472 ** 
## hippocampus.log~groupCase == 0  -8.2077    2.3929      Inf -13.1353  -3.0005   -3.3105 0.0009971 ***
## caudate.log~groupCase == 0      -6.9893    2.0391      Inf -11.2612  -2.5118   -3.2869 0.0012870 ** 
## putamen.log~groupCase == 0      -6.7507    1.9498      Inf -10.8502  -2.4627   -3.3158 0.0009341 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00049876 (seed 10) 
