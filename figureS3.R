## * set working directory and path to dataset
if(Sys.info()["login"] == "hpl802"){
    path <- "c:/Users/hpl802/Documents/Consult/Consult-NRU/C45-WP1-Kristin/"
}else{
    path <- "C:/Users/localadmin/Dropbox/C45 - WP1 - Kristin"
}
path.code <- file.path(path,"code")
path.data <- file.path(path,"source")
path.report <- file.path(path,"article")

## * Load data
source(file.path(path.code,"2-analysis-longitudinal.R"))

## * Figure
## ** gamma: group difference at the latent variable level
lvmW8F.etaEffect.longi
## Linear Hypotheses:
##                 estimate std.error        df  ci.lower  ci.upper statistic  p.value   
## RE vs. HC == 0 -0.099776  0.034441       Inf -0.167280 -0.032272   -2.8970 0.003768 **
## NR vs. HC == 0 -0.039645  0.039245       Inf -0.116563  0.037273   -1.0102 0.312402   
## RE vs. NR == 0 -0.060131  0.044802       Inf -0.147942  0.027680   -1.3421 0.179550   
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## (Adjusted p values reported -- none method) 

## ** beta: loadings
type <- coefType(lvmfitW8F.longi, as.lava = FALSE)
name.loadings <- c("neocortex.log~eta","hippocampus.log~eta","neostriatum.log~eta")
summary(lvmfitW8F.longi)$coef[name.loadings,]
##                      Estimate Std. Error  Z-value      P-value
## neocortex.log~eta   1.0000000         NA       NA           NA
## hippocampus.log~eta 1.0468129 0.10187361 10.27560 9.077354e-25
## neostriatum.log~eta 0.8567374 0.08394638 10.20577 1.868341e-24

## ** regional effects
lvmW8F.regionEffect.longi
## $`RE vs. HC`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmW8.longi, data = df.longiHCNRRE, control = list(start = coef(lvmfitW80.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                             estimate std.error       df ci.lower ci.upper statistic  p.value   
## neocortex: RE vs. HC == 0    -9.4960    3.1171      Inf -15.8502  -2.6619   -2.8970 0.005147 **
## hippocampus: RE vs. HC == 0  -9.9177    3.2532      Inf -16.5377  -2.7726   -2.8922 0.006506 **
## neostriatum: RE vs. HC == 0  -8.1930    2.7160      Inf -13.7577  -2.2692   -2.8895 0.005220 **
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00072739 (seed 10) 


## $`NR vs HC`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmW8.longi, data = df.longiHCNRRE, control = list(start = coef(lvmfitW80.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                             estimate std.error       df ci.lower ci.upper statistic p.value
## neocortex: NR vs. HC == 0    -3.8869    3.7719      Inf -11.1597   3.9812   -1.0102  0.3414
## hippocampus: NR vs. HC == 0  -4.0651    3.9420      Inf -11.6519   4.1732   -1.0100  0.3408
## neostriatum: NR vs. HC == 0  -3.3395    3.2510      Inf  -9.6427   3.4035   -1.0099  0.3410
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00096841 (seed 10) 


## $`RE vs NR`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmW8.longi, data = df.longiHCNRRE, control = list(start = coef(lvmfitW80.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                             estimate std.error       df ci.lower ci.upper statistic p.value
## neocortex: RE vs. NR == 0    -5.8359    4.2188      Inf -14.0443   3.1564   -1.3421  0.2056
## hippocampus: RE vs. NR == 0  -6.1006    4.4054      Inf -14.6540   3.3101   -1.3417  0.2055
## neostriatum: RE vs. NR == 0  -5.0212    3.6477      Inf -12.1642   2.7026   -1.3414  0.2060
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00052095 (seed 10) 

