## * Packages
library(lava)
library(lavaSearch2) ## installation: devtools::install_github("bozenne/lavaSearch2", ref = "9e7fa13ed7522b45871f5bfdea02ffeddaee1b90") 
library(LMMstar) ## installation: devtools::install_github("bozenne/LMMstar", ref = "19bf4587e55a4821d369d937e8480b82b26cdc60")
## LMMstar version 0.4.4
## lavaSearch2 version 2.0.0

library(multcomp)
library(qqtest)
library(nlme)

## * Set working directory and path to dataset
if(Sys.info()["login"] == "hpl802"){
    path <- "~/GitHub/article-petNP1/"    
}else if(Sys.info()["login"] == "bozenne"){
    path <- "~/Documents/GitHub/article-petNP1/"    
}else{
    path <- "C:/Users/localadmin/Dropbox/C45 - WP1 - Kristin"
}
path.code <- file.path(path,"")
path.data <- file.path(path,"source")


## * Load data
source(file.path(path.code,"data-management-longitudinal-prediction.R"))
source(file.path(path.code,"FUN_partialCorrelation.R"))

## * Check data
## keep.var <- c("group","age","sex","bmi","mr","sb.per.kg","neocortex.log","hippocampus.log","neostriatum.log")
## all(df.baseline[match(df.longi$id,df.baseline$id),keep.var]==df.longi[,keep.var])

## * Main analysis (result section): Group comparison (HC vs NR and RE) using LVM at week 8
NROW(df.longiHCNRRE)

## ** fit LVM
## 0- fit simplify model to initialize the model of interest
lvmW80.longi <- lvm(neocortex.log ~ age+sex+sert+sb.per.kg+mr,
                    hippocampus.log ~ age+sex+sert+sb.per.kg+mr,
                    neostriatum.log ~ age+sex+sert+sb.per.kg+mr
                
)
## manifest(lvmW80.longi) %in% names(df.longiHCNRRE)
## colSums(is.na(df.longi[,manifest(lvmW80.longi)]))
lvmfitW80.longi <- estimate(lvmW80.longi, data = df.longiHCNRRE)
## logLik(lvmfitW80.longi)
## 'log Lik.' 207.5397 (df=21)

## 1- model of interest
lvmW8.longi <- lvm(neocortex.log ~ eta + age+sex+sert+sb.per.kg+mr,
                   hippocampus.log ~ eta + age+sex+sert+sb.per.kg+mr,
                   neostriatum.log ~ eta + age+sex+sert+sb.per.kg+mr,
                   eta  ~ group4
                   )
latent(lvmW8.longi) <- ~eta
## plot(lvmW8.longi)

lvmfitW8.longi <- estimate(lvmW8.longi, data = df.longiHCNRRE,
                           control = list(start = coef(lvmfitW80.longi), constrain = TRUE))
## logLik(lvmfitW8.longi)
## 'log Lik.' 308.3652 (df=26)

## ** diagnostics
## score tests
searchW8.longi <- modelsearch(lvmfitW8.longi)
#searchW8.longi$res[NROW(searchW8.longi$res),]
##Score: S                   P(S>s)                    Index 
##"3.479"                "0.06214" "group4RE~neocortex.log"  
lvmW8F.longi <- lvmW8.longi
lvmfitW8F.longi <- lvmfitW8.longi

## chi2-test
summary(lvmfitW8F.longi)

## Latent variables: eta 
## Number of rows in data=126
## ______________________________________________________________________
##                               Estimate Std. Error  Z-value   P-value   std.xy
## Measurements:                                                                
##    neocortex.log~eta           1.00000                                0.79247
##    hippocampus.log~eta         1.04681    0.10187 10.27560    <1e-12  0.79879
##    neostriatum.log~eta         0.85674    0.08395 10.20577    <1e-12  0.78600
## Regressions:                                                                 
##    neocortex.log~age          -0.00257    0.00151 -1.69891   0.08934 -0.14022
##    neocortex.log~sb.per.kg    -2.63966    0.80873 -3.26395  0.001099 -0.26375
##    neocortex.log~sexMale       0.10525    0.03467  3.03583  0.002399  0.33010
##    neocortex.log~sertLALA     -0.00459    0.02732 -0.16800    0.8666 -0.01361
##    neocortex.log~mrprisma      0.06762    0.03940  1.71632    0.0861  0.20044
##     eta~group4NR              -0.03964    0.03924 -1.01019    0.3124 -0.09828
##     eta~group4RE              -0.09978    0.03444 -2.89696  0.003768 -0.30870
##    hippocampus.log~age        -0.00143    0.00161 -0.88667    0.3753 -0.07490
##    hippocampus.log~sb.per.kg  -2.79822    0.85988 -3.25421  0.001137 -0.26921
##    hippocampus.log~sexMale     0.07417    0.03682  2.01439   0.04397  0.22398
##    hippocampus.log~sertLALA   -0.00249    0.02905 -0.08574    0.9317 -0.00711
##    hippocampus.log~mrprisma    0.11769    0.04177  2.81763  0.004838  0.33592
##     neostriatum.log~age       -0.00431    0.00133 -3.23862  0.001201 -0.27166
##     neostriatum.log~sb.per.kg -1.47122    0.71022 -2.07149   0.03831 -0.17018
##     neostriatum.log~sexMale    0.03104    0.03039  1.02148     0.307  0.11272
##     neostriatum.log~sertLALA   0.00439    0.02399  0.18312    0.8547  0.01508
##     neostriatum.log~mrprisma   0.04743    0.03444  1.37700    0.1685  0.16275
## Intercepts:                                                                  
##    neocortex.log               0.00000                                0.00000
##    eta                        -0.37724    0.06102 -6.18182 6.337e-10 -3.07447
##    hippocampus.log             0.46556    0.06012  7.74425    <1e-12  2.89526
##    neostriatum.log             1.70229    0.04981 34.17250    <1e-12 12.72810
## Residual Variances:                                                          
##    neocortex.log               0.00577    0.00120  4.80291            0.24055
##    eta                         0.01362    0.00247  5.50397            0.90451
##    hippocampus.log             0.00700    0.00137  5.10614            0.27057
##    neostriatum.log             0.00496    0.00094  5.27192            0.27726
## ______________________________________________________________________
## Estimator: gaussian 
## ______________________________________________________________________

##  Number of observations = 126 
##  BIC = -462.4232 
##  AIC = -564.7304 
##  log-Likelihood of model = 308.3652 

##  log-Likelihood of saturated model = 311.0671 
##  Chi-squared statistic: q = 5.403727 , df = 4 
##   P(Q>q) = 0.2483224 

##  RMSEA (90% CI): 0.0528 (0;0.1527)
##   P(RMSEA<0.05)=0.3960849

## normality-check, limbic region is scewed and neostriatum have an extreme outlier.

## qqplot2(lvmfitW8F.longi)
## boxplot(residuals(lvmfitW8F.longi))

## ** inference
lvmfitW8F.longiC <- estimate2(lvmfitW8F.longi, df = NA, ssc = NA)

## logLik(lvmfitW8F2.longi)
## 'log Lik.' 308.3652 (df=26)

## global effect: HC vs NR or RE
## names(coef(lvmfitW8F.longi))
lvmW8F.etaEffect.longi <- glht2(lvmfitW8F.longiC, linfct = c("RE vs. HC" = "eta~group4RE=0",
                                                             "NR vs. HC" = "eta~group4NR=0",
                                                             "RE vs. NR" = "eta~group4RE-eta~group4NR=0"))
lvmW8F.etaEffect.longi <- summary(lvmW8F.etaEffect.longi, test = adjusted("none"))
lvmW8F.etaEffect.longi

## Fit: estimate.lvm(x = lvmW8.longi, data = df.longiHCNRRE, control = list(start = coef(lvmfitW80.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                 estimate std.error        df  ci.lower  ci.upper statistic  p.value   
## RE vs. HC == 0 -0.099776  0.034441       Inf -0.167280 -0.032272   -2.8970 0.003768 **
## NR vs. HC == 0 -0.039645  0.039245       Inf -0.116563  0.037273   -1.0102 0.312402   
## RE vs. NR == 0 -0.060131  0.044802       Inf -0.147942  0.027680   -1.3421 0.179550   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- none method) 

summary(lvmW8F.etaEffect.longi, test = adjusted("Westfall"))
## Fit: estimate.lvm(x = lvmW8.longi, data = df.longiHCNRRE, control = list(start = coef(lvmfitW80.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                 estimate std.error        df statistic p.value  
## RE vs. HC == 0 -0.099776  0.034441       Inf   -2.8970 0.01051 *
## NR vs. HC == 0 -0.039645  0.039245       Inf   -1.0102 0.31240  
## RE vs. NR == 0 -0.060131  0.044802       Inf   -1.3421 0.17955  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- step down max-test with logical restrictions) 
## Error when computing the p-value by numerical integration: 0.00068727 



## region specific effect: HC vs NR or RE
lvmW8F.regionEffect.longi <- list("RE vs HC" = effects2(lvmfitW8F.longiC,
                                                         linfct = c("neocortex: RE vs HC" = "neocortex.log~group4RE",
                                                                    "hippocampus: RE vs HC" = "hippocampus.log~group4RE",
                                                                    "neostriatum: RE vs HC" = "neostriatum.log~group4RE")),
                                  "NR vs HC" =  effects2(lvmfitW8F.longiC,
                                                         linfct = c("neocortex: NR vs HC" = "neocortex.log~group4NR",
                                                                    "hippocampus: NR vs HC" = "hippocampus.log~group4NR",
                                                                    "neostriatum: NR vs HC" = "neostriatum.log~group4NR")),
                                  "RE vs NR" =  effects2(lvmfitW8F.longiC,
                                                         linfct = c("neocortex: RE vs NR" = "neocortex.log~group4RE-neocortex.log~group4NR",
                                                                    "hippocampus: RE vs NR" = "hippocampus.log~group4RE-hippocampus.log~group4NR",
                                                                    "neostriatum: RE vs NR" = "neostriatum.log~group4RE-neostriatum.log~group4NR"
                                                                    )))
lvmW8F.regionEffect.longi <- lapply(lvmW8F.regionEffect.longi, summary,
                                    test = adjusted("single-step"),
                                    transform = function(x){100*(exp(x)-1)},
                                    seed = 10)

## $`RE vs. HC`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmW8.longi, data = df.longiHCNRRE, control = list(start = coef(lvmfitW80.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                             estimate std.error       df ci.lower ci.upper statistic  p.value   
## neocortex: RE vs. HC == 0    -9.4960    3.1171      Inf -15.8502  -2.6619   -2.8970 0.005147 **
## hippocampus: RE vs. HC == 0  -9.9177    3.2684      Inf -16.5377  -2.7726   -2.8922 0.006506 **
## neostriatum: RE vs. HC == 0  -8.1930    2.6774      Inf -13.7577  -2.2692   -2.8895 0.005220 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
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
## hippocampus: NR vs. HC == 0  -4.0651    3.9493      Inf -11.6519   4.1732   -1.0100  0.3408
## neostriatum: NR vs. HC == 0  -3.3395    3.2326      Inf  -9.6427   3.4035   -1.0099  0.3410
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
## hippocampus: RE vs. NR == 0  -6.1006    4.4178      Inf -14.6540   3.3101   -1.3417  0.2055
## neostriatum: RE vs. NR == 0  -5.0212    3.6164      Inf -12.1642   2.7026   -1.3414  0.2060
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00052095 (seed 10) 

## * Secondary analysis (result section): Group comparison (HC vs NR and RE) using LVM at week 4
NROW(df.longiHCENRER)
## [1] 139

## ** fit LVM
## 0- fit simplify model to initialize the model of interest
lvmW40.longi <- lvm(neocortex.log ~ age+sex+sert+sb.per.kg+mr,
                   hippocampus.log ~ age+sex+sert+sb.per.kg+mr,
                   neostriatum.log ~ age+sex+sert+sb.per.kg+mr
                   )
## manifest(lvmW40.bin4) %in% names(df.longiHCENRER)
## colSums(is.na(df.longiHCENRER[,manifest(lvmW40.bin4)]))
lvmfitW40.longi <- estimate(lvmW40.longi, data = df.longiHCENRER)
## logLik(lvmfitW40.longi)
## 'log Lik.' 210.4613 (df=21)

## 1- model of interest
lvmW4.longi <- lvm(neocortex.log ~ eta + age+sex+sert+sb.per.kg+mr,
                   hippocampus.log ~ eta + age+sex+sert+sb.per.kg+mr,
                   neostriatum.log ~ eta + age+sex+sert+sb.per.kg+mr,
                   eta  ~ response.w4
                   )
latent(lvmW4.longi) <- ~eta
## plot(lvmW4.longi)

lvmfitW4.longi <- estimate(lvmW4.longi, data = df.longiHCENRER,
                           control = list(start = coef(lvmfitW40.longi), constrain = TRUE))
## logLik(lvmfitW4.longi)
## 'log Lik.' 330.9654 (df=26)

## ** diagnostics
## score tests
searchW4.longi <- modelsearch(lvmfitW4.longi)
#searchW4.longi$res[NROW(searchW4.longi$res),]
#      Score: S                         P(S>s)                          Index 
#       "2.807"                      "0.09386" "response.w4ERE~neocortex.log" 

lvmW4F.longi <- lvmW4.longi
lvmfitW4F.longi <- lvmfitW4.longi

## chi2-test
gof(lvmfitW4F.longi)

##  Number of observations = 139 
##  BIC = -505.0705 
##  AIC = -609.9307 
##  log-Likelihood of model = 330.9654 

##  log-Likelihood of saturated model = 332.9461 
##  Chi-squared statistic: q = 3.961454 , df = 4 
##   P(Q>q) = 0.4112476 

##  RMSEA (90% CI): 0 (0;0.1275)
##   P(RMSEA<0.05)=0.5775247

## rank(Information) = 26 (p=26)

## normality, limbic region is scewed in the lower end, neostriatum has an extreme outlier.
## qqplot2(lvmfitW4F.longi)
## boxplot(residuals(lvmfitW4F.longi))

## ** inference
lvmfitW4F.longiC <- estimate2(lvmfitW4F.longi, df = NA, ssc = NA)

## global effect: HC vs NR or RE
lvmW4F.etaEffect.longi <- glht2(lvmfitW4F.longiC, linfct = c("ER vs. HC" = "eta~response.w4ER=0",
                                                            "ENR vs. HC" = "eta~response.w4ENR=0",
                                                            "ER vs. ENR" = "eta~response.w4ER-eta~response.w4ENR=0"))
lvmW4F.etaEffect.longi <- summary(lvmW4F.etaEffect.longi, test = adjusted("none"), seed = 10)
lvmW4F.etaEffect.longi
## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmW4.longi, data = df.longiHCENRER, control = list(start = coef(lvmfitW40.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                   estimate  std.error         df   ci.lower   ci.upper statistic  p.value   
## ER vs. HC == 0  -0.0938826  0.0305557        Inf -0.1537706 -0.0339946   -3.0725 0.002123 **
## ENR vs. HC == 0 -0.0103620  0.0391167        Inf -0.0870293  0.0663053   -0.2649 0.791087   
## ER vs. ENR == 0 -0.0835206  0.0418137        Inf -0.1654740 -0.0015673   -1.9974 0.045777 * 
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## (Adjusted p values reported -- none method) 

summary(lvmW4F.etaEffect.longi, test = adjusted("Westfall"))
## Fit: estimate.lvm(x = lvmW4.longi, data = df.longiHCENRER, control = list(start = coef(lvmfitW40.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                  estimate std.error        df statistic  p.value   
## ER vs. HC == 0  -0.093883  0.030551       Inf   -3.0730 0.005805 **
## ENR vs. HC == 0 -0.010362  0.039117       Inf   -0.2649 0.791087   
## ER vs. ENR == 0 -0.083521  0.041811       Inf   -1.9976 0.045763 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- step down max-test with logical restrictions) 
## Error when computing the p-value by numerical integration: 0.00064274 
lvmW4F.regionEffect.longi <- list("RE vs. HC" = effects2(lvmfitW4F.longiC,
                                                         linfct = c("neocortex: ER vs. HC" = "neocortex.log~response.w4ER",
                                                                    "hippocampus: ER vs. HC" = "hippocampus.log~response.w4ER",
                                                                    "neostriatum: ER vs. HC" = "neostriatum.log~response.w4ER")),
                                  "NR vs HC" =  effects2(lvmfitW4F.longiC,
                                                         linfct = c("neocortex: ENR vs. HC" = "neocortex.log~response.w4ENR",
                                                                    "hippocampus: ENR vs. HC" = "hippocampus.log~response.w4ENR",
                                                                    "neostriatum: ENR vs. HC" = "neostriatum.log~response.w4ENR")),
                                  "RE vs NR" =  effects2(lvmfitW4F.longiC,
                                                         linfct = c("neocortex: ER vs. ENR" = "neocortex.log~response.w4ER-neocortex.log~response.w4ENR",
                                                                    "hippocampus: ER vs. ENR" = "hippocampus.log~response.w4ER-hippocampus.log~response.w4ENR",
                                                                    "neostriatum: ER vs. ENR" = "neostriatum.log~response.w4ER-neostriatum.log~response.w4ENR"
                                                                    )))
lvmW4F.regionEffect.longi <- lapply(lvmW4F.regionEffect.longi, summary,
                                    test = adjusted("single-step"),
                                    transform = function(x){100*(exp(x)-1)},
                                    seed = 10)
lvmW4F.regionEffect.longi
## $`RE vs. HC`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmW4.longi, data = df.longiHCENRER, control = list(start = coef(lvmfitW40.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                             estimate std.error       df ci.lower ci.upper statistic  p.value   
## neocortex: ER vs. HC == 0    -8.9610    2.7818      Inf -14.6373  -2.9074   -3.0725 0.002551 **
## hippocampus: ER vs. HC == 0  -9.9955    3.0896      Inf -16.2751  -3.2449   -3.0679 0.003864 **
## neostriatum: ER vs. HC == 0  -7.6751    2.4055      Inf -12.6066  -2.4653   -3.0650 0.002605 **
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00070959 (seed 10) 


## $`NR vs HC`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmW4.longi, data = df.longiHCENRER, control = list(start = coef(lvmfitW40.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                              estimate std.error       df ci.lower ci.upper statistic p.value
## neocortex: ENR vs. HC == 0   -1.03085   3.87134      Inf -8.37116  6.89748   -0.2649  0.8019
## hippocampus: ENR vs. HC == 0 -1.15560   4.33714      Inf -9.34113  7.76900   -0.2649  0.8018
## neostriatum: ENR vs. HC == 0 -0.87752   3.29813      Inf -7.16665  5.83768   -0.2649  0.8017
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00026711 (seed 10) 


## $`RE vs NR`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmW4.longi, data = df.longiHCENRER, control = list(start = coef(lvmfitW40.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                               estimate std.error        df  ci.lower  ci.upper statistic p.value  
## neocortex: ER vs. ENR == 0    -8.01279   3.84633       Inf -15.61066   0.26915   -1.9974 0.05774 .
## hippocampus: ER vs. ENR == 0  -8.94321   4.27360       Inf -17.34133   0.30816   -1.9962 0.05744 .
## neostriatum: ER vs. ENR == 0  -6.85779   3.31621       Inf -13.45000   0.23652   -1.9954 0.05791 .
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00062522 (seed 10) 


## * Secondary analysis (Supplement): correlation (PET, change in HMD6) using LVM at week 2, 4, 8, and 12
NROW(df.longiCase)
## [1] 78

## ** fit LVM
## 0- fit simplify model to initialize the model of interest
lvmCor0.longi <- lvm(neocortex.log ~ age+sex+sert+sb.per.kg,
                     hippocampus.log ~ age+sex+sert+sb.per.kg,
                     neostriatum.log ~ age+sex+sert+sb.per.kg
                     )

lvmfitCor0.longi <- estimate(lvmCor0.longi, data = df.longiCase)
logLik(lvmfitCor0.longi)
## 'log Lik.' 99.86569 (df=18)

lvmCor.longi <- list(W2 = lvm(neocortex.log ~ eta + age+sex+sert+sb.per.kg,
                              hippocampus.log ~ eta + age+sex+sert+sb.per.kg,
                              neostriatum.log ~ eta + age+sex+sert+sb.per.kg,
                              eta ~ change.hamd6.w2
                              ),
                     W4 = lvm(neocortex.log ~ eta + age+sex+sert+sb.per.kg,
                              hippocampus.log ~ eta + age+sex+sert+sb.per.kg,
                              neostriatum.log ~ eta + age+sex+sert+sb.per.kg,
                              eta ~ change.hamd6.w4
                              ),
                     W8 = lvm(neocortex.log ~ eta + age+sex+sert+sb.per.kg,
                              hippocampus.log ~ eta + age+sex+sert+sb.per.kg,
                              neostriatum.log ~ eta + age+sex+sert+sb.per.kg,
                              eta ~ change.hamd6.w8
                              ),
                     W12 = lvm(neocortex.log ~ eta + age+sex+sert+sb.per.kg,
                               hippocampus.log ~ eta + age+sex+sert+sb.per.kg,
                               neostriatum.log ~ eta + age+sex+sert+sb.per.kg,
                               eta ~ change.hamd6.w12
                               )
                     )
latent(lvmCor.longi$W2) <- ~eta
latent(lvmCor.longi$W4) <- ~eta
latent(lvmCor.longi$W8) <- ~eta
latent(lvmCor.longi$W12) <- ~eta


lvmfitCor.longi <- lapply(lvmCor.longi, function(iM){ ## iM <- lvmCor.longi[[1]]
    iE <- estimate(iM, data = df.longiCase,
                   control = list(start = coef(lvmfitCor0.longi), constrain = TRUE))
    return(iE)
})
lapply(lvmfitCor.longi, logLik)
    
## $W2
## 'log Lik.' 168.2604 (df=22)

## $W4
## 'log Lik.' 167.6823 (df=22)

## $W8
## 'log Lik.' 165.3629 (df=22)

## $W12
## 'log Lik.' 168.7894 (df=22)

## ** diagnostics
## score tests
searchCor.longi <- lapply(lvmfitCor.longi, modelsearch)
## lapply(searchCor.longi, function(iS){iS$res[NROW(iS$res),]})

## $W2
##                        Score: S                          P(S>s)                           Index 
##                         "5.139"                       "0.02339" "change.hamd6.w2~neocortex.log" 

## $W4
##                       Score: S                         P(S>s)                          Index 
##                        "4.094"                      "0.04302" "neostriatum.log~hippocampus.log" 

## $W8
##                        Score: S                          P(S>s)                           Index 
##                         "3.877"                       "0.04896" "change.hamd6.w8~neocortex.log" 

## $W12
##                     Score: S                       P(S>s)                        Index 
##                       "3.19"                    "0.07408" "neostriatum.log~neocortex.log" 

lvmCorF.longi <- lvmCor.longi
lvmfitCorF.longi <- lvmfitCor.longi

## chi2-test
lapply(lvmfitCorF.longi, gof)

## $W2

##  Number of observations = 75 
##  BIC = -216.5037 
##  AIC = -292.5208 
##  log-Likelihood of model = 168.2604 

##  log-Likelihood of saturated model = 170.8268 
##  Chi-squared statistic: q = 5.132792 , df = 2 
##   P(Q>q) = 0.07681187 

##  RMSEA (90% CI): 0.1445 (0;0.3051)
##   P(RMSEA<0.05)=0.1146562

## rank(Information) = 22 (p=22)

## $W4

##  Number of observations = 78 
##  BIC = -215.3475 
##  AIC = -291.3646 
##  log-Likelihood of model = 167.6823 

##  log-Likelihood of saturated model = 169.8287 
##  Chi-squared statistic: q = 4.29276 , df = 2 
##   P(Q>q) = 0.1169066 

##  RMSEA (90% CI): 0.1212 (0;0.2826)
##   P(RMSEA<0.05)=0.1660738

## rank(Information) = 22 (p=22)

## $W8

##  Number of observations = 78 
##  BIC = -210.7087 
##  AIC = -286.7257 
##  log-Likelihood of model = 165.3629 

##  log-Likelihood of saturated model = 167.4974 
##  Chi-squared statistic: q = 4.269162 , df = 2 
##   P(Q>q) = 0.1182942 

##  RMSEA (90% CI): 0.1206 (0;0.2821)
##   P(RMSEA<0.05)=0.1677442

## rank(Information) = 22 (p=22)

## $W12

##  Number of observations = 75 
##  BIC = -217.5616 
##  AIC = -293.5787 
##  log-Likelihood of model = 168.7894 

##  log-Likelihood of saturated model = 170.2077 
##  Chi-squared statistic: q = 2.836641 , df = 2 
##   P(Q>q) = 0.2421203 

##  RMSEA (90% CI): 0.0747 (0;0.2536)
##   P(RMSEA<0.05)=0.3047257

## rank(Information) = 22 (p=22)

## normality-check. Boarderline scewed for hippocampus and neostriatum.
## qqplot2(lvmfitCorF.longi$W2)
## qqplot2(lvmfitCorF.longi$W4)
## qqplot2(lvmfitCorF.longi$W8)
## qqplot2(lvmfitCorF.longi$W12)

## ggResPlot(lvmfitCorF.longi$W2, x = "change.hamd6.w2")
## ggResPlot(lvmfitCorF.longi$W4, x = "change.hamd6.w4")
## ggResPlot(lvmfitCorF.longi$W8, x = "change.hamd6.w8")
## ggResPlot(lvmfitCorF.longi$W12, x = "change.hamd6.w12")

## ** inference
lvmfitCorF.longiC <- estimate2(lvmfitCorF.longi, ssc = NA, df = NA)
class(lvmfitCorF.longiC) <- "mmm"
## global effect

summary(glht2(lvmfitCorF.longiC, linfct = paste0("W",c(2,4,8,12),": eta~change.hamd6.w",c(2,4,8,12)), cluster = "id"),
        test = adjusted("single-step"), seed = 10)
## summary(effects(lvmfitCorF.longiC[[4]], "eta~change.hamd6.w12"))

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Linear Hypotheses:
##                                   estimate   std.error          df    ci.lower    ci.upper statistic p.value  
## W2: eta~change.hamd6.w2 == 0    5.0123e-04  5.9059e-04         Inf -9.3824e-04  1.9407e-03    0.8487 0.80525  
## W4: eta~change.hamd6.w4 == 0    1.3899e-03  6.3403e-04         Inf -1.5548e-04  2.9352e-03    2.1921 0.09166 .
## W8: eta~change.hamd6.w8 == 0   -1.2865e-05  5.1667e-04         Inf -1.2722e-03  1.2464e-03   -0.0249 1.00000  
## W12: eta~change.hamd6.w12 == 0 -4.3505e-05  5.4870e-04         Inf -1.3809e-03  1.2939e-03   -0.0793 0.99997  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00035101 (seed 10) 

summary(glht2(lvmfitCorF.longiC, linfct = paste0("W",c(2,4,8,12),": eta~change.hamd6.w",c(2,4,8,12)), cluster = "id"),
        test = adjusted("none"), seed = 10)
## Linear Hypotheses:
##                                   estimate   std.error          df    ci.lower    ci.upper statistic p.value  
## W2: eta~change.hamd6.w2 == 0    5.0123e-04  5.9059e-04         Inf -6.5630e-04  1.6588e-03    0.8487 0.39605  
## W4: eta~change.hamd6.w4 == 0    1.3899e-03  6.3403e-04         Inf  1.4719e-04  2.6325e-03    2.1921 0.02837 *
## W8: eta~change.hamd6.w8 == 0   -1.2865e-05  5.1667e-04         Inf -1.0255e-03  9.9979e-04   -0.0249 0.98014  
## W12: eta~change.hamd6.w12 == 0 -4.3505e-05  5.4870e-04         Inf -1.1189e-03  1.0319e-03   -0.0793 0.93680  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- none method) 

## region specific effect
lvmCor.regionEffect.longi <- list(W2 = effects2(lvmfitCorF.longiC$W2, linfct = c(neocortex.w2 = "neocortex.log~change.hamd6.w2",
                                                                                 hippocampus.w2 = "hippocampus.log~change.hamd6.w2",
                                                                                 neostriatum.w2 = "neostriatum.log~change.hamd6.w2")),
                                  W4 = effects2(lvmfitCorF.longiC$W4, linfct = c(neocortex.w4 = "neocortex.log~change.hamd6.w4",
                                                                                 hippocampus.w4 = "hippocampus.log~change.hamd6.w4",
                                                                                 neostriatum.w4 = "neostriatum.log~change.hamd6.w4")),
                                  W8 = effects2(lvmfitCorF.longiC$W8, linfct = c(neocortex.w8 = "neocortex.log~change.hamd6.w8",
                                                                                 hippocampus.w8 = "hippocampus.log~change.hamd6.w8",
                                                                                 neostriatum.w8 = "neostriatum.log~change.hamd6.w8")),
                                  W12 = effects2(lvmfitCorF.longiC$W12, linfct = c(neocortex.w12 = "neocortex.log~change.hamd6.w12",
                                                                                   hippocampus.w12 = "hippocampus.log~change.hamd6.w12",
                                                                                   neostriatum.w12 = "neostriatum.log~change.hamd6.w12"))
                                  )


lvmCor.regionEffect.longi <- lapply(lvmCor.regionEffect.longi, summary,
                                    test = adjusted("single-step"),
                                    transform = function(x){100*(exp(x)-1)},
                                    seed = 10)
lvmCor.regionEffect.longi

## lava:::effects.lvmfit(lvmfitCorF.longi$W2, neocortex.log~change.hamd6.w2)
## lava:::effects.lvmfit(lvmfitCorF.longi$W12, hippocampus.log~change.hamd6.w12)

## $W2

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = iM, data = df.longiCase, control = list(start = coef(lvmfitCor0.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                        estimate   std.error          df    ci.lower    ci.upper statistic p.value
## neocortex.w2 == 0    0.00050123  0.00059059         Inf -0.00068918  0.00169164    0.8487  0.4304
## hippocampus.w2 == 0  0.00056153  0.00066161         Inf -0.00077203  0.00189510    0.8487  0.4305
## neostriatum.w2 == 0  0.00047515  0.00056022         Inf -0.00065406  0.00160436    0.8481  0.4307
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00076117 (seed 10) 


## $W4

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = iM, data = df.longiCase, control = list(start = coef(lvmfitCor0.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                       estimate  std.error         df   ci.lower   ci.upper statistic p.value  
## neocortex.w4 == 0   1.3899e-03 6.3403e-04        Inf 5.7782e-05 2.7219e-03    2.1921 0.04007 *
## hippocampus.w4 == 0 1.6072e-03 7.3558e-04        Inf 6.1776e-05 3.1526e-03    2.1850 0.04032 *
## neostriatum.w4 == 0 1.3001e-03 5.9580e-04        Inf 4.8361e-05 2.5519e-03    2.1821 0.04094 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00057475 (seed 10) 


## $W8

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = iM, data = df.longiCase, control = list(start = coef(lvmfitCor0.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                        estimate   std.error          df    ci.lower    ci.upper statistic p.value
## neocortex.w8 == 0   -1.2865e-05  5.1667e-04         Inf -1.0258e-03  1.0000e-03   -0.0249  0.9815
## hippocampus.w8 == 0 -1.5159e-05  6.0880e-04         Inf -1.2087e-03  1.1784e-03   -0.0249  0.9815
## neostriatum.w8 == 0 -1.2297e-05  4.9388e-04         Inf -9.8054e-04  9.5594e-04   -0.0249  0.9815
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 2.7116e-05 (seed 10) 


## $W12

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = iM, data = df.longiCase, control = list(start = coef(lvmfitCor0.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                         estimate   std.error          df    ci.lower    ci.upper statistic p.value
## neocortex.w12 == 0   -4.3505e-05  5.4870e-04         Inf -1.1211e-03  1.0341e-03   -0.0793  0.9412
## hippocampus.w12 == 0 -4.8664e-05  6.1376e-04         Inf -1.2540e-03  1.1567e-03   -0.0793  0.9412
## neostriatum.w12 == 0 -3.9569e-05  4.9906e-04         Inf -1.0197e-03  9.4053e-04   -0.0793  0.9412
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 8.4084e-05 (seed 10) 

## * Secondary analysis (Supplement): Group comparison (HC vs NR and RE) using linear regression at week 4 and 8
NROW(df.longiHCENRER)

## ** fit MLM
df.longiHCENRER$response.w4 <- relevel(df.longiHCENRER$response.w4,"HC")
gls.W4.longi <- list(neocortex_w4 = lmm(neocortex.log ~ response.w4 + age + sex + sert + sb.per.kg,
                                        repetition = ~group|id, data = df.longiHCENRER),
                     limbic_w4 = lmm(limbic.log ~ response.w4 + age + sex + sert + sb.per.kg,
                                     repetition = ~group|id, data = df.longiHCENRER),
                     neostriatum_w4 = lmm(neostriatum.log ~ response.w4 + age + sex + sert + sb.per.kg,
                                          repetition = ~group|id, data = df.longiHCENRER))
class(gls.W4.longi) <- "mmm"

df.longiHCNRRE$group4 <- relevel(df.longiHCNRRE$group4,"HC")
gls.W8.longi <- list(neocortex_w8 = lmm(neocortex.log ~ group4 + age + sex + sert + sb.per.kg,
                                        repetition = ~group|id, data = df.longiHCNRRE),
                     limbic_w8 = lmm(limbic.log ~ group4 + age + sex + sert + sb.per.kg,
                                     repetition = ~group|id, data = df.longiHCNRRE),
                     neostriatum_w8 = lmm(neostriatum.log ~ group4 + age + sex + sert + sb.per.kg,
                                          repetition = ~group|id, data = df.longiHCNRRE))
class(gls.W8.longi) <- "mmm"

lapply(gls.W4.longi, logLik)
## $neocortex_w4
## 'log Lik.' 75.56494 (df=9)

## $neostriatum_w4
## 'log Lik.' 94.07269 (df=9)

## $limbic_w4
## 'log Lik.' 82.80038 (df=9)

lapply(gls.W8.longi, logLik)
## $neocortex_w8
## 'log Lik.' 70.55141 (df=9)

## $neostriatum_w8
## 'log Lik.' 87.12986 (df=9)

## $limbic_w8
## 'log Lik.' 80.91462 (df=9)

## ** diagnostics
## general diagnostic

## influencial observations
## hist(iid(gls.W4.longi$neocortex_w4)[,"response.w4ER"], main = paste0("coef=",coef(gls.W4.longi$neocortex_w4)["response.w4ER"]))
## hist(iid(gls.W4.longi$neostriatum_w4)[,"response.w4ER"], main = paste0("coef=",coef(gls.W4.longi$neostriatum_w4)["response.w4ER"]))
## hist(iid(gls.W4.longi$limbic_w4)[,"response.w4ER"], main = paste0("coef=",coef(gls.W4.longi$limbic_w4)["response.w4ER"]))

## hist(iid(gls.W8.longi$neocortex_w4)[,"response.w4ENR"], main = paste0("coef=",coef(gls.W8.longi$neocortex_w4)["response.w4ENR"]))
## hist(iid(gls.W8.longi$neostriatum_w4)[,"response.w4ENR"], main = paste0("coef=",coef(gls.W8.longi$neostriatum_w4)["response.w4ENR"]))
## hist(iid(gls.W8.longi$limbic_w4)[,"response.w4ENR"], main = paste0("coef=",coef(gls.W8.longi$limbic_w4)["response.w4ENR"]))

## hist(iid(gls.W4.longi$neocortex_w8)[,"group4RE"], main = paste0("coef=",coef(gls.W4.longi$neocortex_w8)["group4RE"]))
## hist(iid(gls.W4.longi$neostriatum_w8)[,"group4RE"], main = paste0("coef=",coef(gls.W4.longi$neostriatum_w8)["group4RE"]))
## hist(iid(gls.W4.longi$limbic_w8)[,"group4RE"], main = paste0("coef=",coef(gls.W4.longi$limbic_w8)["group4RE"]))

## hist(iid2(gls.W8.longi$neocortex_w8)[,"group4NR"], main = paste0("coef=",coef(gls.W8.longi$neocortex_w8)["group4NR"]))
## hist(iid2(gls.W8.longi$neostriatum_w8)[,"group4NR"], main = paste0("coef=",coef(gls.W8.longi$neostriatum_w8)["group4NR"]))
## hist(iid2(gls.W8.longi$limbic_w8)[,"group4NR"], main = paste0("coef=",coef(gls.W8.longi$limbic_w8)["group4NR"]))
## not so nice ...

## normality
## qqtest(residuals(gls.W4.longi$neocortex_w4))
## qqtest(residuals(gls.W4.longi$limbic_w4))
## qqtest(residuals(gls.W4.longi$neostriatum_w4))

## qqtest(residuals(gls.W8.longi$neocortex_w8))
## qqtest(residuals(gls.W8.longi$limbic_w8))
## qqtest(residuals(gls.W8.longi$neostriatum_w8))

## ** inference
## *** correction per group and time (i.e. over regions)
gls.regionEffect.longi <- list("W4: ER vs. HC" = rbind(anova(gls.W4.longi$neocortex, "response.w4ER = 0", ci = TRUE),
                                                       anova(gls.W4.longi$limbic, "response.w4ER = 0", ci = TRUE),
                                                       anova(gls.W4.longi$neostriatum, "response.w4ER = 0", ci = TRUE)
                                                       ),
                               "W4: ENR vs. HC" = rbind(anova(gls.W4.longi$neocortex, "response.w4ENR = 0", ci = TRUE),
                                                        anova(gls.W4.longi$limbic, "response.w4ENR = 0", ci = TRUE),
                                                        anova(gls.W4.longi$neostriatum, "response.w4ENR = 0", ci = TRUE)
                                                        ),
                               "W4: ER vs. ENR" = rbind(anova(gls.W4.longi$neocortex, "response.w4ER - response.w4ENR = 0", ci = TRUE),
                                                        anova(gls.W4.longi$limbic, "response.w4ER - response.w4ENR = 0", ci = TRUE),
                                                        anova(gls.W4.longi$neostriatum, "response.w4ER - response.w4ENR = 0", ci = TRUE)
                                                        ),
                               "W8: RE vs. HC" = rbind(anova(gls.W8.longi$neocortex, "group4RE = 0", ci = TRUE),
                                                       anova(gls.W8.longi$limbic, "group4RE = 0", ci = TRUE),
                                                       anova(gls.W8.longi$neostriatum, "group4RE = 0", ci = TRUE)
                                                       ),
                               "W8: NR vs. HC" = rbind(anova(gls.W8.longi$neocortex, "group4NR = 0", ci = TRUE),
                                                       anova(gls.W8.longi$limbic, "group4NR = 0", ci = TRUE),
                                                       anova(gls.W8.longi$neostriatum, "group4NR = 0", ci = TRUE)
                                                       ),
                               "W8: RE vs. NR" = rbind(anova(gls.W8.longi$neocortex, "group4RE - group4NR = 0", ci = TRUE),
                                                       anova(gls.W8.longi$limbic, "group4RE - group4NR = 0", ci = TRUE),
                                                       anova(gls.W8.longi$neostriatum, "group4RE - group4NR = 0", ci = TRUE)
                                                       )
                               )


gls.regionEffect.longi <- lapply(gls.regionEffect.longi, function(iGLS){
    summary(iGLS,test = adjusted("single-step"),
            transform = function(x){100*(exp(x)-1)},
            seed = 10)
})
gls.regionEffect.longi
## > $`W4: ER vs. HC`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Linear Hypotheses:
##                              estimate std.error        df  ci.lower  ci.upper statistic p.value  
## neocortex: ER vs. HC == 0    -7.95365   2.94516  65.00000 -14.51016  -0.89431   -2.5902  0.0249 *
## limbic: ER vs. HC == 0       -4.47731   3.05953  65.00000 -11.28816   2.85645   -1.4301  0.2791  
## neostriatum: ER vs. HC == 0  -4.92359   2.78999  65.00000 -11.15347   1.74313   -1.7206  0.1684  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00085981 (seed 10) 


## $`W4: ENR vs. HC`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Linear Hypotheses:
##                              estimate std.error       df ci.lower ci.upper statistic p.value
## neocortex: ENR vs. HC == 0    2.16121   4.76882 54.00000 -8.06522 13.52518    0.4581  0.8517
## limbic: ENR vs. HC == 0       2.78822   4.85870 54.00000 -7.62414 14.37423    0.5818  0.7683
## neostriatum: ENR vs. HC == 0  0.93483   4.36567 54.00000 -8.46277 11.29723    0.2151  0.9734
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 8.1368e-05 (seed 10) 


## $`W4: ER vs. ENR`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Linear Hypotheses:
##                              estimate std.error       df ci.lower ci.upper statistic p.value  
## neocortex: ER vs. ENR == 0    -9.9009    4.7645  46.0000 -20.1797   1.7015   -1.9716 0.09885 .
## limbic: ER vs. ENR == 0       -7.0684    5.0228  46.0000 -17.8903   5.1798   -1.3563 0.29905  
## neostriatum: ER vs. ENR == 0  -5.8042    4.6539  46.0000 -15.8836   5.4830   -1.2102 0.37311  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00041246 (seed 10) 


## $`W8: RE vs. HC`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Linear Hypotheses:
##                               estimate  std.error         df   ci.lower   ci.upper statistic p.value  
## neocortex: RE vs. HC == 0    -8.516309   3.488701  43.000000 -16.363635   0.067305   -2.3341 0.05173 .
## limbic: RE vs. HC == 0       -4.083402   3.421891  43.000000 -11.802413   4.311174   -1.1686 0.43621  
## neostriatum: RE vs. HC == 0  -5.253899   3.437341  43.000000 -13.002337   3.184653   -1.4876 0.26965  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00077362 (seed 10) 


## $`W8: NR vs. HC`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Linear Hypotheses:
##                               estimate  std.error         df   ci.lower   ci.upper statistic p.value
## neocortex: NR vs. HC == 0    -0.062762   4.804113  39.000000 -10.540256  11.641853   -0.0131  1.0000
## limbic: NR vs. HC == 0        0.132712   4.509427  39.000000  -9.735937  11.080310    0.0294  0.9999
## neostriatum: NR vs. HC == 0  -1.423866   4.538116  39.000000 -11.344041   9.606329   -0.3115  0.9421
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 4.5843e-05 (seed 10) 


## $`W8: RE vs. NR`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Linear Hypotheses:
##                             estimate std.error       df ci.lower ci.upper statistic p.value
## neocortex: RE vs. NR == 0    -8.4589    5.3177  33.0000 -20.1617   4.9594   -1.5214  0.2449
## limbic: RE vs. NR == 0       -4.2105    5.2243  33.0000 -15.7548   8.9157   -0.7887  0.6667
## neostriatum: RE vs. NR == 0  -3.8854    5.4010  33.0000 -15.7974   9.7119   -0.7052  0.7250
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00045578 (seed 10) 

## * Secondary analysis (Supplement): association PET, change in HMD6 using linear regression at week 2, 4, 8, and 12
NROW(df.longiCase)

## ** fit MLM
lmCor.longi <- list(W2.neocortex = lm(neocortex.log ~ change.hamd6.w2 + age + sex+ sert + sb.per.kg,
                                      data = df.longiCase),
                    W2.limbic = lm(limbic.log ~ change.hamd6.w2 + age + sex+ sert + sb.per.kg,
                                   data = df.longiCase),
                    W2.neostriatum = lm(neostriatum.log ~ change.hamd6.w2 + age + sex+ sert + sb.per.kg,
                                     data = df.longiCase),
                    W4.neocortex = lm(neocortex.log ~ change.hamd6.w4 + age + sex+ sert + sb.per.kg,
                                      data = df.longiCase),
                    W4.limbic = lm(limbic.log ~ change.hamd6.w4 + age + sex+ sert + sb.per.kg,
                                   data = df.longiCase),
                    W4.neostriatum = lm(neostriatum.log ~ change.hamd6.w4 + age + sex+ sert + sb.per.kg,
                                     data = df.longiCase),
                    W8.neocortex = lm(neocortex.log ~ change.hamd6.w8 + age + sex+ sert + sb.per.kg,
                                      data = df.longiCase),
                    W8.limbic = lm(limbic.log ~ change.hamd6.w8 + age + sex+ sert + sb.per.kg,
                                   data = df.longiCase),
                    W8.neostriatum = lm(neostriatum.log ~ change.hamd6.w8 + age + sex+ sert + sb.per.kg,
                                     data = df.longiCase),
                    W12.neocortex = lm(neocortex.log ~ change.hamd6.w12 + age + sex+ sert + sb.per.kg,
                                      data = df.longiCase),
                    W12.limbic = lm(limbic.log ~ change.hamd6.w12 + age + sex+ sert + sb.per.kg,
                                      data = df.longiCase),
                    W12.neostriatum = lm(neostriatum.log ~ change.hamd6.w12 + age + sex+ sert + sb.per.kg,
                                      data = df.longiCase))
class(lmCor.longi) <- "mmm"

## ** diagnostics                    
dt.residuals <- do.call(rbind,lapply(lmCor.longi, function(iX){ ## iX <- lmCor.longi[[1]]
    iOut <- data.frame(change.hamd6 = df.longiCase[[names(coef(iX)[2])]],
                       region = all.vars(formula(iX))[1],
                       time = gsub("change.hamd6.","",names(coef(iX)[2]), fixed = TRUE),
                       residuals = NA)
    
    iOut$residuals[setdiff(1:NROW(iOut),iX$na.action)] <- residuals(iX)
    return(iOut)
}))
rownames(dt.residuals)  <- NULL

gg.resCor <- ggplot(dt.residuals, aes(x = change.hamd6, y = residuals))
gg.resCor <- gg.resCor + geom_point() + stat_smooth()
gg.resCor <- gg.resCor + facet_grid(time~region, scale = "free")
## gg.resCor

gg.resQQ <- ggplot(dt.residuals, aes(sample = residuals))
gg.resQQ <- gg.resQQ + stat_qq() + stat_qq_line()
gg.resQQ <- gg.resQQ + facet_grid(time~region, scale = "free")

## ** inference

## partialCorrelation(lmCor.longi[["W2.neocortex"]], var = c("change.hamd6.w2"), fisher.transform = TRUE)
## lava::partialcor(~ age + sex + sert + sb.per.kg,
##                  data = df.longiCase[, c("neocortex.log", "change.hamd6.w2", "age", "sex", "sert", "sb.per.kg")])
## difference due to how lava handle missing values

## partialCorrelation(update(lmCor.longi[["W2.neocortex"]], data = df.longiCase[1:30,]), var = c("change.hamd6.w2"), fisher.transform = TRUE)
## lava::partialcor(~ age + sex + sert + sb.per.kg,
##                  data = df.longiCase[1:30, c("neocortex.log", "change.hamd6.w2", "age", "sex", "sert", "sb.per.kg")])

lmCor.partial.longi <- list("W2" = partialCorrelation(lmCor.longi, var = c("change.hamd6.w2"), cluster = "id"),
                            "W4" = partialCorrelation(lmCor.longi, var = c("change.hamd6.w4"), cluster = "id"),
                            "W8" = partialCorrelation(lmCor.longi, var = c("change.hamd6.w8"), cluster = "id"),
                            "W12" = partialCorrelation(lmCor.longi, var = c("change.hamd6.w12"), cluster = "id"))
lapply(lmCor.partial.longi, summary)
## $W2

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Linear Hypotheses:
##                      estimate std.error        df  ci.lower  ci.upper statistic p.value
## W2.neocortex == 0    0.185995  0.112530       Inf -0.066840  0.438831    1.6529  0.1813
## W2.limbic == 0       0.080820  0.112708       Inf -0.172417  0.334056    0.7171  0.7129
## W2.neostriatum == 0  0.057082  0.115453       Inf -0.202320  0.316484    0.4944  0.8630
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00099822 


## $W4

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Linear Hypotheses:
##                      estimate std.error        df  ci.lower  ci.upper statistic  p.value   
## W4.neocortex == 0    0.306986  0.109169       Inf  0.063446  0.550526    2.8120 0.009762 **
## W4.limbic == 0       0.222506  0.111832       Inf -0.026975  0.471987    1.9896 0.087000 . 
## W4.neostriatum == 0  0.141948  0.113546       Inf -0.111357  0.395253    1.2501 0.345278   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00091935 


## $W8

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Linear Hypotheses:
##                      estimate std.error        df  ci.lower  ci.upper statistic p.value
## W8.neocortex == 0    0.087399  0.114269       Inf -0.166203  0.341000    0.7649  0.6435
## W8.limbic == 0       0.014382  0.114696       Inf -0.240168  0.268931    0.1254  0.9952
## W8.neostriatum == 0 -0.029940  0.114656       Inf -0.284401  0.224521   -0.2611  0.9634
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00037648 


## $W12

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Linear Hypotheses:
##                       estimate std.error        df  ci.lower  ci.upper statistic p.value
## W12.neocortex == 0    0.045079  0.117312       Inf -0.215687  0.305846    0.3843  0.9078
## W12.limbic == 0      -0.017905  0.115518       Inf -0.274683  0.238873   -0.1550  0.9913
## W12.neostriatum == 0  0.024525  0.113462       Inf -0.227684  0.276734    0.2162  0.9782
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 2.628e-05 


## * Supplementary analysis (50%+/-): Group comparison (HC vs 50+or= vs 50-) using LVM at week 8

## table(df.longi$response50.w8)
  ## HC  50m 50pe 
  ## 91   44   34 

## ** visualisation
df.tempo <- reshape2::melt(df.longi, id.vars = c("id","response50.w8"),
                           measure.vars = c("neocortex.log","hippocampus.log","neostriatum.log"))
gg50 <- ggplot(df.tempo, aes(x = response50.w8, y = value))
gg50 <- gg50 + geom_boxplot() + geom_jitter(width = 0.1, height = 0)
gg50 <- gg50 + facet_wrap(~variable)
## gg50

## ** estimation
lvmW8.longi50 <- lvm(neocortex.log ~ eta + age+sex+sert+sb.per.kg+mr,
                   hippocampus.log ~ eta + age+sex+sert+sb.per.kg+mr,
                   neostriatum.log ~ eta + age+sex+sert+sb.per.kg+mr,
                   eta  ~ response50.w8
                   )
latent(lvmW8.longi50) <- ~eta

lvmfitW8.longi50 <- estimate(lvmW8.longi50, data = df.longi,
                             control = list(start = coef(lvmfitW80.longi), constrain = TRUE))

## ** diagnostic
## modelsearch(lvmfitW8.longi50)
## gof(lvmfitW8.longi50)

## ** inference
lvmfitW8.longi50C <- estimate2(lvmfitW8.longi50, df = NA, ssc = NA)
## coef(lvmfitW8.longi50)

## global effect
lvmW8.etaEffect.longi50 <- glht2(lvmfitW8.longi50C, linfct = c("50- vs. HC" = "eta~response50.w850m=0",
                                                               "50=+ vs. HC" = "eta~response50.w850pe=0",
                                                               "50=+ vs. 50-" = "eta~response50.w850pe-eta~response50.w850m=0"))
lvmW8.etaEffect.longi50 <- summary(lvmW8.etaEffect.longi50, test = adjusted("none"))
lvmW8.etaEffect.longi50
## Linear Hypotheses:
##                    estimate std.error        df  ci.lower  ci.upper statistic  p.value   
## 50- vs. HC == 0   -0.084915  0.028042       Inf -0.139876 -0.029955   -3.0282 0.002460 **
## 50=+ vs. HC == 0  -0.082768  0.029199       Inf -0.139997 -0.025539   -2.8346 0.004588 **
## 50=+ vs. 50- == 0  0.002147  0.030286       Inf -0.057213  0.061507    0.0709 0.943484   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- none method) 

## region specific effect
lvmW8.regionEffect.longi50 <- list("50- vs. HC" = effects2(lvmfitW8.longi50C,
                                                           linfct = c("neocortex: 50- vs. HC" = "neocortex.log~response50.w850m",
                                                                      "hippocampus: 50- vs. HC" = "hippocampus.log~response50.w850m",
                                                                      "neostriatum: 50- vs. HC" = "neostriatum.log~response50.w850m")),
                                   "50=+ vs HC" =  effects2(lvmfitW8.longi50C,
                                                            linfct = c("neocortex: 50+ vs. HC" = "neocortex.log~response50.w850pe",
                                                                       "hippocampus: 50+ vs. HC" = "hippocampus.log~response50.w850pe",
                                                                       "neostriatum: 50+ vs. HC" = "neostriatum.log~response50.w850pe")),
                                   "50=+ vs 50-" =  effects2(lvmfitW8.longi50C,
                                                             linfct = c("neocortex: 50+ vs. 50-" = "neocortex.log~response50.w850pe-neocortex.log~response50.w850m",
                                                                        "hippocampus: 50+ vs. 50-" = "hippocampus.log~response50.w850pe-hippocampus.log~response50.w850m",
                                                                        "neostriatum: 50+ vs. 50-" = "neostriatum.log~response50.w850pe-neostriatum.log~response50.w850m"
                                                                        )))
lvmW8.regionEffect.longi50 <- lapply(lvmW8.regionEffect.longi50, summary,
                                     test = adjusted("single-step"),
                                     transform = function(x){100*(exp(x)-1)},
                                     seed = 10)
lvmW8.regionEffect.longi50

## $`50- vs. HC`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmW8.longi50, data = df.longi, control = list(start = coef(lvmfitW80.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                              estimate std.error       df ci.lower ci.upper statistic  p.value   
## neocortex: 50- vs. HC == 0    -8.1410    2.5759      Inf -13.3818  -2.5830   -3.0282 0.002876 **
## hippocampus: 50- vs. HC == 0  -8.7564    2.7900      Inf -14.3813  -2.7620   -3.0171 0.004370 **
## neostriatum: 50- vs. HC == 0  -7.1679    2.2621      Inf -11.8356  -2.2532   -3.0204 0.002942 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00074479 (seed 10) 


## $`50=+ vs HC`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmW8.longi50, data = df.longi, control = list(start = coef(lvmfitW80.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                              estimate std.error       df ci.lower ci.upper statistic  p.value   
## neocortex: 50+ vs. HC == 0    -7.9436    2.6880      Inf -13.3867  -2.1584   -2.8346 0.005754 **
## hippocampus: 50+ vs. HC == 0  -8.5447    2.9101      Inf -14.3846  -2.3065   -2.8255 0.007390 **
## neostriatum: 50+ vs. HC == 0  -6.9932    2.3597      Inf -11.8388  -1.8812   -2.8282 0.005825 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00075577 (seed 10) 


## $`50=+ vs 50-`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmW8.longi50, data = df.longi, control = list(start = coef(lvmfitW80.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                               estimate std.error       df ci.lower ci.upper statistic p.value
## neocortex: 50+ vs. 50- == 0    0.21493   3.03512      Inf -5.56605  6.34981    0.0709  0.9462
## hippocampus: 50+ vs. 50- == 0  0.23197   3.27539      Inf -5.99321  6.86937    0.0709  0.9462
## neostriatum: 50+ vs. 50- == 0  0.18824   2.65849      Inf -4.89255  5.54045    0.0709  0.9462
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 7.6504e-05 (seed 10) 


## * Supplementary analysis (Missing values): Group comparison (HC vs NR and RE) using LVM at week 8

## ** load data baseline and merge
source(file.path(path.code,"data-management-baseline.R"))
id.missing <- setdiff(df.baseline$id,as.character(df.longi$id))

id.missing.xlsx <- c("55838" = "baseline",
                     "56179" = "baseline",
                     "55735" = "baseline",
                     "55254" = "baseline",
                     "56217" = "baseline",
                     "56000" = "baseline",
                     "55870" = "baseline",
                     "56173" = "baseline",
                     "55830" = "baseline",
                     "56109" = "non-compliance",
                     "56085" = "non-compliance",
                     "56131" = "non-compliance",
                     "55969" = "non-compliance",
                     "55151" = "drop-out",
                     "55981" = "suicidal",
                     "56123" = "remission",
                     "56188" = "drop-out",
                     "56177" = "drop-out",
                     "55851" = "suicidal", ## not exactly but to avoid an additional category
                     "56105" = "drop-out",
                     "55815" = "side-effect",
                     "55276" = "drop-out")
## identical(sort(names(id.missing.xlsx[id.missing.xlsx!="baseline"])), sort(id.missing))
## table(id.missing.xlsx[id.missing.xlsx!="baseline"])
## drop-out non-compliance      remission    side-effect       suicidal 
##        5              4              1              1              2 

keep.col.IPW <- c("id","age","sex","sert","sb.per.kg","mr","neocortex.log","hippocampus.log","neostriatum.log","group4","hamd6.baseline","hamd17.baseline")
df.baseline2 <- cbind(df.baseline, group4 = as.character(NA))
df.longiHCNRRE.IPW <- rbind(df.longiHCNRRE[,keep.col.IPW],
                            df.baseline2[df.baseline$id %in% names(id.missing.xlsx),keep.col.IPW]
                            )
id.suicidal <- names(id.missing.xlsx)[id.missing.xlsx=="suicidal"]
id.side_effect <- names(id.missing.xlsx)[id.missing.xlsx=="side-effect"]
id.remission <- names(id.missing.xlsx)[id.missing.xlsx=="remission"]
df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$id %in% id.suicidal,"group4"] <- "NR"
df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$id %in% id.side_effect,"group4"] <- "NR"
df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$id %in% id.remission,"group4"] <- "RE"
df.longiHCNRRE.IPW$missing <- is.na(df.longiHCNRRE.IPW$group4) - sapply(as.character(df.longiHCNRRE.IPW$group4),identical,"HC")
## table(df.longiHCNRRE.IPW$missing, useNA = "always")
  ## -1    0    1 <NA> 
  ## 91   39    9    0 

## ** IPCW
df.longiHCNRRE.IPW$P.IPCW <- !is.na(df.longiHCNRRE.IPW$group4)
df.longiHCNRRE.IPW$S.IPCW <- !is.na(df.longiHCNRRE.IPW$group4)
df.longiHCNRRE.IPW$SW.IPCW <- !is.na(df.longiHCNRRE.IPW$group4)
## *** step 1
## Calculate probability weights:
## Among subjects who participated at baseline, fit a logistic regression model to predict participation using covariates from baseline:
## - that are key predictors of participation (Z – define these as the set we’ve currently identified in our participation models)
## - that are any baseline versions of the current outcome (D)
## - risk factor of interest (E)
##  Results of this modeling process will be summarized to describe factors associated with participation
## Calculate predicted probabilities from this model – call these P.
## table(df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing>=0,"missing"])
e.IPCW_step1 <- glm(missing ~ age + sex + neocortex.log + hamd6.baseline,
                   data = df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing>=0,],
                   family = binomial(link = "logit"))
logLik(e.IPCW_step1)
## 'log Lik.' -21.66558 (df=5)
summary(e.IPCW_step1)$coef
##                   Estimate Std. Error    z value  Pr(>|z|)
## (Intercept)    -5.62019590 3.77589550 -1.4884405 0.1366348
## age             0.01319784 0.04157114  0.3174760 0.7508825
## sexMale         1.12670935 0.78863865  1.4286763 0.1530973
## neocortex.log  -1.30132579 2.45393215 -0.5303023 0.5959024
## hamd6.baseline  0.22135919 0.23970034  0.9234830 0.3557556
df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing==0,"P.IPCW"] <- predict(e.IPCW_step1, newdata = df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing==0,], type = "response")
## range(df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing==0,"P.IPCW"])

## *** step 2
## Calculate stabilized versions of probability weights:
## A stabilizing calculation involves fitting the same prediction model as above, but only with E as the covariate.
## Calculate predicted probabilities from this model – call these S.
## Then calculate stabilized weights = SW = S/P. 
e.IPCW_step2 <- glm(missing ~ age + sex,
                   data = df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing>=0,],
                   family = binomial(link = "logit"))
logLik(e.IPCW_step2)
## 'log Lik.' -22.18149 (df=3)
summary(e.IPCW_step2)$coef
##                Estimate Std. Error    z value   Pr(>|z|)
## (Intercept) -2.22631143  1.2679918 -1.7557775 0.07912641
## age          0.01127897  0.0403387  0.2796068 0.77977918
## sexMale      1.04173300  0.7568015  1.3764943 0.16866865
df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing==0,"S.IPCW"] <- predict(e.IPCW_step2, newdata = df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing==0,], type = "response")
df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing==0,"SW.IPCW"] <- df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing==0,"S.IPCW"]/df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing==0,"P.IPCW"]

## range(1/df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing==0,"P.IPCW"], na.rm = TRUE)
## [1]  2.500294 25.125787
## quantile(df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing==0,"SW.IPCW"], na.rm = TRUE)
##        0%       25%       50%       75%      100% 
## 0.4865550 0.8279943 1.0520485 1.3830973 3.3415816 
## median(df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing==0,"SW.IPCW"])
## hist(df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing==0,"SW.IPCW"])

## *** step 3
## fit weighted LVM
table(df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing<1,"group4"])
## HC NR RE 
## 91 16 23 
tapply(1/df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing<1,"P.IPCW"],df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing<1,"group4"],sum)
 ##      HC       NR       RE 
 ## 91.0000 150.6365 149.2932 
tapply(df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing<1,"SW.IPCW"],df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing<1,"group4"],sum)
##       HC       NR       RE 
## 91.00000 20.80881 25.16651 

## fit LVM with stabilizing weights
lvmW8IPCW.longi <- lvmW8.longi
lvmfitW8IPCW.longi <- estimate(lvmW8.longi,
                               data = df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing<1,],
                               control = list(start = coef(lvmfitW80.longi), constrain = TRUE),
                               weights = df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing<1,"SW.IPCW"],
                               estimator = "gaussian_weight")
logLik(lvmfitW8IPCW.longi)
## 'log Lik.' 345.1343 (df=26)
## colSums(sCorrect(lvmfitW8IPCW.longi, ssc = NA, df = NA)$sCorrect$score)-score(lvmfitW8IPCW.longi)

## ** inference
lvmfitW8IPCW.longiC <- estimate2(lvmfitW8IPCW.longi, ssc = NA, df = NA)

## global effect: HC vs NR or RE
lvmfitW8IPCW.etaEffect.longi <- glht2(lvmfitW8IPCW.longiC, linfct = c("RE vs. HC" = "eta~group4RE=0",
                                                                     "NR vs. HC" = "eta~group4NR=0",
                                                                     "RE vs. NR" = "eta~group4RE-eta~group4NR=0"))
lvmfitW8IPCW.etaEffect.longi <- summary(lvmfitW8IPCW.etaEffect.longi, test = adjusted("none"))
lvmfitW8IPCW.etaEffect.longi

## Fit: estimate.lvm(x = lvmW8.longi, data = df.longiHCNRRE, control = list(start = coef(lvmfitW80.longi), 
##     constrain = TRUE))
## Standard errors: Model-based

## Linear Hypotheses:
##                 estimate std.error        df  ci.lower  ci.upper statistic p.value  
## RE vs. HC == 0 -0.073225  0.031927       Inf -0.135800 -0.010649   -2.2935 0.02182 *
## NR vs. HC == 0 -0.020161  0.031932       Inf -0.082747  0.042424   -0.6314 0.52779  
## RE vs. NR == 0 -0.053063  0.037611       Inf -0.126780  0.020654   -1.4108 0.15829  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- none method) 

## region specific effect: HC vs NR or RE
lvmW8IPCW.regionEffect.longi <- list("RE vs. HC" = effects2(lvmfitW8IPCW.longiC,
                                                            linfct = c("neocortex: RE vs. HC" = "neocortex.log~group4RE",
                                                                       "hippocampus: RE vs. HC" = "hippocampus.log~group4RE",
                                                                       "neostriatum: RE vs. HC" = "neostriatum.log~group4RE")),
                                     "NR vs HC" =  effects2(lvmfitW8IPCW.longiC,
                                                            linfct = c("neocortex: NR vs. HC" = "neocortex.log~group4NR",
                                                                     "hippocampus: NR vs. HC" = "hippocampus.log~group4NR",
                                                                     "neostriatum: NR vs. HC" = "neostriatum.log~group4NR")),
                                     "RE vs NR" =  effects2(lvmfitW8IPCW.longiC,
                                                            linfct = c("neocortex: RE vs. NR" = "neocortex.log~group4RE-neocortex.log~group4NR",
                                                                     "hippocampus: RE vs. NR" = "hippocampus.log~group4RE-hippocampus.log~group4NR",
                                                                     "neostriatum: RE vs. NR" = "neostriatum.log~group4RE-neostriatum.log~group4NR"
                                                                     )))
lvmW8IPCW.regionEffect.longi <- lapply(lvmW8IPCW.regionEffect.longi, summary,
                                       test = adjusted("single-step"),
                                       transform = function(x){100*(exp(x)-1)},
                                       seed = 10)
lvmW8IPCW.regionEffect.longi
## $`RE vs. HC`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmW8.longi, data = df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing < 
##     1, ], estimator = "gaussian_weight", control = list(start = coef(lvmfitW80.longi), 
##     constrain = TRUE), weights = df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing < 
##     1, "SW.IPCW"])
## Standard errors: Model-based

## Linear Hypotheses:
##                              estimate std.error        df  ci.lower  ci.upper statistic p.value  
## neocortex: RE vs. HC == 0    -7.06079   2.96680       Inf -13.04773  -0.66164   -2.2939 0.02998 *
## hippocampus: RE vs. HC == 0  -7.45838   3.12410       Inf -13.75082  -0.70686   -2.2960 0.02930 *
## neostriatum: RE vs. HC == 0  -5.97938   2.53040       Inf -11.11215  -0.55022   -2.2909 0.03012 *
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00075467 (seed 10) 


## $`NR vs HC`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmW8.longi, data = df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing < 
##     1, ], estimator = "gaussian_weight", control = list(start = coef(lvmfitW80.longi), 
##     constrain = TRUE), weights = df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing < 
##     1, "SW.IPCW"])
## Standard errors: Model-based

## Linear Hypotheses:
##                             estimate std.error      df ci.lower ci.upper statistic p.value
## neocortex: NR vs. HC == 0    -1.9960    3.1294     Inf  -8.0345   4.4391   -0.6314  0.5520
## hippocampus: NR vs. HC == 0  -2.1116    3.3085     Inf  -8.4839   4.7045   -0.6314  0.5518
## neostriatum: NR vs. HC == 0  -1.6833    2.6437     Inf  -6.8100   3.7254   -0.6313  0.5518
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00059548 (seed 10) 


## $`RE vs NR`

## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Fit: estimate.lvm(x = lvmW8.longi, data = df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing < 
##     1, ], estimator = "gaussian_weight", control = list(start = coef(lvmfitW80.longi), 
##     constrain = TRUE), weights = df.longiHCNRRE.IPW[df.longiHCNRRE.IPW$missing < 
##     1, "SW.IPCW"])
## Standard errors: Model-based

## Linear Hypotheses:
##                             estimate std.error       df ci.lower ci.upper statistic p.value
## neocortex: RE vs. NR == 0    -5.1680    3.5665      Inf -12.1683   2.3903   -1.4109  0.1831
## hippocampus: RE vs. NR == 0  -5.4621    3.7623      Inf -12.8304   2.5290   -1.4114  0.1827
## neostriatum: RE vs. NR == 0  -4.3696    3.0298      Inf -10.3521   2.0121   -1.4102  0.1833
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00047276 (seed 10) 


## * Sensitivity analysis (): Bootstrap for the main/secondary analyses
## Bootstrap

if(FALSE){
    lvmfitW8Boot.longi <- bootReg(lvmfitW8F.longi, n.boot = 1000)
    summary(lvmfitW8Boot.longi, index =  which(names(coef(lvmfitW8F.longi)) %in% c("eta~group4NR","eta~group4RE")))
    ##                  estimate estimate.boot        se      lower       upper p.value nBoot.effective

    lvmfitW4Boot.longi <- bootReg(lvmfitW4F.longi, n.boot = 1000)
    summary(lvmfitW4Boot.longi, index =  which(names(coef(lvmfitW4F.longi)) %in% c("eta~group4NR","eta~group4RE")))
    ##                  estimate estimate.boot        se      lower       upper p.value nBoot.effective
}

