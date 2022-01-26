## * Packages
library(lavaSearch2) ## installation: devtools::install_github("bozenne/lavaSearch2", ref = "9e7fa13ed7522b45871f5bfdea02ffeddaee1b90")
library(LMMstar)  ## installation: devtools::install_github("bozenne/LMMstar", ref = "19bf4587e55a4821d369d937e8480b82b26cdc60")
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
source(file.path(path.code,"data-management-baseline.R"))

## * check data
## hist(df.baseline$neocortex.log)
## hist(df.baseline$hippocampus.log)
## hist(df.baseline$caudate.log)
## hist(df.baseline$putamen.log)
## hist(df.baseline$neostriatum.log)

## hist(df.baseline$age)
## table(df.baseline$sex)
## table(df.baseline$sert)
## table(df.baseline$mr)
## hist(df.baseline$sb.per.kg)
## table(df.baseline$group)

keep.var <- c("neocortex.log","hippocampus.log","caudate.log","putamen.log","neostriatum.log","age","sex","sert","mr","sb.per.kg","group")
colSums(is.na(df.baseline[,keep.var]))

## * Main analysis (result section): Group comparison (HC vs MDD) at baseline using LVM
NROW(df.baseline)
## [1] 182

## ** fit LVM

## 0- fit simplify model to initialize the model of interest
lvm0.baseline <- lvm(neocortex.log ~ age+sex+sert+mr+sb.per.kg,
                     hippocampus.log ~ age+sex+sert+mr+sb.per.kg,
                     caudate.log ~ age+sex+sert+mr+sb.per.kg,
                     putamen.log ~ age+sex+sert+mr+sb.per.kg
                     )
## manifest(lvm0.baseline) %in% names(df.baseline)
## colSums(is.na(df.baseline[,manifest(lvm0.baseline)]))
lvmfit0.baseline <- estimate(lvm0.baseline, data = df.baseline)
## logLik(lvmfit0.baseline)
## 'log Lik.' 353.2183 (df=28)
 
## 1- model of interest
lvm.baseline <- lvm(neocortex.log ~ eta + age+sex+sert+mr+sb.per.kg,
                    hippocampus.log ~ eta + age+sex+sert+mr+sb.per.kg,
                    caudate.log ~ eta + age+sex+sert+mr+sb.per.kg,
                    putamen.log ~ eta + age+sex+sert+mr+sb.per.kg,
                    eta  ~ group
                    )
latent(lvm.baseline) <- ~eta
## plot(lvm.baseline)

df.baseline$group <- relevel(df.baseline$group, ref = "Healthy Control")
lvmfit.baseline <- estimate(lvm.baseline, data = df.baseline,
                            control = list(start = coef(lvmfit0.baseline), constrain = TRUE))
logLik(lvmfit.baseline)
## 'log Lik.' 606.5701 (df=33)
## summary(lvmfit.baseline)

## ** diagnostics
## score tests
search.baseline <- modelsearch(lvmfit.baseline)
## search.baseline$res[NROW(search.baseline$res),]
##                  Score: S                    P(S>s)                     Index 
##                   "14.33"               "0.0001531" "putamen.log~caudate.log" 


lvmF.baseline <- lvm.baseline
covariance(lvmF.baseline) <- putamen.log~caudate.log

lvmfitF.baseline <- estimate(lvmF.baseline, data = df.baseline,
                             control = list(start = coef(lvmfit0.baseline), constrain = TRUE))

gof(lvmfitF.baseline)
 ## Number of observations = 182 
 ## BIC = -1002.232 
 ## AIC = -1158.302 
 ## log-Likelihood of model = 613.151 

 ## log-Likelihood of saturated model = 614.8999 
 ## Chi-squared statistic: q = 3.497907 , df = 4 
 ##  P(Q>q) = 0.4781967 

## logLik(eF.baseline)
## 'log Lik.' 613.151 (df=34)
searchF.baseline <- modelsearch(lvmfitF.baseline)
## searchF.baseline$res[NROW(searchF.baseline$res),]
## Score: S                    P(S>s)                     Index 
##  "2.022"                   "0.155" "groupCase~neocortex.log" 

## chi2-test
summary(lvmfitF.baseline)
## Latent variables: eta 
## Number of rows in data=182
## ______________________________________________________________________
##                              Estimate Std. Error  Z-value   P-value   std.xy
## Measurements:                                                               
##    neocortex.log~eta          1.00000                                0.82372
##    hippocampus.log~eta        1.09847    0.08203 13.39108    <1e-12  0.83389
##    caudate.log~eta            0.92934    0.07846 11.84519    <1e-12  0.77351
##    putamen.log~eta            0.89648    0.06660 13.46049    <1e-12  0.80971
## Regressions:                                                                
##    neocortex.log~age         -0.00203    0.00134 -1.51204    0.1305 -0.10439
##    neocortex.log~sb.per.kg   -2.45924    0.72668 -3.38424 0.0007138 -0.23287
##    neocortex.log~sexMale      0.07624    0.02744  2.77870  0.005458  0.23099
##    neocortex.log~sertLALA    -0.02495    0.02367 -1.05412    0.2918 -0.07248
##    neocortex.log~mrprisma     0.03974    0.03639  1.09224    0.2747  0.10327
##     eta~groupCase            -0.07796    0.02349 -3.31963 0.0009014 -0.30255
##    hippocampus.log~age       -0.00094    0.00152 -0.62083    0.5347 -0.04479
##    hippocampus.log~sb.per.kg -2.15395    0.82376 -2.61479  0.008928 -0.18797
##    hippocampus.log~sexMale    0.02982    0.03107  0.95968    0.3372  0.08326
##    hippocampus.log~sertLALA  -0.00039    0.02684 -0.01436    0.9885 -0.00103
##    hippocampus.log~mrprisma   0.08665    0.04096  2.11564   0.03438  0.20751
##     caudate.log~age          -0.00349    0.00137 -2.53532   0.01123 -0.18135
##     caudate.log~sb.per.kg    -0.88957    0.74481 -1.19437    0.2323 -0.08511
##     caudate.log~sexMale      -0.02012    0.02804 -0.71754     0.473 -0.06159
##     caudate.log~sertLALA     -0.01969    0.02428 -0.81099    0.4174 -0.05778
##     caudate.log~mrprisma      0.04102    0.03653  1.12286    0.2615  0.10771
##    putamen.log~age           -0.00417    0.00122 -3.42900 0.0006058 -0.23559
##    putamen.log~sb.per.kg     -1.76418    0.65947 -2.67515   0.00747 -0.18317
##    putamen.log~sexMale        0.03510    0.02489  1.41024    0.1585  0.11661
##    putamen.log~sertLALA       0.01218    0.02149  0.56692    0.5708  0.03879
##    putamen.log~mrprisma      -0.00110    0.03293 -0.03336    0.9734 -0.00313
## Intercepts:                                                                 
##    neocortex.log              0.00000                                0.00000
##    eta                       -0.36473    0.05184 -7.03601 1.978e-12 -2.83070
##    hippocampus.log            0.48342    0.05233  9.23799    <1e-12  2.84820
##    caudate.log                1.73346    0.04970 34.87693    <1e-12 11.19770
##    putamen.log                1.70597    0.04162 40.99258    <1e-12 11.95889
## Residual Variances:                                                         
##    neocortex.log              0.00553    0.00094  5.86364            0.22622
##    eta                        0.01508    0.00218  6.92048            0.90846
##    hippocampus.log            0.00831    0.00125  6.62975            0.28832
##    caudate.log                0.00866    0.00121  7.13215            0.36127
##    caudate.log~~putamen.log   0.00276    0.00082  3.35263 0.0008005  0.12490
##    putamen.log                0.00486    0.00081  6.02286            0.23890
## ______________________________________________________________________
## Estimator: gaussian 
## ______________________________________________________________________

##  Number of observations = 182 
##  BIC = -1002.232 
##  AIC = -1158.302 
##  log-Likelihood of model = 613.151 

##  log-Likelihood of saturated model = 614.8999 
##  Chi-squared statistic: q = 3.497907 , df = 4 
##   P(Q>q) = 0.4781967 

##  RMSEA (90% CI): 0 (0;0.1055)
##   P(RMSEA<0.05)=0.6777366

## normality
## qqplot2(lvmfitF.baseline)
## hist(iid(lvmfitF.baseline)[,"eta~groupCase"])
## hist(iid(lvmfitF.baseline)[,"hippocampus.log~eta"])
## hist(iid(lvmfitF.baseline)[,"putamen.log~eta"])
## coef(lvmfitF.baseline)["putamen.log~eta"]

## ** inference
lvmfitF.baselineC <- estimate2(lvmfitF.baseline, ssc = NA, df = NA) ## pre-compute quantities for glht2/effect2

## summary(lvmfitF.baseline)
## summary2(lvmfitF.baseline)$table2["eta~groupCase",]
lvm.etaEffect.baselineC <- glht2(lvmfitF.baselineC, linfct = c("eta~groupCase = 0"))
lvm.etaEffect.baselineC <- summary(lvm.etaEffect.baselineC)
lvm.etaEffect.baselineC
##                     estimate std.error        df  ci.lower  ci.upper statistic   p.value    
## eta~groupCase == 0 -0.077965  0.023486       Inf -0.123997 -0.031933   -3.3196 0.0009014 ***

## lava:::effects.lvmfit(lvmfitF.baseline, neocortex.log~groupCase)
## lava:::effects.lvmfit(lvmfitF.baseline, hippocampus.log~groupCase)

## effects(lvmfitF.baseline, putamen.log~groupCase)

lvm.regionEffect.baselineC <- effects2(lvmfitF.baselineC,
                                      linfct = c("neocortex.log~groupCase",
                                                 "hippocampus.log~groupCase",
                                                 "caudate.log~groupCase",
                                                 "putamen.log~groupCase")
                                      )
lvm.regionEffect.baselineC <- summary(lvm.regionEffect.baselineC,
                                     test = adjusted("single-step"),
                                     ## test = adjusted("bonferroni"),
                                     ## test = adjusted("free"),
                                     transform = function(x){100*(exp(x)-1)},
                                     seed = 10)
lvm.regionEffect.baselineC
## Linear Hypotheses:
##                                estimate       se       df    lower    upper statistic   p.value    
## neocortex.log~groupCase == 0    -7.5003   2.1724      Inf -12.0198  -2.7487   -3.3196 0.0011472 ** 
## hippocampus.log~groupCase == 0  -8.2077   2.3746      Inf -13.1353  -3.0005   -3.3105 0.0009971 ***
## caudate.log~groupCase == 0      -6.9893   2.0503      Inf -11.2612  -2.5118   -3.2869 0.0012870 ** 
## putamen.log~groupCase == 0      -6.7507   1.9656      Inf -10.8502  -2.4627   -3.3158 0.0009341 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single step max-test) 
## Error when computing the p-value by numerical integration: 0.00049876 (seed 10) 

## ** export latent variable
df.lvm <- data.frame(id = df.baseline$id, group = df.baseline$group, lvm = as.double(predict(lvmfitF.baseline, x = manifest(lvmfitF.baseline), y = latent(lvmfitF.baseline))))
## boxplot(df.lvm$lvm ~ df.lvm$group)
## t.test(df.lvm$lvm ~ df.lvm$group)
## write.csv(df.lvm, file = "c:/Users/hpl802/Documents/Projects/BD-WP3-prediction/data/NeuropharmNP1-lvm.csv")

## * Secondary analysis (supplementary material): Group comparison (HC vs MDD) at baseline using gls

## ** fit
gls.baseline <- list(neocortex = lmm(neocortex.log~group+age+sex+sert+mr+sb.per.kg, repetition =  ~ group|id, data=df.baseline),
                     limbic = lmm(limbic.log~group+age+sex+sert+mr+sb.per.kg, repetition =  ~ group|id, data=df.baseline), 
                     neostriatum = lmm(neostriatum.log~group+age+sex+sert+mr+sb.per.kg, repetition = ~group|id, data=df.baseline))
class(gls.baseline)<- "mmm"

## ** diagnostic

## qqtest(residuals(gls.baseline$neocortex, type = "normalized", format = "long"))
## qqtest(residuals2(gls.baseline$hippocampus, type = "normalized", format = "long"))
## qqtest(residuals2(gls.baseline$neostriatum, type = "normalized", format = "long"))

## score(gls.baseline$neocortex, indiv = TRUE)
## hist(iid(gls.baseline$neocortex)[,"groupCase"], xlim = c(-0.1,0.1));abline(v=coef(gls.baseline$neocortex)["groupCase"], col ="red")
## hist(iid(gls.baseline$limbic)[,"groupCase"], xlim = c(-0.1,0.1));abline(v=coef(gls.baseline$limbic)["groupCase"], col ="red")
## hist(iid(gls.baseline$neostriatum)[,"groupCase"], xlim = c(-0.1,0.1));abline(v=coef(gls.baseline$neostriatum)["groupCase"], col ="red")

## ** inference
a1 <- anova(gls.baseline$neocortex, "groupCase=0", ci = TRUE)
a2 <- anova(gls.baseline$limbic, "groupCase=0", ci = TRUE)
a3 <- anova(gls.baseline$neostriatum, "groupCase=0", ci = TRUE)

summary(rbind(a1,a2,a3),
        test = adjusted("single-step"),
        transform = function(x){100*(exp(x)-1)},
        seed = 10)
## summary(glht(gls.baseline, linfct = mlf("groupCase=0")))


## 	 Simultaneous Tests for General Linear Hypotheses

## Multiple Comparisons of Means (two sided tests) 

## Standard errors: Model-based
## Linear Hypotheses:
##                                  estimate        se        df     lower     upper statistic   p.value    
## neocortex.log: groupCase == 0    -8.85837   2.31438 175.00000 -13.99306  -3.41714   -3.6528 0.0007737 ***
## limbic.log: groupCase == 0       -6.52723   2.19713 175.00000 -11.41220  -1.37288   -2.8717 0.0106746 *  
## neostriatum.log: groupCase == 0  -6.21050   2.22067 175.00000 -11.14686  -0.99989   -2.7080 0.0166410 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (CIs/p-values adjusted for multiple comparisons -- single step max-test) 
## Error when computing the adjusted p-value by numerical integration: 0.00063344 (seed 10) 

## * Sensitivity analysis (): Bootstrap for the main analysis
## Bootstrap

if(FALSE){
    lvmfitBoot.baseline <- bootReg(lvmfitF.baseline, n.boot = 1000)
    summary(lvmfitBoot.baseline, index =  which(names(coef(lvmfitF.baseline)) == "eta~groupCase"))
}
##                  estimate estimate.boot        se      lower       upper p.value nBoot.effective
## eta~groupCase -0.07796484    -0.0780074 0.0219448 -0.1198022 -0.03600017       0             986

