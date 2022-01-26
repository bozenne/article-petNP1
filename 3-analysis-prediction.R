## because of extra-text + multiple comparisons

## * Packages
library(Publish)
library(xtable)
library(BuyseTest) ## install.packages("BuyseTest")
## version 2.3.10
library(data.table)
library(pROC)
library(cvAUC)
library(epiR)
library(ggplot2)
library(ggpubr)
BuyseTest.options(order.Hprojection=2)
LOO.pv <- function(class, biomarker){
    n <- length(class)
    out <- list(CV = data.frame(matrix(NA, ncol = 5, nrow = n,
                                       dimnames = list(NULL, c("obs","threshold","class","biomarker","prediction")))
                                ),
                table = NULL,
                ppv = NA,
                npv = NA)
    for(i in 1:n){## i <- 1
        df <- data.frame(class = class[-i], biomarker = biomarker[-i])
        ## from the doc ">": if the predictor values for the control group are higher than the values of the case group
        suppressMessages(iRoc <- roc(class ~ biomarker, data = df, direction = ">")) 
        iCoords <- coords(iRoc, x = "best", input = "threshold",
                          ret = c("threshold"), best.method = "youden", transpose = TRUE)
        
        out$CV[i,"obs"] <- i
        out$CV[i,"threshold"] <- iCoords
        out$CV[i,"class"] <- class[i]
        out$CV[i,"biomarker"] <- biomarker[i]
        out$CV[i,"prediction"] <- biomarker[i] < out$CV[i,"threshold"]
    }
    out$table <- table(prediction = out$CV[,"prediction"], class = out$CV[,"class"])
    out$ppv <- out$table[1,1]/sum(out$table[1,])
    out$npv <- out$table[2,2]/sum(out$table[2,])
    return(out)
}

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
path.report <- file.path(path,"article")


## * Load data
source(file.path(path.code,"data-management-longitudinal-prediction.R"))

## names(df.longi)
regions <- c("neocortex", "neostriatum", "limbic")##c("neocortex","hippocampus","limbic","neostriatum")
n.region <- length(regions)

## * Main analysis week 8 (Remitters vs. non-responders)
ls.roc.w8 <- setNames(vector(mode = "list", length = n.region), regions)
ls.logit.w8 <- setNames(vector(mode = "list", length = n.region), regions)
table.pred.w8 <- data.table(matrix(as.numeric(NA), nrow = n.region, ncol = 14,
                                   dimnames = list(NULL, c("region","threshold",
                                                           "OR","OR(CI)","OR(p-value)",
                                                           "ppv","ppv(CI)","ppv.corrected","npv","npv(CI)","npv.corrected",
                                                           "AUC","AUC(CI)","AUC(p-value)"))
                                ))
table.pred.w8[["OR(CI)"]] <- as.character(NA)
table.pred.w8[["ppv(CI)"]] <- as.character(NA)
table.pred.w8[["npv(CI)"]] <- as.character(NA)
table.pred.w8[["AUC(CI)"]] <- as.character(NA)
table.pred.w8$region <- regions

## boxplot(df.longi$neocortex ~ df.longi$group4)
## table(df.longi$hamd6.w8<=5)
## table(df.longi$change.hamd6.w8<=-50)


for(iR in 1:n.region){ ## iR <- 1 
    iRegion <- regions[iR]
    cat("region: ",iRegion,"\n")
    iFormula <- as.formula(paste0("group4 ~",iRegion))
    
    iDf.prediction <- df.longi[df.longi$group4 %in% c("Non-responder","Remitter"),c("group4",iRegion)]
    iDf.prediction$group4 <- droplevels(as.factor(iDf.prediction$group4))
    iDf.prediction$group4 <- relevel(iDf.prediction$group4, "Non-responder") ## reference level
    ## boxplot(iDf.prediction$neocortex ~ iDf.prediction$group4)
    
    ## ** AUC
    e.auc <- BuyseTest::auc(labels = iDf.prediction$group4,
                            predictions = iDf.prediction[[iRegion]],
                            direction = "<")
    
    table.pred.w8[iR, "AUC"] <- confint(e.auc)[,"estimate"]
    table.pred.w8[iR, "AUC(CI)"] <- paste0("[",round(confint(e.auc)[,"lower"],3),";",round(confint(e.auc)[,"upper"],3),"]")
    table.pred.w8[iR, "AUC(p-value)"] <- confint(e.auc)[,"p.value"]## wilcox.test(fit ~ group4, data = iDf.prediction)$p.value

    ## ** ROC
    ## from the doc direction = ">": if the predictor values for the control group are higher than the values of the case group
    suppressMessages(ls.roc.w8[[iR]] <- roc(iFormula, data = iDf.prediction, direction = ">") )
    iCoords <- coords(ls.roc.w8[[iR]], x = "best", input = "threshold",
                      ret = c("ppv","npv","se","sp","threshold"), best.method = "youden", transpose = TRUE)
    table.pred.w8[iR, threshold := iCoords["threshold"]]
    table.pred.w8[iR, ppv := iCoords["ppv"]]
    table.pred.w8[iR, npv := iCoords["npv"]]
    iEpi <- epi.tests(table(iDf.prediction[[iRegion]] < table.pred.w8[iR, threshold], iDf.prediction$group4))
    if(abs(iEpi$detail$pv.neg[,"est"]-table.pred.w8[iR, ppv])>0 || abs(iEpi$detail$pv.pos[,"est"]-table.pred.w8[iR, npv])>0){
        stop("PPV and NPV do not match between pROC package and epiR package")
        ## NOTE: ppv and npv seems "inverted" - TOFIX
    }
    table.pred.w8[iR, c("ppv(CI)") := paste0("[",round(iEpi$detail$pv.pos[,"lower"],3),";",round(iEpi$detail$pv.pos[,"lower"],3),"]")]
    table.pred.w8[iR, c("npv(CI)") := paste0("[",round(iEpi$detail$pv.neg[,"lower"],3),";",round(iEpi$detail$pv.neg[,"lower"],3),"]")]
    
    iLOO <- LOO.pv(class = iDf.prediction$group4, biomarker = iDf.prediction[[iRegion]])
    table.pred.w8[iR, ppv.corrected := iLOO$ppv]
    table.pred.w8[iR, npv.corrected := iLOO$npv]


    ## ** Logistic regression for predicting 
    ls.logit.w8[[iR]] <- glm(iFormula, data = iDf.prediction, family = binomial(link = "logit"))

    ## store OR from logistic regression
    table.pred.w8[iR, "OR"] <- exp(summary(ls.logit.w8[[iR]])$coef[iRegion,"Estimate"]/10)
    table.pred.w8[iR, "OR(CI)"] <- paste0("[",paste0(round(exp(confint(ls.logit.w8[[iR]])[iRegion,]/10),3),collapse=";"),"]")
    table.pred.w8[iR, "OR(p-value)"] <- summary(ls.logit.w8[[iR]])$coef[iRegion,"Pr(>|z|)"]
}

print(table.pred.w8[,.SD, .SDcols = c("region","OR","OR(CI)","OR(p-value)")])
##         region        OR        OR(CI) OR(p-value)
## 1:   neocortex 0.5912343 [0.272;1.143]   0.1395285
## 2: neostriatum 0.9265018 [0.784;1.069]   0.3235028
## 3:      limbic 0.7358635 [0.383;1.301]   0.3091366
print(table.pred.w8[,.SD, .SDcols = c("region","AUC","AUC(CI)","AUC(p-value)")])
##         region       AUC       AUC(CI) AUC(p-value)
## 1:   neocortex 0.6328671 [0.428;0.838]    0.2039595
## 2: neostriatum 0.5664336  [0.363;0.77]    0.5219302
## 3:      limbic 0.5699301 [0.346;0.794]    0.5410268
print(table.pred.w8[,.SD, .SDcols = c("region","threshold","ppv","ppv(CI)","npv","npv(CI)")])
##         region threshold       ppv       ppv(CI)       npv       npv(CI)
## 1:   neocortex 0.6919132 0.7600000 [0.549;0.906] 0.7000000 [0.348;0.933]
## 2: neostriatum 3.7628335 0.7200000 [0.506;0.879] 0.6000000 [0.262;0.878]
## 3:      limbic 0.9033387 0.7241379 [0.528;0.873] 0.8333333 [0.359;0.996]
mean(iDf.prediction$group4=="Remitter")
## [1] 0.6285714

if(FALSE){
    for(iRegion in 1:length(ls.roc.w8)){
        pdf(file.path(path.report,paste0("roc-",names(ls.roc.w8)[iRegion],"-w8.pdf")))
        plot(ls.roc.w8[[iRegion]], type="shape", col="lightblue", main = gsub("\\.log","",names(ls.roc.w8)[iRegion]))
        plot(attr(ls.roc.w8[[iRegion]],"ci"), type="shape", col="lightblue")
        abline(a=1,b=-1)
        dev.off()
    }
}


## * Secondary analysis week 4 (Remitters vs. non-responders)
ls.roc.w4 <- setNames(vector(mode = "list", length = n.region), regions)
ls.logit.w4 <- setNames(vector(mode = "list", length = n.region), regions)
table.pred.w4 <- data.table(matrix(as.numeric(NA), nrow = n.region, ncol = 13,
                                   dimnames = list(NULL, c("region","threshold",
                                                           "OR","OR(CI)","OR(p-value)",
                                                           "ppv","ppv(CI)","ppv.corrected","npv","npv(CI)","npv.corrected",
                                                           "AUC","AUC(p-value)"))
                                   ))
table.pred.w4[["OR(CI)"]] <- as.character(NA)
table.pred.w4[["ppv(CI)"]] <- as.character(NA)
table.pred.w4[["npv(CI)"]] <- as.character(NA)
table.pred.w4$region <- regions

for(iR in 1:n.region){ ## iR <- 1
    iRegion <- regions[iR]
    cat("region: ",iRegion,"\n")
    iFormula <- as.formula(paste0("response.w4 ~",iRegion))

    iDf.prediction <- df.longi[df.longi$response.w4 %in% c("Early non-responder","Early responder"),c("response.w4",iRegion)]
    iDf.prediction$response.w4 <- droplevels(as.factor(iDf.prediction$response.w4))
    iDf.prediction$response.w4 <- relevel(iDf.prediction$response.w4,"Early non-responder")
    ## boxplot(iDf.prediction$neocortex ~ iDf.prediction$response.w4)
 
    ## ** AUC
    e.auc <- BuyseTest::auc(labels = iDf.prediction$response.w4,
                            predictions = iDf.prediction[[iRegion]],
                            direction = "<")
    
    table.pred.w4[iR, "AUC"] <- confint(e.auc)[,"estimate"]
    table.pred.w4[iR, "AUC(p-value)"] <- confint(e.auc)[,"p.value"]## wilcox.test(fit ~ response.w4, data = iDf.prediction)$p.value

    ## ** ROC
    ## from the doc direction = ">": if the predictor values for the control group are higher than the values of the case group
    suppressMessages(ls.roc.w4[[iR]] <- roc(iFormula, data = iDf.prediction, direction = ">") )
    iCoords <- coords(ls.roc.w4[[iR]], x = "best", input = "threshold",
                      ret = c("ppv","npv","se","sp","threshold"), best.method = "youden", transpose = TRUE)
    table.pred.w4[iR, threshold := iCoords["threshold"]]
    table.pred.w4[iR, ppv := iCoords["ppv"]]
    table.pred.w4[iR, npv := iCoords["npv"]]
    iEpi <- epi.tests(table(iDf.prediction[[iRegion]] < table.pred.w4[iR, threshold], iDf.prediction$response.w4))
    if(abs(iEpi$detail$pv.neg[,"est"]-table.pred.w4[iR, ppv])>0 || abs(iEpi$detail$pv.pos[,"est"]-table.pred.w4[iR, npv])>0){
        stop("PPV and NPV do not match between pROC package and epiR package")
        ## NOTE: ppv and npv seems "inverted" - TOFIX
    }
    table.pred.w4[iR, c("ppv(CI)") := paste0("[",round(iEpi$detail$pv.pos[,"lower"],3),";",round(iEpi$detail$pv.pos[,"lower"],3),"]")]
    table.pred.w4[iR, c("npv(CI)") := paste0("[",round(iEpi$detail$pv.neg[,"lower"],3),";",round(iEpi$detail$pv.neg[,"lower"],3),"]")]
    
    iLOO <- LOO.pv(class = iDf.prediction$response.w4, biomarker = iDf.prediction[[iRegion]])
    table.pred.w4[iR, ppv.corrected := iLOO$ppv]
    table.pred.w4[iR, npv.corrected := iLOO$npv]

    ## ** Logistic regression for predicting 
    ls.logit.w4[[iR]] <- glm(iFormula, data = iDf.prediction, family = binomial(link = "logit"))

    table.pred.w4[iR, "OR"] <- exp(summary(ls.logit.w4[[iR]])$coef[iRegion,"Estimate"]/10)
    table.pred.w4[iR, "OR(CI)"] <- paste0("[",paste0(round(exp(confint(ls.logit.w4[[iR]])[iRegion,]/10),3),collapse=";"),"]")
    table.pred.w4[iR, "OR(p-value)"] <- summary(ls.logit.w4[[iR]])$coef[iRegion,"Pr(>|z|)"]
}

print(table.pred.w4)
##         region threshold        OR        OR(CI) OR(p-value)       ppv       ppv(CI) ppv.corrected       npv       npv(CI)
## 1:   neocortex 0.6919132 0.5732328 [0.296;1.023]   0.0717004 0.8235294 [0.655;0.932]     0.5000000 0.5714286 [0.289;0.823]
## 2: neostriatum 3.7628335 0.9124649 [0.793;1.033]   0.1662798 0.7878788  [0.611;0.91]     0.4375000 0.4666667 [0.213;0.734]
## 3:      limbic 0.9042679 0.6870908 [0.404;1.096]   0.1304746 0.7948718 [0.635;0.907]     0.6666667 0.6666667 [0.299;0.925]
##    npv.corrected       AUC AUC(p-value)
## 1:     0.7777778 0.6491597    0.1249798
## 2:     0.7812500 0.6071429    0.2412129
## 3:     0.7948718 0.5966387    0.3527738

mean(iDf.prediction$response.w4=="Early responder")
## [1] 0.7083333

## * Secondary analysis week 8 (alternative endpoints)
table.auc.bis <- NULL

table(df.longi$hamd6.w8<=5)
## FALSE  TRUE 
##    36    42 
table(df.longi$change.hamd6.w8<=-50)
## FALSE  TRUE 
##    32    46 
table(df.longi$hamd6.w8<=5,df.longi$change.hamd6.w8<=-50)
  ##       FALSE TRUE
  ## FALSE    31    5
  ## TRUE      1   41



for(iR in 1:n.region){ ## iR <- 1 
    iRegion <- regions[iR]
    cat("region: ",iRegion,"\n")
    
    iDf.prediction <- df.longi[df.longi$group4 != "Healthy Control",c("hamd6.w8","change.hamd6.w8",iRegion,"response.w8")]
    
    ## ** AUC
    ## boxplot(iDf.prediction[[iRegion]] ~ iDf.prediction$response.w8 == "Remitter")
    e.auc.remission <- BuyseTest::auc(labels = iDf.prediction$response.w8 == "Remitter",
                                      predictions = iDf.prediction[[iRegion]],
                                      direction = "<")

    e.auc.nonresponse <- BuyseTest::auc(labels = iDf.prediction$response.w8 == "Non-responder",
                                        predictions = iDf.prediction[[iRegion]],
                                        direction = ">")

    e.auc.healthy <- BuyseTest::auc(labels = iDf.prediction$hamd6.w8 <= 5,
                                      predictions = iDf.prediction[[iRegion]],
                                      direction = "<")

    e.auc.improvement <- BuyseTest::auc(labels = iDf.prediction$change.hamd6.w8 <= -50,
                                        predictions = iDf.prediction[[iRegion]],
                                        direction = "<")

    
    df.tempo <- rbind(data.frame(outcome = "remission",
                                 region = iRegion,
                                 estimate = confint(e.auc.remission)[,"estimate"],
                                 CI = paste0("[",round(confint(e.auc.remission)[,"lower"],3),";",round(confint(e.auc.remission)[,"upper"],3),"]"),
                                 p.value = confint(e.auc.remission)[,"p.value"]),
                      data.frame(outcome = "non-response",
                                 region = iRegion,
                                 estimate = confint(e.auc.nonresponse)[,"estimate"],
                                 CI = paste0("[",round(confint(e.auc.nonresponse)[,"lower"],3),";",round(confint(e.auc.nonresponse)[,"upper"],3),"]"),
                                 p.value = confint(e.auc.nonresponse)[,"p.value"]),
                      data.frame(outcome = "hamd6(w8)<=5",
                                 region = iRegion,
                                 estimate = confint(e.auc.healthy)[,"estimate"],
                                 CI = paste0("[",round(confint(e.auc.healthy)[,"lower"],3),";",round(confint(e.auc.healthy)[,"upper"],3),"]"),
                                 p.value = confint(e.auc.healthy)[,"p.value"]),
                      data.frame(outcome = "relative change in hamd6(w8)<=-50%",
                                 region = iRegion,
                                 estimate = confint(e.auc.improvement)[,"estimate"],
                                 CI = paste0("[",round(confint(e.auc.improvement)[,"lower"],3),";",round(confint(e.auc.improvement)[,"upper"],3),"]"),
                                 p.value = confint(e.auc.improvement)[,"p.value"])
                      )
    
    table.auc.bis <- rbind(table.auc.bis, df.tempo)

}

print(table.auc.bis)
##                               outcome      region  estimate            CI   p.value
## 1                           remission   neocortex 0.4935065 [0.356;0.631] 0.9260938
## 2                        non-response   neocortex 0.6508876 [0.464;0.837] 0.1127455
## 3                        hamd6(w8)<=5   neocortex 0.5105820  [0.381;0.64] 0.8726586
## 4  relative change in hamd6(w8)<=-50%   neocortex 0.5129076  [0.38;0.646] 0.8495681
## 5                           remission neostriatum 0.4805195 [0.337;0.624] 0.7898718
## 6                        non-response neostriatum 0.5905325  [0.42;0.761] 0.2967524
## 7                        hamd6(w8)<=5 neostriatum 0.4781746 [0.349;0.607] 0.7407721
## 8  relative change in hamd6(w8)<=-50% neostriatum 0.5373641 [0.406;0.668] 0.5757497
## 9                           remission      limbic 0.4577922  [0.325;0.59] 0.5320470
## 10                       non-response      limbic 0.5928994 [0.391;0.795] 0.3665711
## 11                       hamd6(w8)<=5      limbic 0.4874339 [0.356;0.619] 0.8509450
## 12 relative change in hamd6(w8)<=-50%      limbic 0.4911685  [0.352;0.63] 0.9008741

## display
df.ggplot <- reshape2::melt(df.longi[df.longi$group4 != "Healthy Control",],
                            id.vars = c("id","hamd6.w8","change.hamd6.w8"), measure.vars = regions,
                            variable.name = "region", value.name = "binding")

gg <- ggarrange(
    ggplot(df.ggplot, aes(x = binding, fill = hamd6.w8<=5)) + geom_histogram() + facet_wrap(~ region, scales = "free"),
    ggplot(df.ggplot, aes(x = binding, fill = change.hamd6.w8<=-50)) + geom_histogram() + facet_wrap(~ region, scales = "free"),
    nrow = 2
)
## ggsave(gg, filename = "c:/Users/hpl802/Documents/Consult/Consult-NRU/C45-WP1-Kristin/meeting/fig-alternativePred.pdf")


## sum(!is.na(df.longi$hamd6.w8))
## sum(!is.na(df.longi$change.hamd6.w8))

table(df.longi$hamd6.w12<=5)
## sum(!is.na(df.longi$hamd6.w12))
## table(df.longi$change.hamd6.w12<=-50)
## sum(!is.na(df.longi$change.hamd6.w12))

table(100*(df.longi$hamd6.w12-df.longi$hamd6.baseline)/df.longi$hamd6.baseline <= -50)
    ## range(100*(df.longi$hamd6.w12-df.longi$hamd6.baseline)/df.longi$hamd6.baseline - df.longi$change.hamd6.w12,
          ## na.rm=TRUE)




### cor.test(df0.longi[["HAMD.6.score...Week.8"]], df0.longi[["Neocortex_SB_BPnd_NonPV_GM"]])
### cor.test(df0.longi[["HAMD.6.score...Week.12"]], df0.longi[["Neocortex_SB_BPnd_NonPV_GM"]])

### cor.test(df0.longi[["NP1.secondary.outcome...Percent.change.in.HAMD.6.at.week.8.compared.to.baseline"]], df0.longi[["Neocortex_SB_BPnd_NonPV_GM"]])
## * naive classifier
n <- 1e3
Y <- rbinom(n, prob = 0.65, size = 1)
X <- rbinom(n, prob = 0.65, size = 1)

epi.tests(table(Y,X))
## Positive predictive value              0.39 (0.34, 0.44)
## Negative predictive value              0.65 (0.61, 0.69)
