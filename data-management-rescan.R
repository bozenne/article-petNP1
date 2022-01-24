library(testthat)
library(reshape2)
library(data.table)

## * import
df0.rescan <- read.csv(file.path(path.data,"NP1_rescan_compliance_final.csv"),
                       header = TRUE, dec = ",", sep = ";", na.strings = "")

## * rename columns
names(df0.rescan)[1] <- "id"
keep.cols <- c("id" = "id" ,
               "timepoint"="PET.scan.type" ,
               "change.hamd6.w8"="NP1.secondary.outcome...Percent.change.in.HAMD.6.at.week.8.compared.to.baseline",
               "response.w8" = "NP1.primary.outcome...Categorical.treatment.response",
               "weight" = "Weight..kg.",
               "sb"="SB.injected.mass..microgram.",
               "sb.per.kg"="SB.injected.mass.per.kg..microgram.kg.",
               "escit"="Escitalopram.concentration..nM." ,
               "dulox"="Duloxetin.concentration..nM.",
               "neocortex" = "Neocortex_SB_BPnd_NonPV_GM",
               "hippocampus" = "Total_hippocampus_SB_BPnd_NonPV_GM",
               "caudate" = "Total_cau_SB_BPnd_NonPV_GM",
               "putamen"="Total_put_SB_BPnd_NonPV_GM",
               "neostriatum"="HighBinding_SB_BPnd_NonPV_GM",
               "limbic"="Limbic_SB_BPnd_NonPV_GM"
)
##grep("Limbic",names(d), value = TRUE)
df.rescan <- df0.rescan[,keep.cols] 
names(df.rescan) <- names(keep.cols)

## impute weight for id = 56103 (weight week 0, 4, 8, 12: 66.8kg, 67kg, ?, 67.9kg)
df.rescan[df.rescan$id=="56103" & df.rescan$timepoint == "Intervention Rescan","weight"] <- 67.4
##range(df.rescan$sb/df.rescan$weight - df.rescan$sb.per.kg, na.rm = TRUE)
##df.rescan[which(abs(df.rescan$sb/df.rescan$weight - df.rescan$sb.per.kg)>1e-5),]
##       id           timepoint change.hamd6.w8 response.w8 weight        sb   sb.per.kg 
## 38 56058 Intervention Rescan       -36.36364       Other   62.3 0.7641968 0.011848012 
## 54 56137 Intervention Rescan       -92.30769       Other   51.0 0.3811930 0.007151838
df.rescan$sb.per.kg <- df.rescan$sb/df.rescan$weight
    
df.rescan$timepoint <- factor(df.rescan$timepoint,
                              levels= c("Baseline", "Intervention Rescan"),
                              labels = c("baseline","rescan"))

dfL.rescan <- reshape2::melt(df.rescan, measure.vars = c("sb.per.kg","neocortex", "hippocampus", "caudate", "putamen", "neostriatum", "limbic"),
                   id.vars = c("id", "escit", "response.w8", "change.hamd6.w8", "timepoint"))

dfW.rescan <- reshape2::dcast(dfL.rescan,
                              value.var = "value",
                              formula = id + escit+change.hamd6.w8+ response.w8 ~ variable + timepoint)

## head(dfW.rescan)


## * Log-transform my brain bindings, b for baseline and rs for rescan.
dfW.rescan$b_neocortex.log <- log(dfW.rescan$neocortex_baseline)
dfW.rescan$rs_neocortex.log <- log(dfW.rescan$neocortex_rescan)
dfW.rescan$b_limbic <- log(dfW.rescan$limbic_baseline)
dfW.rescan$rs_limbic.log <- log(dfW.rescan$limbic_rescan)
dfW.rescan$b_neostriatum <- log(dfW.rescan$neostriatum_baseline)
dfW.rescan$rs_neostriatum.log <- log(dfW.rescan$neostriatum_rescan)
dfW.rescan$b_caudate <- log(dfW.rescan$caudate_baseline)
dfW.rescan$rs_caudate.log <- log(dfW.rescan$caudate_rescan)
dfW.rescan$b_hippocampus <- log(dfW.rescan$hippocampus_baseline)
dfW.rescan$rs_hippocampus.log <- log(dfW.rescan$hippocampus_rescan)
dfW.rescan$b_putamen <- log(dfW.rescan$putamen_baseline)
dfW.rescan$rs_putamen.log <- log(dfW.rescan$putamen_rescan)

## * Take the difference of the log-values (NB! this gives a ratio!)

dfW.rescan$dif_neocortex.log <- dfW.rescan$rs_neocortex.log - dfW.rescan$b_neocortex.log
dfW.rescan$dif_limbic.log <- dfW.rescan$rs_limbic.log - dfW.rescan$b_limbic
dfW.rescan$dif_neostriatum.log <- dfW.rescan$rs_neostriatum.log - dfW.rescan$b_neostriatum
dfW.rescan$dif_hippocampus.log <- dfW.rescan$rs_hippocampus.log - dfW.rescan$b_hippocampus
dfW.rescan$dif_caudate.log <- dfW.rescan$rs_caudate.log - dfW.rescan$b_caudate
dfW.rescan$dif_putamen.log <- dfW.rescan$rs_putamen.log - dfW.rescan$b_putamen

## * Fill missing value
dfW.rescan[dfW.rescan$id == "56103",]

## * Difference in sb.per.kg at baseline and rescan

dfW.rescan$dif_sb.per.kg <- dfW.rescan$sb.per.kg_rescan-dfW.rescan$sb.per.kg_baseline


## ** keep NR and RE
dfW.rescan.binary <- dfW.rescan[dfW.rescan$response.w8!="Other",]
dfW.rescan.binary$response.w8 <- droplevels(as.factor(dfW.rescan.binary$response.w8))
## table(dfW.rescan.binary$response.w8)
