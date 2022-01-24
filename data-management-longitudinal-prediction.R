library(testthat)

## * import
source(file.path(path.code,"data-management-baseline.R"))
## list.files(path.data)
## file.exists(file.path(path.data,"NP1_all_matched_baseline_final.csv"))
df0.longi <- read.csv(file.path(path.data,"NP1_all_matched_baseline_compliance_final.csv"),
                      sep=";", dec=",", na.strings = "")
tempo <- read.csv(file.path(path.data,"NP1_all_matched_baseline_final.csv"),
                  sep=";", dec=",", na.strings = "")

## (df0.longi[["HAMD.6.score...Week.8"]] - df0.longi[["HAMD.6.score...Baseline"]])/

## * rename columns
names(df0.longi)[1] <- "id"
##grep("fifty", names(df0.longi), value=TRUE)
rename.longi <- c("id" = "id",
                  "group"="Person.status",
                  "compliance"="Documented.compliance.at.week.8.",
                  "age" = "Age.at.SB.scan",
                  "sex"="Gender",
                  "bmi"="BMI",
                  "education"="Education.score.individual..1.5." ,
                  "fam.disp"="FHAM.OS.FHAM.....of.1deg..relatives.w..depression.problem",
                  "fifty.response"="fifty_response",
                  ## "mdi"="MDI",
                  ## "total.mde"="Total.number.of.MDD.episodes..self.reported.",
                  "sert"="LALA_1_nonLALA_0",
                  "mr"="prism_1_trio_0",
                  "hamd6.baseline"="HAMD.6.score...Baseline",                                                        
                  "hamd17.baseline"= "HAMD.17.score...Baseline",  
                  "hamd6.w2"="HAMD.6.score...Week.2",                                                        
                  "hamd6.w4"="HAMD.6.score...Week.4",                                                        
                  "hamd6.w8"="HAMD.6.score...Week.8",                                                        
                  "hamd17.w8"= "HAMD.17.score...Week.8",  
                  "hamd6.w12"="HAMD.6.score...Week.12",                                                        
                  "hamd17.w12"= "HAMD.17.score...Week.12",  
                  "change.hamd6.w8"="NP1.secondary.outcome...Percent.change.in.HAMD.6.at.week.8.compared.to.baseline",
                  "change.hamd6.w12"="Percent.change.in.HAMD.6.at.week.12.compared.to.baseline",
                  "response.w8" = "NP1.primary.outcome...Categorical.treatment.response",
                  "response.w4"="Categorical.early..week.4..treatment.response", 
                  "group4"="Group.4",
                  "escit"="Escitalopram.concentration..nM." ,
                  "dulox"="Duloxetin.concentration..nM.",
                  "drug.w8"="Escitalpram.week.8.yes.no",  
                  "AUC"="SB.Time.normalized.AUC.CB..NRU.ROI..Raw.GM..Bq.ml.",
                  "sb.per.kg"="SB.injected.mass.per.kg..microgram.kg.",
                  "neocortex" = "Neocortex_SB_BPnd_NonPV_GM",
                  "hippocampus" = "Total_hippocampus_SB_BPnd_NonPV_GM",
                  "caudate" = "Total_cau_SB_BPnd_NonPV_GM",
                  "putamen"="Total_put_SB_BPnd_NonPV_GM",
                  "limbic"="Limbic_SB_BPnd_NonPV_GM",
                  "neostriatum"="HighBinding_SB_BPnd_NonPV_GM"
                  )

df.longi <- df0.longi[,rename.longi] 
names(df.longi) <- names(rename.longi)

## * re-label factor variables
df.longi[["mr"]] <- factor(df.longi[["mr"]], levels = c(0,1), labels = c("trio","prisma"))
df.longi[["sert"]] <- factor(df.longi[["sert"]], levels = c(0,1), labels = c("nonLALA","LALA"))
df.longi[["drug.w8"]] <- factor(df.longi[["drug.w8"]], levels = c("Yes","No"), labels = c("Escitalopram","Duloxetine"))
df.longi[["change.hamd6.w2"]] <- 100*(df.longi[["hamd6.w2"]]-df.longi[["hamd6.baseline"]])/df.longi[["hamd6.baseline"]]
df.longi[["change.hamd6.w4"]] <- 100*(df.longi[["hamd6.w4"]]-df.longi[["hamd6.baseline"]])/df.longi[["hamd6.baseline"]]
## range(df.longi[["change.hamd6.w12"]]-100*(df.longi[["hamd6.w12"]]-df.longi[["hamd6.baseline"]])/df.longi[["hamd6.baseline"]], na.rm = TRUE)
df.longi[["change50.w8"]] <- as.numeric(df.longi$change.hamd6.w8>=-50)
df.longi[["change50.w8"]][is.na(df.longi[["change50.w8"]])] <- -1
df.longi[["response50.w8"]] <- factor(df.longi$change50.w8, levels = c(-1,0,1), labels = c("HC","50m","50pe"))
## table(df.longi[["response50.w8"]],df.longi[["response.w8"]])

## * check data
expect_true(sum(duplicated(df.longi$id)) == 0) 

expect_true(all(df.longi$group %in% c("Healthy Control","Case")))

expect_true(min(df.longi$age) >= 18) 
expect_true(max(df.longi$age) <= 100) 

expect_true(all(df.longi$sex %in% c("Female","Male")))

expect_true(min(df.longi$bmi) >= 10) 
expect_true(max(df.longi$bmi) <= 50)  ## !! bmi of 45

expect_true(all(df.longi$education %in% c(1:5,NA)))

expect_true(all(df.longi$sb.per.kg > 0))

expect_true(all(df.longi$neocortex > 0))
expect_true(all(df.longi$hippocampus > 0))
expect_true(all(df.longi$caudate > 0))
expect_true(all(df.longi$putamen > 0))
expect_true(all(df.longi$limbic > 0))
expect_true(all(df.longi$neostriatum > 0))


## * log transformation
df.longi$neocortex.log <- log(df.longi$neocortex)
df.longi$hippocampus.log <- log(df.longi$hippocampus)
df.longi$caudate.log <- log(df.longi$caudate)
df.longi$putamen.log <- log(df.longi$putamen)
df.longi$neostriatum.log <- log(df.longi$neostriatum)
df.longi$limbic.log <- log(df.longi$limbic)

## * subset
## ** HC, NR, RE but no Others (week 8)
## df.HCNRRE
df.longiHCNRRE <- df.longi[df.longi$group4 %in% c( "Healthy Control","Non-responder", "Remitter"),]
## table(df.longi2$group4, useNA = "always")
df.longiHCNRRE$group4 <- factor(df.longiHCNRRE$group4, 
                           levels = c("Healthy Control", "Non-responder", "Remitter"),
                           labels = c("HC","NR", "RE"))

## ** NR, RE, Other, but no HC (week 8)
df.longiCase <- df.longi[df.longi$group4!="Healthy Control",]
df.longiCase$group4 <- droplevels(as.factor(df.longiCase$group4))
## table(df.case$group4)

## ** NR and RE (week 8)
df.longiNRRE <- df.longiHCNRRE[df.longiHCNRRE$group4!="HC",]
df.longiNRRE$group4 <- droplevels(as.factor(df.longiNRRE$group4))
## table(df.NRRE$group4)

## ** HC, ENR, ER but no Others (week 4)
## df.HCNRRE
df.longiHCENRER <- df.longi[df.longi$response.w4 %in% c( "HC","Early non-responder", "Early responder"),]
## table(df.longi2$group4, useNA = "always")
df.longiHCENRER$response.w4 <- factor(df.longiHCENRER$response.w4, 
                                 levels = c( "HC","Early non-responder", "Early responder"),
                                 labels = c("HC","ENR", "ER"))
