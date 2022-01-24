library(testthat)

## * import
## list.files(path.data)
##file.exists(file.path(path.data,"NP1_all_matched_baseline_final.csv"))
df0.baseline <- read.csv(file.path(path.data,"NP1_all_matched_baseline_final.csv"),
                         sep=";", dec=",", na.strings = "")

## * rename columns
names(df0.baseline)[1] <- "id"
rename.baseline <- c("id" = "id",
                     "group"="Person.status",
                     ## "compliance"="Documented.compliance.at.week.8.",
                     "age" = "Age.at.SB.scan",
                     "sex"="Gender",
                     "bmi"="BMI",
                     "education"="Education.score.individual..1.5." ,
                     "school.years"="Number.of.years.in.school..max..12.",
                     "fam.disp"="FHAM.OS.FHAM.....of.1deg..relatives.w..depression.problem",
                     "mdi"="MDI",
                     "total.mde"="Total.number.of.MDD.episodes..self.reported.",
                     "sert"="LALA_1_nonLALA_0",
                     "mr"="prism_1_trio_0",
                     "hamd6.baseline"="HAMD.6.score...Baseline",                                                        
                     "hamd17.baseline"= "HAMD.17.score...Baseline",  
                     ## "pct.hamd6.w8"="NP1.secondary.outcome...Percent.change.in.HAMD.6.at.week.8.compared.to.baseline",
                     ## "pct.hamd6.w4"="Percent.change.in.HAMD.6.at.week.4.compared.to.baseline",
                     ## "response.w8" = "NP1.primary.outcome...Categorical.treatment.response",
                     ## "response.w4"="Categorical.early..week.4..treatment.response", 
                     ## "group4"="Group.4",
                     ## "escit"="Escitalopram.concentration..nM." ,
                     ## "dulox"="Duloxetin.concentration..nM.",
                     ## "drug.w8"="Escitalpram.week.8.yes.no",  
                     "AUC"="SB.Time.normalized.AUC.CB..NRU.ROI..Raw.GM..Bq.ml.",
                     "sb.per.kg"="SB.injected.mass.per.kg..microgram.kg.",
                     "inj.mass.ug"="SB.injected.mass..microgram.",
                     "inj.dose.Mbq"="SB.injected.dose..MBq." ,
                     "neocortex" = "Neocortex_SB_BPnd_NonPV_GM",
                     "hippocampus" = "Total_hippocampus_SB_BPnd_NonPV_GM",
                     "caudate" = "Total_cau_SB_BPnd_NonPV_GM",
                     "putamen"="Total_put_SB_BPnd_NonPV_GM",
                     "limbic"="Limbic_SB_BPnd_NonPV_GM",
                     "neostriatum"="HighBinding_SB_BPnd_NonPV_GM" 
               
)
##grep("Number",names(df0.baseline), value = TRUE)
df.baseline <- df0.baseline[,rename.baseline] 
names(df.baseline) <- names(rename.baseline)

## * re-label factor variables
df.baseline[["mr"]] <- factor(df.baseline[["mr"]], levels = c(0,1), labels = c("trio","prisma"))
df.baseline[["sert"]] <- factor(df.baseline[["sert"]], levels = c(0,1), labels = c("nonLALA","LALA"))
## df.baseline[["drug.w8"]] <- factor(df.baseline[["drug.w8"]], levels = c("Yes","No"), labels = c("Escitalopram","Duloxetine"))
df.baseline[["id"]] <- as.character(df.baseline[["id"]])

df.baseline$group <- relevel(as.factor(df.baseline$group), "Healthy Control")

## head(df.baseline)
## str(df.baseline)

## * check data
## str(df.baseline)

## each subject only appears once
expect_true(sum(duplicated(df.baseline$id)) == 0) 

expect_true(all(df.baseline$group %in% c("Healthy Control","Case")))

expect_true(min(df.baseline$age) >= 18) 
expect_true(max(df.baseline$age) <= 100) 

expect_true(all(df.baseline$sex %in% c("Female","Male")))

expect_true(min(df.baseline$bmi) >= 10) 
expect_true(max(df.baseline$bmi) <= 50)  ## !! bmi of 45

expect_true(all(df.baseline$education %in% c(1:5,NA)))

expect_true(all(df.baseline$sb.per.kg > 0))

expect_true(all(df.baseline$neocortex > 0))
expect_true(all(df.baseline$hippocampus > 0))
expect_true(all(df.baseline$caudate > 0))
expect_true(all(df.baseline$putamen > 0))
expect_true(all(df.baseline$limbic > 0))
expect_true(all(df.baseline$neostriatum > 0))

## * log transformation of the bindings
df.baseline$neocortex.log <- log(df.baseline$neocortex)
df.baseline$hippocampus.log <- log(df.baseline$hippocampus)
df.baseline$caudate.log <- log(df.baseline$caudate)
df.baseline$putamen.log <- log(df.baseline$putamen)
df.baseline$neostriatum.log <- log(df.baseline$neostriatum)
df.baseline$limbic.log <- log(df.baseline$limbic)

