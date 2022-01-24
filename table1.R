## * package
library(butils) ## devtools::install_github("bozenne/butils")
library(data.table)
library(officer)

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
source(file.path(path.code,"data-management-baseline.R"))
source(file.path(path.code,"data-management-longitudinal-prediction.R"))
df.baseline$AUC1000 <- df.baseline$AUC/1e3

## * Create table 
## colnames(df.baseline)

## continuous variables
col.continuous <- c("age","school.years","fam.disp","bmi","hamd17.baseline", "hamd6.baseline", "mdi", "inj.dose.Mbq", "inj.mass.ug", "sb.per.kg", "AUC1000")

## categorical variables
col.categorical <- c("sex","sert")

ff <- as.formula(paste0("group ~ ",paste0(c(col.continuous,col.categorical), collapse = "+")))


## add one decimal
table1 <- descriptiveTable(ff, data = df.baseline, guess.categorical = 2, add.groupAll = FALSE,
                           test.categorical = fisher.test, test.continuous = wilcox.test)
##    ** categorical variables ** 
##                   Case (n=91)            Healthy Control (n=91)                  
## variable   level            n frequency                       n frequency p.value
##      sex  Female           65     71.43                      55     60.44    <NA>
##             Male           26     28.57                      36     39.56    0.16
##     sert    LALA           26     28.57                      27     29.67    <NA>
##          nonLALA           65     71.43                      64     70.33       1

 ##    ** continuous variables ** 
 ##                  Case (n=91)                               Healthy Control (n=91)                                   
 ##        variable         mean      sd       [min;max] n.NA                    mean      sd     [min;max] n.NA p.value
 ##             age        27.07   (8.2)   [18.27;57.32]    0                   27.13     (8)  [19.23;60.1]    0    0.57
 ##    school.years        11.58  (1.07)          [5;12]   15                   11.88  (0.51)        [9;12]    0   <0.01
 ##        fam.disp          0.5  (0.64)           [0;2]   15                    0.25  (0.46)         [0;2]    0   <0.01
 ##             bmi        24.54  (5.63)    [17.1;45.07]    0                   23.58  (3.06) [18.34;36.85]    0    0.96
 ## hamd17.baseline        22.86  (3.36)         [18;31]    0                    <NA>    (NA)       [NA;NA]   91    <NA>
 ##  hamd6.baseline        12.33  (1.65)          [7;17]    0                    <NA>    (NA)       [NA;NA]   91    <NA>
 ##             mdi        34.65  (7.18)         [16;50]    2                    5.62  (4.22)        [0;18]    0   <0.01
 ##    inj.dose.Mbq       577.38 (55.95) [263.02;614.95]    0                  569.44 (76.27)     [226;617]    0     0.2
 ##     inj.mass.ug         0.95  (1.04)     [0.23;6.49]    0                    1.18  (0.99)   [0.21;4.35]    0    0.03
 ##         AUC1000        10.26  (2.56)    [3.95;17.75]    0                   10.32  (2.51)  [3.24;16.19]    6    0.75

df.table2 <- merge(df.longiNRRE, df.baseline[df.baseline$id %in% df.longiNRRE$id,c("id","mdi")],
                   by = "id")
table2 <- descriptiveTable(group4 ~ hamd17.baseline + hamd6.baseline + mdi, data = df.table2, add.groupAll = FALSE,
                           test.categorical = fisher.test, test.continuous = wilcox.test)
 ##    ** continuous variables ** 
 ##                  RE (n=22)                        NR (n=13)                              
 ##        variable       mean     sd [min;max] n.NA       mean     sd [min;max] n.NA p.value
 ## hamd17.baseline      22.86 (3.03)   [18;29]    0      21.54 (2.15)   [18;27]    0    0.23
 ##  hamd6.baseline      11.91 (1.54)    [8;14]    0      11.23 (1.83)    [7;14]    0    0.18
 ##             mdi      32.91 (9.03)   [16;50]    0      35.92  (5.9)   [29;45]    1    0.19

pROC::auc(group4 ~ hamd6.baseline, df.table2)
## Area under the curve: 0.6346
pROC::ci.auc(group4 ~ hamd6.baseline, df.table2)
## 95% CI: 0.4373-0.832 (DeLong)

## * Export table
table1print <- print(table1,
                     digit.frequency = 3,
                     digit.center = 3,
                     digit.spread = 3,
                     digit.p = 3
                     )

table2print <- print(table2,
                     digit.frequency = 3,
                     digit.center = 3,
                     digit.spread = 3,
                     digit.p = 3
                     )
myTable1.doc <- read_docx()
myTable1.doc <- body_add_par(x = myTable1.doc, value = "Continuous variables", style = "heading 1")
myTable1.doc <- body_add_table(x = myTable1.doc, 
                               value =  table1print$table.officer[[1]],
                               header = FALSE,
                               style = "table_template") 
myTable1.doc <- body_add_par(x = myTable1.doc, value = "Categorical variables", style = "heading 1")
myTable1.doc <- body_add_table(x = myTable1.doc, 
                               value = table1print$table.officer[[2]],
                               header = FALSE,
                               style = "table_template") 
myTable1.doc <- body_add_par(x = myTable1.doc, value = "Psychopathology", style = "heading 1")
myTable1.doc <- body_add_table(x = myTable1.doc, 
                               value = table2print$table.officer[[1]],
                               header = FALSE,
                               style = "table_template") 
myTable1.doc <- body_end_section_landscape(myTable1.doc)
print(myTable1.doc, target = file.path(path.report,"./table1.docx"))

