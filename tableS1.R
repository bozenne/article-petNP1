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
source(file.path(path.code,"1-analysis-baseline.R"))

## * Table
tableS1 <- cbind(round(gls.regionEffect.baseline$table2[,c("estimate","std.error","ci.lower","ci.upper")], digits = 2),
                 p.value = format.pval(gls.regionEffect.baseline$table[,"p.value"], digits = 2, eps = 10^(-3)))

## * Export table
tableS1 <- gls.regionEffect.baseline$table2
tableS1[,c("estimate","std.error","ci.lower","ci.upper")] <- round(tableS1[,c("estimate","std.error","ci.lower","ci.upper")], digits = 2)
rownames(tableS1) <- sapply(strsplit(rownames(gls.regionEffect.baseline$table2), split=":"),"[",1)
names(tableS1)[names(tableS1) == "estimate"] <- "group difference (%)"
tableS1$p.value <- signif(tableS1$p.value, digits = 2)
##             group difference (%) std.error  df ci.lower ci.upper statistic p.value
## neocortex                  -8.86      2.31 174   -14.00    -3.41 -3.653587 0.00078
## limbic                     -6.53      2.20 174   -11.42    -1.37 -2.872835 0.01100
## neostriatum                -6.21      2.22 174   -11.15    -0.99 -2.707451 0.01700

myTableS1.doc <- read_docx()
myTableS1.doc <- body_add_table(x = myTableS1.doc, 
                               value =  cbind(" " = rownames(tableS1), tableS1),
                               header = TRUE,
                               style = "table_template") 
myTableS1.doc <- body_end_section_landscape(myTableS1.doc)
print(myTableS1.doc, target = file.path(path.report,"./tableS1.docx"))


