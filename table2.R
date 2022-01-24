library(officer)

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
source(file.path(path.code,"2-analysis-longitudinal.R"))

## * Table
digits <- 2
digits.p.value <- 3

## latent variables
vec.n <- c(table(lvmfitW4F.longi$data$model.frame$response.w4),table(lvmfitW8F.longi$data$model.frame$group4))
df.table <- do.call("rbind", c(unname(lapply(lvmW4F.regionEffect.longi, "[[", "table2")),
                               unname(lapply(lvmW8F.regionEffect.longi, "[[", "table2"))))
rownames(df.table) <- gsub(" == 0","",rownames(df.table))
M.neocortex <- round(rbind(df.table[c("neocortex: ER vs. HC","neocortex: ENR vs. HC","neocortex: ER vs. ENR"),c("estimate","ci.lower","ci.upper")],
                           df.table[c("neocortex: RE vs. HC","neocortex: NR vs. HC","neocortex: RE vs. NR"),c("estimate","ci.lower","ci.upper")]),
                     digits = digits) 
M.hippocampus <- round(rbind(df.table[c("hippocampus: ER vs. HC","hippocampus: ENR vs. HC","hippocampus: ER vs. ENR"),c("estimate","ci.lower","ci.upper")],
                             df.table[c("hippocampus: RE vs. HC","hippocampus: NR vs. HC","hippocampus: RE vs. NR"),c("estimate","ci.lower","ci.upper")]),
                       digits = digits) 
M.neostriatum <- round(rbind(df.table[c("neostriatum: ER vs. HC","neostriatum: ENR vs. HC","neostriatum: ER vs. ENR"),c("estimate","ci.lower","ci.upper")],
                             df.table[c("neostriatum: RE vs. HC","neostriatum: NR vs. HC","neostriatum: RE vs. NR"),c("estimate","ci.lower","ci.upper")]),
                       digits = digits) 
vec.pval <- c(lvmW4F.etaEffect.longi$table2[c("ER vs. HC","ENR vs. HC","ER vs. ENR"),"p.value"],
              lvmW8F.etaEffect.longi$table2[c("RE vs. HC","NR vs. HC","RE vs. NR"),"p.value"])

pasteVS <- function(...){paste(..., collapse = " vs. ")}
table2 <- as.data.frame(rbind("n" = c(pasteVS(vec.n[c("ER","HC")]),pasteVS(vec.n[c("ENR","HC")]),pasteVS(vec.n[c("ER","ENR")]),
                                      pasteVS(vec.n[c("RE","HC")]),pasteVS(vec.n[c("NR","HC")]),pasteVS(vec.n[c("RE","NR")])),
                              "week" = c(rep(4,3),rep(8,3)),
                              "p.value" = format.pval(vec.pval, digits = digits, eps = 10^(-digits.p.value)),
                              "neocortex" = paste0(M.neocortex[,"estimate"],"% [",M.neocortex[,"ci.lower"],";",M.neocortex[,"ci.upper"],"]"),
                              "hippocampus" = paste0(M.hippocampus[,"estimate"],"% [",M.hippocampus[,"ci.lower"],";",M.hippocampus[,"ci.upper"],"]"),
                              "neostriatum" = paste0(M.neostriatum[,"estimate"],"% [",M.neostriatum[,"ci.lower"],";",M.neostriatum[,"ci.upper"],"]")
                              ))
names(table2) <- c("ER vs. HC"," ENR vs. HC","ER vs. ENR","RE vs. HC","NR vs. HC","RE vs. NR")

table2
                        ## ER vs. HC          ENR vs. HC           ER vs. ENR             RE vs. HC            NR vs. HC            RE vs. NR
## n                       34 vs. 91           14 vs. 91            34 vs. 14             22 vs. 91            13 vs. 91            22 vs. 13
## week                            4                   4                    4                     8                    8                    8
## p.value                    0.0021              0.7911               0.0458                0.0038               0.3124               0.1795
## neocortex   -8.96% [-14.63;-2.91]  -1.03% [-8.37;6.9] -8.01% [-15.61;0.27]  -9.5% [-15.85;-2.66] -3.89% [-11.16;3.98] -5.84% [-14.04;3.16]
## hippocampus   -10% [-16.27;-3.25] -1.16% [-9.34;7.77] -8.94% [-17.34;0.31] -9.92% [-16.54;-2.77] -4.07% [-11.65;4.17]  -6.1% [-14.65;3.31]
## neostriatum  -7.68% [-12.6;-2.47] -0.88% [-7.17;5.84] -6.86% [-13.45;0.23] -8.19% [-13.76;-2.27]   -3.34% [-9.64;3.4]  -5.02% [-12.16;2.7]


## * Export table
myTable2.doc <- read_docx()
myTable2.doc <- body_add_table(x = myTable2.doc, 
                               value = cbind(" " = rownames(table2), table2),
                               header = TRUE,
                               style = "table_template") 
myTable2.doc <- body_end_section_landscape(myTable2.doc)
print(myTable2.doc, target = file.path(path.report,"./table2.docx"))

