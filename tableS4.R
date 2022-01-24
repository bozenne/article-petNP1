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
source(file.path(path.code,"2-analysis-longitudinal.R"))

## * Table
digits <- 3
digits.p.value <- 3

## latent variables
vec.n <- table(lvmfitW8IPCW.longi$data$model.frame$group4)
df.table <- do.call("rbind", c(unname(lapply(lvmW8IPCW.regionEffect.longi, "[[", "table2"))))
rownames(df.table) <- gsub(" == 0","",rownames(df.table))
M.neocortex <- round(df.table[c("neocortex: RE vs. HC","neocortex: NR vs. HC","neocortex: RE vs. NR"),c("estimate","ci.lower","ci.upper")],
                     digits = digits) 
M.hippocampus <- round(df.table[c("hippocampus: RE vs. HC","hippocampus: NR vs. HC","hippocampus: RE vs. NR"),c("estimate","ci.lower","ci.upper")],
                       digits = digits) 
M.neostriatum <- round(df.table[c("neostriatum: RE vs. HC","neostriatum: NR vs. HC","neostriatum: RE vs. NR"),c("estimate","ci.lower","ci.upper")],
                       digits = digits) 
vec.pval <- c(lvmfitW8IPCW.etaEffect.longi$table2[c("RE vs. HC == 0","NR vs. HC == 0","RE vs. NR == 0"),"p.value"])

pasteVS <- function(...){paste(..., collapse = " vs. ")}
tableS4 <- as.data.frame(rbind("n" = c(pasteVS(vec.n[c("RE","HC")]),pasteVS(vec.n[c("NR","HC")]),pasteVS(vec.n[c("RE","NR")])),
                               "week" = c(rep(8,3)),
                               "p.value" = format.pval(vec.pval, digits = digits, eps = 10^(-digits.p.value)),
                               "neocortex" = paste0(M.neocortex[,"estimate"],"% [",M.neocortex[,"ci.lower"],";",M.neocortex[,"ci.upper"],"]"),
                               "hippocampus" = paste0(M.hippocampus[,"estimate"],"% [",M.hippocampus[,"ci.lower"],";",M.hippocampus[,"ci.upper"],"]"),
                               "neostriatum" = paste0(M.neostriatum[,"estimate"],"% [",M.neostriatum[,"ci.lower"],";",M.neostriatum[,"ci.upper"],"]")
                               ))
names(tableS4) <- c("RE vs. HC","NR vs. HC","RE vs. NR")
##                            RE vs. HC              NR vs. HC              RE vs. NR
## n                          23 vs. 91              16 vs. 91              23 vs. 16
## week                               8                      8                      8
## p.value                       0.0218                 0.5278                 0.1583
## neocortex   -7.061% [-13.048;-0.662] -1.996% [-8.035;4.439] -5.168% [-12.168;2.39]
## hippocampus -7.458% [-13.751;-0.707] -2.112% [-8.484;4.704] -5.462% [-12.83;2.529]
## neostriatum  -5.979% [-11.112;-0.55]  -1.683% [-6.81;3.725] -4.37% [-10.352;2.012]


## * Export table
myTableS4.doc <- read_docx()
myTableS4.doc <- body_add_table(x = myTableS4.doc, 
                                value = cbind(" " = rownames(tableS4), tableS4),
                                header = TRUE,
                                style = "table_template")
myTableS4.doc <- body_end_section_landscape(myTableS4.doc)

print(myTableS4.doc, target = file.path(path.report,"./tableS4.docx"))


