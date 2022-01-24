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
digits <- 2
digits.p.value <- 3

dt.tableS2 <- do.call(rbind,unname(lapply(1:length(gls.regionEffect.longi), function(iL){ ## iL <- 1
    iTable <- gls.regionEffect.longi[[iL]]$table2
    data.table(time = strsplit(names(gls.regionEffect.longi)[iL],":")[[1]][1],
               group = trim(strsplit(rownames(iTable),":|=")[[1]][2]),
               region = sapply(strsplit(rownames(iTable),":"),"[",1),
               iTable)
})))

## vec.n <- c(table(lvmfitW4F.longi$data$model.frame$response.w4),table(lvmfitW8F.longi$data$model.frame$group4))
order.group <- c("ER vs. HC",  "ENR vs. HC", "ER vs. ENR", "RE vs. HC",  "NR vs. HC",  "RE vs. NR" )

vec.n <- c(table(lvmfitW4F.longi$data$model.frame$response.w4),table(lvmfitW8F.longi$data$model.frame$group4))

dt.neocortex <- rbind(dt.tableS2[time == "W4" & region == "neocortex",c("group","estimate","ci.lower","ci.upper","p.value")],
                      dt.tableS2[time == "W8" & region == "neocortex",c("group","estimate","ci.lower","ci.upper","p.value")])
dt.neocortex[, estimate := round(estimate, digits)]
dt.neocortex[, ci.lower := round(ci.lower, digits)]
dt.neocortex[, ci.upper := round(ci.upper, digits)]
dt.neocortex[, group := factor(group, order.group)]
dt.neocortex[, p.value := format.pval(p.value, digits, eps = 10^(-digits.p.value))]
setkeyv(dt.neocortex, "group")

dt.limbic <- rbind(dt.tableS2[time == "W4" & region == "limbic",c("group","estimate","ci.lower","ci.upper","p.value")],
                     dt.tableS2[time == "W8" & region == "limbic",c("group","estimate","ci.lower","ci.upper","p.value")])
dt.limbic[, estimate := round(estimate, digits)]
dt.limbic[, ci.lower := round(ci.lower, digits)]
dt.limbic[, ci.upper := round(ci.upper, digits)]
dt.limbic[, group := factor(group, order.group)]
dt.limbic[, p.value := format.pval(p.value, digits, eps = 10^(-digits.p.value))]
setkeyv(dt.limbic, "group")

dt.neostriatum <- rbind(dt.tableS2[time == "W4" & region == "neostriatum",c("group","estimate","ci.lower","ci.upper","p.value")],
                        dt.tableS2[time == "W8" & region == "neostriatum",c("group","estimate","ci.lower","ci.upper","p.value")])
dt.neostriatum[, estimate := round(estimate, digits)]
dt.neostriatum[, ci.lower := round(ci.lower, digits)]
dt.neostriatum[, ci.upper := round(ci.upper, digits)]
dt.neostriatum[, group := factor(group, order.group)]
dt.neostriatum[, p.value := format.pval(p.value, digits, eps = 10^(-digits.p.value))]
setkeyv(dt.neostriatum, "group")

pasteVS <- function(...){paste(..., collapse = " vs. ")}
tableS2 <- as.data.frame(rbind("n" = c(pasteVS(vec.n[c("ER","HC")]),pasteVS(vec.n[c("ENR","HC")]),pasteVS(vec.n[c("ER","ENR")]),
                                       pasteVS(vec.n[c("RE","HC")]),pasteVS(vec.n[c("NR","HC")]),pasteVS(vec.n[c("RE","NR")])),
                               "week" = c(rep(4,3),rep(8,3)),
                               "neocortex" = paste0(dt.neocortex[,estimate],"% [",dt.neocortex[,ci.lower],";",dt.neocortex[,ci.upper],"]"),
                               "p.value" = dt.neocortex[,p.value],
                               "limbic" = paste0(dt.limbic[,estimate],"% [",dt.limbic[,ci.lower],";",dt.limbic[,ci.upper],"]"),
                               "p.value" = dt.limbic[,p.value],
                               "neostriatum" = paste0(dt.neostriatum[,estimate],"% [",dt.neostriatum[,ci.lower],";",dt.neostriatum[,ci.upper],"]"),
                               "p.value" = dt.neostriatum[,p.value]
                               ))
names(tableS2) <- order.group
##                        ER vs. HC          ENR vs. HC           ER vs. ENR            RE vs. HC             NR vs. HC            RE vs. NR
## n                      34 vs. 91           14 vs. 91            34 vs. 14            22 vs. 91             13 vs. 91            22 vs. 13
## week                           4                   4                    4                    8                     8                    8
## neocortex    -7.95% [-14.5;-0.9] 2.16% [-8.08;13.55]   -9.9% [-20.2;1.73] -8.52% [-16.38;0.09] -0.06% [-10.55;11.66] -8.46% [-20.15;4.94]
## p.value                    0.025               0.852                0.099                0.052                 1.000                0.245
## limbic      -4.48% [-11.28;2.85]  2.79% [-7.64;14.4] -7.07% [-17.92;5.21] -4.08% [-11.82;4.33]    0.13% [-9.75;11.1]  -4.21% [-15.74;8.9]
## p.value.1                   0.28                0.77                 0.30                 0.44                  1.00                 0.67
## neostriatum -4.92% [-11.15;1.73] 0.93% [-8.48;11.32]  -5.8% [-15.91;5.51]  -5.25% [-13.02;3.2]  -1.42% [-11.36;9.62] -3.89% [-15.78;9.69]
## p.value.2                   0.17                0.97                 0.37                 0.27                  0.94                 0.72


## * Export table
myTableS2.doc <- read_docx()
myTableS2.doc <- body_add_table(x = myTableS2.doc, 
                               value = cbind(" " = rownames(tableS2),tableS2),
                               header = TRUE,
                               style = "table_template")
myTableS2.doc <- body_end_section_landscape(myTableS2.doc)

print(myTableS2.doc, target = file.path(path.report,"./tableS2.docx"))
