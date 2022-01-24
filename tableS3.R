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

df.tableS3 <- do.call(rbind,unname(lapply(lmCor.partial.longi, function(iM){ ## iM <- lmCor.partial.longi[[1]]
    cbind(summary(iM, test = adjusted("none"))$table2[,c("estimate","ci.lower","ci.upper","p.value")],
          adjusted.p.value = summary(iM, test = adjusted("single-step"))$table2[,c("p.value")])
})))
df.tableS3$region <- factor(trim(sapply(strsplit(rownames(df.tableS3), split = "\\.|=="),"[",2)),c("neocortex","limbic","neostriatum",""))
df.tableS3$week <- factor(trim(sapply(strsplit(rownames(df.tableS3), split = "\\.|=="),"[",1)),c("W2","W4","W8","W12"))
df.tableS3[,c("estimate","ci.lower","ci.upper")] <- round(df.tableS3[,c("estimate","ci.lower","ci.upper")], digits = digits)
df.tableS3[,c("p.value","adjusted.p.value")] <- format.pval(df.tableS3[,c("p.value","adjusted.p.value")], digits = digits-1, eps = 10^(-digits.p.value))
df.tableS3[,"CI"] <- paste0("[",df.tableS3[,"ci.lower"],";",df.tableS3[,"ci.upper"],"]")

tableS3 <- df.tableS3[,c("region","week","estimate","CI","p.value","adjusted.p.value")]
tableS3 <- tableS3[order(tableS3$region,tableS3$week),]
tableS3$region[duplicated(tableS3$region)] <- ""
rownames(tableS3) <- NULL
names(tableS3)[names(tableS3)=="estimate"] <- "partial correlation"
##         region week partial correlation             CI p.value adjusted.p.value
## 1    neocortex   W2               0.186 [-0.035;0.407]  0.0984           0.1818
## 2                W4               0.307  [0.093;0.521]  0.0049           0.0096
## 3                W8               0.087 [-0.137;0.311]  0.4444           0.6439
## 4               W12               0.045 [-0.185;0.275]  0.7008           0.9078
## 5       limbic   W2               0.081  [-0.14;0.302]  0.4733           0.7129
## 6                W4               0.223  [0.003;0.442]  0.0466           0.0868
## 7                W8               0.014  [-0.21;0.239]  0.9002           0.9952
## 8               W12              -0.018 [-0.244;0.209]  0.8768           0.9913
## 9  neostriatum   W2               0.057 [-0.169;0.283]  0.6210           0.8630
## 10               W4               0.142 [-0.081;0.364]  0.2113           0.3456
## 11               W8              -0.030 [-0.255;0.195]  0.7940           0.9634
## 12              W12               0.025 [-0.198;0.247]  0.8289           0.9782

## * Export table
myTableS3.doc <- read_docx()
myTableS3.doc <- body_add_table(x = myTableS3.doc, 
                                value = tableS3,
                                header = TRUE,
                                style = "table_template")
myTableS3.doc <- body_end_section_landscape(myTableS3.doc)

print(myTableS3.doc, target = file.path(path.report,"./tableS3.docx"))
