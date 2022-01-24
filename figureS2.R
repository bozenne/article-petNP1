library(ggplot2)
.simpleCap <- function(x) {
    sapply(x, function(iX){
        s <- strsplit(iX, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = " ")
    })
}

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

## * Figure
all.regions <- gsub("\\.log","",endogenous(lvmfitF.baseline))
index.p.value <- sapply(all.regions, grep, rownames(lvm.regionEffect.baseline$table2))
## vec.p.value.S2 <- format.pval(lvm.regionEffect.baseline$table2[index.p.value,"p.value"], digits = 2)
vec.p.value.S2 <- gsub("^0","=0",format.pval(lvm.regionEffect.baseline$table2[index.p.value,"p.value"], digit = 1, eps = 10^(-3)))


df.figure.S2 <- melt(df.baseline, id.vars = c("group"),
                     measure = all.regions,
                     variable.name = "region",value.name = "bp")
## df.figure.S2$type.region <- factor(df.figure.S2$region, levels = c("neocortex","limbic","neostriatum"), labels = c("A","A","B"))
df.figure.S2$region <- factor(df.figure.S2$region, levels = all.regions, labels = paste0(.simpleCap(all.regions)," (p.adj",vec.p.value.S2,")"))
df.figure.S2$group <- factor(df.figure.S2$group, levels = c("Healthy Control","Case"),
                             labels = c("Control","Major \n depressive disorder"))

set.seed(10)
figure.S2 <- ggplot(df.figure.S2, aes(x = group, y = bp))
figure.S2 <- figure.S2 + facet_wrap(~region, scales = "free", nrow = 1, ncol = 4)
figure.S2 <- figure.S2 + geom_boxplot(aes(color = group), outlier.shape = NA, lwd=1.1)
figure.S2 <- figure.S2 + geom_jitter(aes(color = group), size = 3, height = 0, width = 0.1)
##geom_dotplot(aes(fill = group), binaxis = "y", stackdir = "center", dotsize = 0.4, stackratio = 0.8)
figure.S2 <- figure.S2 + scale_fill_manual(name = "",
                                           values = c("Control" = "black", "Major \n depressive disorder" = "gray40"))
figure.S2 <- figure.S2 + scale_color_manual(name = "",
                                            values = c("Control" = "black", "Major \n depressive disorder" = "gray40"))
figure.S2 <- figure.S2 + labs(x = "", y = expression("BP"[ND]*" at baseline"))
## figure.S2 <- figure.S2 + geom_point(data = data.frame(group = factor("Case", bp = 0, region = levels(df.figure.S2$region)), color = NA)
figure.S2 <- figure.S2 + theme_bw() + theme(legend.position="none",
                                            legend.key.height=unit(1.5, "cm"),
                                            axis.title.x=element_blank(),
                                            text = element_text(size=20))
figure.S2

## figure.S2
## * Export
ggsave(figure.S2, filename = file.path(path.report,"./figureS2.pdf"), width = 18)


## ggsave(figure.S2, filename = "c:/Users/hpl802/Documents/Presentations/NeuroPharm/2020/figures/scatterplot_BP.eps")
