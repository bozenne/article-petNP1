library(ggplot2)
library(ggpubr)

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

## * Figure
ls2df.figure.3 <- do.call(rbind, c(unname(lapply(lvmW4F.regionEffect.longi,"[[","table2")),
                                   unname(lapply(lvmW8F.regionEffect.longi,"[[","table2"))))

## ** figure (week 4)
df.figure.3a <- df.longiHCENRER[,c("id","response.w4","neocortex")]
df.figure.3a$group <- factor(df.figure.3a$response.w4,
                            levels = c("HC","ENR","ER"),
                            labels = c("Control","Early non-responder","Early responder"))

df.pvalue.3a <- data.frame(group1 = c("Control","Control","Early non-responder"),
                           group2 = c("Early non-responder","Early responder","Early responder"),
                           p = ls2df.figure.3[paste0("neocortex: ",c("ENR vs. HC","ER vs. HC","ER vs. ENR")),"p.value"],
                           y.position = c(0.95,1,1.05),
                           stringsAsFactors = FALSE)
df.pvalue.3a$p <- paste0("p.adj=",format.pval(df.pvalue.3a$p, digits = 2, eps = 0.001))

figure.3a <- ggplot(df.figure.3a, aes(x = group, y = neocortex))
figure.3a <- figure.3a + geom_boxplot(aes(color = group), outlier.shape = NA)
figure.3a <- figure.3a + geom_dotplot(aes(fill = group), binaxis = "y", dotsize = 0.4, stackdir = "center")
figure.3a <- figure.3a + labs(x = "", y = expression("Neocortex BP"[ND]*" at baseline"))
figure.3a <- figure.3a + scale_fill_manual(name = "",
                                           values = c("Control" = "black", "Early non-responder" = "gray45", "Early responder" = "gray75"))
figure.3a <- figure.3a + scale_color_manual(name = "",
                                            values = c("Control" = "black", "Early non-responder" = "gray45", "Early responder" = "gray75"))
figure.3a <- figure.3a + stat_pvalue_manual(df.pvalue.3a)
figure.3a <- figure.3a + theme_bw() + theme(legend.position="none", text = element_text(size=15))

## ** figure (week 8)
df.figure.3b <- df.longiHCNRRE[,c("id","group4","neocortex")]
df.figure.3b$group <- factor(df.figure.3b$group4,
                            levels = c("HC","NR","RE"),
                            labels = c("Control","Non-responder","Remitter"))

df.pvalue.3b <- data.frame(group1 = c("Control","Control","Non-responder"),
                           group2 = c("Non-responder","Remitter","Remitter"),
                           p = ls2df.figure.3[paste0("neocortex: ",c("NR vs. HC","RE vs. HC","RE vs. NR")),"p.value"],
                           y.position = c(0.95,1,1.05),
                           stringsAsFactors = FALSE)

df.pvalue.3b$p <- paste0("p.adj=",format.pval(df.pvalue.3b$p, digits = 2, eps = 0.001))

figure.3b <- ggplot(df.figure.3b, aes(x = group, y = neocortex))
figure.3b <- figure.3b + geom_boxplot(aes(color = group), outlier.shape = NA)
figure.3b <- figure.3b + geom_dotplot(aes(fill = group), binaxis = "y", dotsize = 0.4, stackdir = "center")
figure.3b <- figure.3b + labs(x = "", y = expression("Neocortex BP"[ND]*" at baseline"))
figure.3b <- figure.3b + scale_fill_manual(name = "",
                                           values = c("Control" = "black", "Non-responder" = "gray45", "Remitter" = "gray75"))
figure.3b <- figure.3b + scale_color_manual(name = "",
                                            values = c("Control" = "black", "Non-responder" = "gray45", "Remitter" = "gray75"))
figure.3b <- figure.3b + stat_pvalue_manual(df.pvalue.3b)
figure.3b <- figure.3b + theme_bw() + theme(legend.position="none", text = element_text(size=15))
figure.3b


## ** assemble
figure.3 <- ggarrange(figure.3a + ggtitle("Week 4"), figure.3b + ggtitle("Week 8"), ncol=2, nrow=1)
figure.3
## stat.test$group1 <- factor(stat.test$group1, levels = levels(df.figure.3$group))
## stat.test$group2 <- factor(stat.test$group2, levels = levels(df.figure.3$group))

## * Export
ggsave(figure.3, filename = file.path(path.report,"./figure3.pdf"), width = 12)
ggsave(figure.3, filename = file.path(path.report,"./figure3 - modified.eps"), width = 12)
