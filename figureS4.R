library(ggpubr)
library(data.table)
library(ggplot2)
library(qqtest)

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
source(file.path(path.code,"data-management-rescan.R"))

## * Compute slopes
all.regions <- c("neocortex", "hippocampus", "caudate", "putamen", "neostriatum", "limbic")
n.regions <- length(all.regions)

df_baseline.rescan <- dfL.rescan[dfL.rescan$variable %in% all.regions & dfL.rescan$timepoint=="baseline",]
df_rescan.rescan <- dfL.rescan[dfL.rescan$variable %in% all.regions & dfL.rescan$timepoint=="rescan",]
## identical(df_baseline.rescan$id,df_rescan.rescan$id)
## identical(df_baseline.rescan$region,df_rescan.rescan$region)
dfL2.rescan <- cbind(df_baseline.rescan[,c("id","escit","response.w8","change.hamd6.w8")],
                     region = df_baseline.rescan$variable,
                     baseline = df_baseline.rescan$value,
                     rescan = df_rescan.rescan$value)
dfL2.rescan$id <- as.character(dfL2.rescan$id)
dfL2.rescan <- dfL2.rescan[order(dfL2.rescan$id),]

## * Visualization
gg.biv <- ggplot(dfL2.rescan, aes(x=baseline, y = rescan, color = escit))
gg.biv <- gg.biv + geom_point() + geom_smooth(method = "lm", formula=y~x-1)
gg.biv <- gg.biv + facet_wrap(~id)
gg.biv <- gg.biv + geom_abline(intercept = 0, slope = 1, color = "gray")
gg.biv


## * Extract slopes slopes
all.id <- unique(dfL2.rescan$id)
n.id <- length(all.id)
keep.regions <- c("neocortex","hippocampus","neostriatum")
lsId.lm <- setNames(vector(mode = "list",length = n.id), all.id)
for(iId in 1:n.id){
    lsId.lm[[iId]] <- lm(rescan ~ 0 + baseline, data = dfL2.rescan[dfL2.rescan$id == all.id[iId] & dfL2.rescan$region %in% keep.regions, ])
}
df.slope <- data.frame(id = names(lsId.lm), slope = unname(sapply(lsId.lm,coef)))

     
## cbind(df.slope,dt.slope[order(dt.slope$id),])
## (df.slope$baseline-dt.slope[order(dt.slope$id),"slope"])
## * test slopes vs 1
## wilcox.test(df.slope$slope-1)
t.test(df.slope$slope-1)
## 	One Sample t-test

## data:  df.slope$slope - 1
## t = -4.1257, df = 39, p-value = 0.0001877
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  -0.10899222 -0.03728012
## sample estimates:
##   mean of x 
## -0.07313617 

## * figure S4
set.seed(10)
df.slope$group <- df.slope$id == df.slope$id[1]
df.slope$x <- 1
figure.S4a <- ggplot(df.slope, aes(x = x, y = slope)) 
figure.S4a <- figure.S4a + geom_boxplot()
figure.S4a <- figure.S4a + geom_jitter(aes(color = group, shape = group), height = 0, width = 0, size = 4)
figure.S4a <- figure.S4a + scale_color_manual(name = "",
                                              values = c("FALSE" = "black", "TRUE" = "gray40"))
figure.S4a <- figure.S4a + scale_shape_manual(name = "",
                                              values = c("FALSE" = 20, "TRUE" = 18))
figure.S4a <- figure.S4a + geom_hline(yintercept = 1, linetype="dashed", color = "black")
figure.S4a <- figure.S4a + theme_bw() + theme(text = element_text(size=20),
                                              legend.position="none",
                                              axis.title.x=element_blank(),
                                              axis.text.x=element_blank(),
                                              axis.ticks.x=element_blank()
                                              )
figure.S4a <- figure.S4a + coord_cartesian(xlim = c(0.25,1.75))
figure.S4a <- figure.S4a + ggtitle("B")

df.example <- dfL2.rescan[dfL2.rescan$id == as.character(df.slope[df.slope$group,"id"]) & dfL2.rescan$region %in% keep.regions,]
figure.S4b <- ggplot()
figure.S4b <- figure.S4b + geom_abline(slope = df.slope[df.slope$group,"slope"], intercept = 0, linetype="solid", color = "black", size = 1.25)
figure.S4b <- figure.S4b + geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 1.25)
figure.S4b <- figure.S4b + geom_smooth(data = df.example, mapping = aes(x = baseline, y = rescan), method = "lm", formula=y~x-1, color = "black")
figure.S4b <- figure.S4b + geom_point(data = df.example, mapping = aes(x = baseline, y = rescan, shape=region), size = 3)
figure.S4b <- figure.S4b + theme_bw() + theme(text = element_text(size=20),
                                              legend.position="bottom")
figure.S4b <- figure.S4b + xlab(expression(BP[ND]~at~baseline))
figure.S4b <- figure.S4b + ylab(expression(BP[ND]~at~week~8))
figure.S4b <- figure.S4b + coord_cartesian(x = c(0, max(df.example$baseline)*1.1), y = c(0, max(df.example$rescan)*1.2))
figure.S4b <- figure.S4b + ggtitle("A")
                                              
## assemble 
figure.S4 <- ggarrange(figure.S4b, figure.S4a, ncol=2, nrow=1, common.legend = FALSE)

## * Export
ggsave(figure.S4, filename = file.path(path.report,"./figureS4.pdf"), width = 14)

