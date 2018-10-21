# R_figures
This is a file to generate figures using R


## Figure to create dodged dot points and SEs

```{R}
pd <- position_dodge(width = 0.3)
ggplot(data1, aes(x = Phenotype, y = rg, fill = Trait, colour = Trait)) + 
geom_point(size = 5, position=pd) + geom_errorbar(width=.3, aes(ymin = rg-1.96*SE, ymax = rg+1.96*SE), position = pd) +
theme_classic() + ylab("Genetic correlation") + geom_hline(yintercept = 0) + theme(axis.text.x=element_text(size=rel(1.5), angle=90))
```


## Figure to create power curve

```{R}
ggplot(data1,aes(x = N,y = Variance)) +
+     stat_smooth(method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=0)),se=FALSE) + theme_classic() 
```

## Figures from the PNAS paper

```{R}
library(ggplot2)
library(gridExtra)
library(grid)

grid_arrange_shared_legend <- function(...) {
plots <- list(...)
g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)
grid.arrange(
do.call(arrangeGrob, lapply(plots, function(x)
x + theme(legend.position="none"))),
legend,
ncol = 1,
heights = unit.c(unit(1, "npc") - lheight, lheight))
}

## Figure 1

data2$key = ifelse(data2$sex == 1 & data2$autism == 1 , "autism males", "other")
data2$key = ifelse(data2$sex == 2 & data2$autism == 1 , "autism females", data2$key)
data2$key = ifelse(data2$sex == 2 & data2$autism == 0 , "control females", data2$key)
data2$key = ifelse(data2$sex == 1 & data2$autism == 0 , "control males", data2$key)


a = ggplot(data2, aes(x=SPQ_full, colour=key)) +   geom_density(adjust = 3) + theme_classic(base_size = 15) + xlab("SPQ-10")

b = ggplot(data2, aes(x=SQ_full, colour=key)) +   geom_density(adjust = 3) + theme_classic(base_size = 15) + xlab("SQ-10")

c = ggplot(data2, aes(x=EQ_full, colour=key)) +   geom_density(adjust = 3) + theme_classic(base_size = 15) + xlab ("EQ-10")

d = ggplot(data2, aes(x=AQ_full, colour=key)) +   geom_density(adjust = 4) + theme_classic(base_size = 15) + xlab("AQ-10") + scale_x_continuous(breaks=c(0,2,4,6,8,10))

grid_arrange_shared_legend(d,b,c,a)


## Figure 2

ggplot(data2, aes(wheelwrightD, colour = category)) + stat_ecdf() +theme_classic(base_size = 16) + ylab ("Cumulative frequency") + xlab ("D-score")

## Supplementary Figure 1

controls = subset(data2, autism == "0")
controls = controls[!is.na(controls$STEM),]

controls$key = ifelse(controls$sex == 1 & controls$STEM == 1 , "STEM males", "other")
controls$key = ifelse(controls$sex == 2 & controls$STEM == 1 , "STEM females", controls$key)
controls$key = ifelse(controls$sex == 2 & controls$STEM == 0 , "Non-STEM females", controls$key)
controls$key = ifelse(controls$sex == 1 & controls$STEM == 0 , "Non-STEM males", controls$key)


a = ggplot(controls, aes(x=SPQ_full, colour=key)) +   geom_density(adjust = 3) + theme_classic(base_size = 15) + xlab("SPQ-10")

b = ggplot(controls, aes(x=SQ_full, colour=key)) +   geom_density(adjust = 3) + theme_classic(base_size = 15) + xlab("SQ-10")

c = ggplot(controls, aes(x=EQ_full, colour=key)) +   geom_density(adjust = 3) + theme_classic(base_size = 15) + xlab ("EQ-10")

d = ggplot(controls, aes(x=AQ_full, colour=key)) +   geom_density(adjust = 4) + theme_classic(base_size = 15) + xlab("AQ-10") + scale_x_continuous(breaks=c(0,2,4,6,8,10))

grid_arrange_shared_legend(d,b,c,a)

```
