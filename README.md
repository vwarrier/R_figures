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


data2$category = ifelse(data2$sex == 1 & data2$autism == 1 , "autism_males", "other")
data2$category = ifelse(data2$sex == 2 & data2$autism == 1 , "autism_females", data2$category)
data2$category = ifelse(data2$sex == 2 & data2$autism == 0 , "control_females", data2$category)
data2$category = ifelse(data2$sex == 1 & data2$autism == 0 , "control_males", data2$category)


a = ggplot(data2, aes(x=SPQ_full, colour=category)) +   geom_density(adjust = 3) + theme_classic(base_size = 15)

b = ggplot(data2, aes(x=SQ_full, colour=category)) +   geom_density(adjust = 3) + theme_classic(base_size = 15)

c = ggplot(data2, aes(x=EQ_full, colour=category)) +   geom_density(adjust = 3) + theme_classic(base_size = 15)

d = ggplot(data2, aes(x=AQ_full, colour=category)) +   geom_density(adjust = 4) + theme_classic(base_size = 15)


grid_arrange_shared_legend(d,b,c,a)


ggplot(data2, aes(wheelwrightD, colour = category)) + stat_ecdf() +theme_classic(base_size = 16) + ylab ("Cumulative frequency") + xlab ("D-score")
```
