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

## Supplementary Figures from the Autism, childhood trauma and SSBI paper (forthcoming)

```R
a = ggplot(merged, aes(x = `1.000000`)) + geom_histogram() + theme_classic() + xlab ("Polygenic scores, P = 1")
b = ggplot(merged, aes(x = `0.750000`)) + geom_histogram() + theme_classic() + xlab ("Polygenic scores, P = 0.75")
c = ggplot(merged, aes(x = `0.500000`)) + geom_histogram() + theme_classic() + xlab ("Polygenic scores, P = 0.5")
d = ggplot(merged, aes(x = `0.250000`)) + geom_histogram() + theme_classic() + xlab ("Polygenic scores, P = 0.25")
e = ggplot(merged, aes(x = `0.100000`)) + geom_histogram() + theme_classic() + xlab ("Polygenic scores, P = 0.1")
f = ggplot(merged, aes(x = `0.010000`)) + geom_histogram() + theme_classic() + xlab ("Polygenic scores, P = 0.01")
g = ggplot(merged, aes(x = `0.001000`)) + geom_histogram() + theme_classic() + xlab ("Polygenic scores, P = 0.001")

SF1 = multiplot(a,b,c,d,e,f,g, cols = 2)

a = ggplot(merged, aes(x = childtraumasum)) + geom_histogram() + theme_classic() + xlab ("Child trauma scores")
b = ggplot(merged, aes(x = selfharmscore)) + geom_histogram() + theme_classic() + xlab ("Self-harm scores")
c = ggplot(merged, aes(x = selfharmideation)) + geom_histogram(binwidth = 5) + theme_classic() + xlab ("Self-harm ideation scores")

SF2 = multiplot(a,b,c, cols = 3)

a = ggplot(merged, aes(x = f.20489.0.0)) + geom_histogram() + theme_classic() + xlab ("Felt loved as a child (inv)")
b = ggplot(merged, aes(x = f.20490.0.0)) + geom_histogram() + theme_classic() + xlab ("Sexually molested as a child")
c = ggplot(merged, aes(x = f.20491.0.0)) + geom_histogram() + theme_classic() + xlab ("Someone to take to doctor when needed as a child (inv)")
d = ggplot(merged, aes(x = f.20488.0.0)) + geom_histogram() + theme_classic() + xlab ("Physically abused as a child")
e = ggplot(merged, aes(x = f.20487.0.0)) + geom_histogram() + theme_classic() + xlab ("Felt hated as a child")


SF3 = multiplot(a,b,c,d,e, cols = 2)


a = ggplot(merged, aes(x = f.20485.0.0)) + geom_histogram() + theme_classic() + xlab ("Ever contemplated self-harm")
b = ggplot(merged, aes(x = f.20479.0.0)) + geom_histogram() + theme_classic() + xlab ("Ever thought life not worth living")
c = ggplot(merged, aes(x = f.20480.0.0)) + geom_histogram() + theme_classic() + xlab ("Ever attempted self-harm")
d = ggplot(merged, aes(x = recentsuicideselfharm)) + geom_histogram() + theme_classic() + xlab ("Recent thoughts of suicide or self-harm")


SF4 = multiplot(a,b,c,d, cols = 2)


a = ggplot(merged, aes(x = anxietyscore)) + geom_histogram() + theme_classic() + xlab ("Anxiety symptom score")
b = ggplot(merged, aes(x = depressionscore)) + geom_histogram() + theme_classic() + xlab ("Depression symptom score")
c = ggplot(merged, aes(x = friendship)) + geom_histogram() + theme_classic() + xlab ("Friendship dissatisfaction")
d = ggplot(merged, aes(x = family)) + geom_histogram() + theme_classic() + xlab ("Family dissatisfaction")
e = ggplot(merged, aes(x = socfreq)) + geom_histogram() + theme_classic() + xlab ("Frequency of friendship/family visits")
f = ggplot(merged, aes(x = f.4537.0.0)) + geom_histogram() + theme_classic() + xlab ("Job dissatisfaction")
g = ggplot(merged, aes(x = f.20522.0.0)) + geom_histogram() + theme_classic() + xlab ("Been in a confiding relationship")

SF5 = multiplot(a,b,c,d,e,f,g, cols = 2)
```
