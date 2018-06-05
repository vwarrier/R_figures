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
