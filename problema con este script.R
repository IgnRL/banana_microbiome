
#crear un grafico con la media de la los valores de materia organica de t0, y la letra de HSDTukey)
avgm <- aggregate(soilt0$trans.org ~ treatment, data=soilt0, FUN= mean)
avgm
org.mat.trans

plot.org.t01 <- soilt0 %>% ggplot(aes(x= treatment, y= soilt0$trans.org))+
  geom_point()+
  stat_summary(fun=mean, aes(x= treatment, y = ..y..),
               geom= "crossbar", data=avgm)+
  geom_text(data=hsd.t0.org, aes(x=treatment, y = ..y.., label= groups), nudge_x= 0.2, nudge_y= 3)

plot.org.t01


