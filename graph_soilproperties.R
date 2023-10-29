#graficos independientes de soil properties
org.mat.plotsd <- soil.summ.sd%>% ggplot(aes(x=time, group= treatment, col= treatment, y= org.matter_mean))+
  geom_line()+
  scale_color_manual(values= treat.colors)+
  geom_point()+
  geom_errorbar(aes(ymin=org.matter_mean-org.matter_sd, ymax=org.matter_mean+org.matter_sd), width=0.2)+
  labs(title = "Organic matter.", subtitle = , caption = "Mean and sd of each treatment" )
org.mat.plotsd

phosp.plotsd <- soil.summ.sd%>% ggplot(aes(x=time, group= treatment, col= treatment, y=phosphorus_mean))+
  geom_line()+
  scale_color_manual(values= treat.colors)+
  geom_point()+
  geom_errorbar(aes(ymin=phosphorus_mean-phosphorus_sd, ymax=phosphorus_mean+phosphorus_sd), width=0.2)+
  labs(title = "Phosphorus", subtitle = , caption = "Mean and sd of each treatment" )
phosp.plotsd

calcium.plotsd <- soil.summ.sd%>% ggplot(aes(x=time, group= treatment, col= treatment, y= calcium_mean))+
  geom_line()+
  scale_color_manual(values= treat.colors)+
  geom_point()+
  geom_errorbar(aes(ymin=calcium_mean-calcium_sd, ymax=calcium_mean+calcium_sd), width=0.2)+
  labs(title = "Calcium.", subtitle = , caption = "Mean and sd of each treatment" )
calcium.plotsd

sodium.plotsd <- soil.summ.sd%>% ggplot(aes(x=time, group= treatment, col= treatment, y= sodium_mean))+
  geom_line()+
  scale_color_manual(values= treat.colors)+
  geom_point()+
  geom_errorbar(aes(ymin=sodium_mean-sodium_sd, ymax=sodium_mean+sodium_sd), width=0.2)+
  labs(title = "Sodium.", subtitle = , caption = "Mean and sd of each treatment" )
sodium.plotsd

potassium.plotsd <- soil.summ.sd%>% ggplot(aes(x=time, group= treatment, col= treatment, y= potassium_mean))+
  geom_line()+
  scale_color_manual(values= treat.colors)+
  geom_point()+
  geom_errorbar(aes(ymin=potassium_mean-potassium_sd, ymax=potassium_mean+potassium_sd), width=0.2)+
  labs(title = "Potassium.", subtitle = , caption = "Mean and sd of each treatment" )
potassium.plotsd

pH_water.plotsd <- soil.summ.sd%>% ggplot(aes(x=time, group= treatment, col= treatment, y= ph.water_mean))+
  geom_line()+
  scale_color_manual(values= treat.colors)+
  geom_point()+
  geom_errorbar(aes(ymin=ph.water_mean-ph.water_sd, ymax=ph.water_mean+ph.water_sd), width=0.2)+
  labs(title = "pH water.", subtitle = , caption = "Mean and sd of each treatment" )
pH_water.plotsd

conductivity.plotsd <- soil.summ.sd%>% ggplot(aes(x=time, group= treatment, col= treatment, y= conductivity_mean))+
  geom_line()+
  scale_color_manual(values= treat.colors)+
  geom_point()+
  geom_errorbar(aes(ymin=conductivity_mean-conductivity_sd, ymax=conductivity_mean+conductivity_sd), width=0.2)+
  labs(title = "Conductivity.", subtitle = , caption = "Mean and sd of each treatment" )
conductivity.plotsd

nitrogen.plotsd <- soil.summ.sd%>% ggplot(aes(x=time, group= treatment, col= treatment, y= nitrogen_mean))+
  geom_line()+
  scale_color_manual(values= treat.colors)+
  geom_point()+
  geom_errorbar(aes(ymin=nitrogen_mean-nitrogen_sd, ymax=nitrogen_mean+nitrogen_sd), width=0.2)+
  labs(title = "Nitrogen.", subtitle = , caption = "Mean and sd of each treatment" )
nitrogen.plotsd