soilt0t4
mat.org.t0 <- subset(soilt0t4, soilt0t4$time == "t0")
mat.org.t0 <- data.frame( treatment=mat.org.t0$treatment, org.matter.t0= mat.org.t0$org.matter)
mat.org.t4 <- subset(soilt0t4, soilt0t4$time == "t4")
mat.org.t4 <- data.frame( treatment=mat.org.t4$treatment, org.matter.t4= mat.org.t4$org.matter)
mat.org.t4
mat.org.t0
inner_join(mat.org.t0, mat.org.t4, by= "treatment")

delta.mat.org <- data.frame(treatment= mat.org.t0$treatment,t4= mat.org.t4$org.matter.t4 ,t0= mat.org.t0$org.matter.t0)
delta.mat.org$delta <- delta.mat.org$t4-delta.mat.org$t0
delta.mat.org$ratio <- delta.mat.org$t4/delta.mat.org$t0

delta.org.lm <- lm(delta~ treatment, data=delta.mat.org)#model
anova(delta.org.lm)

emm.delta.org <- emmeans(delta.org.lm, pairwise ~  treatment, adjust= "tukey")#how to add tukey
emm.delta.org$contrasts %>% rbind(adjust= "fdr") %>% 
  summary(infer = TRUE)
emm.delta.org <- cld(emmeans(delta.org.lm, specs = pairwise~treatment), Letters= letters, reversed= TRUE)
emm.delta.org$.group

delta.mat.org %>% ggplot(aes(x= treatment, y= delta, fill= treatment))+
  geom_point(pch= 21, size= 5)+
  scale_fill_manual(values= treat.colors)+
  ylab("Delta (t4-t0)")+
  geom_text(data= emm.delta.org, aes(x= treatment, y= emmean, label= `.group`), nudge_x = 0.2, nudge_y = 0.2)

soilt0t4mean <- soilt0t4 %>% group_by(treatment,  time) %>% 
  summarise(across(.cols=everything(),.fns=mean),.groups = "drop")

soilt0t4mean
plot1 <- soilt0t4mean %>% ggplot(aes(x= treatment, y= org.matter))+
  geom_col(aes(fill= time),position= "dodge")+
  geom_text(data = pval.org.mat.time, aes(x= treatment, y= emmean, label= sign))
plot1


