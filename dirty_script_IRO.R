
treat.pot <- paste(data$treatment, data$rep, sep= "_")
data <- data.frame(data, treat.pot)


lm1 <- lmer(shannon ~ time*treatment + (1|treat.pot), data = data)
summary(lm1)
aov1 <- aov(shannon ~time*treatment + Error(treat.pot), data= data)
summary(aov1)
aov.lm1 <- aov(lm1)

lm2 <- lmer(shannon ~ time*treatment + (1|treat.pot), data = data)
summary(lm2)

car::Anova(lm2)
aov2<- aov(shannon ~time*treatment + Error(treat.pot), data= data)
summary(aov2)





## emmeans tutoral

dat = structure(list(f1 = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("a","c"), class = "factor"), 
                     f2 = structure(c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L), .Label = c("1", "c"), class = "factor"), 
                     resp = c(1.6, 0.3, 3, 0.1, 3.2, 0.2,  0.4, 0.4, 2.8, 0.7, 3.8, 3, 0.3, 14.3, 1.2, 0.5, 1.1, 4.4, 0.4, 8.4)), row.names = c(NA, -20L), class = "data.frame")
str(dat)

anova.simple1 <- Anova(aov(resp ~ f1*f2, data= dat))
anova.simple1
anova.simple2 <- Anova(aov(resp ~ f1, data= dat))
anova.simple2
anova.simple3 <- Anova(aov(resp ~ f1+f2, data= dat))
anova.simple3

aov.simple1 <- aov(resp ~ f1*f2, data= dat)
aov.simple1
aov.simple2 <- aov(resp ~ f1, data= dat)
aov.simple2

fit1 = lmer(shannon ~ time+treatment +time*treatment+(1|treat.pot) , data = data)
summary(fit1)
coef(summary(fit1))
str(data)
Anova.2 <- car::Anova(fit1, type= "III")

print(car::Anova(fit1, type= "III"))
anova.3 <- anova(fit1)
as.data.frame(anova(fit1))
summary(aov(shannon ~time*treatment + Error(treat.pot), data= data))

Anova.alt <- Anova(lm(resp ~f1 + f2 + f1:f2, data= dat) ,type= "III")
