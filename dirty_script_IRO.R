library(lme4)

size <- c(3,4,5,6,4,5,6,7,7,8,9,10)
pop <- c("A","A","A","A","B","B","B","B","C","C","C","C")

lm.model <- lm(size ~ pop)
summary(lm.model)
aov.model <- aov(size~pop)
summary(aov.model)

aov.lm <- aov(lm.model)
summary(lm.model)
summary(aov.model)
summary(aov.lm)


treat.pot <- paste(data$treatment, data$rep, sep= "_")
data <- data.frame(data, treat.pot)
abc.pot <- summary(aov(shannon ~ time*treatment+ Error(rep), data = data))
unique.pot <- summary(aov(shannon ~ time*treatment+ Error(treat.pot), data = data))


lm1 <- lmer(shannon ~ time*treatment + (1|rep), data = data)
summary(lm1)
aov1 <- aov(shannon ~time*treatment + Error(rep), data= data)
summary(aov1)

lm2 <- lmer(shannon ~ time*treatment + (1|treat.pot), data = data)
summary(lm2)
aov2<- aov(shannon ~time*treatment + Error(treat.pot), data= data)
summary(aov2)


