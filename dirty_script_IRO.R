
install.packages("lmerTest")
library(lmerTest)
model4 <- lmer(shannon ~ time*treatment+(1|treat.pot) , data = data)
anova4 <- anova(model4)


