ibrary(ggplot2)
library(gridExtra)
library(tidyr)

actualIris <- data.frame(Sepal.Length=6.1, Sepal.Width=3.1, Petal.Length=5.0, Petal.Width=1.7)


labels <- c(paste('Petal Length\none-tailed test=', round(sum(actualIris$Sepal.Length < iris$Sepal.Length)/nrow(iris), 2)),
            paste('Petal Width\none-tailed test=', round(sum(actualIris$Sepal.Width < iris$Sepal.Width)/nrow(iris), 2)),
            paste('Sepal Length\none-tailed test=', round(sum(actualIris$Petal.Length < iris$Petal.Length)/nrow(iris), 2)),
            paste('Sepal Width\none-tailed test=', round(sum(actualIris$Petal.Width < iris$Petal.Width)/nrow(iris), 2)))

tmp <- iris[,-5] %>% gather(Type, value)
actuals <- data.frame(col1=colnames(actualIris), col2=as.numeric(actualIris[1,]))
tmp$Actual <- actuals$col2[match(tmp$Type, actuals$col1)]
tmp$Type <- factor(tmp$Type, levels = c('Petal.Length', 'Petal.Width', 'Sepal.Length', 'Sepal.Width'), 
                   labels = labels)
ggplot(tmp, aes(value)) + facet_wrap(~Type, scales="free", nrow = 1) + geom_histogram() + 
  geom_vline(aes(xintercept=Actual), colour="blue", lwd=2) 
