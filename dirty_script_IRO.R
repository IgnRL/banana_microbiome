install.packages("datarium")
library(datarium)
selfsteem <- data("selfesteem", package = "datarium")
head(selfesteem, 3)


# Gather columns t1, t2 and t3 into long format
# Convert id and time into factor variables
selfesteem2 <- selfesteem %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)
head(selfesteem2, 3)

