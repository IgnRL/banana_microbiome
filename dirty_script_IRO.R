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



cvi_colours = list(
  paleta.treat = c("red", "blue", "yellow3", "green4",
                  "red4", "skyblue", "gold"),
  my_favourite_colours = c("#702963", "#637029",    "#296370")
)

cvi_palettes = function(name, n, all_palettes = cvi_colours, type = c("discrete", "continuous")) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}
cvi_palettes("paleta.treat", all_palettes = cvi_colours, type= "discrete")

df = data.frame(x = c("A", "B", "C"),
                y = 1:3)
g = ggplot(data = df,
           mapping = aes(x = x, y = y)) +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1),
        legend.title = element_blank(), 
        axis.title = element_blank())
g
g + geom_col(aes(fill = x), colour = "black", size = 2) + ggtitle("Fill")
g + geom_col(aes(colour = x), fill = "white", size = 2) + ggtitle("Colour")


scale_colour_cvi_d = function(name) {
  ggplot2::scale_colour_manual(values = cvi_palettes(name,
                                                     type = "discrete"))
}

scale_fill_cvi_d = function(name) {
  ggplot2::scale_fill_manual(values = cvi_palettes(name,
                                                     type = "discrete"))
}
g +  geom_col(aes(fill = x), size = 3) +
  scale_fill_cvi_d("paleta.treat")
g
