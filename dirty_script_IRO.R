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



banana.paleta = list(
  paleta.treat = c("red", "blue", "yellow3", "green4",
                  "red4", "skyblue", "gold"),
  my_favourite_colours = c("#702963", "#637029",    "#296370")
)

banana.paleta.f = function(name, n, all_palettes = banana.paleta, type = c("discrete", "continuous")) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n] )
  structure(out, name = name, class = "palette")
}
banana.paleta.f("paleta.treat", all_palettes = banana.paleta, type= "discrete")

scale_colour_banana_disc = function(name) {
  ggplot2::scale_colour_manual(values = banana.paleta.f(name, type = "discrete"))
}

scale_fill_banana_disc = function(name) {
  ggplot2::scale_fill_manual(values = banana.paleta.f(name,
                                                     type = "discrete"))
}

