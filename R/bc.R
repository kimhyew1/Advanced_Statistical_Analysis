## (b)
read.csv("./data/data.csv")

library(ggplot2)
ggplot(df, aes(x = x, y = y)) + 
  geom_point(color = "black") + 
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3),
              color = "red") +
  theme_minimal()

## (c)
p = ggplot(df, aes(x = x, y = y)) + 
  geom_point(color = "black") + 
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3),
              color = "red") +
  theme_minimal()
ggsave(filename = "./plots/plot.png", plot = p,
       width = 6, height = 4, units = "in",)