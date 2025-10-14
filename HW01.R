# Problem 01
## (a)
set.seed(1)
n = 200
x = seq(0, 1, length.out = n)
y = sin(2*pi*x) + rnorm(n, sd = 0.15)

# dir.create("./data")
# dir.create("./R")
# dir.create("./plots")

df <- data.frame(x = x, y = y)
write.csv(df, "./data/data.csv", row.names = F)

## (b)
read.csv("../data/data.csv")

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
ggsave(filename = "./plot/plot.png", plot = p)