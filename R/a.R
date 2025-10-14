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
