b_spline = function(x, knots, degree, i) {   # i: 각 B-스플라인 함수가 적용될 구간의 인덱스
  # 0차 B-스플라인 기저함수의 경우 (구간별 상수 함수)
  if (degree == 0) {
    return(ifelse(knots[i] <= x & x < knots[i + 1], 1, 0))
  }
  
  # 재귀적인 경우 (차수 d > 0)
  B_i_d1 = b_spline(x, knots, degree - 1, i)
  B_i1_d1 = b_spline(x, knots, degree - 1, i + 1)
  
  denom1 = knots[i + degree] - knots[i]
  denom2 = knots[i + degree + 1] - knots[i + 1]
  
  term1 = ifelse(denom1 == 0, 0, ((x - knots[i]) / denom1) * B_i_d1)
  term2 = ifelse(denom2 == 0, 0, ((knots[i + degree + 1] - x) / denom2) * B_i1_d1)
  
  return(term1 + term2)
}

##########################
# Design matrix of the Linear reg. (Example)
x = runif(100)
y = 1 + x + rnorm(100, 0, 0.1)
G = cbind(1, x) 
beta_hat = solve(t(G) %*% G) %*% t(G) %*% y 

# tmp = b_spline(x, knots = c(0, 0.2, 0.3, 0.7, 1), degree = 2, 1)



create_design_matrix = function(x_values, knots, degree) {
  n = length(x_values)
  num_basis = length(knots) - (degree + 1)
  design_matrix = matrix(NA, nrow = n, ncol = num_basis)
  
  for (i in 1:n) {
    for (j in 1:num_basis) {
      design_matrix[i, j] = b_spline(x_values[i], knots, degree, j)
    }
  }
  
  return (design_matrix)
}

# tmp = create_design_matrix(x, knots = c(0, 0.2, 0.3, 0.7, 1), 2)


##################################
# Example. Cubic spline basis
degree = 3

## Knot sequence
tiny = 1e-5
knots = c(rep(0-tiny, degree), 0, 1, 2, 3, 4, rep(4+tiny, degree))   # plotting 할때 그래프 예쁘게 그리려고 
x_values = seq(0, 4, length.out = 200)

## Design matrix
design_matrix = create_design_matrix(x_values, knots, degree)

plot(x_values, design_matrix[, 1], type = "l")

## 한 번에 모든 design matrix 출력
matplot(x_values, design_matrix, type = "l", col = rainbow(ncol(design_matrix)), 
        xlab = "x", ylab = "B-spline values", main = "Cubic B-splines")


library(ggplot2)

coef = runif(ncol(design_matrix), -2, 2)
spline_values = design_matrix %*% coef

plot(x_values, spline_values, type = "l")

spline_data = data.frame(x = x_values, y = spline_values)
ggplot(data = spline_data, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  labs(title = "Cubic B-spline function", x = "x", y = "Spline values") +
  theme_bw()



fit_spline = function(x_values, y_values, knots, degree) {
  design_matrix = create_design_matrix(x_values, knots, degree)
  beta_hat = solve(t(design_matrix) %*% design_matrix, t(design_matrix) %*% y_values) 

  return(list(beta = beta_hat, knots = knots, degree = degree))
}

tmp = fit_spline(x, y, c(0, 0.1, 0.3, 0.7, 0.9, 1), 3)

predict_spline = function(model, new_x) {   # model = fit_spline 결과 
  G = create_design_matrix(new_x, model$knots, model$degree)
  y_pred = G %*% model$beta

  return(y_pred)
}

new_x = runif(100, 0, 4)
predict_spline(tmp, new_x)

##############################
# Example
## f(x) = sin(2 pi x) + cos(4 pi x)
set.seed(123)
n = 50
x_values = runif(n, 0, 1)

f = function(x) {
  return(sin(2*pi * x) + cos(4*pi * x))
}

y_values = f(x_values) + rnorm(n, 0, 0.2)

plot(x_values, y_values) 
# lines(x_values[order(x_values)], f(x_values)[order(x_values)])
x_values_sort = sort(x_values)
lines(x_values_sort, f(x_values_sort), col = "blue")   # line이 울퉁불퉁함 

x_grid = seq(0, 1, length.out = 200)
lines(x_grid, f(x_grid), col = "black")   # True f 


degree = 3
num_knots = 5   # interior knots
tiny = 1e-5
knots = c(rep(0-tiny, degree), seq(0, 1, length.out = num_knots), rep(1+tiny, degree))

model = fit_spline(x_values, y_values, knots, degree)

x_new = c(0.1, 0.9)
predict_spline(model, x_new)

x_grid = seq(0, 1, length.out = 200)
lines(x_grid, predict_spline(model, x_grid), col = "red")   # f_hat



####################################
plot_spline = function(x_values, y_values, model, grid_x) {
  y_pred = predict_spline(model, grid_x)
  
  data_plot = data.frame(x = x_values, y = y_values)
  spline_plot = data.frame(x = grid_x, y = y_pred)
  
  ggplot() +
    geom_point(data = data_plot, aes(x, y), color = "black") +
    geom_line(data = spline_plot, aes(x, y), color = "blue") +
    labs(title = "Fitted B-spline Regression", x = "x", y = "y") +
    theme_minimal()
}

plot_spline(x_values, y_values, model, seq(0, 1, length.out = 100))
