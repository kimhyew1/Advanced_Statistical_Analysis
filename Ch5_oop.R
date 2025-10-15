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


# Design matrix 내에 
# - x_values, knots, degree 이름으로 attributes 생성 
# - basis 이름으로 class 생성 
create_design_matrix = function(x_values, knots, degree) {
  n = length(x_values)
  num_basis = length(knots) - (degree + 1)
  design_matrix = matrix(NA, nrow = n, ncol = num_basis)
  
  for (i in 1:n) {
    for (j in 1:num_basis) {
      design_matrix[i, j] = b_spline(x_values[i], knots, degree, j)
    }
  }
  
  attr(design_matrix, "x_values") = x_values
  attr(design_matrix, "knots") = knots
  attr(design_matrix, "degree") = degree
  
  class(design_matrix) = "basis"
  
  return (design_matrix)
}


##################################
# Example. Cubic spline basis
degree = 3

## Knot sequence
tiny = 1e-5
knots = c(rep(0-tiny, degree), 0, 1, 2, 3, 4, rep(4+tiny, degree))   # plotting 할때 그래프 예쁘게 그리려고 
x_values = seq(0, 4, length.out = 200)

## Design matrix
design_matrix = create_design_matrix(x_values, knots, degree)
design_matrix

# Design matrix처럼 본질이 “행렬”인 경우 →
# 👉 attr() + class() 조합이 더 자연스럽습니다.
# (행렬 연산 유지 + S3 메서드 활용 가능)
# 
# 모델 전체 객체나 여러 결과물을 한꺼번에 보관할 때 →
# 👉 list 구조가 더 유용합니다.
# (예: fit = list(coeff, sigma, bic, settings))

heatmap(design_matrix)    # Compact support 를 갖는 design matrix (= 없는 부분은 죄다 0, Sparse matrix)


## 한 번에 모든 design matrix 출력
matplot(x_values, design_matrix, type = "l", col = rainbow(ncol(design_matrix)), 
        xlab = "x", ylab = "B-spline values", main = "Cubic B-splines")

## 위의 matplot을 plot을 통해 그릴 수 있도록 method 생성
plot.basis = function(basis_obj, ...) {   # ...: 가변 인자 (원래 plot 에서의 인자를 받음)
  matplot(attr(basis_obj, "x_values"), 
          basis_obj, 
          type = "l",
          col = rainbow(ncol(basis_obj)),
          xlab = "x", ylab = "B-spline values", main = "Cubic B-splines", ...)
}

plot(design_matrix, lwd = 2, lty = 1)
