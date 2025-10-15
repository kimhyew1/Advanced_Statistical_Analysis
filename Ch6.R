num_vec = 1:5
class(num_vec)

class(num_vec) = "new class" 
class(num_vec) 

num_vec2 = structure(1:5, class = "new class")   # class 자체가 하나의 attribute로 들어가 있어서 가능
num_vec2

df = data.frame(a = 1:4, b = 5:8)
df
class(df)
attributes(df)

attributes(df)$names
names(df)   # 위의 결과와 같음 (help 함수)

print(df)
print(num_vec)
print(runif(10, 0, 1))   
# data.frame, int, numeric 각각에 대해서 적용되는 print 함수가 다름
# Method는 class 바깥에서 정의되고 있음
# => 캡슐화의 원칙은 지켜지지 않음

print   # UseMethod("print") 이 작동됨을 알 수 있음


# Generic function
methods(print)
print.data.frame   # 함수 정의 확인 가능
print.data.frame(df)


x = 1:5
class(x)   # integer
plot(x)   # plot.integer 실행 

lm_fit = lm(log(mpg) ~ log(disp), data = mtcars)
summary(lm_fit)
summary.lm(lm_fit)

plot(lm_fit)   # plot.lm 실행 


my_function = function(x) UseMethod("my_function")

my_function.default = function(x) {
  cat("일반 객체: ", x, "\n")
}

my_function.person_info = function(x) {
  cat("이름: ", x$name, "나이: ", x$age, "\n")
}

p = list(name = "Kwan-Young Bak", age = 35)
class(p)   # my_function.default 실행 

class(p) = "person_info"
my_function(p)   # my_function.person_info 실행 


