# install.packages("pryr")
library(pryr)
parenvs(all = T)   # 작업환경 확인: Scopping


# Basic function in R
environment()   # 현재 작업 환경 return
globalenv()   # 함수가 실행되고 있는 환경 return
parent.env(globalenv())   # global env의 부모 환경 return


a <- 1:5; b <- 1:10; d <- c("a", "b", "c")

ls()   # 현재 env에 있는 객체 return 
ls.str()   # 객체 + 내용 return 
ls(envir = globalenv())   # global env에 있는 객체 return
ls(envir = as.environment("package:stats"))   # 특정 env에 있는 함수 return
# ls(package:stats)   # 되긴 하는데 권장하지 않음
# rm(list = ls())   # remove할 env 지정 가능


# runtime environment
add_numbers <- function() {   # global env에서 정의 
  nums <- 1:10
  sum_nums <- 0
  for (i in 1:10) {
    sum_nums <- sum_nums + nums[i]
  }
  
  runtime_env <- environment()   # 동적 env 
  parent_env <- parent.env(runtime_env)
  objects_env <- ls.str(envir = runtime_env)
  
  return (list(value = sum_nums,
               runtime_env = runtime_env,
               parent_env = parent_env,
               objects_env = objects_env))
}

add_numbers()
# runtime env := add_numbers()가 실행되는 환경 
# runtime env에서 만들어진 객체는 실행이 종료되면 삭제됨


nums <- 1:5   # global env에서 nums 변수 생성
add_numbers()   # runtime env에서의 nums 변수는 변하지 않음

add_numbers_2 <- function() {   # NOT RECOMMEND
  return(sum(nums))
}
add_numbers_2()   # global env에 있는 변수로 정상작동
# runtime env에 nums가 있는지 확인
# parent env(= global env) 에 nums가 있는지 확인 -> 작동


nums
modify_nums <- function(nums) {
  nums <- nums + 1
  return(nums)
}
modify_nums(nums)   # runtime env에서 nums 변경 =/=> global env에서 nums
nums <- modify_nums(nums)


############### 참고 ###############
# install.packages("lobstr")
library(lobstr)
obj_addr(nums)   # modify_nums() 전후 할당된 메모리 주소가 다름
####################################

obj_addr(nums)
modify_nums_3 <- function() {
  assign("nums", nums + 1, envir = globalenv())
}
modify_nums_3()
obj_addr(nums)
