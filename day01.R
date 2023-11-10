input <- readLines("inputs/day01.txt")


floor_counter <- function(x){
  x <- strsplit(x, "")[[1]]
  x[x=="("] <- 1
  x[x==")"] <- -1
  sum(as.numeric(x))
  
}


testthat::expect_equal(floor_counter("(())"), 0)
testthat::expect_equal(floor_counter("()()"), 0)
testthat::expect_equal(floor_counter("((("), 3)
testthat::expect_equal(floor_counter("(()(()("), 3)
testthat::expect_equal(floor_counter("))((((("), 3)
testthat::expect_equal(floor_counter("())"), -1)
testthat::expect_equal(floor_counter("))("), -1)
testthat::expect_equal(floor_counter(")))"), -3)
testthat::expect_equal(floor_counter(")())())"), -3)


floor_counter(input)

basement_counter <- function(x){
  x <- strsplit(x, "")[[1]]
  x[x=="("] <- 1
  x[x==")"] <- -1
  x <- as.numeric(x)
  total <- 0
  i <- 1
  while(total > -1){
    total <- total + x[i]
    i <- i + 1
  }
  return(i - 1)
  
}


testthat::expect_equal(basement_counter(")"), 1)
testthat::expect_equal(basement_counter("()())"), 5)

basement_counter(input)
