library("tidyverse")
library("testthat")

input <- readLines("input/day03.txt")

#How many houses receive at least one present?

present_delivery <- function(x){ 
  mover <- function(pos, dir){
    case_when(dir == "<" ~ c(pos[1] - 1, pos[2]), 
              dir == ">" ~ c(pos[1] + 1, pos[2]), 
              dir == "^" ~ c(pos[1], pos[2] + 1), 
              dir == "v" ~ c(pos[1], pos[2] - 1))
  }
  
  if(length(x) == 1){
    moves <- strsplit(x, "")[[1]] 
  } else {
    moves <- x
  }
  
  pos <- c(0, 0)
  tracker <- list(pos)
  for(move in moves){
    pos <- mover(pos, move)
    tracker <- c(tracker, list(pos))
    
  }
  
  return(tracker)
}

expect_equal(present_delivery(">") %>% unique() %>% length(), 2)
expect_equal(present_delivery("^>v<") %>% unique() %>% length(), 4)
expect_equal(present_delivery("^v^v^v^v^v") %>% unique() %>% length(), 2)

present_delivery(input) %>% unique() %>% length()

robo_present_delivery <- function(x){
  moves <- strsplit(x, "")[[1]]
  santa_moves <- moves[seq(1, length(moves), by = 2)] %>% present_delivery()
  robot_moves <- moves[seq(2, length(moves), by = 2)] %>% present_delivery()
  
  c(santa_moves, robot_moves) %>% 
    unique() %>% 
    length()
}


expect_equal(robo_present_delivery("^v"), 3)
expect_equal(robo_present_delivery("^>v<"), 3)
expect_equal(robo_present_delivery("^v^v^v^v^v"), 11)

robo_present_delivery(input) 
