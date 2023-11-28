library("tidyverse")
library("testthat")

input <- readLines("input/day02.txt")

paper_calculator <- function(x){
  out <- as.numeric(str_extract_all(x, "\\d+")[[1]])
  face_areas <- setNames(c(out[1]*out[2], out[2]*out[3], out[3]*out[1]), paste0("face", seq_along(out)))
  face_perims <- setNames(c(2*out[1]+2*out[2], 2*out[2]+2*out[3], 2*out[3]+2*out[1]), paste0("face", seq_along(out)))
  total <- map_dbl(face_areas, ~2*.x) %>% sum() + unique(min(face_areas))
  
  ribbon <- min(face_perims) + reduce(out, ~.x * .y)
  
  list("paper" = total, 
       "ribbon" = ribbon)
}

expect_equal(paper_calculator("2x3x4")$paper, 58)
expect_equal(paper_calculator("1x1x10")$paper, 43)

map_dbl(input, ~paper_calculator(.x)$paper) %>% sum()
map_dbl(input, ~paper_calculator(.x)$ribbon) %>% sum()
