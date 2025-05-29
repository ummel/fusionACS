library(dplyr)
library(rlang)

myfun <- function(data, ...) {

  # Capture all expressions passed to `...` as quosures
  dots <- enquos(...)

  # Separate filter and mutate expressions
  filter_exprs <- dots[names(dots) == "" | is.null(names(dots))]
  mutate_exprs <- dots[names(dots) != "" & !is.null(names(dots))]

  # Apply mutate expressions (all named expressions)
  data <- data %>%
    mutate(!!!mutate_exprs) %>%
    filter(!!!filter_exprs)

  return(data)
}

myfun(
  mtcars,
  mpg2 = mpg * 2,    # This will be used in mutate
  mpg2 > 30          # This will be used in filter
)



myfun <- function(data, a = 1, ...) {

  # Capture all expressions passed to `...` as quosures
  dots <- enquos(...)

  # Separate filter and mutate expressions
  filter_exprs <- dots[names(dots) == "" | is.null(names(dots))]
  mutate_exprs <- dots[names(dots) != "" & !is.null(names(dots))]

  # Apply mutate expressions first (all named expressions)
  #data_mutated <- data %>% mutate(!!!mutate_exprs)

  # Apply filter expressions (all unnamed expressions)
  data_filtered <- data %>% mutate(!!!mutate_exprs) %>% filter(!!!filter_exprs)

  return(data_filtered)
}

myfun(
  mtcars,
  mpg2 = mpg * 2,    # This will be used in mutate
  mpg2 > 30          # This will be used in filter
)

