getwd()
styler::style_dir()

library(tidyverse)

library(statart)

install.packages("xfun")
lifeexp |>
  codebook(safewater)


codebook(seeds)
codebook(starwars, hair_color)
summ(seeds, everything())

test <- function(.data, ...) {
  expr <- rlang::expr(c(...))
  pos <- tidyselect::eval_select(expr, data = .data)
  if (length(pos) == 0) {
    pos <- seq_along(.data)
  }
  .data <- .data[pos]
  return(.data)
}

test(starwars, c(name, hair_color))

seeds %>%
  summarise(type_sum(groupid))

tabulator::tabcount(diamonds, cut)
