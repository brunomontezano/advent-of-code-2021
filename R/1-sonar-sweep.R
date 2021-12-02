library(magrittr, include.only = "%>%")

sonar_sweep <- "data-raw/sonar-sweep.txt" %>%
  readr::read_table(col_names = "depth")

# Part 1 dataset
part_one <- sonar_sweep %>%
  dplyr::mutate(
    previous_depth = dplyr::lag(depth),
    increased = depth > previous_depth
  )

# Part 2 dataset
part_two <- sonar_sweep %>%
  dplyr::mutate(
    second_depth = dplyr::lead(depth, n = 1),
    third_depth = dplyr::lead(depth, n = 2),
    sum_depth = depth + second_depth + third_depth,
    previous_sum_depth = dplyr::lag(sum_depth),
    increased = sum_depth > previous_sum_depth
  )

# Function to extract answers
get_n_increased <- function(data) {
  data %>%
    dplyr::count(increased) %>%
    dplyr::filter(increased) %>%
    dplyr::pull(n)
}

# Get both answers
exercises <- list(part_one, part_two)

purrr::map(
  exercises,
  get_n_increased
) %>%
  purrr::set_names(c("Part 1 - answer", "Part 2 - answer"))
