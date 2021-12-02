library(magrittr, include.only = "%>%")

# Read data
sonar_sweep <- "data-raw/sonar-sweep.txt" %>%
  readr::read_table(col_names = "depth")

# PART 1

# Exercise in summary:
# Count the number of times a depth measurement increases from the previous measurement.

# Basically, count number of times the expression x_i > x_{i-1} is true.

# The lag function from dplyr is pretty helpful because it finds the previous value in
# a vector (in this case, the variable depth), making it possible to create a new vector
# and just compare both to get a logical vector.
part_one <- sonar_sweep %>%
  dplyr::mutate(
    previous_depth = dplyr::lag(depth),
    increased = depth > previous_depth
  )

# PART 2

# The part two made it a little harder because now we need to compare the sum of depths
# in groups of three measures.

#It's possible with the lead function that finds the next
# value in vector. Then, we can sum each three-measure values and create a logical vector
# with TRUE and FALSES.
part_two <- sonar_sweep %>%
  dplyr::mutate(
    second_depth = dplyr::lead(depth, n = 1),
    third_depth = dplyr::lead(depth, n = 2),
    sum_depth = depth + second_depth + third_depth,
    previous_sum_depth = dplyr::lag(sum_depth),
    increased = sum_depth > previous_sum_depth
  )

# Function to extract answers
# This function was created to extract the sum of the TRUE values in the increased vector.
# It counts increased TRUE values.
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
