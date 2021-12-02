library(magrittr, include.only = "%>%")

# Read data
dive <- "data-raw/dive.txt" %>%
  readr::read_table(col_names = c("command", "units"))

# PART 1

# Forward increases the horizontal position by X units
# Down INCREASES the depth by X units
# Up DECREASES the depth by X units

# First, I'm going to check if there's some forward negative value.
dive %>%
  dplyr::filter(command == "forward", units < 0)

# Looks like there is not.

# I'm going to transform the up values into negative, because they decrease our depth.
# Then, sum the units grouped by command, calculate depth and multiply horizontal
# position and depth values to get the answer.
dive %>%
  dplyr::mutate(
    units = ifelse(command == "up", -units, units)
  ) %>%
  dplyr::group_by(command) %>%
  dplyr::summarise(sum = sum(units)) %>%
  tidyr::pivot_wider(names_from = command, values_from = sum) %>%
  dplyr::transmute(
    forward,
    depth = down + up,
    final_position = forward * depth) %>%
  dplyr::pull(final_position)

# PART 2

# In addition to horizontal position and depth,
# you'll also need to track a third value, AIM,
# which also starts at 0.
# The commands also mean something entirely different than you first thought:
#
#    - Down X INCREASES your aim by X units.
#    - Up X DECREASES your aim by X units.
#    - Forward X does two things:
#        - It increases your horizontal position by X units.
#        - It increases your depth by your aim multiplied by X.

# First, create a mutate pipeline (creating variables and calculating at the same time).
# The cumsum function is key, it calculates the cumulative sum of the elements in a vector.
# Finally, slice the last row (the one that's gonna contain the final position).
# Pull function extracts the value of final position as R vector.
dive %>%
  dplyr::mutate(depth = 0,
    aim = ifelse(command == "down", units, 0),
    aim = ifelse(command == "up", -units, aim),
    horizontal = ifelse(command == "forward", units, 0),
    horizontal = cumsum(horizontal),
    aim = cumsum(aim),
    depth = ifelse(command == "forward", aim * units, 0),
    depth = cumsum(depth),
    final_position = horizontal * depth
  ) %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::pull(final_position)
