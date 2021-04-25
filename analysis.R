library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(maps)
library(scales)

# County level incarceration data
incarceration_df <- read.csv('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')

# Evaluate column names
features <- colnames(incarceration_df)

# Evaluate first few rows of data set
# View(head(incarceration_df))

# Number of observations found in the data set
num_observations <- nrow(incarceration_df)

# Time range of the data set (in years)
time_range <- range(incarceration_df$year)
time_range <- paste(time_range[1], "-", time_range[2], sep = "")

# Most recent year of data for jail and prison population counts according 
# to the data sets documentation
recent_jail_data_year <- 2018
recent_prison_data_year <- 2016
recent_merged_year <- 2016

# Total jail population in most recent year (avg held per day in jail)
recent_jail_pop <- incarceration_df %>% 
  group_by(year) %>% 
  summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 
  filter(year == recent_jail_data_year) %>% 
  pull(total_jail_pop)

# Total prison population in most recent year
recent_prison_pop <- incarceration_df %>% 
  group_by(year) %>% 
  summarize(total_prison_pop = sum(total_prison_pop, na.rm = TRUE)) %>% 
  filter(year == recent_prison_data_year) %>% 
  pull(total_prison_pop)

# Total population of jail and prison combined (using data from the year 2016 
# since that is the most recent prison data)
jail_pop_2016 <- incarceration_df %>% 
  group_by(year) %>% 
  summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 
  filter(year == 2016) %>% 
  pull(total_jail_pop)

# Extra conversions for rmd formatting
total_jail_prison_pop_2016 <- as.integer(jail_pop_2016 + recent_prison_pop) 
total_jail_prison_pop_2016 <- paste(total_jail_prison_pop_2016)

# Add total incarceration columns to the incarceration_df
incarceration_df <- incarceration_df %>% 
  replace_na(list(
    total_jail_pop = 0, total_prison_pop = 0,
    white_jail_pop = 0, white_prison_pop = 0,
    black_jail_pop = 0, black_prison_pop = 0,
    latinx_jail_pop = 0, latinx_prison_pop = 0,
    aapi_jail_pop = 0, aapi_prison_pop = 0,
    native_jail_pop = 0, native_prison_pop = 0
  )) %>% 
  mutate(
    total_joint_pop = total_jail_pop + total_prison_pop,
    white_joint_pop = white_jail_pop + white_prison_pop,
    black_joint_pop = black_jail_pop + black_prison_pop,
    latinx_joint_pop = latinx_jail_pop + latinx_prison_pop,
    aapi_joint_pop = aapi_jail_pop + aapi_prison_pop,
    native_joint_pop = native_jail_pop + native_prison_pop,
  ) 

# Time series of total incarceration populations for each race
incarceration_time_series_df <- incarceration_df %>% 
  group_by(year) %>% 
  summarize(across(total_joint_pop:native_joint_pop, sum)) %>% 
  pivot_longer(total_joint_pop:native_joint_pop, names_to = 'race') %>% 
  mutate(race = factor(race, levels = unique(race)))

# Plot the time series
incarceration_over_time_plot <- ggplot(incarceration_time_series_df) +
  geom_point(mapping = aes(x = year, y = value, color = race)) +
  geom_line(mapping = aes(x = year, y = value, color = race)) +
  xlim(1980, 2016) +
  scale_color_brewer(palette = "Set2", labels = c('aapi_joint_pop' = 'AAPI', 'black_joint_pop' = 'Black',
                                            'latinx_joint_pop' = 'Latinx', 'native_joint_pop' = 'Native',
                                            'total_joint_pop' = 'Total', 'white_joint_pop' = 'White')) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title = 'Incarcerated Population Over Time',
    x = 'Year',
    y = 'Population (# of people)',
    color = 'Race'
  )

# Wrangle data to find total incarceration rates and black incarceration rates in 2016
top_10_black_incarceration_states_df <- incarceration_df %>%
  filter(year == recent_prison_data_year) %>% 
  group_by(state) %>% 
  summarize(
    black_inc_rate = sum(black_joint_pop) / sum(black_pop_15to64, na.rm = TRUE),
    total_inc_rate = sum(total_joint_pop) / sum(total_pop_15to64, na.rm = TRUE)
  ) %>% 
  slice_max(black_inc_rate, n = 10) %>%
  arrange(black_inc_rate) %>%
  mutate(state = factor(state, state)) %>%
  pivot_longer(black_inc_rate:total_inc_rate, names_to = "incarceration_pop_type") %>%
  mutate(incarceration_pop_type = factor(incarceration_pop_type, levels = c('total_inc_rate', 'black_inc_rate')))

# Create visualization for incarceration rates
top_10_black_incarceration_plot <- ggplot(top_10_black_incarceration_states_df) +
  geom_col(mapping = aes(x = state,y = value, fill = incarceration_pop_type),
    position = position_dodge2(padding = 0.1)
  ) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) + # import scales package to convert y values to %
  scale_fill_manual(values = c('Red', 'Black'), labels = c('total_inc_rate' = 'Total', 'black_inc_rate' = 'Black')
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title = 'States with Highest Rate of Black Incarceration',
    x = 'State',
    y = 'Percent Incarcerated',
    fill = ''
  )

# Wrangle map and fips data for incarceration ratio maps of WA
wa_county_map_data <- map_data('county') %>% 
  filter(region == 'washington')

wa_county_fips_data <- county.fips %>% 
  filter(str_starts(polyname, 'washington')) %>% 
  mutate(subregion = str_remove_all(polyname, 'washington,')) %>% 
  select(fips, subregion)

# Add in two additional rows for the pierce and san juan fips data
wa_county_fips_data[nrow(wa_county_fips_data) + 1] = list(53053, 'pierce')
wa_county_fips_data[nrow(wa_county_fips_data) + 1] = list(53055, 'san juan')

# Wrangle incarceration data for black:white ratio map of WA
wa_county_black_white_ratio_df <- incarceration_df %>% 
  filter(
    year == recent_prison_data_year,
    state == 'WA'
  ) %>% 
  group_by(county_name) %>% 
  summarize(
    black_inc_rate = sum(black_joint_pop) / sum(black_pop_15to64, na.rm = TRUE),
    white_inc_rate = sum(white_joint_pop) / sum(white_pop_15to64, na.rm = TRUE),
    fips = fips) %>% 
  mutate(black_to_white_ratio = black_inc_rate / white_inc_rate) %>% 
  select(black_to_white_ratio, fips)

# Join the black:white ratio_df with the map data for plotting
wa_county_black_white_ratio_df <- wa_county_map_data %>% 
  left_join(wa_county_fips_data, by = 'subregion') %>% 
  left_join(wa_county_black_white_ratio_df, by = 'fips')

# Write caption for the black:white ratio map
caption1 <- str_wrap('Displays the ratio of the black incarceration rate to the white 
             incarceration rate. A ratio of 2.0 means that black people are 
             twice as likely to be incarcerated as white people.')

# Create visualization for the WA county black:white incarceration rates
black_white_ratio_map_plot <- ggplot(wa_county_black_white_ratio_df) +
  geom_polygon(mapping = aes(
    x = long,
    y = lat,
    group = group,
    fill = black_to_white_ratio
  )) +
  coord_quickmap() +
  scale_fill_distiller(
    palette = 'BuPu', 
    direction = 1,
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(
    title = 'Discrepancies Between Black & White Incarceration Rates in WA',
    fill = 'Ratio of Black:White',
    caption = caption1
  )

# Wrangle incarceration data for latinx:white ratio map of WA
wa_county_latinx_white_ratio_df <- incarceration_df %>% 
  filter(
    year == recent_prison_data_year,
    state == 'WA'
  ) %>% 
  group_by(county_name) %>% 
  summarize(
    latinx_inc_rate = sum(latinx_joint_pop) / sum(latinx_pop_15to64, na.rm = TRUE),
    white_inc_rate = sum(white_joint_pop) / sum(white_pop_15to64, na.rm = TRUE),
    fips = fips) %>% 
  mutate(latinx_to_white_ratio = latinx_inc_rate / white_inc_rate) %>% 
  select(latinx_to_white_ratio, fips)

# Join the latinx:white ratio_df with the map data for plotting
wa_county_latinx_white_ratio_df <- wa_county_map_data %>% 
  left_join(wa_county_fips_data, by = 'subregion') %>% 
  left_join(wa_county_latinx_white_ratio_df, by = 'fips')

# Write caption for latinx:white ratio map
caption2 <- str_wrap('Displays the ratio of the latinx incarceration rate to the white 
             incarceration rate. A ratio of 2.0 means that latinx people are 
             twice as likely to be incarcerated as white people.')

# Create visualization for the WA county latinx:white incarceration rates
latinx_white_ratio_map_plot <- ggplot(wa_county_latinx_white_ratio_df) +
  geom_polygon(mapping = aes(
    x = long,
    y = lat,
    group = group,
    fill = latinx_to_white_ratio
  )) +
  coord_quickmap() +
  scale_fill_distiller(
    palette = 'OrRd', 
    direction = 1,
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(
    title = 'Discrepancies Between Latinx & White Incarceration Rates in WA',
    fill = 'Ratio of Latinx:White',
    caption = caption2
  )

# Wrangle incarceration data for aapi:white ratio map of WA
wa_county_aapi_white_ratio_df <- incarceration_df %>% 
  filter(
    year == recent_prison_data_year,
    state == 'WA'
  ) %>% 
  group_by(county_name) %>% 
  summarize(
    aapi_inc_rate = sum(aapi_joint_pop) / sum(aapi_pop_15to64, na.rm = TRUE),
    white_inc_rate = sum(white_joint_pop) / sum(white_pop_15to64, na.rm = TRUE),
    fips = fips) %>% 
  mutate(aapi_to_white_ratio = aapi_inc_rate / white_inc_rate) %>% 
  select(aapi_to_white_ratio, fips)

# Join the aapi:white ratio_df with the map data for plotting
wa_county_aapi_white_ratio_df <- wa_county_map_data %>% 
  left_join(wa_county_fips_data, by = 'subregion') %>% 
  left_join(wa_county_aapi_white_ratio_df, by = 'fips')

# Write the caption for the aapi:white ratio map
caption3 <- str_wrap('Displays the ratio of the aapi incarceration rate to the white 
             incarceration rate. A ratio of 2.0 means that aapi people are 
             twice as likely to be incarcerated as white people.')

# Create visualization for the WA county aapi:white incarceration rates
aapi_white_ratio_map_plot <- ggplot(wa_county_aapi_white_ratio_df) +
  geom_polygon(mapping = aes(
    x = long,
    y = lat,
    group = group,
    fill = aapi_to_white_ratio
  )) +
  coord_quickmap() +
  scale_fill_distiller(
    palette = 'RdPu', 
    direction = 1,
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(
    title = 'Discrepancies Between AAPI & White Incarceration Rates in WA',
    fill = 'Ratio of AAPI:White',
    caption = caption3
  )

# Investigate outlier
adams_wa_2016_df <- incarceration_df %>% 
  filter(
    year == 2016,
    state == 'WA',
    county_name == 'Adams County'
  ) # aapi_pop is 92 with 2.7 jail pop

# Wrangle incarceration data for native:white ratio map of WA
wa_county_native_white_ratio_df <- incarceration_df %>% 
  filter(
    year == recent_prison_data_year,
    state == 'WA'
  ) %>% 
  group_by(county_name) %>% 
  summarize(
    native_inc_rate = sum(native_joint_pop) / sum(native_pop_15to64, na.rm = TRUE),
    white_inc_rate = sum(white_joint_pop) / sum(white_pop_15to64, na.rm = TRUE),
    fips = fips) %>% 
  mutate(native_to_white_ratio = native_inc_rate / white_inc_rate) %>% 
  select(native_to_white_ratio, fips)

# Join the native:white ratio_df with the map data for plotting
wa_county_native_white_ratio_df <- wa_county_map_data %>% 
  left_join(wa_county_fips_data, by = 'subregion') %>% 
  left_join(wa_county_native_white_ratio_df, by = 'fips')

# Create caption for the native:white ratio map
caption4 <- str_wrap('Displays the ratio of the native incarceration rate to the white 
             incarceration rate. A ratio of 2.0 means that native people are 
             twice as likely to be incarcerated as white people.')

# Create visualization for the WA county native:white incarceration rates
wa_county_native_white_ratio_df <- ggplot(wa_county_native_white_ratio_df) +
  geom_polygon(mapping = aes(
    x = long,
    y = lat,
    group = group,
    fill = native_to_white_ratio
  )) +
  coord_quickmap() +
  scale_fill_distiller(
    palette = 'BuGn', 
    direction = 1
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(
    title = 'Discrepancies Between Native & White Incarceration Rates in WA',
    fill = 'Ratio of Native:White',
    caption = caption4
  )

# Investigate outlier
garfield_wa_2016_df <- incarceration_df %>% 
  filter(
    year == 2016,
    state == 'WA',
    county_name == 'Garfield County'
  ) # There are only 2 people listed in native pop 15to64 and 2 are in jail!!!

# Wrangle incarceration data for native:white ratio map of WA excluding the outlier
wa_county_native_white_ratio_no_outlier_df <- incarceration_df %>% 
  filter(
    year == recent_prison_data_year,
    state == 'WA',
    county_name != 'Garfield County'
  ) %>% 
  group_by(county_name) %>% 
  summarize(
    native_inc_rate = sum(native_joint_pop) / sum(native_pop_15to64, na.rm = TRUE),
    white_inc_rate = sum(white_joint_pop) / sum(white_pop_15to64, na.rm = TRUE),
    fips = fips) %>% 
  mutate(native_to_white_ratio = native_inc_rate / white_inc_rate) %>% 
  select(native_to_white_ratio, fips)

# Join the native:white ratio_df with the map data for plotting
wa_county_native_white_ratio_no_outlier_df <- wa_county_map_data %>% 
  left_join(wa_county_fips_data, by = 'subregion') %>% 
  left_join(wa_county_native_white_ratio_no_outlier_df, by = 'fips')

# Create visualization for the WA county native:white incarceration rates 
# excluding the outlier
native_white_ratio_map_no_outlier_plot <- ggplot(wa_county_native_white_ratio_no_outlier_df) +
  geom_polygon(mapping = aes(
    x = long,
    y = lat,
    group = group,
    fill = native_to_white_ratio
  )) +
  coord_quickmap() +
  scale_fill_distiller(
    palette = 'BuGn', 
    direction = 1,
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    title = 'Discrepancies Between Native & White Incarceration Rates in WA',
    subtitle = 'Excluding Garfield County',
    fill = 'Ratio of Native:White',
    caption = caption4
  )

# Wrangle data to analyze the growth rate of the incarceration of women compared
# to men in different urbanicities
fem_incarceration_by_race_df <- incarceration_df %>% 
  replace_na(list(
    black_female_prison_pop = 0, latinx_female_prison_pop = 0,
    native_female_prison_pop = 0, aapi_female_prison_pop = 0,
    other_race_female_prison_pop = 0, white_female_prison_pop = 0
  )) %>%
  select(year, urbanicity, black_female_prison_pop, latinx_female_prison_pop,
         native_female_prison_pop, aapi_female_prison_pop, other_race_female_prison_pop,
         white_female_prison_pop) %>% 
  pivot_longer(black_female_prison_pop:white_female_prison_pop, names_to = 'race') %>% 
  group_by(year, race, urbanicity) %>% 
  summarize(value = sum(value)) %>% 
  filter(urbanicity != "") %>% 
  mutate(race = factor(race, levels = c('white_female_prison_pop', 'black_female_prison_pop', 'latinx_female_prison_pop',
                             'native_female_prison_pop', 'aapi_female_prison_pop', 'other_race_female_prison_pop')))

# Generate plot for female incarceration trends across urbanicity/race
urbanicity.labs <- c('Rural', 'Small/Mid', 'Suburban', 'Urban')
names(urbanicity.labs) <- c('rural', 'small/mid', 'suburban', 'urban')

fem_incarceration_plot <- ggplot(fem_incarceration_by_race_df) +
  geom_smooth(mapping = aes(x = year, y = value, color = race), se = FALSE, method = 'loess', formula = 'y ~ x') +
  xlim(1980, 2016) +
  theme_minimal() +
  facet_wrap(~urbanicity, labeller = labeller(urbanicity = urbanicity.labs)
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title = 'Female Incarcerated Populations Across Urbanicity',
    x = 'Year',
    y = 'Population (# of people)',
    color = 'Race'
  ) +
  scale_color_brewer(palette = "Set2", labels = c('aapi_female_prison_pop' = 'AAPI', 'black_female_prison_pop' = 'Black',
                                            'latinx_female_prison_pop' = 'Latinx', 'native_female_prison_pop' = 'Native',
                                            'other_race_female_prison_pop' = 'Other', 'white_female_prison_pop' = 'White'))

# Create a data description list variable for use in the .Rmd file
data_description <- list(
  column_names = features,
  num_observations = num_observations,
  year_range = time_range,
  recent_jail_data_year = recent_jail_data_year,
  recent_prison_data_year = recent_prison_data_year,
  recent_total_year = recent_merged_year,
  recent_jail_pop = recent_jail_pop,
  recent_prison_pop = recent_prison_pop,
  recent_jail_prison_pop = total_jail_prison_pop_2016
)