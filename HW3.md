p8105_hw3_NL2836
================
Ngan Le
2023-10-12

Load the necessary libraries and datasets.

``` r
library(tidyverse)
library(p8105.datasets)
data("instacart")
```

# Problem 1

Count the number of distinct aisles.

``` r
aisle_count = 
instacart %>% 
  summarize(n_distinct(aisle))
```

Arrange the `aisle` variable according to observation frequencies and
output the first (highest frequency) row.

``` r
aisle_most_order =
  instacart %>% 
  group_by(aisle) %>% 
  summarize(frequency = n()) %>% 
  arrange(desc(frequency)) %>% 
  pull(aisle) %>% 
  first()
```

There are 134 different aisles. The aisle from with most items are
ordered is fresh vegetables.
