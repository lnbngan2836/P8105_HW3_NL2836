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

##### How many aisles are there, and which aisles are the most items ordered from?

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

Answer: There are 134 different aisles. The aisle from which most items
are ordered is fresh vegetables.

##### Make a plot that shows the number of items ordered in each aisle, limiting this to aisles with more than 10000 items ordered. Arrange aisles sensibly, and organize your plot so others can read it.

Filter aisles to those with more than 10000 and arrange.

``` r
aisle_plot =
  instacart %>% 
  group_by(aisle) %>% 
  summarize(frequency = n()) %>%
  filter(frequency > 10000)
```

Make a plot to show the number of items ordered in each aisle.

``` r
aisle_plot %>% 
  ggplot(aes(x= reorder(aisle, frequency), y = frequency)) + 
  geom_bar(stat = "identity", fill = "skyblue") + 
  coord_flip() +
  labs(x = "Aisle name", y = "Number of Items Ordered") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))
```

![](HW3_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->
