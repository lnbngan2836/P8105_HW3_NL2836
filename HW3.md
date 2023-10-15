p8105_hw3_NL2836
================
Ngan Le
2023-10-12

Load the necessary libraries and datasets.

``` r
library(tidyverse)
library(p8105.datasets)
library(gt)
library(patchwork)
data("instacart")
data("brfss_smart2010")
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
  summarize(freq_order = n()) %>% 
  arrange(desc(freq_order)) %>% 
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
  summarize(freq_aisle= n()) %>%
  filter(freq_aisle > 10000)
```

Make a plot to show the number of items ordered in each aisle.

``` r
aisle_plot %>% 
  ggplot(aes(x= reorder(aisle, freq_aisle), y = freq_aisle)) + 
  geom_bar(stat = "identity", fill = "skyblue") + 
  coord_flip() +
  labs(x = "Aisle name", y = "Number of Items Ordered") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7)) +
  ggtitle("Number of Items Ordered by Aisle")
```

![](HW3_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

##### Make a table showing the three most popular items in each of the aisles “baking ingredients”, “dog food care”, and “packaged vegetables fruits”. Include the number of times each item is ordered in your table.

Get the 3 most popular items of each aisle.

``` r
popular_items =
  instacart %>% 
  filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) %>% 
  group_by(aisle, product_name) %>% 
  summarize(freq_pop=n(),.groups = 'drop') %>% 
  arrange(aisle, desc(freq_pop)) %>% 
  group_by(aisle)%>%
  top_n(3, wt = freq_pop)
```

Make a table.

``` r
popular_items %>%
  gt() %>%
  cols_label(
    aisle = "Aisle",
    product_name = "Product name",
    freq_pop = "Times ordered") %>% 
  tab_header(title = "Top 3 Popular Items by Aisle") %>%
  fmt_number(columns = vars(freq_pop), decimals = 0) 
```

<div id="apcplnkfss" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#apcplnkfss table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#apcplnkfss thead, #apcplnkfss tbody, #apcplnkfss tfoot, #apcplnkfss tr, #apcplnkfss td, #apcplnkfss th {
  border-style: none;
}
&#10;#apcplnkfss p {
  margin: 0;
  padding: 0;
}
&#10;#apcplnkfss .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#apcplnkfss .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#apcplnkfss .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#apcplnkfss .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#apcplnkfss .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#apcplnkfss .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#apcplnkfss .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#apcplnkfss .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#apcplnkfss .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#apcplnkfss .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#apcplnkfss .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#apcplnkfss .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#apcplnkfss .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#apcplnkfss .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#apcplnkfss .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#apcplnkfss .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#apcplnkfss .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#apcplnkfss .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#apcplnkfss .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#apcplnkfss .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#apcplnkfss .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#apcplnkfss .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#apcplnkfss .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#apcplnkfss .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#apcplnkfss .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#apcplnkfss .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#apcplnkfss .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#apcplnkfss .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#apcplnkfss .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#apcplnkfss .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#apcplnkfss .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#apcplnkfss .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#apcplnkfss .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#apcplnkfss .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#apcplnkfss .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#apcplnkfss .gt_left {
  text-align: left;
}
&#10;#apcplnkfss .gt_center {
  text-align: center;
}
&#10;#apcplnkfss .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#apcplnkfss .gt_font_normal {
  font-weight: normal;
}
&#10;#apcplnkfss .gt_font_bold {
  font-weight: bold;
}
&#10;#apcplnkfss .gt_font_italic {
  font-style: italic;
}
&#10;#apcplnkfss .gt_super {
  font-size: 65%;
}
&#10;#apcplnkfss .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#apcplnkfss .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#apcplnkfss .gt_indent_1 {
  text-indent: 5px;
}
&#10;#apcplnkfss .gt_indent_2 {
  text-indent: 10px;
}
&#10;#apcplnkfss .gt_indent_3 {
  text-indent: 15px;
}
&#10;#apcplnkfss .gt_indent_4 {
  text-indent: 20px;
}
&#10;#apcplnkfss .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Top 3 Popular Items by Aisle</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Product name">Product name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Times ordered">Times ordered</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="baking ingredients">baking ingredients</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="baking ingredients  product_name" class="gt_row gt_left">Light Brown Sugar</td>
<td headers="baking ingredients  freq_pop" class="gt_row gt_right">499</td></tr>
    <tr><td headers="baking ingredients  product_name" class="gt_row gt_left">Pure Baking Soda</td>
<td headers="baking ingredients  freq_pop" class="gt_row gt_right">387</td></tr>
    <tr><td headers="baking ingredients  product_name" class="gt_row gt_left">Cane Sugar</td>
<td headers="baking ingredients  freq_pop" class="gt_row gt_right">336</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="dog food care">dog food care</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="dog food care  product_name" class="gt_row gt_left">Snack Sticks Chicken &amp; Rice Recipe Dog Treats</td>
<td headers="dog food care  freq_pop" class="gt_row gt_right">30</td></tr>
    <tr><td headers="dog food care  product_name" class="gt_row gt_left">Organix Chicken &amp; Brown Rice Recipe</td>
<td headers="dog food care  freq_pop" class="gt_row gt_right">28</td></tr>
    <tr><td headers="dog food care  product_name" class="gt_row gt_left">Small Dog Biscuits</td>
<td headers="dog food care  freq_pop" class="gt_row gt_right">26</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="2" class="gt_group_heading" scope="colgroup" id="packaged vegetables fruits">packaged vegetables fruits</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="packaged vegetables fruits  product_name" class="gt_row gt_left">Organic Baby Spinach</td>
<td headers="packaged vegetables fruits  freq_pop" class="gt_row gt_right">9,784</td></tr>
    <tr><td headers="packaged vegetables fruits  product_name" class="gt_row gt_left">Organic Raspberries</td>
<td headers="packaged vegetables fruits  freq_pop" class="gt_row gt_right">5,546</td></tr>
    <tr><td headers="packaged vegetables fruits  product_name" class="gt_row gt_left">Organic Blueberries</td>
<td headers="packaged vegetables fruits  freq_pop" class="gt_row gt_right">4,966</td></tr>
  </tbody>
  &#10;  
</table>
</div>

##### Make a table showing the mean hour of the day at which Pink Lady Apples and Coffee Ice Cream are ordered on each day of the week; format this table for human readers (i.e. produce a 2 x 7 table).

``` r
time_order =
  instacart %>% 
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>%
  group_by(product_name, order_dow) %>% 
  summarize(mean_hour = round(mean(order_hour_of_day, na.rm = TRUE),2), .groups = 'drop') %>% 
 pivot_wider(
    names_from = order_dow,
    values_from = mean_hour) %>% 
 rename(
   Monday = "0", 
   Tuesday = "1", 
   Wednesday = "2", 
   Thursday = "3", 
   Friday = "4", 
   Saturday = "5", 
   Sunday = "6"
       )
```

``` r
time_order %>%
  gt() %>%
  cols_label(
    product_name = "Product name") %>% 
  tab_header(title = "Mean time order by week date") %>%
  fmt_number(columns = vars(product_name), decimals = 0) 
```

<div id="oxyvztgpnx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#oxyvztgpnx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#oxyvztgpnx thead, #oxyvztgpnx tbody, #oxyvztgpnx tfoot, #oxyvztgpnx tr, #oxyvztgpnx td, #oxyvztgpnx th {
  border-style: none;
}
&#10;#oxyvztgpnx p {
  margin: 0;
  padding: 0;
}
&#10;#oxyvztgpnx .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#oxyvztgpnx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#oxyvztgpnx .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#oxyvztgpnx .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#oxyvztgpnx .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#oxyvztgpnx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#oxyvztgpnx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#oxyvztgpnx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#oxyvztgpnx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#oxyvztgpnx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#oxyvztgpnx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#oxyvztgpnx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#oxyvztgpnx .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#oxyvztgpnx .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#oxyvztgpnx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#oxyvztgpnx .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#oxyvztgpnx .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#oxyvztgpnx .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#oxyvztgpnx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oxyvztgpnx .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#oxyvztgpnx .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#oxyvztgpnx .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#oxyvztgpnx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oxyvztgpnx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#oxyvztgpnx .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#oxyvztgpnx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#oxyvztgpnx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oxyvztgpnx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#oxyvztgpnx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#oxyvztgpnx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#oxyvztgpnx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#oxyvztgpnx .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#oxyvztgpnx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oxyvztgpnx .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#oxyvztgpnx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oxyvztgpnx .gt_left {
  text-align: left;
}
&#10;#oxyvztgpnx .gt_center {
  text-align: center;
}
&#10;#oxyvztgpnx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#oxyvztgpnx .gt_font_normal {
  font-weight: normal;
}
&#10;#oxyvztgpnx .gt_font_bold {
  font-weight: bold;
}
&#10;#oxyvztgpnx .gt_font_italic {
  font-style: italic;
}
&#10;#oxyvztgpnx .gt_super {
  font-size: 65%;
}
&#10;#oxyvztgpnx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#oxyvztgpnx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#oxyvztgpnx .gt_indent_1 {
  text-indent: 5px;
}
&#10;#oxyvztgpnx .gt_indent_2 {
  text-indent: 10px;
}
&#10;#oxyvztgpnx .gt_indent_3 {
  text-indent: 15px;
}
&#10;#oxyvztgpnx .gt_indent_4 {
  text-indent: 20px;
}
&#10;#oxyvztgpnx .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="8" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Mean time order by week date</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Product name">Product name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Monday">Monday</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Tuesday">Tuesday</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Wednesday">Wednesday</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Thursday">Thursday</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Friday">Friday</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Saturday">Saturday</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Sunday">Sunday</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="product_name" class="gt_row gt_left">Coffee Ice Cream</td>
<td headers="Monday" class="gt_row gt_right">13.77</td>
<td headers="Tuesday" class="gt_row gt_right">14.32</td>
<td headers="Wednesday" class="gt_row gt_right">15.38</td>
<td headers="Thursday" class="gt_row gt_right">15.32</td>
<td headers="Friday" class="gt_row gt_right">15.22</td>
<td headers="Saturday" class="gt_row gt_right">12.26</td>
<td headers="Sunday" class="gt_row gt_right">13.83</td></tr>
    <tr><td headers="product_name" class="gt_row gt_left">Pink Lady Apples</td>
<td headers="Monday" class="gt_row gt_right">13.44</td>
<td headers="Tuesday" class="gt_row gt_right">11.36</td>
<td headers="Wednesday" class="gt_row gt_right">11.70</td>
<td headers="Thursday" class="gt_row gt_right">14.25</td>
<td headers="Friday" class="gt_row gt_right">11.55</td>
<td headers="Saturday" class="gt_row gt_right">12.78</td>
<td headers="Sunday" class="gt_row gt_right">11.94</td></tr>
  </tbody>
  &#10;  
</table>
</div>

# Problem 2

##### Data cleaning

``` r
p2_clean = 
  brfss_smart2010 %>%  
  janitor::clean_names() %>% 
  rename(
    location_abbr = locationabbr,
    location_desc = locationdesc,
    resp_id = respid,
        ) %>% 
  filter(
    topic == "Overall Health",
    response %in% c("Excellent", "Very good", "Good", "Fair", "Poor")
         ) %>% 
    mutate(response = factor(response, levels = c("Poor", "Fair", "Good", "Very good", "Excellent"), ordered = TRUE))
```

##### In 2002, which states were observed at 7 or more locations? What about in 2010?

``` r
df2002 =
p2_clean %>% 
  filter(year == "2002") %>% 
  group_by(location_abbr) %>% 
  summarize(dist_2002 = n_distinct(location_desc)) %>% 
  filter(dist_2002 >= 7) %>% 
  pull(location_abbr)
```

In 2002, the states that were observed at 7 or more locations are CT,
FL, MA, NC, NJ, PA.

``` r
df2010 =
p2_clean %>% 
  filter(year == "2010") %>% 
  group_by(location_abbr) %>% 
  summarize(dist_2010 = n_distinct(location_desc)) %>% 
  filter(dist_2010 >= 7) %>% 
  pull(location_abbr)
```

In 2010, the states that were observed at 7 or more locations are CA,
CO, FL, MA, MD, NC, NE, NJ, NY, OH, PA, SC, TX, WA.

##### Construct a dataset that is limited to Excellent responses, and contains, year, state, and a variable that averages the data_value across locations within a state. Make a “spaghetti” plot of this average value over time within a state (that is, make a plot showing a line for each state across years – the geom_line geometry and group aesthetic will help).

Make the dataframe.

``` r
excellent =
  p2_clean %>% 
  filter(response == "Excellent") %>% 
  select(year, location_abbr, data_value) %>% 
  group_by(year, location_abbr) %>% 
  summarize(mean_value = round(mean(data_value, na.rm = TRUE), 2),.groups = 'drop')
```

Plot the average value.

``` r
ggplot(excellent, aes(x = year, y = mean_value, group = location_abbr, color = location_abbr)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Average Value",
    title = "Spaghetti Plot of Average Value Over Time by State"
  )
```

![](HW3_files/figure-gfm/spaghetti-1.png)<!-- -->

The plot shows the trend of the average data value of each states from
2002 to 2010.WV average data value fluctuates more than others’, but
overall stay the lowest across the states over the year. DC average data
value overall stay the highest acrros the states over the year.

##### Make a two-panel plot showing, for the years 2006, and 2010, distribution of data_value for responses (“Poor” to “Excellent”) among locations in NY State.

``` r
plot_2006 = 
  p2_clean %>% 
  filter(location_abbr=="NY",year==2006)|>
  ggplot(aes(x =location_desc , y = data_value,fill=response)) + 
  geom_bar(position="dodge",stat="identity") +  
  labs(title = "Average data value by responses in New York State (2006)") + 
  theme(plot.title = element_text(size = 10), axis.text.x = element_text(angle = 20, hjust = 1),legend.position = "none")

 plot_2010 =
  p2_clean %>%  
  filter(location_abbr == "NY", year == "2010") %>% 
  ggplot(aes(x = location_desc, y = data_value, fill = response)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = " Average data value by responses in New York State (2010)") +
  theme(plot.title = element_text(size = 10), axis.text.x = element_text(angle = 20, hjust = 1)) 

options(repr.plot.width = 10, repr.plot.height = 4)
 
 
plot_2006 + plot_2010
```

![](HW3_files/figure-gfm/plot%202006+2010-1.png)<!-- -->
