Homework 3
================
Yuan Meng
2021-10-18

``` r
aisle_df = 
  instacart %>%
  select(aisle) %>%
  distinct() %>%
  mutate(,order_num = sort((table(instacart$aisle)))) #create a new aisle data frame shows each aisles with there corresponding order number 
as.numeric(count(aisle_df[1])) #number of aisles
```

    ## [1] 134

``` r
tail(aisle_df, n =1) #the aisle of the most items ordered from is the last row of the data frame
```

    ## # A tibble: 1 × 2
    ##   aisle        order_num
    ##   <chr>        <table>  
    ## 1 frozen juice 150609
