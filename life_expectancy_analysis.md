---
title: "Life Expectancy Analysis"
author: "Dominik Grzegorzewicz"
date: "13 12 2020"
output: 
  html_document:
    toc: true
    toc_float: true
    keep_md: true
---



## Preliminaries

Used libraries

```r
library(readr)
library(dplyr)
library(knitr)
library(ggplot2)
library(tidyr)
library(stringr)
library(DMwR)
library(reshape2)
library(leaflet)
```

The line making the code reproducible

```r
set.seed(101)
```

Loading dataset

```r
df <- read_csv("data/Life_Expectancy_Data.csv", quote = "")
```

```
## 
## -- Column specification ------------------------------------------------------------------------------------
## cols(
##   .default = col_double(),
##   Country = col_character(),
##   Status = col_character()
## )
## i Use `spec()` for the full column specifications.
```

## Data summary


```r
kable(head(df), digits=2)
```


|Country     | Year|Status     | Life expectancy| Adult Mortality| infant deaths| Alcohol| percentage expenditure| Hepatitis B| Measles|  BMI|
|:-----------|----:|:----------|---------------:|---------------:|-------------:|-------:|----------------------:|-----------:|-------:|----:|
|Afghanistan | 2015|Developing |            65.0|             263|            62|    0.01|                  71.28|          65|    1154| 19.1|
|Afghanistan | 2014|Developing |            59.9|             271|            64|    0.01|                  73.52|          62|     492| 18.6|
|Afghanistan | 2013|Developing |            59.9|             268|            66|    0.01|                  73.22|          64|     430| 18.1|
|Afghanistan | 2012|Developing |            59.5|             272|            69|    0.01|                  78.18|          67|    2787| 17.6|
|Afghanistan | 2011|Developing |            59.2|             275|            71|    0.01|                   7.10|          68|    3013| 17.2|
|Afghanistan | 2010|Developing |            58.8|             279|            74|    0.01|                  79.68|          66|    1989| 16.7|



| under-five deaths| Polio| Total expenditure| Diphtheria| HIV/AIDS|    GDP| Population| thinness  1-19 years| thinness 5-9 years| Income composition of resources| Schooling|
|-----------------:|-----:|-----------------:|----------:|--------:|------:|----------:|--------------------:|------------------:|-------------------------------:|---------:|
|                83|     6|              8.16|         65|      0.1| 584.26|   33736494|                 17.2|               17.3|                            0.48|      10.1|
|                86|    58|              8.18|         62|      0.1| 612.70|     327582|                 17.5|               17.5|                            0.48|      10.0|
|                89|    62|              8.13|         64|      0.1| 631.74|   31731688|                 17.7|               17.7|                            0.47|       9.9|
|                93|    67|              8.52|         67|      0.1| 669.96|    3696958|                 17.9|               18.0|                            0.46|       9.8|
|                97|    68|              7.87|         68|      0.1|  63.54|    2978599|                 18.2|               18.2|                            0.45|       9.5|
|               102|    66|              9.20|         66|      0.1| 553.33|    2883167|                 18.4|               18.4|                            0.45|       9.2|


```r
kable(summary(select(df, Country:Measles)))
```



|   |  Country        |     Year    |   Status        |Life expectancy |Adult Mortality |infant deaths  |   Alcohol      |percentage expenditure | Hepatitis B  |   Measles       |
|:--|:----------------|:------------|:----------------|:---------------|:---------------|:--------------|:---------------|:----------------------|:-------------|:----------------|
|   |Length:2938      |Min.   :2000 |Length:2938      |Min.   :36.30   |Min.   :  1.0   |Min.   :   0.0 |Min.   : 0.0100 |Min.   :    0.000      |Min.   : 1.00 |Min.   :     0.0 |
|   |Class :character |1st Qu.:2004 |Class :character |1st Qu.:63.10   |1st Qu.: 74.0   |1st Qu.:   0.0 |1st Qu.: 0.8775 |1st Qu.:    4.685      |1st Qu.:77.00 |1st Qu.:     0.0 |
|   |Mode  :character |Median :2008 |Mode  :character |Median :72.10   |Median :144.0   |Median :   3.0 |Median : 3.7550 |Median :   64.913      |Median :92.00 |Median :    17.0 |
|   |NA               |Mean   :2008 |NA               |Mean   :69.22   |Mean   :164.8   |Mean   :  30.3 |Mean   : 4.6029 |Mean   :  738.251      |Mean   :80.94 |Mean   :  2419.6 |
|   |NA               |3rd Qu.:2012 |NA               |3rd Qu.:75.70   |3rd Qu.:228.0   |3rd Qu.:  22.0 |3rd Qu.: 7.7025 |3rd Qu.:  441.534      |3rd Qu.:97.00 |3rd Qu.:   360.2 |
|   |NA               |Max.   :2015 |NA               |Max.   :89.00   |Max.   :723.0   |Max.   :1800.0 |Max.   :17.8700 |Max.   :19479.912      |Max.   :99.00 |Max.   :212183.0 |
|   |NA               |NA           |NA               |NA's   :10      |NA's   :10      |NA             |NA's   :194     |NA                     |NA's   :553   |NA               |

```r
kable(summary(select(df, BMI:`thinness 5-9 years`)))
```



|   |     BMI      |under-five deaths |    Polio     |Total expenditure |  Diphtheria  |   HIV/AIDS    |     GDP          |  Population      |thinness  1-19 years |thinness 5-9 years |
|:--|:-------------|:-----------------|:-------------|:-----------------|:-------------|:--------------|:-----------------|:-----------------|:--------------------|:------------------|
|   |Min.   : 1.00 |Min.   :   0.00   |Min.   : 3.00 |Min.   : 0.370    |Min.   : 2.00 |Min.   : 0.100 |Min.   :     1.68 |Min.   :3.400e+01 |Min.   : 0.10        |Min.   : 0.10      |
|   |1st Qu.:19.30 |1st Qu.:   0.00   |1st Qu.:78.00 |1st Qu.: 4.260    |1st Qu.:78.00 |1st Qu.: 0.100 |1st Qu.:   463.94 |1st Qu.:1.958e+05 |1st Qu.: 1.60        |1st Qu.: 1.50      |
|   |Median :43.50 |Median :   4.00   |Median :93.00 |Median : 5.755    |Median :93.00 |Median : 0.100 |Median :  1766.95 |Median :1.387e+06 |Median : 3.30        |Median : 3.30      |
|   |Mean   :38.32 |Mean   :  42.04   |Mean   :82.55 |Mean   : 5.938    |Mean   :82.32 |Mean   : 1.742 |Mean   :  7483.16 |Mean   :1.275e+07 |Mean   : 4.84        |Mean   : 4.87      |
|   |3rd Qu.:56.20 |3rd Qu.:  28.00   |3rd Qu.:97.00 |3rd Qu.: 7.492    |3rd Qu.:97.00 |3rd Qu.: 0.800 |3rd Qu.:  5910.81 |3rd Qu.:7.420e+06 |3rd Qu.: 7.20        |3rd Qu.: 7.20      |
|   |Max.   :87.30 |Max.   :2500.00   |Max.   :99.00 |Max.   :17.600    |Max.   :99.00 |Max.   :50.600 |Max.   :119172.74 |Max.   :1.294e+09 |Max.   :27.70        |Max.   :28.60      |
|   |NA's   :34    |NA                |NA's   :19    |NA's   :226       |NA's   :19    |NA             |NA's   :448       |NA's   :652       |NA's   :34           |NA's   :34         |

```r
kable(summary(select(df, `Income composition of resources`:Schooling)))
```



|   |Income composition of resources |  Schooling   |
|:--|:-------------------------------|:-------------|
|   |Min.   :0.0000                  |Min.   : 0.00 |
|   |1st Qu.:0.4930                  |1st Qu.:10.10 |
|   |Median :0.6770                  |Median :12.30 |
|   |Mean   :0.6276                  |Mean   :11.99 |
|   |3rd Qu.:0.7790                  |3rd Qu.:14.30 |
|   |Max.   :0.9480                  |Max.   :20.70 |
|   |NA's   :167                     |NA's   :163   |

## Data cleaning

Count % of missing values for every column

```r
missing_df <- df %>% 
  summarise_all(list(~sum(is.na(.))/length(.))) %>%
  gather(key="column", value="missing") %>%
  filter(missing > 0) %>%
  ggplot(aes(x=reorder(column, missing), y=missing)) +
  geom_bar(stat="identity") +
  coord_flip()

missing_df
```

![](life_expectancy_analysis_files/figure-html/missing_plot-1.png)<!-- -->


```r
na_count <- apply(df, 1, function(x) sum(is.na(x)))
idx <- 1:nrow(df)
id_na_count <- as.data.frame(cbind(idx, na_count)) %>%
  arrange(desc(na_count))

row_select <- id_na_count %>%
  filter(na_count >= 8) %>%
  select(idx)

most_na_rows <- df[unlist(row_select),]
most_na_rows
```

```
## # A tibble: 13 x 22
##    Country  Year Status `Life expectanc~ `Adult Mortalit~ `infant deaths`
##    <chr>   <dbl> <chr>             <dbl>            <dbl>           <dbl>
##  1 Monaco   2013 Devel~             NA                 NA               0
##  2 South ~  2007 Devel~             53.1              381              27
##  3 South ~  2006 Devel~             52.5              383              28
##  4 South ~  2005 Devel~             51.9              383              28
##  5 South ~  2004 Devel~             51.4              383              29
##  6 South ~  2003 Devel~             58                383              29
##  7 South ~  2002 Devel~             52                382              30
##  8 South ~  2001 Devel~             49.6              381              30
##  9 South ~  2000 Devel~             48.9               38              31
## 10 San Ma~  2013 Devel~             NA                 NA               0
## 11 South ~  2010 Devel~             55                359              27
## 12 South ~  2009 Devel~             54.3              369              27
## 13 South ~  2008 Devel~             53.6              377              27
## # ... with 16 more variables: Alcohol <dbl>, `percentage expenditure` <dbl>,
## #   `Hepatitis B` <dbl>, Measles <dbl>, BMI <dbl>, `under-five deaths` <dbl>,
## #   Polio <dbl>, `Total expenditure` <dbl>, Diphtheria <dbl>, `HIV/AIDS` <dbl>,
## #   GDP <dbl>, Population <dbl>, `thinness 1-19 years` <dbl>, `thinness 5-9
## #   years` <dbl>, `Income composition of resources` <dbl>, Schooling <dbl>
```

Checking Population statistics for countries

```r
by_country_population <- df %>%
  group_by(Country) %>%
  summarise(
    count = n(),
    na = sum(is.na(Population))
  ) %>%
  filter(count == na) %>%
  select(Country)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
kable(pull(by_country_population, Country))
```



|x                                                    |
|:----------------------------------------------------|
|"Cote d'Ivoire"                                      |
|"Democratic People's Republic of Korea"              |
|Antigua and Barbuda                                  |
|Bahamas                                              |
|Bahrain                                              |
|Barbados                                             |
|Bolivia (Plurinational State of)                     |
|Brunei Darussalam                                    |
|Congo                                                |
|Cook Islands                                         |
|Cuba                                                 |
|Czechia                                              |
|Democratic Republic of the Congo                     |
|Dominica                                             |
|Egypt                                                |
|Gambia                                               |
|Grenada                                              |
|Iran (Islamic Republic of)                           |
|Kuwait                                               |
|Kyrgyzstan                                           |
|Lao People's Democratic Republic                     |
|Libya                                                |
|Marshall Islands                                     |
|Micronesia (Federated States of)                     |
|Monaco                                               |
|Nauru                                                |
|New Zealand                                          |
|Niue                                                 |
|Oman                                                 |
|Qatar                                                |
|Republic of Korea                                    |
|Republic of Moldova                                  |
|Saint Kitts and Nevis                                |
|Saint Lucia                                          |
|Saint Vincent and the Grenadines                     |
|San Marino                                           |
|Saudi Arabia                                         |
|Singapore                                            |
|Slovakia                                             |
|Somalia                                              |
|The former Yugoslav republic of Macedonia            |
|United Arab Emirates                                 |
|United Kingdom of Great Britain and Northern Ireland |
|United Republic of Tanzania                          |
|United States of America                             |
|Venezuela (Bolivarian Republic of)                   |
|Viet Nam                                             |
|Yemen                                                |

```r
country_pop <- df %>%
  group_by(Country) %>%
  summarise(
    mean_pop = mean(Population),
    median_pop = median(Population),
    std_pop = sd(Population),
    min_pop = min(Population),
    max_pop = max(Population),
    na = sum(is.na(Population))
  ) %>%
  filter(na == 0)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
kable(sample_n(country_pop, 20))
```



|Country             |    mean_pop|  median_pop|     std_pop| min_pop|    max_pop| na|
|:-------------------|-----------:|-----------:|-----------:|-------:|----------:|--:|
|Costa Rica          |   2309299.4|   2206647.5|   2092412.7|   46324|    4757575|  0|
|Azerbaijan          |   2157370.5|    843820.0|   3566281.5|    8365|    9649341|  0|
|Nigeria             |  70877003.1|  16735354.5|  72003797.9| 1223529|  181181744|  0|
|Palau               |       292.0|       292.0|          NA|     292|        292|  0|
|Pakistan            |  69027961.8|  16348718.5|  79541925.8|   15783|  185546257|  0|
|Indonesia           | 116555259.4|  24904887.0| 115186206.6| 2145652|  258162113|  0|
|Niger               |  11240718.9|  14143393.5|   7407838.5|   12262|   19896965|  0|
|India               | 421467690.9| 125544740.5| 532788375.4| 1395398| 1293859294|  0|
|Benin               |   3942978.9|   1431332.0|   3844086.6|    7754|    9199259|  0|
|Norway              |   2614432.3|   2546687.0|   2193111.2|  449967|    5137232|  0|
|France              |  27581733.1|   6631162.5|  28934588.1|  647744|   66331957|  0|
|Solomon Islands     |    329044.2|    441015.5|    226998.8|   41269|     587482|  0|
|Ethiopia            |  43950760.8|  38262332.0|  40869235.8|    8149|   97366774|  0|
|Myanmar             |  26015702.0|  26307254.0|  24043901.8|   49869|   51924182|  0|
|Fiji                |    650205.1|    817491.0|    337079.5|   84334|     892149|  0|
|Iraq                |  18572840.8|  24595474.0|  14055443.2|    3568|   36115649|  0|
|Trinidad and Tobago |    776133.1|   1272910.5|    635774.6|   13281|    1354493|  0|
|Iceland             |    186177.6|    286245.5|    141294.7|   28125|     327386|  0|
|Mexico              |  27585265.2|  11460708.5|  44950945.4|  119917|  122535969|  0|
|Mauritius           |    663332.9|    656903.5|    588208.1|    1254|    1258653|  0|

Drop rows with most na values, rows without Life expectancy value and Population column

```r
df <- df[-unlist(row_select),]
df <- filter(df, !is.na(`Life expectancy`))
df <- select(df, -Population)

dim(df)
```

```
## [1] 2917   21
```


```r
missing_df <- df %>% 
  summarise_all(list(~sum(is.na(.))/length(.))) %>%
  gather(key="column", value="missing") %>%
  filter(missing > 0) %>%
  ggplot(aes(x=reorder(column, missing), y=missing)) +
  geom_bar(stat="identity") +
  coord_flip()

missing_df
```

![](life_expectancy_analysis_files/figure-html/missing_plot2-1.png)<!-- -->


```r
diphtheria_polio <- df %>%
  filter(is.na(Diphtheria) | is.na(Polio)) %>%
  select(Country, Year)
kable(diphtheria_polio)
```



|Country     | Year|
|:-----------|----:|
|Montenegro  | 2005|
|Montenegro  | 2004|
|Montenegro  | 2003|
|Montenegro  | 2002|
|Montenegro  | 2001|
|Montenegro  | 2000|
|Timor-Leste | 2001|
|Timor-Leste | 2000|

```r
df[["Diphtheria"]][is.na(df[["Diphtheria"]])] <- 0
df[["Polio"]][is.na(df[["Polio"]])] <- 0
```


```r
thinness <- df %>%
  filter(is.na(`thinness 5-9 years`) | is.na(`thinness  1-19 years`) | is.na(BMI)) %>%
  select(Country, Year)
kable(thinness)
```



|Country     | Year|
|:-----------|----:|
|South Sudan | 2015|
|South Sudan | 2014|
|South Sudan | 2013|
|South Sudan | 2012|
|South Sudan | 2011|
|Sudan       | 2015|
|Sudan       | 2014|
|Sudan       | 2013|
|Sudan       | 2012|
|Sudan       | 2011|
|Sudan       | 2010|
|Sudan       | 2009|
|Sudan       | 2008|
|Sudan       | 2007|
|Sudan       | 2006|
|Sudan       | 2005|
|Sudan       | 2004|
|Sudan       | 2003|
|Sudan       | 2002|
|Sudan       | 2001|
|Sudan       | 2000|

```r
thinness <- df %>%
  filter(is.na(`thinness 5-9 years`) | is.na(`thinness  1-19 years`) | is.na(BMI))
kable(unique(thinness$Country))
```



|x           |
|:-----------|
|South Sudan |
|Sudan       |

```r
by_country_alcohol <- df %>%
  group_by(Country) %>%
  summarise(
    count = n(),
    na = sum(is.na(Alcohol))
  ) %>%
  filter(count == na) %>%
  select(Country)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
kable(pull(by_country_alcohol, Country))
```



|x           |
|:-----------|
|South Sudan |

```r
by_country_schooling <- df %>%
  group_by(Country) %>%
  summarise(
    count = n(),
    na = sum(is.na(Schooling))
  ) %>%
  filter(count == na) %>%
  select(Country)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
kable(pull(by_country_schooling, Country))
```



|x                                                    |
|:----------------------------------------------------|
|"Cote d'Ivoire"                                      |
|"Democratic People's Republic of Korea"              |
|Czechia                                              |
|Democratic Republic of the Congo                     |
|Republic of Korea                                    |
|Republic of Moldova                                  |
|Somalia                                              |
|United Kingdom of Great Britain and Northern Ireland |
|United Republic of Tanzania                          |
|United States of America                             |

```r
by_country_income <- df %>%
  group_by(Country) %>%
  summarise(
    count = n(),
    na = sum(is.na(`Income composition of resources`))
  ) %>%
  filter(count == na) %>%
  select(Country)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
kable(pull(by_country_income, Country))
```



|x                                                    |
|:----------------------------------------------------|
|"Cote d'Ivoire"                                      |
|"Democratic People's Republic of Korea"              |
|Czechia                                              |
|Democratic Republic of the Congo                     |
|Republic of Korea                                    |
|Republic of Moldova                                  |
|Somalia                                              |
|United Kingdom of Great Britain and Northern Ireland |
|United Republic of Tanzania                          |
|United States of America                             |

```r
by_country_exp <- df %>%
  group_by(Country) %>%
  summarise(
    count = n(),
    na = sum(is.na(`Total expenditure`))
  ) %>%
  filter(count == na) %>%
  select(Country)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
kable(pull(by_country_exp, Country))
```



|x                                       |
|:---------------------------------------|
|"Democratic People's Republic of Korea" |
|Somalia                                 |

```r
by_country_gdp <- df %>%
  group_by(Country) %>%
  summarise(
    count = n(),
    na = sum(is.na(GDP))
  ) %>%
  filter(count == na) %>%
  select(Country)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
kable(pull(by_country_gdp, Country))
```



|x                                                    |
|:----------------------------------------------------|
|"Cote d'Ivoire"                                      |
|"Democratic People's Republic of Korea"              |
|Bahamas                                              |
|Bolivia (Plurinational State of)                     |
|Congo                                                |
|Czechia                                              |
|Democratic Republic of the Congo                     |
|Egypt                                                |
|Gambia                                               |
|Iran (Islamic Republic of)                           |
|Kyrgyzstan                                           |
|Lao People's Democratic Republic                     |
|Micronesia (Federated States of)                     |
|Republic of Korea                                    |
|Republic of Moldova                                  |
|Saint Lucia                                          |
|Saint Vincent and the Grenadines                     |
|Slovakia                                             |
|The former Yugoslav republic of Macedonia            |
|United Kingdom of Great Britain and Northern Ireland |
|United Republic of Tanzania                          |
|United States of America                             |
|Venezuela (Bolivarian Republic of)                   |
|Viet Nam                                             |
|Yemen                                                |

```r
by_country_hep <- df %>%
  group_by(Country) %>%
  summarise(
    count = n(),
    na = sum(is.na(`Hepatitis B`))
  ) %>%
  filter(count == na) %>%
  select(Country)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
kable(pull(by_country_hep, Country))
```



|x                                                    |
|:----------------------------------------------------|
|Denmark                                              |
|Finland                                              |
|Hungary                                              |
|Iceland                                              |
|Japan                                                |
|Norway                                               |
|Slovenia                                             |
|Switzerland                                          |
|United Kingdom of Great Britain and Northern Ireland |


```r
df_to_impute <- as.data.frame(df)
df_to_impute$Country <- as.factor(df$Country)
df_to_impute$Status <- as.factor(df$Status)

df_clean <- knnImputation(df_to_impute)

kable(filter(df, Country=="United States of America"))
```



|Country                  | Year|Status    | Life expectancy| Adult Mortality| infant deaths| Alcohol| percentage expenditure| Hepatitis B| Measles|  BMI| under-five deaths| Polio| Total expenditure| Diphtheria| HIV/AIDS| GDP| thinness  1-19 years| thinness 5-9 years| Income composition of resources| Schooling|
|:------------------------|----:|:---------|---------------:|---------------:|-------------:|-------:|----------------------:|-----------:|-------:|----:|-----------------:|-----:|-----------------:|----------:|--------:|---:|--------------------:|------------------:|-------------------------------:|---------:|
|United States of America | 2015|Developed |            79.3|              13|            23|      NA|                      0|          92|     188| 69.6|                26|    93|                NA|         95|      0.1|  NA|                  0.8|                0.6|                              NA|        NA|
|United States of America | 2014|Developed |            79.1|              14|            23|    8.82|                      0|          92|     667| 69.1|                27|    93|             17.14|         95|      0.1|  NA|                  0.8|                0.6|                              NA|        NA|
|United States of America | 2013|Developed |            78.9|              16|            23|    8.82|                      0|          91|     187| 68.6|                27|    93|             16.90|         94|      0.1|  NA|                  0.7|                0.6|                              NA|        NA|
|United States of America | 2012|Developed |            78.8|              16|            24|    8.82|                      0|           9|      55| 68.0|                28|    93|             17.20|         94|      0.1|  NA|                  0.7|                0.6|                              NA|        NA|
|United States of America | 2011|Developed |            78.7|              16|            25|    8.67|                      0|          91|     220| 67.5|                29|    94|             17.60|         96|      0.1|  NA|                  0.7|                0.6|                              NA|        NA|
|United States of America | 2010|Developed |            78.7|              15|            25|    8.55|                      0|          92|      63| 66.9|                30|    93|             17.20|         95|      0.1|  NA|                  0.7|                0.6|                              NA|        NA|
|United States of America | 2009|Developed |            78.5|              18|            26|    8.71|                      0|          92|      71| 66.3|                31|    93|             17.00|         95|      0.1|  NA|                  0.7|                0.6|                              NA|        NA|
|United States of America | 2008|Developed |            78.2|              18|            27|    8.74|                      0|          94|     140| 65.7|                31|    94|             16.20|         96|      0.1|  NA|                  0.7|                0.6|                              NA|        NA|
|United States of America | 2007|Developed |            78.1|              11|            27|    8.74|                      0|          93|      43| 65.1|                32|    93|             15.57|         96|      0.1|  NA|                  0.7|                0.6|                              NA|        NA|
|United States of America | 2006|Developed |            77.8|             113|            28|    8.63|                      0|          93|      55| 64.4|                33|    93|             15.27|         96|      0.1|  NA|                  0.7|                0.6|                              NA|        NA|
|United States of America | 2005|Developed |            77.5|             112|            28|    8.52|                      0|          93|      66| 63.8|                33|    92|             15.15|         96|      0.1|  NA|                  0.7|                0.6|                              NA|        NA|
|United States of America | 2004|Developed |            77.5|             111|            28|    8.48|                      0|          92|      37| 63.1|                33|    92|             15.14|         96|      0.1|  NA|                  0.7|                0.6|                              NA|        NA|
|United States of America | 2003|Developed |            77.2|             114|            28|    8.40|                      0|          92|      56| 62.4|                33|    91|             15.60|         96|      0.1|  NA|                  0.7|                0.6|                              NA|        NA|
|United States of America | 2002|Developed |            77.0|             115|            28|    8.33|                      0|          88|      41| 61.7|                33|     9|             14.55|         94|      0.1|  NA|                  0.8|                0.6|                              NA|        NA|
|United States of America | 2001|Developed |            76.9|             115|            28|    8.25|                      0|          89|     116|  6.9|                33|    89|             13.73|         94|      0.1|  NA|                  0.8|                0.6|                              NA|        NA|
|United States of America | 2000|Developed |            76.8|             114|            28|    8.21|                      0|           9|      85|  6.1|                33|     9|             13.70|         94|      0.1|  NA|                  0.8|                0.7|                              NA|        NA|

```r
kable(filter(df_clean, Country=="United States of America"))
```



|Country                  | Year|Status    | Life expectancy| Adult Mortality| infant deaths|  Alcohol| percentage expenditure| Hepatitis B| Measles|  BMI| under-five deaths| Polio| Total expenditure| Diphtheria| HIV/AIDS|      GDP| thinness  1-19 years| thinness 5-9 years| Income composition of resources| Schooling|
|:------------------------|----:|:---------|---------------:|---------------:|-------------:|--------:|----------------------:|-----------:|-------:|----:|-----------------:|-----:|-----------------:|----------:|--------:|--------:|--------------------:|------------------:|-------------------------------:|---------:|
|United States of America | 2015|Developed |            79.3|              13|            23| 5.339279|                      0|          92|     188| 69.6|                26|    93|          8.582594|         95|      0.1| 8983.811|                  0.8|                0.6|                       0.7675697|  15.05997|
|United States of America | 2014|Developed |            79.1|              14|            23| 8.820000|                      0|          92|     667| 69.1|                27|    93|         17.140000|         95|      0.1| 7790.115|                  0.8|                0.6|                       0.7697343|  15.45165|
|United States of America | 2013|Developed |            78.9|              16|            23| 8.820000|                      0|          91|     187| 68.6|                27|    93|         16.900000|         94|      0.1| 7339.546|                  0.7|                0.6|                       0.7682864|  15.42532|
|United States of America | 2012|Developed |            78.8|              16|            24| 8.820000|                      0|           9|      55| 68.0|                28|    93|         17.200000|         94|      0.1| 7470.032|                  0.7|                0.6|                       0.7677787|  15.39568|
|United States of America | 2011|Developed |            78.7|              16|            25| 8.670000|                      0|          91|     220| 67.5|                29|    94|         17.600000|         96|      0.1| 7002.973|                  0.7|                0.6|                       0.7662534|  15.40139|
|United States of America | 2010|Developed |            78.7|              15|            25| 8.550000|                      0|          92|      63| 66.9|                30|    93|         17.200000|         95|      0.1| 6836.312|                  0.7|                0.6|                       0.7656490|  15.39750|
|United States of America | 2009|Developed |            78.5|              18|            26| 8.710000|                      0|          92|      71| 66.3|                31|    93|         17.000000|         95|      0.1| 6644.446|                  0.7|                0.6|                       0.7649377|  15.39300|
|United States of America | 2008|Developed |            78.2|              18|            27| 8.740000|                      0|          94|     140| 65.7|                31|    94|         16.200000|         96|      0.1| 6489.460|                  0.7|                0.6|                       0.7643974|  15.38800|
|United States of America | 2007|Developed |            78.1|              11|            27| 8.740000|                      0|          93|      43| 65.1|                32|    93|         15.570000|         96|      0.1| 6334.680|                  0.7|                0.6|                       0.7638211|  15.38182|
|United States of America | 2006|Developed |            77.8|             113|            28| 8.630000|                      0|          93|      55| 64.4|                33|    93|         15.270000|         96|      0.1| 6040.844|                  0.7|                0.6|                       0.7627705|  15.37696|
|United States of America | 2005|Developed |            77.5|             112|            28| 8.520000|                      0|          93|      66| 63.8|                33|    92|         15.150000|         96|      0.1| 5856.126|                  0.7|                0.6|                       0.7620223|  15.37090|
|United States of America | 2004|Developed |            77.5|             111|            28| 8.480000|                      0|          92|      37| 63.1|                33|    92|         15.140000|         96|      0.1| 4923.338|                  0.7|                0.6|                       0.7591914|  15.32398|
|United States of America | 2003|Developed |            77.2|             114|            28| 8.400000|                      0|          92|      56| 62.4|                33|    91|         15.600000|         96|      0.1| 4819.638|                  0.7|                0.6|                       0.7586520|  15.31925|
|United States of America | 2002|Developed |            77.0|             115|            28| 8.330000|                      0|          88|      41| 61.7|                33|     9|         14.550000|         94|      0.1| 4389.254|                  0.8|                0.6|                       0.7573303|  15.23381|
|United States of America | 2001|Developed |            76.9|             115|            28| 8.250000|                      0|          89|     116|  6.9|                33|    89|         13.730000|         94|      0.1| 3583.715|                  0.8|                0.6|                       0.7522552|  15.11518|
|United States of America | 2000|Developed |            76.8|             114|            28| 8.210000|                      0|           9|      85|  6.1|                33|     9|         13.700000|         94|      0.1| 2694.097|                  0.8|                0.7|                       0.4912451|  12.93502|


## Exploratory Data Analysis


```r
d <- melt(df_clean[,-c(1:3)])
```

```
## No id variables; using all as measure variables
```

```r
ggplot(d,aes(x = value)) + 
    facet_wrap(~variable, scales = "free", ncol=3, nrow=10) + 
    geom_histogram(aes(y=..density..),
                   bins=50,
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666")
```

![](life_expectancy_analysis_files/figure-html/distributions-1.png)<!-- -->


```r
corr_mat <- df_clean %>%
  select(-Country, -Status) %>%
  cor() %>%
  round(2) %>%
  melt()

corr_heat <- ggplot(corr_mat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 9, hjust = 1)) +
  coord_fixed()

corr_heat + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3)
```

![](life_expectancy_analysis_files/figure-html/correlation_matrix-1.png)<!-- -->


```r
development <- df_clean%>%
  group_by(Status) %>%
  summarise(count = n()) %>%
  ggplot(aes(x=Status, y=count))+
    geom_bar(stat="identity") +
    labs(title="Number of cases per development status",
         fill="Status",
         x="Status",
         y="Number of cases")
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
development
```

![](life_expectancy_analysis_files/figure-html/development-1.png)<!-- -->


```r
life_expectancy_by_year <- df_clean %>%
  group_by(Year) %>%
  filter(!is.na(`Life expectancy`)) %>%
  summarise(mean_life_expectancy = mean(`Life expectancy`)) %>%
  ggplot(aes(x=Year, y=mean_life_expectancy)) +
  geom_line(size=1.5) +
  labs(title = "Mean life expectancy in the world 2000-2015",
       x = "Year",
       y = "Mean life expectancy",
       color = "c")
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
life_expectancy_by_year
```

![](life_expectancy_analysis_files/figure-html/life_expectancy_year_plot-1.png)<!-- -->


```r
life_expectancy_development <- df_clean %>%
  group_by(Status,Year) %>%
  summarise(mean_life_expectancy = mean(`Life expectancy`)) %>%
  ggplot(aes(x=Year, y=mean_life_expectancy, group=Status, color=Status)) +
  geom_line(size=1.5) +
  labs(title = "Mean life expectancy in the world 2000-2015",
       x = "Year",
       y = "Mean life expectancy")
```

```
## `summarise()` regrouping output by 'Status' (override with `.groups` argument)
```

```r
life_expectancy_development
```

![](life_expectancy_analysis_files/figure-html/life_expectancy_development-1.png)<!-- -->


```r
life_expectancy_by_country <- df_clean %>%
  group_by(Country) %>%
  filter(!is.na(`Life expectancy`), Year==2000) %>%
  summarise(mean_life_expectancy = mean(`Life expectancy`)) %>%
  arrange(mean_life_expectancy)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
bottom_life_expectancy <- head(life_expectancy_by_country, 10)
bottom_life_expectancy$signal <- rep("worst", 10)
top_life_expectancy <- tail(life_expectancy_by_country, 10)
top_life_expectancy$signal <- rep("best", 10)

to_plot <- rbind(bottom_life_expectancy, top_life_expectancy)
  
ggplot(to_plot, aes(x=reorder(Country, mean_life_expectancy), 
                    y=mean_life_expectancy,
                    fill=signal)) +
  geom_bar(stat="identity") +
  labs(title = "Top 10 best and worst life expectancy in the world in 2000",
       fill = "Top 10",
       x = "Country",
       y = "Life expectancy") +
  scale_fill_manual(values= c("blue", "green")) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 9, hjust = 1))
```

![](life_expectancy_analysis_files/figure-html/life_expectancy_country_plot2000-1.png)<!-- -->


```r
life_expectancy_by_country <- df_clean %>%
  group_by(Country) %>%
  filter(!is.na(`Life expectancy`), Year==2015) %>%
  summarise(mean_life_expectancy = mean(`Life expectancy`)) %>%
  arrange(mean_life_expectancy)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
bottom_life_expectancy <- head(life_expectancy_by_country, 10)
bottom_life_expectancy$signal <- rep("worst", 10)
top_life_expectancy <- tail(life_expectancy_by_country, 10)
top_life_expectancy$signal <- rep("best", 10)

to_plot <- rbind(bottom_life_expectancy, top_life_expectancy)
  
ggplot(to_plot, aes(x=reorder(Country, mean_life_expectancy), 
                    y=mean_life_expectancy,
                    fill=signal)) +
  geom_bar(stat="identity") +
  labs(title = "Top 10 best and worst life expectancy in the world in 2015",
       fill = "Top 10",
       x = "Country",
       y = "Life expectancy") +
  scale_fill_manual(values= c("blue", "green")) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 9, hjust = 1))
```

![](life_expectancy_analysis_files/figure-html/life_expectancy_country_plot2015-1.png)<!-- -->


## Including Plots

You can also embed plots, for example:

![](life_expectancy_analysis_files/figure-html/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
