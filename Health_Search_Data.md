---
title: "Health Searches by US Metropolitan Area, 2004-2017"
author: "Shaun Bray"
date: "February 11, 2018"
output: 
  html_document:
    keep_md: true
---



## Overview
This is the Google Search interest data that powers the Visualisation Searching For Health.
With this data, we are going to explore the search trends of people for common health issues in the United States. 

What I expect to see in this dataset is a constant increase of the amount of searches done each year and that the states/regions that are considered to be sicker would have more searches done, while the healthier states/regions would have less searches regarding the health issues in this dataset. We will take a look at these trends and the relationships between the searched health issures.


## Analysis

### Package Loading and Data Cleaning

```r
# Load packages and search dataset
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(fiftystater)
library(mapproj)
library(noncensus)
library(data.table)
library(corrplot)

df <- read.csv("C:\\Users\\Shaun\\Documents\\Projects\\Kaggle\\Health_Search_Data\\RegionalInterestByConditionOverTime.csv", stringsAsFactors = FALSE)
```
### Exploring the Data

Before this analysis, I would like to first tidy up this data set, making sure that is column is a variable and each row is an observation. Right now, the data set has the year and the searched condition in columns; these are better suited as being an observation than a variable. I would like to use the function *gather* to take these year+condtion columns and gather them into key-value pairs. Then I will use *separate* to split the year and condition into their own columns. Both *gather* and *separate* come from the **dplyr** package. The *dma* column can also be separated into separate columns, city and state.

#### Data Cleaning

```r
names(df)[1] <- "dma" # Rename the first column to 'dma'
df_1 <- df[1:2]
df_2 <- df[3:length(df)]
names(df_2) <- str_sub(names(df_2), 2, -1) # Use str_sub to get the year.condition values
df_1[which(df_1$dma == "Washington DC (Hagerstown MD)"), 1] <- "Washington DC (Hagerstown) MD" # Change Washington DC so state abbreviation is outside of the parenthesis

# Load states data, which will be joined to df
data(states)

# Gather year.condition columns into one column, then separate into two columns
df <- bind_cols(df_1, df_2) %>%
  gather("year.condition", "searches", 3:length(df))  %>%
  separate(col = "year.condition", into = c("year", "condition"), sep = "\\.") %>%
  separate(col = "dma", into = c("city", "state"), sep = -2) %>%
  group_by(city, state, geoCode, year, condition) %>%
  summarize(searches = sum(searches)) %>%
  left_join(states, by = "state") %>% # Join states table to get full names and population
  select(3, 1:2, 7:10, 4:6, 11:12) %>%
  setnames(old = c("state", "name"), new = c("state_abb", "state_name"))
```


```r
head(df, 10)
```

```
## # A tibble: 10 x 12
##    geoCode city       state_abb state_name region division   capital year 
##      <int> <chr>      <chr>     <chr>      <fct>  <fct>      <chr>   <chr>
##  1     662 "Abilene-~ TX        Texas      South  West Sout~ Austin  2004 
##  2     662 "Abilene-~ TX        Texas      South  West Sout~ Austin  2004 
##  3     662 "Abilene-~ TX        Texas      South  West Sout~ Austin  2004 
##  4     662 "Abilene-~ TX        Texas      South  West Sout~ Austin  2004 
##  5     662 "Abilene-~ TX        Texas      South  West Sout~ Austin  2004 
##  6     662 "Abilene-~ TX        Texas      South  West Sout~ Austin  2004 
##  7     662 "Abilene-~ TX        Texas      South  West Sout~ Austin  2004 
##  8     662 "Abilene-~ TX        Texas      South  West Sout~ Austin  2004 
##  9     662 "Abilene-~ TX        Texas      South  West Sout~ Austin  2004 
## 10     662 "Abilene-~ TX        Texas      South  West Sout~ Austin  2005 
## # ... with 4 more variables: condition <chr>, searches <int>, area <chr>,
## #   population <chr>
```

#### Health Search Volume by Year

Plotting a few graphs will be able to give a general idea of what our data looks like.


```r
searches_by_year <- df %>% 
  group_by(year) %>%
  summarize(searches = sum(searches)) %>%
  mutate(difference = searches - lag(searches, default = first(searches)))
ggplot(searches_by_year, aes(x = year, y = searches, group = 1)) +
  geom_line(col = "red", size = 1) +
  labs(title = "Health Search Volume by Year", subtitle = "2004 - 2017")
```

![](Health_Search_Data_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Taking a look at the above graph, there has been an increase in health searches every year, except for a slight decrease of 2303 searches from 2009 - 2010. The biggest increase in search happened between 2010 - 2011, with an increase of 25634 searches.

#### Health Search Volume by Year and Condition

```r
searches_by_yc <- df %>%
  group_by(year, condition) %>%
  summarize(searches = sum(searches))
ggplot(searches_by_yc, aes(x = year, y = searches, group = condition, col = condition)) +
  geom_smooth(size = 1.5, se = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  labs(title = "Health Search Volume by Year and Condition", subtitle = "2004 - 2017") 
```

![](Health_Search_Data_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
From the graph above, in 2004, cancer was the most searched medical condition while cardiovascular was the least searched. Fast forward to 2017, diabetes is now the most searched medical condition; cancer fell to fifth. Cardiovascular still remains the least searched medical condition thirteen years later. It was the least searched condition every year except for 2008, where it just beat out depression. The increase in diabetes searches makes sense given the increase of people with diabetes or prediabetes. [According to the CDC](https://www.cdc.gov/media/releases/2017/p0718-diabetes-report.html), as of 2015, 30.3 million Americans have diabetes and 84.1 million have prediabetes. It is the seventh leading cause of death in 2015.

#### Health Search Volume by State and Region

```r
searches_by_state <- df %>%
  group_by(state_abb, state_name, region) %>%
  summarize(searches = sum(searches))
searches_by_state$lowerState <- tolower(searches_by_state$state_name)
searches_by_state$lowerRegion <- tolower(searches_by_state$region)

ggplot(searches_by_state, aes(map_id = lowerState)) +
  geom_map(aes(fill = searches), map = fifty_states, col = "black") +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"),
        panel.background = element_blank()) +
  fifty_states_inset_boxes()
```

![](Health_Search_Data_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

The map shows that Texas and California have the most searches from 2004-2017, although, Texas and California both also have two of the largest state populations in the U.S. Continuing with the analysis, we can compare search data within each region of the U.S.


```r
searches_by_region <- df %>%
  group_by(year, region) %>%
  summarize(searches = sum(searches))
ggplot(searches_by_region, aes(x = year, y = searches, group = 1)) +
  geom_line(col = "red", size = 1) +
  facet_wrap(~ region, ncol = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Health Search Volume by Year and Region", subtitle = "2004 - 2017")
```

![](Health_Search_Data_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

The graphs here show that the South searches for health conditions the most, which makes sense since [9 out of the top 10 sickest states are located in the South](http://blog.delimmune.com/2015/02/sickest-and-healthiest-states-in-the-us/). The least searches go to the Northeast region, which also makes sense, because 4 out of the top 10 healthiest states reside in the Northeast.

#### Correlation Between Health Conditions

The final item I wanted to take a look at was the relationships between each condition being searched. My thoughts were that people would think they would have a certain condition based on another condition they may believe to have (e.g. I am depressed because I have diabetes). To find these relationships, I will use a correlation matrix.


```r
df <- ungroup(df)
yc_spread <- df %>%
  spread(key = condition, value = searches)
yc_cor <- yc_spread %>%
  select(11:19) 
condition_cor <- cor(yc_cor, method = "pearson")
corrplot(condition_cor, method = "number", type = "upper", order = "hclust", tl.col = "black",tl.srt = 45)
```

![](Health_Search_Data_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

As can be seen in the matrix, all the blue color means every relationship between the variables have a positive correlation. The strongest relationship is between diabetes and depression, with a correlation of 0.75. [According to the American Diabetes Association](http://www.diabetes.org/living-with-diabetes/complications/mental-health/depression.html?referrer=https://www.google.com/), although most people with diabetes don't suffer from depression, studies show that people with diabetes are at greater risk of depression than people without diabetes. It could be possible that as someone searches for diabetes, they would also search for depression.

The other relationship that has a strong correlation above 0.7 is between diabetes and diarrhea. [About 22 percent of people with diabetes experience frequent diarrhea](https://www.healthline.com/health/diabetes/diabetes-and-diarrhea), although it is unclear what causes persistent diarrhea in people with diabetes. I believe it is reasonable to think that if someone had diabetes and was experiencing diarrhea, that they would search for these two terms.

### Conclusion

It appears what I thought would be seen in the data came out to be true.There has been an increase of search for health issues every year except for one. Also, the region which is considered the "sickest" (South) has the most searches for health issues, while the region considered the "healthiest" (Northeast) has the least searches.

What is missing with this data, though, is if the individual people searching for these issues are also sick with the same issues. It would be interesting to see how the spread of these health issues relate to the search of these health issues.
