## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----message=FALSE-------------------------------------------------------
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

## ----message=FALSE, warning=FALSE----------------------------------------
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

## ----message=FALSE, warning=FALSE----------------------------------------
head(df, 10)

## ----fig.width=12, fig.height=6, message=FALSE, warning=FALSE------------
searches_by_year <- df %>% 
  group_by(year) %>%
  summarize(searches = sum(searches)) %>%
  mutate(difference = searches - lag(searches, default = first(searches)))
ggplot(searches_by_year, aes(x = year, y = searches, group = 1)) +
  geom_line(col = "red", size = 1) +
  labs(title = "Health Search Volume by Year", subtitle = "2004 - 2017")

## ----fig.width=12, fig.height=6, message=FALSE---------------------------
searches_by_yc <- df %>%
  group_by(year, condition) %>%
  summarize(searches = sum(searches))
ggplot(searches_by_yc, aes(x = year, y = searches, group = condition, col = condition)) +
  geom_smooth(size = 1.5, se = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  labs(title = "Health Search Volume by Year and Condition", subtitle = "2004 - 2017") 

## ----fig.width=12, fig.height=6, message=FALSE---------------------------
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

## ----fig.width=12, fig.height=6, message=FALSE---------------------------
searches_by_region <- df %>%
  group_by(year, region) %>%
  summarize(searches = sum(searches))
ggplot(searches_by_region, aes(x = year, y = searches, group = 1)) +
  geom_line(col = "red", size = 1) +
  facet_wrap(~ region, ncol = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Health Search Volume by Year and Region", subtitle = "2004 - 2017")

## ----fig.width=12, fig.height=6, message=FALSE---------------------------
df <- ungroup(df)
yc_spread <- df %>%
  spread(key = condition, value = searches)
yc_cor <- yc_spread %>%
  select(11:19) 
condition_cor <- cor(yc_cor, method = "pearson")
corrplot(condition_cor, method = "number", type = "upper", order = "hclust", tl.col = "black",tl.srt = 45)

