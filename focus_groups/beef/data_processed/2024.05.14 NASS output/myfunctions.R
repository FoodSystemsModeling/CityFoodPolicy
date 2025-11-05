

# This document contains functions I have written that will be helpful throughout 
# many different modeling contexts.

## Create metadata 
meta_select_vars <- function(df){
  m <- df %>% 
    select(all_of(varlist)) %>% 
    mutate(across(everything(), 
                  ~as.character(.))) %>%
    pivot_longer(
      cols = -cols_remove, 
      names_to = "variable_name", 
      values_to = "value") %>%
    group_by(year, variable_name) %>% 
    count() %>% 
    select(-n)
}

## Summary statistics with disclosure review data in the format requested by NASS

### Total for one data frame grouped by one variable
summary_total <- function(df){

## Total by groupby
  sum <- df$variables %>% 
    group_by(groupbyvar) %>%
    summarise(across(all_of(varlist), 
              ~ sum(. * weight, na.rm = TRUE))) %>%
    pivot_longer(
      cols = -groupbyvar, 
      names_to = "variable_name", 
      values_to = "value")
  
## Add weighted and unweighted n 
  n <- df$variables %>% 
    group_by(groupbyvar) %>%
    summarise(weighted_n = sum(weight), 
              unweighted_n = n())

## Bind columns 
  sum <- sum %>% left_join(n)
  
## Add statistic 
  sum <- sum %>% 
    mutate(
      statistic = "total")

# Get top 2 observations by scale for each expense category
  dis <- df$variables %>% 
    select(groupbyvar, all_of(varlist)) %>%
    pivot_longer(
      cols = -groupbyvar, 
      names_to = "variable_name", 
      values_to = "value") %>% 
    group_by(groupbyvar, variable_name) %>% 
    arrange(desc(value)) %>% 
    top_n(2) %>% 
    arrange(groupbyvar, variable_name) 
  
# Make data wider so there is a column for second largest and largest obs. First need to add the column that says first and second largest
  dis <- dis %>% 
    group_by(groupbyvar, variable_name) %>% 
    mutate(
      top2 = rank(desc(value), ties.method = "first")) %>%
    filter(
      top2<=2) %>% 
    pivot_wider(
      names_from = top2, 
      values_from = value, 
      names_glue = "ob_{top2}") %>% 
    rename(largest_obs = ob_1, 
           second_largest_obs = ob_2)
  
# Join with original data frame 
  sum <- sum %>% 
    left_join(dis)
  
# Arrange columns in correct order
  sum <- sum %>% 
    select(groupbyvar, variable_name, statistic, everything())
  
  return(sum)
}

### Mean, se and n for one data frame, grouped by one variable

#Create a list of variables in "varlist". 
#The group by variable is called "groupbyvar".

# Define function with input of df
mean_se_median <- function(df) {
  
  ## Summary stats by local and nonlocal
  # get mean, se, and median 
  sum <- df %>% 
    group_by(groupbyvar) %>%
    summarise(across(c(all_of(varlist)), 
                     list(mean = ~survey_mean(., na.rm = TRUE), 
                          median = ~survey_median(., na.rm = TRUE)))) %>%
    pivot_longer(
      cols = -groupbyvar, 
      names_to = "variable_name", 
      values_to = "value") %>% 
    ungroup() 
  
  # Remove median_se
  sum <- sum %>% 
    filter(!str_detect(variable_name, "_median_se"))
  
  # Add a column with the statistic 
  sum <- sum %>% 
    mutate(
      statistic = case_when(
        str_detect(variable_name, "se$") ~ "se",
        str_detect(variable_name, "mean$") ~ "mean",
        str_detect(variable_name, "median$") ~ "median"), 
      variable_name = str_remove_all(variable_name, 
                                     "_mean|_se|_median"))
  
  # Get weigthted N
  n <- df %>% 
    group_by(groupbyvar) %>% 
    survey_tally() %>% select(-n_se) %>% 
    rename(weighted_n = n)
  
  # Get unweighed n
  unweighted_n <- df$variables %>% 
    group_by(groupbyvar) %>% 
    summarise(unweighted_n = n())
  
  # Join data frames
  sum <- left_join(sum, n) %>% 
    left_join(unweighted_n) %>%
    arrange(groupbyvar, statistic) 
  
  # Get top 2 observations for each variables by groupbyvar
  dis <- df$variables %>% 
    select(groupbyvar, all_of(varlist)) %>%
    pivot_longer(
      cols = -groupbyvar, 
      names_to = "variable_name", 
      values_to = "value") %>% 
    group_by(groupbyvar, variable_name) %>% 
    arrange(desc(value)) %>% 
    top_n(2) %>% 
    arrange(groupbyvar, variable_name) 
  
  # Make data wider so there is a column for second largest and largest obs. First need to add the column that says first and second largest
  dis <- dis %>% 
    group_by(groupbyvar, variable_name) %>% 
    mutate(
      top2 = rank(desc(value), ties.method = "first")) %>%
    filter(
      top2<=2) %>% 
    pivot_wider(
      names_from = top2, 
      values_from = value, 
      names_glue = "ob_{top2}") %>% 
    rename(largest_obs = ob_1, 
           second_largest_obs = ob_2)
  
  # Join with original data frame 
  sum <- sum %>% left_join(dis)
  
  # Arrange columns in correct order
  sum <- sum %>% 
    select(groupbyvar, variable_name, statistic, 
           value, weighted_n, unweighted_n, everything())
  
  return(sum)
} 

### T-tests using linear regression 
# xvar is the variable that defines the two groups you are comparing (e.g., xvar = "local")
# yvar is a vector of variables that you would like to t-test for the two groups (e.g., yvar = c("beginning", "young"))
# df is the dataframe of interest
# to map function into a table of results /results <- map_dfr(yvar, ~ttest_lm(.x, df))/
ttest_lm <- function(yvar, df){
  m <- tidy(lm(paste(yvar, " ~ ", xvar),
               data = df, 
               weight = weight)) %>%
    select(term, p.value) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      term = yvar) %>%
    rename(variable_name = term)
  return(m)
}

### T-test using linear regression on a grouped data frame 
# xvar is the variable that defines the two groups you are comparing (e.g., xvar = "local")
# yvar is a vector of variables that you would like to t-test for the two groups (e.g., yvar = c("beginning", "young"))
# groupbyvar is the variable you would like to group by to perform analysis for each group
# df is the dataframe of interest
#to map function into a table of results /results <- map_dfr(yvar, ~ttest_grouped_lm(.x, df))/
ttest_grouped_lm <- function(yvar, df){
  m <- df %>% 
    select(all_of(varkeep)) %>%
    group_by(groupbyvar) %>% 
    nest() %>%
    mutate(
      models = map(data, ~ lm(paste(yvar, " ~ ", xvar),
                              weight = weight, 
                              data = .x)), 
      tidied = map(models, tidy)) %>% 
    select(-data, -models) %>%
    unnest(tidied) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      term = yvar) %>%
    rename(variable_name = term) %>% 
    select(-c(estimate, std.error, statistic))
  return(m)
}

### Total for one data frame grouped by two variables
summary_total2 <- function(df){
  
  ## Total by groupby
  sum <- df$variables %>% 
    group_by(groupbyvar, groupbyvar2) %>%
    summarise(across(all_of(varlist), 
                     ~ sum(. * weight, na.rm = TRUE))) %>%
    pivot_longer(
      cols = -c(groupbyvar, groupbyvar2), 
      names_to = "variable_name", 
      values_to = "value")
  
  ## Add weighted and unweighted n 
  n <- df$variables %>% 
    group_by(groupbyvar, groupbyvar2) %>%
    summarise(weighted_n = sum(weight), 
              unweighted_n = n())
  
  ## Bind columns 
  sum <- sum %>% left_join(n)
  
  ## Add statistic 
  sum <- sum %>% 
    mutate(
      statistic = "total")
  
  # Get top 2 observations by scale for each expense category
  dis <- df$variables %>% 
    select(groupbyvar, groupbyvar2, all_of(varlist)) %>%
    pivot_longer(
      cols = -c(groupbyvar, groupbyvar2),  
      names_to = "variable_name", 
      values_to = "value") %>% 
    group_by(groupbyvar, groupbyvar2, variable_name) %>% 
    arrange(desc(value)) %>% 
    top_n(2) %>% 
    arrange(groupbyvar, groupbyvar2, variable_name) 
  
  # Make data wider so there is a column for second largest and largest obs. First need to add the column that says first and second largest
  dis <- dis %>% 
    group_by(groupbyvar, groupbyvar2, variable_name) %>% 
    mutate(
      top2 = rank(desc(value), ties.method = "first")) %>%
    filter(
      top2<=2) %>% 
    pivot_wider(
      names_from = top2, 
      values_from = value, 
      names_glue = "ob_{top2}") %>% 
    rename(largest_obs = ob_1, 
           second_largest_obs = ob_2)
  
  # Join with original data frame 
  sum <- sum %>% 
    left_join(dis)
  
  # Arrange columns in correct order
  sum <- sum %>% 
    select(groupbyvar, groupbyvar2, variable_name, statistic, everything())
  
  return(sum)
}

### Mean, se and n for one data frame, grouped by two variables

#Create a list of variables in "varlist". 
#The group by variables are called "groupbyvar" and "groupbyvar2".

# Define function with input of df
mean_se_median2 <- function(df) {
  
  ## Summary stats by local and nonlocal
  # get mean, se, and median 
  sum <- df %>% 
    group_by(groupbyvar, groupbyvar2) %>%
    summarise(across(c(all_of(varlist)), 
                     list(mean = ~survey_mean(., na.rm = TRUE), 
                          median = ~survey_median(., na.rm = TRUE)))) %>%
    pivot_longer(
      cols = -c(groupbyvar, groupbyvar2), 
      names_to = "variable_name", 
      values_to = "value") %>% 
    ungroup() 
  
  # Remove median_se
  sum <- sum %>% 
    filter(!str_detect(variable_name, "_median_se"))
  
  # Add a column with the statistic 
  sum <- sum %>% 
    mutate(
      statistic = case_when(
        str_detect(variable_name, "se$") ~ "se",
        str_detect(variable_name, "mean$") ~ "mean",
        str_detect(variable_name, "median$") ~ "median"), 
      variable_name = str_remove_all(variable_name, 
                                     "_mean|_se|_median"))
  
  # Get weigthted N
  n <- df %>% 
    group_by(groupbyvar, groupbyvar2) %>% 
    survey_tally() %>% select(-n_se) %>% 
    rename(weighted_n = n)
  
  # Get unweighed n
  unweighted_n <- df$variables %>% 
    group_by(groupbyvar, groupbyvar2) %>% 
    summarise(unweighted_n = n())
  
  # Join data frames
  sum <- left_join(sum, n) %>% 
    left_join(unweighted_n) %>%
    arrange(groupbyvar, groupbyvar2, statistic) 
  
  # Get top 2 observations for each variables by groupbyvar
  dis <- df$variables %>% 
    select(groupbyvar, groupbyvar2, all_of(varlist)) %>%
    pivot_longer(
      cols = -c(groupbyvar, groupbyvar2), 
      names_to = "variable_name", 
      values_to = "value") %>% 
    group_by(groupbyvar, groupbyvar2, variable_name) %>% 
    arrange(desc(value)) %>% 
    top_n(2) %>% 
    arrange(groupbyvar, groupbyvar2, variable_name) 
  
  # Make data wider so there is a column for second largest and largest obs. First need to add the column that says first and second largest
  dis <- dis %>% 
    group_by(groupbyvar, groupbyvar2, variable_name) %>% 
    mutate(
      top2 = rank(desc(value), ties.method = "first")) %>%
    filter(
      top2<=2) %>% 
    pivot_wider(
      names_from = top2, 
      values_from = value, 
      names_glue = "ob_{top2}") %>% 
    rename(largest_obs = ob_1, 
           second_largest_obs = ob_2)
  
  # Join with original data frame 
  sum <- sum %>% left_join(dis)
  
  # Arrange columns in correct order
  sum <- sum %>% 
    select(groupbyvar, groupbyvar2, variable_name, statistic, 
           value, weighted_n, unweighted_n, everything())
  
  return(sum)
}

### Total for one data frame grouped by three variables
summary_total3 <- function(df){
  
  ## Total by groupby
  sum <- df$variables %>% 
    group_by(groupbyvar, groupbyvar2, groupbyvar3) %>%
    summarise(across(all_of(varlist), 
                     ~ sum(. * weight, na.rm = TRUE))) %>%
    pivot_longer(
      cols = -c(groupbyvar, groupbyvar2, groupbyvar3), 
      names_to = "variable_name", 
      values_to = "value")
  
  ## Add weighted and unweighted n 
  n <- df$variables %>% 
    group_by(groupbyvar, groupbyvar2, groupbyvar3) %>%
    summarise(weighted_n = sum(weight), 
              unweighted_n = n())
  
  ## Bind columns 
  sum <- sum %>% left_join(n)
  
  ## Add statistic 
  sum <- sum %>% 
    mutate(
      statistic = "total")
  
  # Get top 2 observations by scale for each expense category
  dis <- df$variables %>% 
    select(groupbyvar, groupbyvar2, groupbyvar3, all_of(varlist)) %>%
    pivot_longer(
      cols = -c(groupbyvar, groupbyvar2, groupbyvar3),  
      names_to = "variable_name", 
      values_to = "value") %>% 
    group_by(groupbyvar, groupbyvar2, groupbyvar3, variable_name) %>% 
    arrange(desc(value)) %>% 
    top_n(2) %>% 
    arrange(groupbyvar, groupbyvar2, groupbyvar3, variable_name) 
  
  # Make data wider so there is a column for second largest and largest obs. First need to add the column that says first and second largest
  dis <- dis %>% 
    group_by(groupbyvar, groupbyvar2, groupbyvar3, variable_name) %>% 
    mutate(
      top2 = rank(desc(value), ties.method = "first")) %>%
    filter(
      top2<=2) %>% 
    pivot_wider(
      names_from = top2, 
      values_from = value, 
      names_glue = "ob_{top2}") %>% 
    rename(largest_obs = ob_1, 
           second_largest_obs = ob_2)
  
  # Join with original data frame 
  sum <- sum %>% 
    left_join(dis)
  
  # Arrange columns in correct order
  sum <- sum %>% 
    select(groupbyvar, groupbyvar2, groupbyvar3, variable_name, statistic, everything())
  
  return(sum)
}

