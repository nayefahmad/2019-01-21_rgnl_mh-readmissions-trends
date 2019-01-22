

#*****************************************************************************
# Trends in readmission rates, by COC
# 2019-01-21
# Nayef 
# Jira: si-1767

#*****************************************************************************

library(tidyverse)
library(here)


# read in data: 
df1.readmit.rates <- read_csv(here("data",
                                   "2019-01-21_rgnl_readmisison-rates.csv")) %>% 
      mutate_if(is.character, 
                as.factor)

str(df1.readmit.rates)
summary(df1.readmit.rates)


