

#*****************************************************************************
# Trends in readmission rates, by COC
# 2019-01-21
# Nayef 
# Jira: si-1767

#*****************************************************************************

library(tidyverse)
library(here)
library(janitor)
library(fpp)
library(broom)


# read in data: -----------
df1.readmit.rates <- read_csv(here("data",
                                   "2019-01-21_rgnl_readmisison-rates.csv")) %>% 
      mutate_if(is.character, 
                as.factor) %>% 
      clean_names() %>% 
      mutate(period = paste0(fiscal_year, 
                             "-", 
                             quarter))


str(df1.readmit.rates)
summary(df1.readmit.rates)



# plot trends: ----------
p1.trends <- 
      df1.readmit.rates %>% 
      ggplot(aes(x = period, 
                 y = readmission_rate, 
                 group = entity)) + 
      geom_line(aes(colour = entity, 
                    group = entity)) + 
      geom_point(aes(colour = entity)) + 
      
      facet_wrap(~entity) + 
      
      scale_x_discrete(breaks = df1.readmit.rates$period[seq(1, 25, 4)]) + 
      
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, 
                                       hjust = 1), 
            panel.border = element_rect(colour = "grey80", 
                                        fill = NA)); p1.trends 




# analysis for RHS: ------------
ts1.rhs <- df1.readmit.rates %>% 
      filter(entity == "Richmond") %>% 
      filter(!period %in% c("FY2017-Q2", 
                            "FY2017-Q3",
                            "FY2017-Q4",
                            "FY2018-Q1",
                            "FY2018-Q2",
                            "FY2018-Q3",
                            "FY2018-Q4",
                            "FY2017-Q2",
                            "FY2019-Q1")) %>% 
      pull(readmission_rate) %>% 
      ts(frequency = 4)

# ts1.rhs

m1.rhs <- tslm(ts1.rhs ~ season)

summary(m1.rhs)
resid(m1.rhs) %>% density() %>% plot  # looks decent 

df2.rhs.fitted <- augment(m1.rhs)





