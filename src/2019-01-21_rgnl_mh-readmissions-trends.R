

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
library(magrittr)


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
df2.rhs <- df1.readmit.rates %>% 
      filter(entity == "Richmond")  
      
ts1.rhs <- df2.rhs %>% 
      slice(1:21) %>%  # drop quarters after FY18 Q1
      pull(readmission_rate) %>% 
      ts(frequency = 4)

# ts1.rhs

m1.rhs <- tslm(ts1.rhs ~ trend + season)

summary(m1.rhs)
resid(m1.rhs) %>% density() %>% plot  # looks a kinda weird but might be ok



# bind old data and forecast together: -------
# df with lower forecast interval: 
rhs.low <- forecast(m1.rhs, 
                      h = 4) %>% 
      as.data.frame() %>% 
      pull('Lo 95')
rhs.low <- data.frame(low = c(rep(NA, 21), 
                              rhs.low))

# df with higher forecast interval: 
rhs.high <- forecast(m1.rhs, 
                    h = 4) %>% 
      as.data.frame() %>% 
      pull('Hi 95')
rhs.high <- data.frame(high = c(rep(NA, 21), 
             rhs.high))

df2.1.rhs.fcast <- df2.rhs %>%
      bind_cols(rhs.low) %>% 
      set_names(c(names(df2.rhs), 
                        "low")) %>% 
      bind_cols(rhs.high)

df2.1.rhs.fcast %>% View()






# write outputs:----------------------
write_csv(df2.1.rhs.fcast,
          here("results", 
               "dst", 
               "2019-01-23_rhs.csv"))



