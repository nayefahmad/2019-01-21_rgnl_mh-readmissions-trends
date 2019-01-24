

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


# 1) read in data: -----------
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



# 2) plot trends: ----------
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



#************************************************************************
# 3) analysis for RHS: ------------
#************************************************************************
df2.rhs <- df1.readmit.rates %>% 
      filter(entity == "Richmond")  
      
ts1.rhs <- df2.rhs %>% 
      slice(1:21) %>%  # drop quarters after FY18 Q1
      pull(readmission_rate) %>% 
      ts(frequency = 4)

# ts1.rhs

m1.rhs <- auto.arima(ts1.rhs, seasonal = TRUE)

summary(m1.rhs)
resid(m1.rhs) %>% density() %>% plot  # looks a kinda weird but might be ok

forecast(m1.rhs, 4) %>% autoplot()


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

df2.1.rhs.fcast # %>% View()



#************************************************************************
# 4) analysis for Vancouver: ------------
#************************************************************************
df3.van <- df1.readmit.rates %>% 
      filter(entity == "Vancouver")  

ts2.van <- df3.van %>% 
      slice(1:21) %>%  # drop quarters after FY18 Q1
      pull(readmission_rate) %>% 
      ts(frequency = 4)

# ts1.rhs

m2.van <- auto.arima(ts2.van, 
                     seasonal = TRUE)

summary(m2.van)
resid(m2.van) %>% density() %>% plot  # looks a kinda weird but might be ok

forecast(m2.van, 4) %>% autoplot()


# bind old data and forecast together: -------
# df with lower forecast interval: 
van.low <- forecast(m2.van, 
                    h = 4) %>% 
      as.data.frame() %>% 
      pull('Lo 95')
van.low <- data.frame(low = c(rep(NA, 21), 
                              van.low))

# df with higher forecast interval: 
van.high <- forecast(m2.van, 
                     h = 4) %>% 
      as.data.frame() %>% 
      pull('Hi 95')
van.high <- data.frame(high = c(rep(NA, 21), 
                                van.high))

df3.1.van.fcast <- df3.van %>%
      bind_cols(van.low) %>% 
      set_names(c(names(df3.van), 
                  "low")) %>% 
      bind_cols(van.high)

df3.1.van.fcast  # %>% View()







#************************************************************************
# 5) analysis for PHC: ------------
#************************************************************************
df4.phc <- df1.readmit.rates %>% 
      filter(entity == "PHC")  

ts3.phc <- df4.phc %>% 
      slice(1:21) %>%  # drop quarters after FY18 Q1
      pull(readmission_rate) %>% 
      ts(frequency = 4)

# ts1.rhs

m3.phc <- auto.arima(ts3.phc, 
                     seasonal = TRUE)

summary(m3.phc)
resid(m3.phc) %>% density() %>% plot  # looks a kinda weird but might be ok

forecast(m3.phc, 4) %>% autoplot()


# bind old data and forecast together: -------
# df with lower forecast interval: 
phc.low <- forecast(m3.phc, 
                    h = 4) %>% 
      as.data.frame() %>% 
      pull('Lo 95')
phc.low <- data.frame(low = c(rep(NA, 21), 
                              phc.low))

# df with higher forecast interval: 
phc.high <- forecast(m3.phc, 
                     h = 4) %>% 
      as.data.frame() %>% 
      pull('Hi 95')
phc.high <- data.frame(high = c(rep(NA, 21), 
                                phc.high))

df4.1.phc.fcast <- df4.phc %>%
      bind_cols(phc.low) %>% 
      set_names(c(names(df4.phc), 
                  "low")) %>% 
      bind_cols(phc.high)

df4.1.phc.fcast  # %>% View()




#**************************************************************************
# 6) Bind all datasets together
#**************************************************************************
df5.all.areas <- rbind(df2.1.rhs.fcast, 
                       df3.1.van.fcast, 
                       df4.1.phc.fcast)




#**************************************************************************
# 7) Plot with forecast intervals  
#**************************************************************************
p2.fcast.intervals <- 
      
      df5.all.areas %>% 
      
      ggplot(aes(x = period, 
                 y = readmission_rate, 
                 group = entity)) + 
      
      geom_ribbon(aes(x = period, 
                      ymin = low, 
                      ymax = high), 
                  fill = "grey90") + 
      
      geom_line(aes(col = entity), 
                size = 1) + 
      
      geom_point(aes(col = entity)) + 
      
      facet_wrap(~entity, 
                 nrow = 3) + 
      
      scale_x_discrete(breaks = df1.readmit.rates$period[seq(1, 25, 4)]) + 
      
      labs(title = "Time series analysis of readmission rates, by CoC", 
           subtitle = "Grey areas indicate forecast intervals for last 3 quarters, based on all previous data. \nPoints outside the forecast intervals indicate values that cannot be explained by chance variation alone. \n\nRichmond : FY19 Q1 is outside the forecast interval \nVancouver: FY18 Q2 is outside the forecast interval \n", 
           caption = "Data source: VCH Balanced Scorecard") + 
      
      # highlight RHS point: 
      geom_point(data = df5.all.areas %>% filter(entity == "Richmond"), 
               aes(x = "FY2019-Q1", 
                   y = 0.04), 
               col = "red", 
               size = 2) +
      
      # highlight: Vancouver point: 
      geom_point(data = df5.all.areas %>% filter(entity == "Vancouver"), 
                 aes(x = "FY2018-Q2", 
                     y = 0.04), 
                 col = "red", 
                 size = 2) +
      
      theme_minimal(base_size = 12) + 
      theme(axis.text.x = element_text(angle = 45, 
                                       hjust = 1), 
            panel.border = element_rect(colour = "grey80", 
                                        fill = NA));p2.fcast.intervals







# write outputs:----------------------
write_csv(df5.all.areas,
          here("results", 
               "dst", 
               "2019-01-23_rhs-van-phc.csv"))

ggsave(here("results", 
            "dst", 
            "2019-01-23_fcasts-with-CIs.pdf"), 
       p2.fcast.intervals)


