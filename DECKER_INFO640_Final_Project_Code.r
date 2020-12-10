#DECKER_INFO640_Final_Project_Code
#John Decker
#INFO 640
#Prof. McSweeney
#Fall, 2020
#Pratt Institute

#R-code
#load libraries
library(lubridate)
library(tidyverse)
library(readxl)
library(gmodels)
library(dplyr)
library(ggthemes)

#load congress information
congress <- read.csv("~/Documents/GitHub/R_work/data/Congressional_Majorities_1970_2020.csv")
head(congress)

#read in dataset with NEA data
nea_funding_data <- read.csv("~/Documents/GitHub/R_work/data/State_Arts_Funding_w_NEA_data_and_Majority.csv")
names(nea_funding_data)

#strip dollar signs from dollar amount columns
nea_funding_data$Total.Legislative.Appropriations <- gsub("\\$", "", nea_funding_data$Total.Legislative.Appropriations)
nea_funding_data$Total.Other.State.funding <- gsub("\\$", "", nea_funding_data$Total.Other.State.funding)
nea_funding_data$Total.National.Endowment.for.the.Arts.Funding <- gsub("\\$", "", nea_funding_data$Total.National.Endowment.for.the.Arts.Funding)
nea_funding_data$Total.Private.and.Misc..Funding <- gsub("\\$", "", nea_funding_data$Total.Private.and.Misc..Funding)
nea_funding_data$Total.Revenue <- gsub("\\$", "", nea_funding_data$Total.Revenue)

#strip commas from dollar amount columns
nea_funding_data$Total.Legislative.Appropriations <- gsub(",", "", nea_funding_data$Total.Legislative.Appropriations)
nea_funding_data$Total.Other.State.funding <- gsub(",","", nea_funding_data$Total.Other.State.funding)
nea_funding_data$Total.National.Endowment.for.the.Arts.Funding <- gsub(",", "", nea_funding_data$Total.National.Endowment.for.the.Arts.Funding)
nea_funding_data$Total.Private.and.Misc..Funding <- gsub(",", "", nea_funding_data$Total.Private.and.Misc..Funding)
nea_funding_data$Total.Revenue <- gsub(",", "", nea_funding_data$Total.Revenue)

#convert dollar amount columns to numeric
nea_funding_data$Total.Legislative.Appropriations <- as.numeric(nea_funding_data$Total.Legislative.Appropriations, rm.na = TRUE) 
nea_funding_data$Total.Other.State.funding <- as.numeric(nea_funding_data$Total.Other.State.funding, rm.na = TRUE)
nea_funding_data$Total.National.Endowment.for.the.Arts.Funding <- as.numeric(nea_funding_data$Total.National.Endowment.for.the.Arts.Funding, rm.na = TRUE)
nea_funding_data$Total.Private.and.Misc..Funding <- as.numeric(nea_funding_data$Total.Private.and.Misc..Funding, rm.na=TRUE)
nea_funding_data$Total.Revenue <- as.numeric(nea_funding_data$Total.Revenue, rm.na=TRUE)

#initial analysis of funding data
summary(nea_funding_data)

#separate out states with high, medium, and low state-wide arts funding
nea_just_cali <- nea_funding_data %>% filter(State.or.Jurisdiction == "California")
nea_just_tx <- nea_funding_data %>% filter(State.or.Jurisdiction == "Texas")
nea_just_ny <- nea_funding_data %>% filter(State.or.Jurisdiction == "New York")
nea_just_il <- nea_funding_data %>% filter(State.or.Jurisdiction == "Illinois")
nea_just_ak <- nea_funding_data %>% filter(State.or.Jurisdiction == "Alaska")
nea_just_nv <- nea_funding_data %>% filter(State.or.Jurisdiction == "Nevada")

#calculate mean for all of the test states
total_test_state_mean <-(mean(nea_just_cali$Total.National.Endowment.for.the.Arts.Funding) + mean(nea_just_tx$Total.National.Endowment.for.the.Arts.Funding) + mean(nea_just_ny$Total.National.Endowment.for.the.Arts.Funding) + mean(nea_just_il$Total.National.Endowment.for.the.Arts.Funding) + mean(nea_just_ak$Total.National.Endowment.for.the.Arts.Funding) + mean(nea_just_nv$Total.National.Endowment.for.the.Arts.Funding))/6

#t test for all states in unadjusted dollars
t.test(nea_funding_data$Total.National.Endowment.for.the.Arts.Funding~nea_funding_data$Congress.Majority)

#subset all state data for culture wars period
culture_wars_states_rep_maj <- nea_funding_data %>% filter(Fiscal.Year >= 1995)

#t test for all states in unadjusted dollars from 1995 to 2020
t.test(culture_wars_states_rep_maj$Total.National.Endowment.for.the.Arts.Funding~culture_wars_states_rep_maj$Congress.Majority)

# move to dataset with inflation adjusted dollars to refine testing
nea_adjusted_data <-read.csv("~/Documents/GitHub/R_work/data/nea_adjusted_to_2020_w_majority_and_gdp.csv") 
head(nea_adjusted_data)
#t test for aggregated appropriations in unadjusted dollars
t.test(nea_adjusted_data$Total.NEA.Appropriation~nea_adjusted_data$Congress.Majority)

#t test for aggregated appropriations in adjusted dollars
t.test(nea_adjusted_data$In.2020.Dollars~nea_adjusted_data$Congress.Majority)

#t test for culture wars period in unadjusted dollars
culture_wars <- nea_adjusted_data %>% filter(Fiscal.Year >= 1995)
t.test(culture_wars$Total.NEA.Appropriation~culture_wars$Congress.Majority)

#t test for culture wars period in adjusted dollars
culture_wars <- nea_adjusted_data %>% filter(Fiscal.Year >= 1995)
t.test(culture_wars$In.2020.Dollars~culture_wars$Congress.Majority)

nea_adjusted_data_national <- read.csv("~/Documents/GitHub/R_work/data/national_NEA_appropriations_w_Majority.csv")
head(nea_adjusted_data_national)

#examine national appropriations to test hypothesis
#t test for all states in unadjusted dollars (Congress)
t.test(nea_adjusted_data_national$National.NEA.Appropriation~nea_adjusted_data_national$Congress.Majority)

#examine national appropriations to test hypothesis
#t test for all states in 2020 dollars (House)
t.test(nea_adjusted_data_national$Adjusted.National.NEA.Appropriation~nea_adjusted_data_national$Congress.Majority)

#t test for culture wars period in unadjusted dollars for national appropriations (House)
culture_wars_2 <- nea_adjusted_data_national %>% filter(Fiscal.Year >= 1995)
t.test(culture_wars_2$National.NEA.Appropriation~culture_wars_2$Congress.Majority)

#t test for culture wars period in 2020 dollars for national appropriations (House)
culture_wars_2 <- nea_adjusted_data_national %>% filter(Fiscal.Year >= 1995)
t.test(culture_wars_2$Adjusted.National.NEA.Appropriation~culture_wars_2$Congress.Majority)

# Discover for why national nea appropriations show strong congressional effect but weak economic effect and state nea appropriations show the opposite

# linear models for funding by larger economy
funding_lm_just_GDP <- lm(formula = Total.NEA.Appropriation ~ GDP.Per.Capita, data = nea_adjusted_data)

funding_lm_just_GrowthRate <- lm(formula = Total.NEA.Appropriation ~ Growth.Rate, data = nea_adjusted_data)

# multivariable linear model for funding by larger economy
funding_lm_GDP_and_GrowthRate <- lm(formula = Total.NEA.Appropriation ~ Growth.Rate + GDP.Per.Capita, data = nea_adjusted_data)

# multivariable linear model for funding by larger economy with inflation
funding_lm_GDP_GrowthRate_Inflation <- lm(formula = Total.NEA.Appropriation ~ Growth.Rate + GDP.Per.Capita + Avg.Yearly.Inflation, data = nea_adjusted_data)

#report statistics for each model
summary(funding_lm_just_GDP)
summary(funding_lm_just_GrowthRate)
summary(funding_lm_GDP_and_GrowthRate)
summary(funding_lm_GDP_GrowthRate_Inflation)

#use best linear model for test prediction
unknown_funding <- data.frame(Growth.Rate = 3.62, GDP.Per.Capita = 47975.97, Avg.Yearly.Inflation = 2.8)
predict(funding_lm_GDP_GrowthRate_Inflation, unknown_funding)

#code for figures used in paper
#Figure 1
ggplot(nea_adjusted_data, aes(x=Fiscal.Year, y=Total.NEA.Appropriation, color=Congress.Majority)) + geom_line() + scale_y_continuous(labels = scales::comma) +

geom_hline(yintercept = mean(nea_adjusted_data$Total.NEA.Appropriation), color = "dark green", linetype="dashed") +
  
theme_tufte() +
labs(title = "Total State NEA Funding per Year (in unadjusted dollars)",
     x = "Fiscal Year",
     y = "Dollar Amount",
     tag = "Figure 1",
     caption = "Light Blue = Democratic Majority \nDark Blue = Republican Majority \nGreen = Mean Funding Level") +
theme(legend.position = "none") +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.caption = element_text(hjust = 0.0)) +
theme(plot.tag = element_text(size = 10))

#ggsave("Figure1.jpg", width = 6, height = 4)

#Figure 2
ggplot(culture_wars, aes(x=Fiscal.Year, y=Total.NEA.Appropriation, color=Congress.Majority)) + geom_line() + scale_y_continuous(labels = scales::comma) +

geom_hline(yintercept = mean(culture_wars$Total.NEA.Appropriation), color = "dark green", linetype="dashed") +
  
theme_tufte() +
labs(title = "State NEA Funding per Year 1995-2020 (in unadjusted dollars)",
     x = "Fiscal Year",
     y = "Dollar Amount",
     tag = "Figure 2",
     caption = "Light Blue = Democratic Majority \nDark Blue = Republican Majority \nGreen = Mean Funding Level") +
theme(legend.position = "none") +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.caption = element_text(hjust = 0.0)) +
theme(plot.tag = element_text(size = 10))

#ggsave("Figure2.jpg", width = 6, height = 4)

#Figure 3
ggplot(nea_adjusted_data, aes(x=Fiscal.Year, y=In.2020.Dollars, color=Congress.Majority)) + geom_line() + scale_y_continuous(labels = scales::comma) +

geom_hline(yintercept = mean(nea_adjusted_data$In.2020.Dollars), color = "dark green", linetype="dashed") +

theme_tufte() +
labs(title = "Total State NEA Funding per Year (in 2020 Dollars)",
     x = "Fiscal Year",
     y = "Dollar Amount",
     tag = "Figure 3",
     caption = "Light Blue = Democratic Majority \nDark Blue = Republican Majority \nGreen = Mean Funding Level") +
theme(legend.position = "none") +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.caption = element_text(hjust = 0.0)) +
theme(plot.tag = element_text(size = 10))

#ggsave("Figure3.jpg", width = 6, height = 4)

#Figure 4
ggplot(culture_wars, aes(x=Fiscal.Year, y=In.2020.Dollars, color=Congress.Majority)) + geom_line() + scale_y_continuous(labels = scales::comma) +

geom_hline(yintercept = mean(nea_adjusted_data$In.2020.Dollars), color = "dark green", linetype="dashed") +

theme_tufte() +
labs(title = "State NEA Funding 1995-2020 (in 2020 dollars)",
     x = "Fiscal Year",
     y = "Dollar Amount",
     tag = "Figure 4",
     caption = "Light Blue = Democratic Majority \nDark Blue = Republican Majority \nGreen = Mean Funding Level (in 2020 dollars)") +
theme(legend.position = "none") +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.caption = element_text(hjust = 0.0)) +
theme(plot.tag = element_text(size = 10))

#ggsave("Figure4.jpg", width = 6, height = 4)

#Figure 5
ggplot(nea_adjusted_data_national, aes(x=Fiscal.Year, y=National.NEA.Appropriation, color=Congress.Majority)) + geom_line() + 
  scale_y_continuous(labels = scales::comma) +

geom_hline(yintercept = mean(nea_adjusted_data_national$National.NEA.Appropriation), color = "dark green", linetype="dashed") +

theme_tufte() +
labs(title = "National NEA Funding per Year (in unadjusted dollars)",
     x = "Fiscal Year",
     y = "Dollar Amount",
     tag = "Figure 5",
     caption = "Light Blue = Democratic Majority \nDark Blue = Republican Majority \nHorizontal Green Line = Mean of National NEA Appropriations") +
theme(legend.position = "none") +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.caption = element_text(hjust = 0.0)) +
theme(plot.tag = element_text(size = 10))

#ggsave("Figure5.jpg", width = 6, height = 4)

#Figure 6
ggplot(culture_wars_2, aes(x=Fiscal.Year, y=National.NEA.Appropriation, color=Congress.Majority)) + geom_line() + 
  scale_y_continuous(labels = scales::comma) +

geom_hline(yintercept = mean(culture_wars_2$National.NEA.Appropriation), color = " dark green", linetype="dashed") +

theme_tufte() +
labs(title = "National NEA Funding 1995-2020 (in unadjusted dollars)",
     x = "Fiscal Year",
     y = "Dollar Amount",
     tag = "Figure 6",
     caption = "Light Blue = Democratic Majority \nDark Blue = Republican Majority \nHorizontal Green Line = Mean of National NEA Appropriations") +
theme(legend.position = "none") +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.caption = element_text(hjust = 0.0)) +
theme(plot.tag = element_text(size = 10))

#ggsave("Figure6.jpg", width = 6, height = 4)

#Figure 7
ggplot(nea_adjusted_data_national, aes(x=Fiscal.Year, y=Adjusted.National.NEA.Appropriation, color=Congress.Majority)) + geom_line() +
  scale_y_continuous(labels = scales::comma) +

geom_hline(yintercept = mean(nea_adjusted_data_national$Adjusted.National.NEA.Appropriation), color = "green", linetype="dashed") +

theme_tufte() +
labs(title = "National NEA Funding per Year (in 2020 dollars)",
     x = "Fiscal Year",
     y = "Dollar Amount",
     tag = "Figure 7",
     caption = "Light Blue = Democratic Majority \nDark Blue = Republican Majority \nHorizontal Green Line = Mean of National NEA Appropriations") +
theme(legend.position = "none") +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.caption = element_text(hjust = 0.0)) +
theme(plot.tag = element_text(size = 10))

#ggsave("Figure7.jpg", width = 6, height = 4)

#Figure 8
ggplot(culture_wars_2, aes(x=Fiscal.Year, y=Adjusted.National.NEA.Appropriation, color=Congress.Majority)) + geom_line() +
  scale_y_continuous(labels = scales::comma) +

geom_hline(yintercept = mean(culture_wars_2$Adjusted.National.NEA.Appropriation), color = "green", linetype="dashed") +

theme_tufte() +
labs(title = "National NEA Funding 1995-2020 (in 2020 dollars)",
     x = "Fiscal Year",
     y = "Dollar Amount",
     tag = "Figure 8",
     caption = "Light Blue = Democratic Majority \nDark Blue = Republican Majority \nHorizontal Green Line = Mean of National NEA Appropriations") +
theme(legend.position = "none") +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.caption = element_text(hjust = 0.0)) +
theme(plot.tag = element_text(size = 10))

#ggsave("Figure8.jpg", width = 6, height = 4)


#Figure 9
#plot several states with vertical lines showing majority change in congress
ggplot(nea_just_cali, aes(x=Fiscal.Year, y=Total.National.Endowment.for.the.Arts.Funding, color = State.or.Jurisdiction)) + geom_line() + scale_y_continuous(labels = scales::comma) +
geom_line(data = nea_just_tx, aes(x=Fiscal.Year, y=Total.National.Endowment.for.the.Arts.Funding, color = State.or.Jurisdiction)) +
geom_line(data = nea_just_ny, aes(x=Fiscal.Year, y=Total.National.Endowment.for.the.Arts.Funding, color = State.or.Jurisdiction)) +
geom_line(data = nea_just_il, aes(x=Fiscal.Year, y=Total.National.Endowment.for.the.Arts.Funding, color = State.or.Jurisdiction)) +
geom_line(data = nea_just_ak, aes(x=Fiscal.Year, y=Total.National.Endowment.for.the.Arts.Funding, color = State.or.Jurisdiction)) +
geom_line(data = nea_just_nv, aes(x=Fiscal.Year, y=Total.National.Endowment.for.the.Arts.Funding, color = State.or.Jurisdiction)) + 

#vertical lines for congressional majorities (red = repub, blue = dem) 
geom_vline(xintercept = 1995, color = "red", linetype = "dotted") +
geom_vline(xintercept = 2007, color = "blue", linetype = "dotted") +
geom_vline(xintercept = 2011, color = "red", linetype = "dotted") +
geom_vline(xintercept = 2019, color = "blue", linetype = "dotted") +

#mean of funding for all states in sample  
geom_hline(yintercept = total_test_state_mean, linetype = "dashed", color = "brown") +

theme_tufte() +
labs(title="NEA Funding by State",
     x = "Fiscal Year",
     y = "NEA Funding Amount",
     color = "State",
     tag = "Figure 9",
     subtitle = "Source: National Assembly of State Arts Agencies, \nAnnual Appropriations and Revenue Survey Data",
     caption = "Red Vertical Lines = Start of Republican Majorty \nBlue Vertical Lines = Start of Democratic Majority\nBrown Horizontal Line = Mean Funding for all Test States") +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.subtitle = element_text(hjust = 0.5, size=10)) +
theme(plot.caption = element_text(hjust = 0.0, size=6)) +
theme(plot.tag = element_text(size = 10))

#ggsave("Figure9.jpg", width = 6, height = 4)

#Figure 10
#plot several states with vertical lines showing majority change in congress
ggplot(nea_just_cali, aes(x=Fiscal.Year, y=Total.National.Endowment.for.the.Arts.Funding, color = State.or.Jurisdiction)) + geom_line() + scale_y_continuous(labels = scales::comma) +
geom_line(data = nea_just_tx, aes(x=Fiscal.Year, y=Total.National.Endowment.for.the.Arts.Funding, color = State.or.Jurisdiction)) +
geom_line(data = nea_just_ny, aes(x=Fiscal.Year, y=Total.National.Endowment.for.the.Arts.Funding, color = State.or.Jurisdiction)) +
geom_line(data = nea_just_il, aes(x=Fiscal.Year, y=Total.National.Endowment.for.the.Arts.Funding, color = State.or.Jurisdiction)) +
geom_line(data = nea_just_ak, aes(x=Fiscal.Year, y=Total.National.Endowment.for.the.Arts.Funding, color = State.or.Jurisdiction)) +
geom_line(data = nea_just_nv, aes(x=Fiscal.Year, y=Total.National.Endowment.for.the.Arts.Funding, color = State.or.Jurisdiction)) + 

#Vertical lines for major economic events.
#1970s recession 1
geom_vline(xintercept = 1972, color = "gray") +
#1970s recession 2
geom_vline(xintercept = 1976, color = "gray") +
#1980s recession
geom_vline(xintercept = 1982, color = "gray") +
#black monday
geom_vline(xintercept = 1987, color = "gray") +
#1990-1991 recession
geom_vline(xintercept = 1991, color = "gray") +
#major recession tied to Asian Banking Crisis
geom_vline(xintercept = 1997, color = "gray") +
#dot com crash
geom_vline(xintercept = 1999, color = "gray") +
#september 11
geom_vline(xintercept = 2001, color = "gray") +
#2008 recession
geom_vline(xintercept = 2008, color = "gray") +
#global oil drop
geom_vline(xintercept = 2014, color = "gray") +
  
#mean of funding for all states in sample  
geom_hline(yintercept = total_test_state_mean, linetype = "dashed", color = "brown") +

theme_tufte() +
labs(title="NEA Funding by State",
     x = "Fiscal Year",
     y = "NEA Funding Amount",
     color = "State",
     tag = "Figure 10",
     subtitle = "Source: National Assembly of State Arts Agencies, \nAnnual Appropriations and Revenue Survey Data",
     caption = "Brown Horizontal Line = Mean Funding for all Test States \nGray Vertical Lines = Major Economic Events \n\t\tMajor Recession: 1970, Major Recession: 1976, Major Recession: 1982 \n\t\tBlack Monday: 1987, Major Recession: 1990-91, Asian Banking Crisis: 1997, Dot Com Crash: 1999 \n\t\t9/11: 2001, Great Recession: 2008, Global Oil Drop: 2014") +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.subtitle = element_text(hjust = 0.5, size=10)) +
theme(plot.caption = element_text(hjust = 0.0, size=6)) +
theme(plot.tag = element_text(size = 10))

#ggsave("Figure10.jpg", width = 6, height = 4)

#Figure 11

# scale amount used to adjust total dollars to amount in thousands of dollars so that both measures are the same scale
scale_amount = 1000

ggplot(nea_adjusted_data, aes(x=Fiscal.Year, y= GDP.Per.Capita)) + geom_line(color="blue") +
  geom_line(data=nea_adjusted_data, aes(x=Fiscal.Year, y=Total.NEA.Appropriation/scale_amount, color = "red")) +
  stat_smooth(method="lm", formula = y~x, linetype = "dashed", color = "brown", size = 0.3) +
  theme_tufte() +
  labs(title = "GDP Per Capita & State NEA Allocation by Year",
       subtitle = "NEA Scaled to GDP: (NEA/1000)",
       caption = "Blue = GDP Per Capita\nRed = NEA Appropriation\nBrown Dashed = Line of Best Fit",
       tag = "Figure 11",
       x = "Fiscal Year",
       y = "Dollar Amount") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5, size=10)) +
  theme(plot.caption = element_text(hjust = 0.0)) +
  theme(plot.tag = element_text(size = 10))

#ggsave("Figure11.jpg", width = 6, height = 4)

#Figure 12
scale_amount = 1000

ggplot(nea_adjusted_data, aes(x=Fiscal.Year, y=GDP.Per.Capita/Growth.Rate)) + geom_line(color = "blue") +
  geom_line(data=nea_adjusted_data, aes(x=Fiscal.Year, y=Total.NEA.Appropriation/(scale_amount*Growth.Rate), color = "red")) +
  stat_smooth(method="lm", formula = y~x, linetype = "dashed", color = "brown", size = 0.3) +
  
  theme_tufte() +
  labs(title = "GDP Per Capita & State NEA Allocation by Year & Growth Rate",
       subtitle = "Formula: GDP/Growth Rate ~= NEA/(1,000*Growth Rate)",
       caption = "Blue = GDP Per Capita\nRed = NEA Appropriation\nBrown Dashed = Line of Best Fit",
       tag = "Figure 12",
       x = "Fiscal Year",
       y = "Dollar Amount in Thousands of Dollars") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5, size=8)) +
  theme(plot.caption = element_text(hjust = 0.0)) +
  theme(plot.tag = element_text(size = 10))

#ggsave("Figure12.jpg", width = 6, height = 4)

#Figure 13
#shows GDP and funding in relation to economic health indicators, specifically Growth Rate and Inflation.

scale_amount = 1000

ggplot(nea_adjusted_data, aes(x=Fiscal.Year, y=(GDP.Per.Capita/Growth.Rate)*Avg.Yearly.Inflation)) + geom_line(color="blue") +
  geom_line(data=nea_adjusted_data, aes(x=Fiscal.Year, y=Total.NEA.Appropriation/(scale_amount*Growth.Rate)*Avg.Yearly.Inflation, color = "red")) +
  stat_smooth(method="lm", formula=y~x, linetype = "dashed", color = "brown", size = 0.3) +
  
  theme_tufte() +
  labs(title = "GDP Per Capita & State NEA Allocation by Year\nMapped by Growth Rate and Average Yearly Inflation",
       subtitle = "Formula: (GDP/Growth Rate)*Inflation ~= (NEA/(1,000*Growth Rate)*Inflation",
       caption = "Blue = GDP Per Capita\nRed = NEA Appropriation\nBrown Dashed = Line of Best Fit",
       tag = "Figure 13",
       x = "Fiscal Year",
       y = "Dollar Amount in Thousands of Dollars") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5, size=8)) +
  theme(plot.caption = element_text(hjust = 0.0)) +
  theme(plot.tag = element_text(size = 10))

#ggsave("Figure13.jpg", width = 6, height = 4)
