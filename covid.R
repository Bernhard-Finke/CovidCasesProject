library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(RColorBrewer)
library(readxl)


setwd("WD")

# Section 5.1, 5.2: Case Summaries

# Work with cases data set for Germany:

# additional data by state (Germany)
population_thousands_de <- c(11100, 13125, 3669, 2552, 687, 1847, 6288, 1608, 7994, 17947, 4094, 987, 4072, 2195, 2904, 2133)
poverty_rate_de <- c(0.123, 0.119, 0.193, 0.152, 0.249, 0.150, 0.161, 0.194, 0.171, 0.185, 0.156, 0.170, 0.172, 0.195, 0.145, 0.170)
ex_gdr_de <- c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1)
pop_density_de <- c(293.9, 175.7, 3692.9, 83.3, 1552.5, 2260.1, 282.8, 69.4, 163.3, 514.5, 201, 389.2, 220.2, 111.8, 177.2, 135.3)
city_de <- c(0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
gdp_billions_de <- c(524.3, 632.9, 153.2, 74.3, 33.6, 123.3, 294.4, 46.6, 307.0, 711.4, 145.0, 36.2, 128.1, 63.5, 97.8, 63.9)
population_de <- sum(population_thousands_de)


# select relevant columns, rename to English, convert to date datatype and arrange in order of date and state
rki <- as_tibble(read.csv("RKI_COVID19.csv")) %>%
  select(Bundesland, Altersgruppe, Geschlecht, AnzahlFall, AnzahlTodesfall, Meldedatum) %>%
  rename('state'='Bundesland', 'age_group'='Altersgruppe', 'sex'='Geschlecht', 'cases'='AnzahlFall', 'deaths'='AnzahlTodesfall', 'date'='Meldedatum') %>%
  mutate(date = as.Date(date)) %>%
  filter(date <= as.Date("2020-12-31")) %>%
  arrange(date, state) %>%
  mutate(cases_per_thousand = cases / population_de)
rki$state<- recode(rki$state, 'Baden-Württemberg' = 'Baden-Wuerttemberg', 'Bayern' = 'Bavaria', 'Niedersachsen' = 'Lower Saxony', 'Nordrhein-Westfalen' = 'North Rhine-Westphalia',
                   'Rheinland-Pfalz' = 'Rhineland-Palatinate', 'Sachsen' = 'Saxony', 'Sachsen-Anhalt' = 'Saxony-Anhalt', 'Thüringen' = 'Thuringia')


# summary statistics for country
cases_de <- sum(rki$cases)
deaths_de <- sum(rki$deaths)
cases_de_per_thousand <- cases_de / population_de

# daily cases for country
daily_cases_de <- group_by(rki, date) %>%
  summarise(sum_cases = sum(cases)) %>%
  mutate(sum_cases_pop = sum_cases / population_de * 100) %>%
  mutate(rolling_cases_week = rollapplyr(sum_cases, width = 7, FUN = sum, partial = TRUE)) %>%
  mutate(rolling_cases_week_pop = rollapplyr(sum_cases_pop, width = 7, FUN = sum, partial = TRUE)) %>%
  mutate(cumulative_cases = cumsum(sum_cases)) %>%
  mutate(percentage_difference_cases = (rolling_cases_week - lag(rolling_cases_week, 7)) / lag(rolling_cases_week, 7)) %>%
  mutate(country = "DE")

max(daily_cases_de$sum_cases)

# plots

# sum_cases

fig1DE <- ggplot(daily_cases_de, aes(x=date, y=sum_cases)) + geom_line() +
  xlab("Date") + ylab("Cases") 

ggsave("fig1DE.png", scale=0.7)

# cumulative_cases

fig2DE <- ggplot(daily_cases_de, aes(x=date, y=cumulative_cases)) + geom_line() +
  xlab("Date") + ylab("Total Cases")

ggsave("fig2DE.png", scale=0.7)

# sum cases breakdown

fig3DE <- ggplot(daily_cases_de, aes(x=date, y=sum_cases)) + geom_line() + 
  xlab("Date") + ylab("Cases") + 
  scale_x_date(limits = as.Date(c("2020-02-06", "2020-05-06"))) + ylim(0, 7000)

ggsave("fig3DE.png", scale = 0.5)

fig4DE <- ggplot(daily_cases_de, aes(x=date, y=sum_cases)) + geom_line() + 
  xlab("Date") + ylab("Cases") + 
  scale_x_date(limits = as.Date(c("2020-05-01", "2020-08-01"))) + ylim(0, 1500)

ggsave("fig4DE.png", scale = 0.5)

fig5DE <- ggplot(daily_cases_de, aes(x=date, y=sum_cases)) + geom_line() + 
  xlab("Date") + ylab("Cases") + 
  scale_x_date(limits = as.Date(c("2020-08-01", "2020-12-31")))

ggsave("fig5DE.png", scale = 0.5)


# rolling_cases_week_pop

fig6DE <- ggplot(daily_cases_de, aes(x=date, y=rolling_cases_week_pop)) + geom_line() + 
  xlab("Date") + ylab("Rolling Cases per 100,000") +
  geom_vline(aes(xintercept=as.Date("2020-03-23")), colour="blue") + # First lockdown
  geom_vline(aes(xintercept=as.Date("2020-11-02")), colour="darkgreen") + # Second Lockdown
  geom_vline(aes(xintercept=as.Date("2020-12-01")), color="purple") + # Restrictions tightened
  geom_vline(aes(xintercept=as.Date("2020-12-16")), color="orange") + # Further restrictions
  geom_text(aes(label="First lockdown \nbegins (23rd March)", x=as.Date("2020-03-23"), y=100)) +
  geom_text(aes(label="Second 'partial'\nlockdown begins\n(2nd Nov.)", x=as.Date("2020-11-02"), y=200)) +
  geom_text(aes(label="Lockdown restrictions\ntightened (1st Dec.)", x=as.Date("2020-11-22"), y=80)) +
  geom_text(aes(label="Further restrictions\n(16th Dec.)"), x=as.Date("2020-11-28"), y=50)

ggsave("fig6DE.png", scale=1.1)

fig6aDE <- ggplot(daily_cases_de, aes(x=date, y=rolling_cases_week_pop)) + geom_line() + 
  xlab("Date") + ylab("Rolling Cases per 100,000") +
  scale_x_date(limits = as.Date(c("2020-03-10", "2020-05-01"))) + ylim(0, 50) +
  geom_smooth(data = daily_cases_de %>% filter(date %in% seq(from=as.Date("2020-03-27"), to=as.Date("2020-04-03"), by=1)), method="lm") +
  geom_smooth(data = daily_cases_de %>% filter(date %in% seq(from=as.Date("2020-04-03"), to=as.Date("2020-04-10"), by=1)), method="lm")

ggsave("fig6aDE.png", scale=0.5)

summary(lm(data = daily_cases_de %>% filter(date %in% seq(from=as.Date("2020-03-27"), to=as.Date("2020-04-03"), by=1)), rolling_cases_week_pop ~ date))
summary(lm(data = daily_cases_de %>% filter(date %in% seq(from=as.Date("2020-04-03"), to=as.Date("2020-04-10"), by=1)), rolling_cases_week_pop ~ date))

fig6bDE <- ggplot(daily_cases_de, aes(x=date, y=rolling_cases_week_pop)) + geom_line() + 
  xlab("Date") + ylab("Rolling Cases per 100,000") +
  scale_x_date(limits = as.Date(c("2020-11-01", "2020-12-31"))) + ylim(125, 220) + 
  geom_smooth(data = daily_cases_de %>% filter(date %in% seq(from=as.Date("2020-11-07"), to=as.Date("2020-11-14"), by=1)), method="lm") +
  geom_smooth(data = daily_cases_de %>% filter(date %in% seq(from=as.Date("2020-11-14"), to=as.Date("2020-11-21"), by=1)), method="lm") +
  geom_smooth(data = daily_cases_de %>% filter(date %in% seq(from=as.Date("2020-12-16"), to=as.Date("2020-12-23"), by=1)), method="lm") +
  geom_smooth(data = daily_cases_de %>% filter(date %in% seq(from=as.Date("2020-12-23"), to=as.Date("2020-12-30"), by=1)), method="lm") 

ggsave("fig6bDE.png", scale=0.5)

summary(lm(data = daily_cases_de %>% filter(date %in% seq(from=as.Date("2020-11-07"), to=as.Date("2020-11-14"), by=1)), rolling_cases_week_pop ~ date))
summary(lm(data = daily_cases_de %>% filter(date %in% seq(from=as.Date("2020-11-14"), to=as.Date("2020-11-21"), by=1)), rolling_cases_week_pop ~ date))
summary(lm(data = daily_cases_de %>% filter(date %in% seq(from=as.Date("2020-12-16"), to=as.Date("2020-12-23"), by=1)), rolling_cases_week_pop ~ date))
summary(lm(data = daily_cases_de %>% filter(date %in% seq(from=as.Date("2020-12-23"), to=as.Date("2020-12-30"), by=1)), rolling_cases_week_pop ~ date))

# percentage_difference_cases

# zoom in first few months, massive spike, rest

fig7DE <- ggplot(daily_cases_de, aes(x=date, y=percentage_difference_cases)) + geom_line() +
  xlab("Date") + ylab("Change in Cases (%)") +
  scale_x_date(limits = as.Date(c("2020-02-06", "2020-05-06"))) +
  geom_vline(aes(xintercept=as.Date("2020-03-23")), colour="blue") + # First lockdown
  geom_text(aes(label="1st Lockdown\n (23.03)", x=as.Date("2020-03-30"), y=20))


ggsave("fig7DE.png", scale = 0.5)

fig8DE <- ggplot(daily_cases_de, aes(x=date, y=percentage_difference_cases)) + geom_line() + 
  xlab("Date") + ylab("Change in Cases (%)") +
  scale_x_date(limits = as.Date(c("2020-05-01", "2020-08-01"))) + ylim(-1, 2)

ggsave("fig8DE.png", scale = 0.5)

fig9DE <- ggplot(daily_cases_de, aes(x=date, y=percentage_difference_cases)) + geom_line() + 
  xlab("Date") + ylab("Change in Cases (%)") +
  scale_x_date(limits = as.Date(c("2020-08-01", "2020-12-31"))) + ylim(-1, 1.5) + 
  geom_vline(aes(xintercept=as.Date("2020-11-02")), colour="darkgreen") + # Second Lockdown
  geom_vline(aes(xintercept=as.Date("2020-12-01")), color="purple") + # Restrictions tightened
  geom_vline(aes(xintercept=as.Date("2020-12-16")), color="orange") + # Further restrictions 
  geom_text(aes(label="2nd Lockdown \n(02.11)", x=as.Date("2020-11-02"), y=1.2)) +
  geom_text(aes(label="Restrictions\ntightened\n(01.12)", x=as.Date("2020-12-01"), y=-0.6)) + 
  geom_text(aes(label="Further\nrestrictions\n(16.12)", x=as.Date("2020-12-13"), y=0.8))

ggsave("fig9DE.png", scale = 0.5)





# UK data:

# additional data by region
pop_thousands_en <- c(4835, 6236, 8961, 2669, 7341, 9180, 5624, 5934, 5502)
poverty_en <- c(0.21, 0.20, 0.28, 0.24, 0.23, 0.19, 0.19, 0.24, 0.22)
pop_density_en <- c(310, 326, 5701, 311, 520, 481, 236, 457, 357)
gdp_billions_en <- c(124.6, 186.5, 487.1, 62.6, 207.5, 311.3, 158.1, 159.8, 141.7)
pop_en <- sum(pop_thousands_en)

# read uk data, arrange by date, remove NA from data
en <- as_tibble(read.csv("region_2021-01-18.csv")) %>%
  mutate(date = as.Date(date)) %>%
  filter(date <= as.Date("2020-12-31")) %>%
  arrange(date) %>%
  arrange(areaName) %>%
  rename('cases' = 'newCasesBySpecimenDate') %>%
  mutate(cases_per_thousand = cases / pop_en) %>%
  replace_na(list(cases = 0, deaths = 0))


# summary statistics for country
cases_en <- sum(en$cases)
cases_en_per_thousand <- cases_en / pop_en


# daily cases for country
daily_cases_en <- group_by(en, date) %>%
  summarise(sum_cases = sum(cases)) %>%
  mutate(sum_cases_pop = sum_cases / pop_en * 100) %>%
  mutate(rolling_cases_week = rollapplyr(sum_cases, width = 7, FUN = sum, partial = TRUE)) %>%
  mutate(rolling_cases_week_pop = rollapplyr(sum_cases_pop, width = 7, FUN = sum, partial = TRUE)) %>%
  mutate(cumulative_cases = cumsum(sum_cases)) %>%
  mutate(percentage_difference_cases = (rolling_cases_week - lag(rolling_cases_week, 7)) / lag(rolling_cases_week, 7)) %>%
  replace(is.na(.), 0) %>%
  mutate(country = "EN")

daily_cases_en$percentage_difference_cases <- recode(daily_cases_en$percentage_difference_cases, "Inf" = 0)

max(daily_cases_en$sum_cases)


# plots for england

# sum_cases

fig1EN <- ggplot(daily_cases_en, aes(x=date, y=sum_cases)) + geom_line() + xlab("Date") + ylab("Cases") +
  geom_vline(aes(xintercept=as.Date("2020-05-28")), color="red") +
  geom_text(aes(label="Test and Trace\n introduced (28th May)", x=as.Date("2020-05-28"), y=40000))

ggsave("fig1EN.png", scale=0.7)

# cumulative_cases

fig2EN <- ggplot(daily_cases_en, aes(x=date, y=cumulative_cases)) + geom_line() + 
  xlab("Date") + ylab("Total Cases") + geom_vline(aes(xintercept=as.Date("2020-05-28")), color="red") +
  geom_text(aes(label="Test and Trace\n introduced (28th May)", x=as.Date("2020-05-28"), y=1000000))

ggsave("fig2EN.png", scale=0.7)

# sum_cases zoom in

fig3EN <- ggplot(daily_cases_en, aes(x=date, y=sum_cases)) + geom_line() +
  xlab("Date") + ylab("Cases") +
  scale_x_date(limits = as.Date(c("2020-02-06", "2020-05-25"))) + ylim(0, 5000)

ggsave("fig3EN.png", scale = 0.5)

fig4EN <- ggplot(daily_cases_en, aes(x=date, y=sum_cases)) + geom_line() + 
  xlab("Date") + ylab("Cases") +
  scale_x_date(limits = as.Date(c("2020-05-20", "2020-09-01"))) + ylim(0, 3200) +
  geom_vline(aes(xintercept=as.Date("2020-05-28")), color="red") +
  geom_text(aes(label="Test and Trace\n introduced", x=as.Date("2020-06-12"), y=3000))

ggsave("fig4EN.png", scale = 0.5)

fig5EN <- ggplot(daily_cases_en, aes(x=date, y=sum_cases)) + geom_line() + 
  xlab("Date") + ylab("Cases") +
  scale_x_date(limits = as.Date(c("2020-08-15", "2020-12-31")))

ggsave("fig5EN.png", scale = 0.5)


# rolling_cases_week_pop

fig6EN <- ggplot(daily_cases_en, aes(x=date, y=rolling_cases_week_pop)) + geom_line() +
  xlab("Date") + ylab("Rolling Cases per 100,000") + 
  geom_vline(aes(xintercept=as.Date("2020-05-28")), color="red") +
  geom_text(aes(label="Test and Trace\n introduced (28th May)", x=as.Date("2020-05-28"), y=300)) + 
  geom_vline(aes(xintercept=as.Date("2020-09-20")), color="yellow") + # Kent variant
  geom_vline(aes(xintercept=as.Date("2020-03-23")), colour="blue") + # First lockdown
  geom_vline(aes(xintercept=as.Date("2020-11-05")), colour="darkgreen") + # Second Lockdown
  geom_text(aes(label="First lockdown \nbegins (23rd March)", x=as.Date("2020-03-23"), y=150)) + 
  geom_text(aes(label="Second lockdown \nbegins (5th Nov.)", x=as.Date("2020-11-05"), y=400)) + 
  geom_text(aes(label="Kent variant first \ndiscovered (20th Sep.)", x=as.Date("2020-09-20"), y=320))

ggsave("fig6EN.png", scale=1.1)

fig6aEN <- ggplot(daily_cases_en, aes(x=date, y=rolling_cases_week_pop)) + geom_line() + 
  xlab("Date") + ylab("Rolling Cases per 100,000") +
  scale_x_date(limits = as.Date(c("2020-03-15", "2020-05-16"))) + ylim(0, 50) +
  geom_smooth(data = daily_cases_en %>% filter(date %in% seq(from=as.Date("2020-04-02"), to=as.Date("2020-04-09"), by=1)), method="lm") +
  geom_smooth(data = daily_cases_en %>% filter(date %in% seq(from=as.Date("2020-04-09"), to=as.Date("2020-04-16"), by=1)), method="lm")

ggsave("fig6aEN.png", scale=0.5)

summary(lm(data = daily_cases_en %>% filter(date %in% seq(from=as.Date("2020-04-02"), to=as.Date("2020-04-09"), by=1)), rolling_cases_week_pop ~ date))
summary(lm(data = daily_cases_en %>% filter(date %in% seq(from=as.Date("2020-04-09"), to=as.Date("2020-04-16"), by=1)), rolling_cases_week_pop ~ date))

fig6bEN <- ggplot(daily_cases_en, aes(x=date, y=rolling_cases_week_pop)) + geom_line() + 
  xlab("Date") + ylab("Rolling Cases per 100,000") +
  scale_x_date(limits = as.Date(c("2020-10-25", "2020-12-03"))) + ylim(100, 300) +
  geom_smooth(data = daily_cases_en %>% filter(date %in% seq(from=as.Date("2020-11-06"), to=as.Date("2020-11-13"), by=1)), method="lm") +
  geom_smooth(data = daily_cases_en %>% filter(date %in% seq(from=as.Date("2020-11-13"), to=as.Date("2020-11-20"), by=1)), method="lm") 

ggsave("fig6bEN.png", scale=0.5)

summary(lm(data = daily_cases_en %>% filter(date %in% seq(from=as.Date("2020-11-06"), to=as.Date("2020-11-13"), by=1)), rolling_cases_week_pop ~ date))
summary(lm(data = daily_cases_en %>% filter(date %in% seq(from=as.Date("2020-11-13"), to=as.Date("2020-11-20"), by=1)), rolling_cases_week_pop ~ date))


# percentage_difference_cases

# zoom in first few months, massive spike, rest

fig7EN <- ggplot(daily_cases_en, aes(x=date, y=percentage_difference_cases)) + geom_line() + 
  xlab("Date") + ylab("Change in Cases (%)") + 
  scale_x_date(limits = as.Date(c("2020-02-06", "2020-05-25"))) + 
  geom_vline(aes(xintercept=as.Date("2020-03-23")), colour="blue") + # First lockdown
  geom_text(aes(label="1st lockdown \n(23.03)", x=as.Date("2020-03-26"), y=20)) 

ggsave("fig7EN.png", scale = 0.5)

fig8EN <- ggplot(daily_cases_en, aes(x=date, y=percentage_difference_cases)) + geom_line() + 
  xlab("Date") + ylab("Change in Cases (%)") + 
  scale_x_date(limits = as.Date(c("2020-05-20", "2020-09-01"))) + ylim(-1, 1) + geom_vline(aes(xintercept=as.Date("2020-05-28")), color="red") +
  geom_text(aes(label="Test and Trace\n introduced (28.05)", x=as.Date("2020-06-16"), y=0.5)) 

ggsave("fig8EN.png", scale = 0.5)

fig9EN <- ggplot(daily_cases_en, aes(x=date, y=percentage_difference_cases)) + geom_line() + 
  xlab("Date") + ylab("Change in Cases (%)") +
  scale_x_date(limits = as.Date(c("2020-08-15", "2020-12-31"))) + ylim(-1, 2) + 
  geom_vline(aes(xintercept=as.Date("2020-09-20")), color="yellow") + # Kent variant
  geom_vline(aes(xintercept=as.Date("2020-11-05")), colour="darkgreen") + # Second Lockdown
  geom_text(aes(label="2nd lockdown \n(05.11)", x=as.Date("2020-11-05"), y=1.2)) + 
  geom_text(aes(label="Kent variant\n detected (20.09)", x=as.Date("2020-09-20"), y=1.8))

ggsave("fig9EN.png", scale = 0.5)



# Compare sum_pop_cases by country
daily_cases_both <- full_join(daily_cases_de, daily_cases_en)
fig10 <- ggplot(daily_cases_both, aes(x=date, y=sum_cases_pop, group=country, color=country)) + geom_line() +
  xlab("Date") + ylab("Cases per 100,000") + theme(text = element_text(size = 16))

ggsave("fig10.png", width = 5.98*2)




# Section 5.3: Regional differences

# tibble containing all demographic data for germany

state_rki <- group_by(rki, state)
states_de <- cbind(summarise(state_rki, cases=sum(cases), deaths=sum(deaths)), population_thousands_de, poverty_rate_de, ex_gdr_de, pop_density_de, city_de, gdp_billions_de) %>%
  mutate(cases_per_capita = cases / population_thousands_de / 1000) %>%
  mutate(cases_per_thousand = cases / population_thousands_de) %>%
  mutate(gdp_per_capita = gdp_billions_de / population_thousands_de*1000000)

# correlation: between each factor and cases_per_capita, lm graphs for better fits
cor.test(states_de$cases_per_capita, states_de$poverty_rate_de)
cor.test(states_de$cases_per_capita, log(states_de$pop_density_de))
cor.test(states_de$cases_per_capita, states_de$gdp_billions_de)
cor.test(states_de$cases_per_capita, states_de$gdp_per_capita)
cor.test(states_de$cases_per_capita, states_de$ex_gdr_de)


t.test(states_de$cases_per_capita ~ states_de$ex_gdr_de)
t.test(states_de$cases_per_capita ~ states_de$city_de)

fig11DE <- ggplot(states_de, aes(x=reorder(state, cases_per_thousand), y=cases_per_thousand*100)) + geom_bar(stat="identity", fill="white", color="blue") + coord_flip() +
  ylab("Cases per 100,000") + xlab("State") 

ggsave("fig11DE.png", scale=0.4, width=21)


# tibble containing all demographic data for england

state_en <- group_by(en, areaName)
regions_en <- cbind(summarise(state_en, cases=sum(cases)), pop_thousands_en, poverty_en, pop_density_en, gdp_billions_en) %>%
  mutate(cases_per_thousand = cases / pop_thousands_en) %>%
  mutate(cases_per_capita = cases / pop_thousands_en / 1000) %>%
  mutate(gdp_per_capita = gdp_billions_en / pop_thousands_en*1000000)

# correlation: between each factor and cases_per_capita, lm graphs for better fits
cor.test(regions_en$cases_per_capita, regions_en$poverty_en)
cor.test(regions_en$cases_per_capita, log(regions_en$pop_density_en))
cor.test(regions_en$cases_per_capita, regions_en$gdp_billions_en)
cor.test(regions_en$cases_per_capita, regions_en$gdp_per_capita, method="spearman")

fig11EN <- ggplot(regions_en, aes(x=reorder(areaName, cases_per_thousand), y=cases_per_thousand*100)) + 
  geom_bar(stat="identity", fill="white", color="blue") + coord_flip() + xlab("Region") + ylab("Cases per 100,000")

ggsave("fig11EN.png", scale=0.7, width=10)



# Lower-level population density

# population density by district

rki2 <- read.csv("RKI_COVID19.csv", encoding="UTF-8") %>%
  group_by(Landkreis) %>%
  summarize(cases=sum(AnzahlFall))

kreise_de <- read_excel("04-kreise.xlsx", sheet="Kreisfreie St�dte u. Landkreise") %>%
  tail(-7) %>%
  select("...3", "...5", "...6", "...9") %>%
  rename('Landkreis' = '...3', 'Area (sq km)' = '...5', 'Population' = '...6', 'Pop_Density' = '...9') %>% 
  na.omit() %>%
  filter('Landkreis' != 'Insgesamt')

pop_dens_de <- left_join(rki2, kreise_de, by='Landkreis') %>%
  na.omit()
pop_dens_de$`Area (sq km)` <- as.numeric(pop_dens_de$`Area (sq km)`)
pop_dens_de$Population <- as.numeric(pop_dens_de$Population)
pop_dens_de$`Pop_Density` <- as.numeric(pop_dens_de$`Pop_Density`)

pop_dens_de <- mutate(pop_dens_de, cases_pop = cases/Population)

fig12DE <- ggplot(pop_dens_de, aes(x=log(Pop_Density), y=cases_pop)) + geom_point() + 
  xlab("Log(Population Density) (per sq. km)") + ylab("Cases per Capita Until 31st December 2020") + 
  theme(text = element_text(size = 18))
  

ggsave("fig12DE.png")

cor.test(log(pop_dens_de$Pop_Density), pop_dens_de$cases_pop)


# Population density by local area

uk_utla <- read.csv("utla_2021-02-13.csv") %>%
  mutate(date = as.Date(date)) %>%
  filter(date < as.Date("2021-01-01")) %>%
  group_by(areaName) %>%
  summarize(cases=sum(newCasesBySpecimenDate)) %>%
  rename("Name" = "areaName")

uk_pop_dens_utla <- read_excel('ukmidyearestimates20192020ladcodes.xls', sheet="MYE 5") %>%
  tail(-3) 
colnames(uk_pop_dens_utla) <- uk_pop_dens_utla[1,]

uk_pop_dens_utla <- uk_pop_dens_utla[-1, ] %>%
  select("Name", "Area (sq km)", "Estimated Population mid-2019") %>%
  na.omit() 

uk_pop_dens_utla[427, ] <- c("Hackney and City of London", 19+3, 281120+9721)
uk_pop_dens_utla[428, ] <- c("Cornwall and Isles of Scilly", 3545+16, 569578+2224)

uk_pop_dens_utla$"Estimated Population mid-2019" <- as.numeric(uk_pop_dens_utla$"Estimated Population mid-2019")
uk_pop_dens_utla$"Area (sq km)" <- as.numeric(uk_pop_dens_utla$"Area (sq km)")

uk_pop_dens_utla$populationDensity <- uk_pop_dens_utla$`Estimated Population mid-2019`/uk_pop_dens_utla$`Area (sq km)`

uk_utla_full <- left_join(uk_utla, uk_pop_dens_utla, by="Name")

uk_utla_full$casesPop <- uk_utla_full$cases / uk_utla_full$`Estimated Population mid-2019`

fig12EN <- ggplot(uk_utla_full, aes(x=log(populationDensity), y=casesPop)) + geom_point() + 
  xlab("Log(Population Density) (per sq. km)") + ylab("Cases per Capita Until 31st December 2020") + 
  theme(text = element_text(size = 18))

ggsave("fig12EN.png")

cor.test(uk_utla_full$casesPop, log(uk_utla_full$populationDensity))



# Section 5.4: Cases by demographics

# daily cases by sex
daily_sex_cases_de <- group_by(rki, date, sex) %>%
  summarise(sum_cases = sum(cases)) %>%
  spread(sex, sum_cases) %>%
  rename('F' = 'W', 'unknown' = 'unbekannt') %>%
  replace(is.na(.), 0) %>%
  mutate(m_greater = M - F) %>%
  gather('M', 'F', 'unknown', 'm_greater', key='sex', value='sum_cases') 

unknown <- daily_sex_cases_de %>%
  filter(sex=="unknown")
sum(unknown$sum_cases) / sum(daily_sex_cases_de$sum_cases) # what percentage of cases have unknown sex

t_test_sex <- daily_sex_cases_de %>%
  filter(sex=="F" | sex=="M") 
t.test(t_test_sex$sum_cases~t_test_sex$sex, alternative = "two.sided", var.equal=FALSE)

(2637.683-2382.513)/2382.513


age_regrouped <- rki
age_regrouped$age_group <- recode(rki$age_group, "A00-A04" = "Child", "A05-A14" = "Child", "A15-A34" = "Young", "A35-A59" = "Middle", "A60-A79" = "Old", "A80+" = "Old")

daily_age_cases_de <- group_by(age_regrouped, date, age_group) %>%
  summarise(sum_cases = sum(cases)) %>%
  spread(age_group, sum_cases) %>%
  mutate(Child = Child/ 100 * 12.8) %>%
  mutate(Young = Young/ 100 * 34.2) %>%
  mutate(Middle = Middle/ 100 * 24.3) %>%
  mutate(Old = Old/100 * 28.7) %>%
  select(-unbekannt) %>%
  gather("Child", "Young", "Middle", "Old", key="Age", value="sum_cases") %>%
  replace(is.na(.), 0)

daily_age_cases_de$Age <- recode(daily_age_cases_de$Age, "Child" = "00-14", "Young" = "15-34", "Middle" = "35-59", "Old" = "60+")

aov_age <- aov(sum_cases ~ Age, data=daily_age_cases_de)
summary(aov_age)
TukeyHSD(aov_age)

# daily_sex_cases_de, m_greater

fig13 <- daily_sex_cases_de %>%
  filter(sex=="m_greater") %>%
  ggplot(aes(x=date, y=sum_cases)) + geom_line() + xlab("Date") + ylab("Difference") 

ggsave("fig13.png", scale=0.8)

# daily_age_cases_de


fig14a <- ggplot(daily_age_cases_de, aes(x=date, y=sum_cases, group=Age, color=Age)) + geom_line() +
  xlab("Date") + ylab("Cases (adjusted for group population)") +
  scale_x_date(limits = as.Date(c("2020-03-01", "2020-06-30"))) + ylim(0, 700) +
  scale_color_brewer(palette="Spectral")

ggsave("fig14a.png", width=11.4, scale=0.7)

fig14b <- ggplot(daily_age_cases_de, aes(x=date, y=sum_cases, group=Age, color=Age)) + geom_line() + 
  xlab("Date") + ylab("Cases (adjusted for group population)") +
  scale_x_date(limits = as.Date(c("2020-07-01", "2020-12-31"))) +
  scale_color_brewer(palette="Spectral")

ggsave("fig14b.png", width=11.4, scale=0.7)






# Chapter 6: Mobility data


# UK mobility

mobility_uk <- read.csv('2020_GB_Region_Mobility_Report.csv') %>%
  mutate(date = as.Date(date)) %>%
  filter(sub_region_2 == "") %>%
  rename('retail_and_recreation' = 'retail_and_recreation_percent_change_from_baseline', 'grocery_and_pharmacy' = 'grocery_and_pharmacy_percent_change_from_baseline',
         'parks' = 'parks_percent_change_from_baseline', 'transit_stations' = 'transit_stations_percent_change_from_baseline',
         'workplaces' = 'workplaces_percent_change_from_baseline', 'residential' = 'residential_percent_change_from_baseline') %>%
  gather('retail_and_recreation', 'grocery_and_pharmacy', 'parks', 'transit_stations', 'workplaces', 'residential', 
         key='type_mobility', value='percent_change_from_baseline') %>%
  filter(date < as.Date("2021-01-01"))

mobility_uk$percent_change_from_baseline <- na.locf(mobility_uk$percent_change_from_baseline)


# Constituent counties and populations of regions

summarise_regions <- function(region){
  ret <- mobility_uk %>%
    filter(sub_region_1 %in% row.names(region) & sub_region_2 == "") %>%
    mutate(percent_change_from_baseline = percent_change_from_baseline * (region[as.character(sub_region_1),] / sum(region))) %>%
    group_by(date, type_mobility) %>%
    summarise(percent_change_from_baseline = sum(percent_change_from_baseline)) 
  return(ret)
}

east_midlands_counties <- c('Derby', 'Leicester', 'Nottingham', 'Lincolnshire', 'Northamptonshire', 'Derbyshire', 'Nottinghamshire', 'Leicestershire')
# excluded Rutland as mostly NAs, also only 30,000 population
east_midlands_pop <- c(270000, 508000, 768000, 1087000, 747000, 1053000-270000, 1154000-768000, 1053000-508000)
east_midlands <- as.data.frame(east_midlands_pop, row.names=east_midlands_counties)
colnames(east_midlands) <- c("Population")

em_mob <- summarise_regions(east_midlands) %>% mutate(Region = "east_midlands")
  

east_of_england_counties <- c('Bedford', 'Central Bedfordshire', 'Cambridgeshire', 'Peterborough', 'Essex', 'Hertfordshire', 'Norfolk', 'Suffolk', 'Luton', 'Southend-on-Sea', 'Thurrock')
east_of_england_pop <- c(173000, 288000, 852000, 202000, 1832000-183000-174000, 1184000, 903000, 758000, 213000, 183000, 174000)
east_of_england <- as.data.frame(east_of_england_pop, row.names=east_of_england_counties)
colnames(east_of_england) <- c("Population")

eoe_mob <- summarise_regions(east_of_england) %>% mutate(Region = "east_of_england")


london_counties <- c('Greater London')
london_pop <- c(8899000)
london <- as.data.frame(london_pop, row.names=london_counties)
colnames(london) <- c("Population")

lo_mob <- summarise_regions(london) %>% mutate(Region = "london")


north_west_counties <- c('Blackburn with Darwen', 'Blackpool', 'Cheshire East', 'Cheshire West and Chester', 'Cumbria', 'Greater Manchester', 'Borough of Halton', 'Lancashire', 'Merseyside', 'Warrington')
north_west_pop <- c(149000, 139000, 384000, 343000, 498000, 2812000, 129000, 1498000-149000-139000, 1423000, 210000)
north_west <- as.data.frame(north_west_pop, row.names=north_west_counties)
colnames(north_west) <- c("Population")

nw_mob <- summarise_regions(north_west) %>% mutate(Region = "north_west")


north_east_counties <- c('County Durham', 'Darlington', 'Hartlepool', 'Middlesbrough', 'Northumberland', 'Redcar and Cleveland', 'Stockton-on-Tees', 'Tyne and Wear')
north_east_pop <- c(866000-106000-93000-197000, 106000, 93000, 174000, 320000, 137000, 197000, 1136000)
north_east <- as.data.frame(north_east_pop, row.names=north_east_counties)
colnames(north_east) <- c("Population")

ne_mob <- summarise_regions(north_east) %>% mutate(Region = "north_east")


south_east_counties <- c('Bracknell Forest', 'Buckinghamshire', 'Brighton and Hove', 'Portsmouth', 'Southampton', 'East Sussex', 'Hampshire', 'Isle of Wight', 'Kent', 'Medway', 'Milton Keynes',
                'Oxfordshire', 'Reading', 'Slough', 'Surrey', 'West Berkshire', 'West Sussex', 'Windsor and Maidenhead', 'Wokingham')
south_east_pop <- c(122000, 806000-248000, 290000, 238000, 269000, 844000-290000, 1844000-238000-269000, 141000, 1846000-278000, 278000, 248000,
                    687000, 161000, 164000, 1189000, 158000, 858000, 151000, 171000)
south_east <- as.data.frame(south_east_pop, row.names=south_east_counties)
colnames(south_east) <- c("Population")

se_mob <- summarise_regions(south_east) %>% mutate(Region = "south_east")


south_west_counties <- c('Bath and North East Somerset', 'Bristol City', 'Plymouth', 'Cornwall', 'Devon', 'Dorset', 'Gloucestershire', 'North Somerset', 'Somerset',
                'South Gloucestershire', 'Swindon', 'Torbay' , 'Wiltshire')
south_west_pop <- c(193000, 463000, 262000, 568000, 1194000-262000-130000, 772000, 916000-285000, 215000, 965000-193000-215000,
                    285000, 222000, 130000, 720000-222000)
south_west <- as.data.frame(south_west_pop, row.names=south_west_counties)
colnames(south_west) <- c("Population")

sw_mob <- summarise_regions(south_west) %>% mutate(Region = "south_west")


west_midlands_counties <- c('Stoke-on-Trent', 'Herefordshire', 'Shropshire', 'Staffordshire', 'Warwickshire', 'West Midlands', 'Worcestershire')
west_midlands_pop <- c(256000, 192000, 498000, 1131000-256000, 571000, 2916000, 592000)
west_midlands <- as.data.frame(west_midlands_pop, row.names=west_midlands_counties)
colnames(west_midlands) <- c("Population")

wm_mob <- summarise_regions(west_midlands) %>% mutate(Region = "west_midlands")


yorkshire_and_the_humber_counties <- c('Kingston upon Hull', 'East Riding of Yorkshire', 'North East Lincolnshire', 'North Lincolnshire', 'North Yorkshire', 'South Yorkshire', 'West Yorkshire', 'York')
yorkshire_pop <- c(259000, 600000-259000, 159000, 172000, 1158000-210000-197000-106000-137000, 1402000, 2320000, 210000)
yorkshire_and_the_humber <- as.data.frame(yorkshire_pop, row.names=yorkshire_and_the_humber_counties)
colnames(yorkshire_and_the_humber) <- c("Population")

yh_mob <- summarise_regions(yorkshire_and_the_humber) %>% mutate(Region = "yorkshire_and_the_humber")


england_regions <- c("east_midlands", "east_of_england", "london", "north_west", "north_east", "south_east", "south_west", "west_midlands", "yorkshire_and_the_humber")
england_pop <- c(sum(east_midlands), sum(east_of_england), sum(london), sum(north_west), sum(north_east), sum(south_east), sum(south_west), sum(west_midlands), sum(yorkshire_and_the_humber))
england <- as.data.frame(england_pop, row.names=england_regions)
colnames(england) <- "Population"



mobility_en <- rbind(em_mob, eoe_mob, lo_mob, ne_mob, nw_mob, se_mob, sw_mob, wm_mob, yh_mob) %>%
  mutate(percent_change_from_baseline = percent_change_from_baseline * (england[as.character(Region),] / sum(england))) %>%
  group_by(date, type_mobility) %>%
  summarise(percent_change_from_baseline = sum(percent_change_from_baseline)) %>%
  mutate(country_region_code = as.factor("EN")) 


  
ggplot(mobility_en, aes(x=date, y=percent_change_from_baseline)) + geom_line() +
  geom_vline(aes(xintercept=as.Date("2020-03-23")), colour="blue") + # First lockdown
  geom_vline(aes(xintercept=as.Date("2020-11-05")), colour="darkgreen") + # Second Lockdown
  facet_wrap(~type_mobility) +
  ggtitle("Change in Different Types of Mobility From Baseline, England") + xlab("Date") + ylab("Change in Mobility (%)") +
  theme(plot.title = element_text(face="bold", hjust="0.5", size = 11)) 



# English Mobility Graphs per Type

fig16EN <- mobility_en %>%
  filter(type_mobility == "parks") %>%
  ggplot(aes(x=date, y=percent_change_from_baseline)) + geom_line() +
  xlab("Date") + ylab("Change in Mobility (%)") +
  ylim(-60, 170) + 
  geom_vline(aes(xintercept=as.Date("2020-03-23")), colour="blue") + # First lockdown
  geom_vline(aes(xintercept=as.Date("2020-11-05")), colour="darkgreen") + # Second Lockdown
  geom_vline(aes(xintercept=as.Date("2020-05-13")), color="brown") + # Allowed outside for exercise
  geom_vline(aes(xintercept=as.Date("2020-05-22")), color="red") + # Cummings
  geom_text(aes(label="First lockdown \nbegins (23.03)", x=as.Date("2020-03-23"), y=70)) + 
  geom_text(aes(label="Outdoor exercise\nallowed (10.05)", x=as.Date("2020-05-01"), y=170)) +
  geom_text(aes(label="Second lockdown \nbegins (05.11)", x=as.Date("2020-11-05"), y=70)) +
  geom_text(aes(label="Cummings Scandal\n(22.05)", x=as.Date("2020-05-31"), y=145)) +
  geom_line(aes(y=rollmean(percent_change_from_baseline, 7, na.pad=TRUE)), color="orange", size=1.05)

ggsave("fig16EN.png", scale=1.4)
  

fig17EN <- mobility_en %>%
  filter(type_mobility == "retail_and_recreation") %>%
  ggplot(aes(x=date, y=percent_change_from_baseline)) + geom_line() +
  xlab("Date") + ylab("Change in Mobility (%)") +
  geom_vline(aes(xintercept=as.Date("2020-03-20")), colour="darkgrey") + # First closings
  geom_vline(aes(xintercept=as.Date("2020-03-23")), colour="blue") + # First lockdown
  geom_vline(aes(xintercept=as.Date("2020-11-05")), colour="darkgreen") + # Second Lockdown
  geom_vline(aes(xintercept=as.Date("2020-06-01")), color="magenta") + # Stay-at-home order revoked
  geom_vline(aes(xintercept=as.Date("2020-07-04")), color="orange") + # Pubs, restaurants, hotels, hairdressers allowed to reopen
  geom_vline(aes(xintercept=as.Date("2020-08-03")), color="darkblue") + # Eat Out to Help Out start
  geom_vline(aes(xintercept=as.Date("2020-08-31")), color="cyan") + # Eat Out to Help Out ends
  geom_vline(aes(xintercept=as.Date("2020-12-02")), color="red") + # Lockdown ends
  geom_text(aes(label="First lockdown \nbegins (23.03)", x=as.Date("2020-03-23"), y=20)) + 
  geom_text(aes(label="Stay at home\n order revoked \n(01.06)", x=as.Date("2020-06-01"), y=-30)) +
  geom_text(aes(label="Pubs, restaurants,\n hotels, hairdresses\n closed (20.03)", x=as.Date("2020-03-15"), y=-10)) +
  geom_text(aes(label="Pubs, restaurants,\n hotels, hairdresses\n allowed to reopen\n(04.07)", x=as.Date("2020-07-01"), y=-10)) + 
  geom_text(aes(label="Eat Out to \nHelp Out begins\n (03.08)", x=as.Date("2020-08-03"), y=5)) + 
  geom_text(aes(label="Eat Out to \nHelp Out ends\n(31.08)", x=as.Date("2020-08-31"), y=18)) +
  geom_text(aes(label="Second lockdown \nbegins (5.11)", x=as.Date("2020-11-05"), y=10)) +
  geom_text(aes(label="Second lockdown \nends (02.12)", x=as.Date("2020-12-02"), y=0)) + 
  geom_line(aes(y=rollmean(percent_change_from_baseline, 7, na.pad=TRUE)), color="orange", size=1.05)

ggsave("fig17EN.png", scale=1.4)

fig18EN <- mobility_en %>%
  filter(type_mobility == "transit_stations") %>%
  ggplot(aes(x=date, y=percent_change_from_baseline)) + geom_line() +
  xlab("Date") + ylab("Change in Mobility (%)") +
  geom_vline(aes(xintercept=as.Date("2020-03-23")), colour="blue") + # First lockdown
  geom_vline(aes(xintercept=as.Date("2020-11-05")), colour="darkgreen") + # Second Lockdown
  geom_vline(aes(xintercept=as.Date("2020-06-01")), color="magenta") + # Stay-at-home order revoked
  geom_vline(aes(xintercept=as.Date("2020-07-17")), color="violet") + # Public transport unrestricted
  geom_vline(aes(xintercept=as.Date("2020-12-02")), color="red") + # Lockdown ends
  geom_text(aes(label="First lockdown \nbegins (23.03)", x=as.Date("2020-03-23"), y=10)) + 
  geom_text(aes(label="Stay at home\n order revoked \n(01.06)", x=as.Date("2020-06-01"), y=-30)) +
  geom_text(aes(label="Use of Public \nTransport for non-emergencies\n allowed (17.07)", x=as.Date("2020-07-17"), y=-10)) +
  geom_text(aes(label="Second lockdown \nbegins (05.11)", x=as.Date("2020-11-05"), y=0)) +
  geom_text(aes(label="Second lockdown \nends (02.12)", x=as.Date("2020-12-02"), y=-20)) + 
  geom_line(aes(y=rollmean(percent_change_from_baseline, 7, na.pad=TRUE)), color="orange", size=1.05)

ggsave("fig18EN.png", scale=1.4)



# Germany Mobility


mobility_de <- read.csv('2020_DE_Region_Mobility_Report.csv') %>%
  mutate(date = as.Date(date)) %>%
  rename('retail_and_recreation' = 'retail_and_recreation_percent_change_from_baseline', 'grocery_and_pharmacy' = 'grocery_and_pharmacy_percent_change_from_baseline',
         'parks' = 'parks_percent_change_from_baseline', 'transit_stations' = 'transit_stations_percent_change_from_baseline',
         'workplaces' = 'workplaces_percent_change_from_baseline', 'residential' = 'residential_percent_change_from_baseline') %>%
  gather('retail_and_recreation', 'grocery_and_pharmacy', 'parks', 'transit_stations', 'workplaces', 'residential', 
         key='type_mobility', value='percent_change_from_baseline') %>%
  filter(date < as.Date("2021-01-01")) %>%
  filter(is.na(sub_region_2))
mobility_de$sub_region_1 <- recode(mobility_de$sub_region_1, 'Baden-Württemberg' = 'Baden-Wuerttemberg')
mobility_de$percent_change_from_baseline <- na.locf(mobility_de$percent_change_from_baseline)

mobility_de_overall <- mobility_de %>%
  filter(sub_region_1 == "") %>%
  arrange(date, type_mobility) %>%
  select(date, type_mobility, percent_change_from_baseline, country_region_code)


# Mobility by Type Germany

ggplot(mobility_de_overall, aes(x=date, y=percent_change_from_baseline)) + geom_line() +
  geom_vline(aes(xintercept=as.Date("2020-03-23")), colour="blue") + # First lockdown
  geom_vline(aes(xintercept=as.Date("2020-11-02")), colour="darkgreen") + # Second "Partial" Lockdown
  facet_wrap(~type_mobility) +
  ggtitle("Change in Different Types of Mobility From Baseline, Germany") + xlab("Date") + ylab("Change in Mobility (%)") +
  theme(plot.title = element_text(face="bold", hjust="0.5", size = 11)) 

fig16DE <- mobility_de_overall %>%
  filter(type_mobility == "parks") %>%
  ggplot(aes(x=date, y=percent_change_from_baseline)) + geom_line() +
  xlab("Date") + ylab("Change in Mobility (%)") +
  geom_vline(aes(xintercept=as.Date("2020-03-23")), colour="blue") + # First lockdown
  geom_vline(aes(xintercept=as.Date("2020-11-02")), colour="darkgreen") + # Second Lockdown
  geom_vline(aes(xintercept=as.Date("2020-05-06")), color="magenta") + # First wave over, max of two households
  geom_text(aes(label="First lockdown \nbegins (23.03)", x=as.Date("2020-03-23"), y=120)) + 
  geom_text(aes(label="First wave 'over,'\n meeting of two households\n allowed (06.05)", x=as.Date("2020-05-06"), y=150)) +
  geom_text(aes(label="Second 'partial' lockdown \nbegins (02.11)", x=as.Date("2020-11-02"), y=150)) +
  geom_line(aes(y=rollmean(percent_change_from_baseline, 7, na.pad=TRUE)), color="orange", size=1.05)

ggsave("fig16DE.png", scale=1.4)

fig17DE <- mobility_de_overall %>%
  filter(type_mobility == "retail_and_recreation") %>%
  ggplot(aes(x=date, y=percent_change_from_baseline)) + geom_line() +
  xlab("Date") + ylab("Change in Mobility (%)") +
  geom_vline(aes(xintercept=as.Date("2020-03-23")), colour="blue", label="hi") + # First lockdown
  geom_vline(aes(xintercept=as.Date("2020-11-02")), colour="darkgreen") + # Second Lockdown
  geom_vline(aes(xintercept=as.Date("2020-05-06")), color="magenta") + # First wave over, max of two households
  geom_vline(aes(xintercept=as.Date("2020-04-20")), color="orange") + # Large shops allowed to reopen
  geom_text(aes(label="First lockdown \nbegins (23.03)", x=as.Date("2020-03-23"), y=50)) +
  geom_text(aes(label="First wave 'over,'\n all shops open\n allowed (06.05)", x=as.Date("2020-05-10"), y=30)) +
  geom_text(aes(label="Second 'partial' lockdown \nbegins (02.11)", x=as.Date("2020-11-02"), y=20)) +
  geom_text(aes(label="Shops with retail\n space of up to 800\n sq. m. reopen (20.04)", x=as.Date("2020-04-10"), y=5)) +
  geom_line(aes(y=rollmean(percent_change_from_baseline, 7, na.pad=TRUE)), color="orange", size=1.05)

ggsave("fig17DE.png", scale=1.4)

fig18DE <- mobility_de_overall %>%
  filter(type_mobility == "transit_stations") %>%
  ggplot(aes(x=date, y=percent_change_from_baseline)) + geom_line() +
  xlab("Date") + ylab("Change in Mobility (%)") + 
  geom_vline(aes(xintercept=as.Date("2020-03-23")), colour="blue") + # First lockdown
  geom_vline(aes(xintercept=as.Date("2020-11-02")), colour="darkgreen") + # Second Lockdown
  geom_vline(aes(xintercept=as.Date("2020-05-06")), color="magenta") + # First wave over, max of two households
  geom_text(aes(label="First lockdown \nbegins (23.03)", x=as.Date("2020-03-23"), y=10)) + 
  geom_text(aes(label="First wave 'over,'\n meeting of two households\n allowed (06.05)", x=as.Date("2020-05-06"), y=-10)) +
  geom_text(aes(label="Second 'partial' lockdown \nbegins (02.11)", x=as.Date("2020-11-02"), y=10)) +
  geom_line(aes(y=rollmean(percent_change_from_baseline, 7, na.pad=TRUE)), color="orange", size=1.05)

ggsave("fig18DE.png", scale=1.4)



# overall mobility changes both countries

mobility_both <- full_join(mobility_de_overall, mobility_en)
fig15 <- ggplot(mobility_both, aes(x=date, y=percent_change_from_baseline, group=country_region_code, color=country_region_code)) + geom_line() +
  facet_wrap(~type_mobility) + xlab("Date") + ylab("Change in Mobility (%)") + 
  theme(text = element_text(size = 16))

ggsave("fig15.png", scale = 1.5, width = 7)

parks_both <- mobility_both %>% filter(type_mobility == "parks")
t.test(parks_both$percent_change_from_baseline ~ parks_both$country_region_code)

rar_both <- mobility_both %>% filter(type_mobility == "retail_and_recreation")
t.test(rar_both$percent_change_from_baseline ~ rar_both$country_region_code)

resid_both <- mobility_both %>% filter(type_mobility == "residential")
t.test(resid_both$percent_change_from_baseline ~ resid_both$country_region_code)

gam_both <- mobility_both %>% filter(type_mobility == "grocery_and_pharmacy")
t.test(gam_both$percent_change_from_baseline ~ gam_both$country_region_code)

work_both <- mobility_both %>% filter(type_mobility == "workplaces")
t.test(work_both$percent_change_from_baseline ~ work_both$country_region_code)

tran_both <- mobility_both %>% filter(type_mobility == "transit_stations")
t.test(tran_both$percent_change_from_baseline ~ tran_both$country_region_code)


# Section 6.2: # Did first cases have an effect on mobility

transit_en_sept <- mobility_en %>%
  filter(type_mobility == "transit_stations") %>%
  full_join(daily_cases_en, by="date") %>%
  filter(date >= as.Date("2020-08-25")) %>%
  filter(date <= as.Date("2020-10-31")) %>%
  mutate(mob_capped = (percent_change_from_baseline+45.63)/(-17.84+45.63)) %>%
  mutate(cases_capped = (sum_cases-970)/(22578-970)) 
  

transit_de_sept <- mobility_de_overall %>%
  filter(type_mobility == "transit_stations") %>%
  full_join(daily_cases_de, by="date") %>%
  filter(date >= as.Date("2020-09-01")) %>%
  filter(date <= as.Date("2020-11-03")) %>%
  mutate(mob_capped = (percent_change_from_baseline+28)/(4+28)) %>%
  mutate(cases_capped = (sum_cases-580)/(19875-580))


fig19EN <- ggplot(transit_en_sept, aes(x=date, y=mob_capped, color="Mobility Change")) + geom_line() + 
  geom_line(aes(y=cases_capped, color="New Daily Cases")) +
  geom_line(aes(y=rollmean(mob_capped, 7, na.pad=TRUE)), color="orange", size=1.05) + ylim(0, 1) + 
  geom_line(aes(y=rollmean(cases_capped, 7, na.pad=TRUE)), color="green", size=1.05) + labs(x = "Date", y = "Normalised Values", color = "Legend")

ggsave("fig19EN.png")

cor.test(transit_en_sept$mob_capped, lag(transit_en_sept$cases_capped))
cor.test(transit_en_sept$mob_capped, lag(transit_en_sept$cases_capped, 7))

fig19DE <- ggplot(transit_de_sept, aes(x=date, y=mob_capped, color="Mobility Change")) + geom_line() + 
  geom_line(aes(y=cases_capped, color="New Daily Cases")) +
  geom_line(aes(y=rollmean(mob_capped, 7, na.pad=TRUE)), color="orange", size=1.05) + ylim(0, 1) + 
  geom_line(aes(y=rollmean(cases_capped, 7, na.pad=TRUE)), color="green", size=1.05) + labs(x = "Date", y = "Normalised Values", color = "Legend")

ggsave("fig19DE.png")

cor.test(transit_de_sept$mob_capped, lag(transit_de_sept$cases_capped))
cor.test(transit_de_sept$mob_capped, lag(transit_de_sept$cases_capped, 7))


# COMPARE BOTH COUNTRIES TO sum_cases, ccf to determine forecasting ability and best lag, lm

mobility_en$seven_day_diff <- (mobility_en$percent_change_from_baseline - lag(mobility_en$percent_change_from_baseline, 42))
mobility_de_overall$seven_day_diff <- mobility_de_overall$percent_change_from_baseline - lag(mobility_de_overall$percent_change_from_baseline, 42)

transit_en <- mobility_en %>%
  filter(type_mobility == "transit_stations") %>%
  full_join(daily_cases_en, by="date") %>%
  filter(date > as.Date("2020-03-31"))

retail_en <- mobility_en %>%
  filter(type_mobility == "retail_and_recreation") %>%
  full_join(daily_cases_en, by="date") %>%
  filter(date > as.Date("2020-03-31"))

resident_en <- mobility_en %>%
  filter(type_mobility == "residential") %>%
  full_join(daily_cases_en, by="date") %>%
  filter(date > as.Date("2020-03-31"))

grocery_en <- mobility_en %>%
  filter(type_mobility == "grocery_and_pharmacy") %>%
  full_join(daily_cases_en, by="date") %>%
  filter(date > as.Date("2020-03-31"))

parks_en <- mobility_en %>%
  filter(type_mobility == "parks") %>%
  full_join(daily_cases_en, by="date") %>%
  filter(date > as.Date("2020-03-31"))

work_en <- mobility_en %>%
  filter(type_mobility == "workplaces") %>%
  full_join(daily_cases_en, by="date") %>%
  filter(date > as.Date("2020-03-31"))


ccf(transit_en$seven_day_diff, transit_en$percentage_difference_cases, main = "CCF for transit mobility and percent diff. cases (EN)") 
ccf(retail_en$seven_day_diff, retail_en$percentage_difference_cases, main = "CCF for retail mobility and percent diff. cases (EN)")
ccf(resident_en$seven_day_diff, resident_en$percentage_difference_cases, main = "CCF for residential mobility and percent diff. cases (EN)")
ccf(grocery_en$seven_day_diff, grocery_en$percentage_difference_cases, main = "CCF for grocery mobility and percent diff. cases (EN)")
ccf(parks_en$seven_day_diff, parks_en$percentage_difference_cases, main = "CCF for parks mobility and percent diff. cases (EN)")
ccf(work_en$seven_day_diff, work_en$percentage_difference_cases, main = "CCF for workplace mobility and percent diff. cases (EN)")

transit_de <- mobility_de_overall %>%
  filter(type_mobility == "transit_stations") %>%
  full_join(daily_cases_de, by="date") %>%
  filter(date > as.Date("2020-03-31"))

retail_de <- mobility_de_overall %>%
  filter(type_mobility == "retail_and_recreation") %>%
  full_join(daily_cases_de, by="date") %>%
  filter(date > as.Date("2020-03-31"))

resident_de <- mobility_de_overall %>%
  filter(type_mobility == "residential") %>%
  full_join(daily_cases_de, by="date") %>%
  filter(date > as.Date("2020-03-31"))

grocery_de <- mobility_de_overall %>%
  filter(type_mobility == "grocery_and_pharmacy") %>%
  full_join(daily_cases_de, by="date") %>%
  filter(date > as.Date("2020-03-31"))

parks_de <- mobility_de_overall %>%
  filter(type_mobility == "parks") %>%
  full_join(daily_cases_de, by="date") %>%
  filter(date > as.Date("2020-03-31"))

work_de <- mobility_de_overall %>%
  filter(type_mobility == "workplaces") %>%
  full_join(daily_cases_de, by="date") %>%
  filter(date > as.Date("2020-03-31"))


ccf(transit_de$seven_day_diff, transit_de$percentage_difference_cases, main = "CCF for transit mobility and percent diff. cases (DE)")
ccf(retail_de$seven_day_diff, retail_de$percentage_difference_cases, main = "CCF for retail mobility and percent diff. cases (DE)")
ccf(resident_de$seven_day_diff, resident_de$percentage_difference_cases, main = "CCF for residential mobility and percent diff. cases (DE)")
ccf(grocery_de$seven_day_diff, grocery_de$percentage_difference_cases, main = "CCF for grocery mobility and percent diff. cases (DE)")
ccf(parks_de$seven_day_diff, parks_de$percentage_difference_cases, main = "CCF for parks mobility and percent diff. cases (DE)")
ccf(work_de$seven_day_diff, work_de$percentage_difference_cases, main = "CCF for workplace mobility and percent diff. cases (DE)")


summary(lm(retail_en$percentage_difference_cases ~ lag(retail_en$seven_day_diff, 11) + lag(resident_en$seven_day_diff, 9) + 
             lag(work_en$seven_day_diff, 7) + lag(transit_en$seven_day_diff, 9) + lag(grocery_en$seven_day_diff, 11)))

summary(lm(retail_en$percentage_difference_cases ~ lag(retail_en$percentage_difference_cases)))

summary(lm(retail_de$percentage_difference_cases ~ lag(retail_de$seven_day_diff, 6) +
             lag(resident_de$seven_day_diff, 9) + lag(work_de$seven_day_diff, 7) + lag(transit_de$seven_day_diff, 8) +
             lag(grocery_de$seven_day_diff, 9)))

