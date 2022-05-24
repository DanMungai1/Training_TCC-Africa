library(tidyverse)
library(readr)
covid <- read_csv("WHO-COVID-19-global-data.csv")
View(covid)
# select
# |> %>% pipe operator

covid %>% select(Country, New_cases, New_deaths)

covid %>% select(-New_deaths)

covid %>% select(-c(Country, New_cases, New_deaths))

#filter

covid %>% filter(Country == "Kenya")

east_africa <- covid %>% filter(Country %in% c("Kenya", "Uganda", "Rwanda", "Burundi", "United Republic of Tanzania"))

no_uganda <- east_africa %>% filter(!Country == "Uganda")

library(readr)
vaccination <- read_csv("vaccination-data.csv")
View(vaccination)

## mutate

partial <- vaccination %>% 
  mutate(partial_vacc = PERSONS_FULLY_VACCINATED/PERSONS_VACCINATED_1PLUS_DOSE) %>% 
  filter(COUNTRY == "Kenya")


## Group by and summarise

EA_covid <- covid %>% 
  filter(Country %in% c("Kenya", "Uganda", 
                        "United Republic of Tanzania",
                        "Rwanda"))

east_africa %>% group_by(Country) %>% 
  summarise(total_cases = sum(New_cases))

east_africa %>% group_by(Country) %>% 
  summarise(mean_cases = mean(New_cases))

east_africa %>% group_by(Country) %>% 
  summarise(total_deaths = sum(New_deaths))


mut <- east_africa %>% group_by(Country) %>% 
  mutate(total_deaths = sum(New_deaths))

## Arrange
east_africa %>% group_by(Country) %>% 
  summarise(total_deaths = sum(New_deaths)) %>% 
  arrange(total_deaths)

east_africa %>% group_by(Country) %>% 
  summarise(total_deaths = sum(New_deaths)) %>% 
  arrange(desc(total_deaths))

### Relational Data
library(nycflights13)
data("flights")
data("planes")
data("airports")
data("weather")
data("airlines")

## Left Join

flights_weather <- flights %>% left_join(weather)

## Inner join
fffgg <- flights %>% inner_join(weather)

## full join
full_join <- flights %>% full_join(weather)

## Anti_join
library(tidytext)
library(janeaustenr)

books <- austen_books() %>% 
  unnest_tokens(word, text) %>% 
  count(word, sort = T)


books %>% anti_join(stop_words)
## 

library(readr)
Nigeria_soils <- read_csv("Nigeria_soils_data.csv")
View(Nigeria_soils)


## pIVOT longer and pivot wider

pivoted <- Nigeria_soils %>% 
  pivot_longer(cols = pH:Fe, names_to = "Metal", 
               values_to = "Concentration")

## Visualization

## ggplot(data, aes(x = x, y=y)) + geom_line + geom_point + geom_histogram + geom_boxplot

df <- pivoted %>% 
  filter(Metal %in% c("P", "OC", "pH", "K"))

ggplot(df, aes(x = project, y = Concentration, fill = Metal)) +
  geom_col()

ggplot(df, aes(x = project, y = Concentration, fill = Metal)) + 
  geom_bar(stat = "identity", position = "dodge")

ggplot(df, aes(x = project, y = Concentration, fill = Metal)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Metal, scales = "free_y") +
  labs(x = "Projects", y = " Conc", title = "Trying Out",
       subtitle = "Keep On")

ggplot(pivoted, aes(x = X, y =Y)) + 
  geom_point(color = "blue")


library(gapminder)
data("gapminder")
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, 
                      col = continent, size = pop)) +
  geom_point() + scale_x_log10() +
  facet_wrap(~continent)


writexl::write_xlsx(east_africa, "dataframe.xlsx")

write_csv(east_africa, "dataframe.csv")

## lower_case, upper_case, camelCase, upperCase, 

library(tidyverse)
df %>% bind_cols(df2)
bind_cols(df, df2) # number rows must be the same

bind_rows(df, df2) # the number of columns and their names must be the same

