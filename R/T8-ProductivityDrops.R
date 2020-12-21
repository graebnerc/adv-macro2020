library(data.table)
library(here)
library(tidyverse)
library(countrycode)

years_considered <- c(2007, 2009)
country_considered <- c(
  "UnitedStates", "Germany", "France", "Italy", "Sweden", "Greece")

rho_reaktion <- fread(
  here("data/tidy/EPWT6.csv"), 
  select = c("Country"="character", 
             "Year"="double",
             "rhoppp2011"="double", 
             "xppp2011"="double")
  ) %>% 
  mutate(
    rho_index = rhoppp2011 / first(rhoppp2011),
    x_index = xppp2011 / first(xppp2011)
  )  %>%
  filter(
    Country %in% country_considered,
    Year %in% years_considered
  ) %>% 
  group_by(Country) %>%
  mutate(
    rho_change = (rhoppp2011-lag(rhoppp2011))/lag(rhoppp2011),
    x_change = (xppp2011-lag(xppp2011))/lag(xppp2011),
    ) %>%
  ungroup() %>%
  filter(
    Year == years_considered[2]
  ) %>%
  select(
    all_of(c("Country", "rho_change", "x_change"))
    ) %>%
  rename(
    Kapitalproduktivität=rho_change, 
    Arbeitsproduktivität=x_change
  ) %>%
  pivot_longer(cols = -Country, 
               names_to = "Parameter", values_to = "Veränderung") %>%
  mutate(
    Country=countrycode(Country, "country.name", "country.name.de"),
    Country=ifelse(Country=="Vereinigte Staaten", "USA", Country)) %>%
  ggplot(data = ., aes(x=Country, y=Veränderung, fill=Parameter)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Reaktionen in Finanzkrise 2007-2009", caption = "Quelle: EPWT 6.0",
       y= "Veränderung in %") +
  scale_fill_viridis_d(option = "E") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_light() + theme(
    panel.border = element_blank(),
    axis.title.y = element_text(size=12),
    axis.text = element_text(size=11),
    axis.line = element_line(),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggsave(plot = rho_reaktion, 
       filename = here("output/T8/T8-ReaktionRezession.pdf"),
       width = 6, height = 4)
