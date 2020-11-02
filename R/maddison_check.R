library(haven)
library(tidyverse)
library(here)
library(haven)

maddison <- read_dta(here("data/mpd2020.dta"))

maddison_red <- maddison %>%
  filter(
    country %in% c("Germany", "United States", "United Kingdom"),
    year>=1000) %>%
  ggplot(aes(x=year, y=log(gdppc), color=country)) +
  geom_line() +
  labs(
    title = "Wachstum in der langen Frist", 
    x = "Jahr", y="BIP pro Kopf (log)",
    caption = "Maddison data set."
    ) +
  theme_bw() + theme(
    panel.border = element_blank(),
    axis.line = element_line(), legend.title = element_blank(), 
    legend.position = "bottom"
  )
maddison_red
