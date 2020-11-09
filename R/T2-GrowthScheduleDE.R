library(tidyverse)
library(latex2exp)
library(here)
library(data.table)

epwt <- data.table::fread(here("data/tidy/EPWT6.csv"), data.table = F) %>%
  select(one_of("Countrycode", "Year", "N", "xppp2011", "cppp2011", "wppp2011",
                "Delta...ppp2011", "rhoppp2011", "v...ppp2011")) %>%
  filter(Countrycode %in% c("USA", "DEU", "JPN"),
         Year==2014)

# 1. Beispiel USA und Japan ---------------------------------------------------
rho_usa <- filter(epwt, Countrycode=="USA")[["rhoppp2011"]]
c_usa <- filter(epwt, Countrycode=="USA")[["cppp2011"]]

rho_jap <- filter(epwt, Countrycode=="JPN")[["rhoppp2011"]]
c_jap <- filter(epwt, Countrycode=="JPN")[["cppp2011"]]

usa_jpn <- ggplot(data = epwt, aes(x=v...ppp2011, y=cppp2011)) +
  geom_segment(
    aes(x = 0, xend=rho_usa,
    y = c_usa, yend=0, color="USA")
    ) +
  geom_segment(
    aes(x = 0, xend=rho_jap,
        y = c_jap, yend=0, color="Japan")
  ) +
  scale_y_continuous(limits = c(0, 100000), expand = expansion(), 
                     breaks = c(c_jap, c_usa), 
                     labels = c(TeX("$c_{JPN}$"),TeX("$c_{USA}$"))) +
  scale_x_continuous(limits = c(0, 0.5), expand = expansion(), 
                     breaks = c(rho_jap, rho_usa), 
                     labels = c(TeX("$\\rho_{JPN}$"),TeX("$\\rho_{USA}$"))) +
  scale_color_viridis_d() +
  labs(title = "Wachstums-Verteilungs-Graph 2014", caption = "Quelle: EPWT 6.0") +
  theme_bw() + 
  theme(axis.title = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        panel.border = element_blank(), 
        axis.line = element_line())

ggsave(plot = usa_jpn, 
       filename = here("output/T2/growth-dist-USA-JPN.pdf"), 
       width = 6, height = 4)
# 2. Beispiel Deutschland 1995 vs. 2014  --------------------------------------

epwt <- data.table::fread(here("data/tidy/EPWT6.csv"), data.table = F) %>%
  select(one_of("Countrycode", "Year", "xppp2011", "cppp2011", "wppp2011",
                "Delta...ppp2011", "rhoppp2011", "v...ppp2011")) %>%
  filter(Countrycode %in% c("DEU"),
         Year%in%c(1995, 2014)) %>% select(-Countrycode) %>%
  pivot_longer(-Year, names_to="Variable", values_to="Wert") %>%
  pivot_wider(names_from = "Year", values_from = "Wert") %>%
  mutate(Variable=gsub("ppp2011", "", Variable), 
         Variable=gsub("\\.", "", Variable))
epwt

x_1995 <- filter(epwt, Variable=="x")[["1995"]]
x_2014 <- filter(epwt, Variable=="x")[["2014"]]

c_1995 <- filter(epwt, Variable=="c")[["1995"]]
c_2014 <- filter(epwt, Variable=="c")[["2014"]]

w_1995 <- filter(epwt, Variable=="w")[["1995"]]
w_2014 <- filter(epwt, Variable=="w")[["2014"]]

delta_1995 <- filter(epwt, Variable=="Delta")[["1995"]]
delta_2014 <- filter(epwt, Variable=="Delta")[["2014"]]

rho_1995 <- filter(epwt, Variable=="rho")[["1995"]]
rho_2014 <- filter(epwt, Variable=="rho")[["2014"]]

v_1995 <- filter(epwt, Variable=="v")[["1995"]]
v_2014 <- filter(epwt, Variable=="v")[["2014"]]

de_1995_2019 <- epwt %>%
  pivot_longer(cols = -Variable, names_to="Jahr", values_to="Wert") %>%
  pivot_wider(names_from = "Variable", values_from = "Wert") %>%
  ggplot(data = ., aes(x=v, y=x)) +
  geom_segment(
    aes(x = 0, xend=rho_1995,
        y = x_1995, yend=0, color="1995")
  ) +
  geom_segment(
    aes(x = 0, xend=rho_2014,
        y = x_2014, yend=0, color="2014")
  ) +
  scale_y_continuous(
    #limits = c(0, 90000), 
    expand = expansion(), 
    breaks = c(w_1995, c_1995,c_2014,w_2014, x_1995, x_2014), 
    labels = c(TeX("$w_{1995}$"),TeX("$c_{1995}$"), TeX("$c_{2014}$"),
               TeX("$w_{2014}$"), TeX("$x_{1995}$"),TeX("$x_{2014}$"))
    ) +
  scale_x_continuous(
    limits = c(0, 0.6), expand = expansion(), 
    breaks = c(rho_1995, rho_2014), 
    labels = c(TeX("$\\rho_{1995}$"), TeX("$\\rho_{2014}$")),
    sec.axis = sec_axis(trans = ~.*40,
      breaks = c(delta_1995, delta_2014, v_1995, v_2014), 
      labels = c(TeX("$\\delta_{1995}$"),TeX("$\\delta_{1995}$"),
                 TeX("$v_{1995}$"),TeX("$v_{1995}$")))
    ) +
  scale_color_viridis_d() +
  labs(title = "Wachstums-Verteilungs-Graph für DE in 1995 u. 2014", 
       caption = "Quelle: EPWT 6.0") +
  theme_bw() + 
  theme(axis.title = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.title = element_blank(),
        panel.border = element_blank(), 
        axis.line = element_line())

ggsave(plot = de_1995_2019, filename = here("output/T2/growth-dist-DE.pdf"), 
       width = 6, height = 4)

