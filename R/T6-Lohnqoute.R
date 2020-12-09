library(tidyverse)
library(data.table)
library(here)
library(icaeDesign)
library(countrycode)

epwt <- data.table::fread(here("data/tidy/EPWT6.csv"))
country_set <- c("Germany", "UnitedStates", "France", "China", "Netherlands", "Norway", "SouthAfrica")

# Lohnquote--------------------------------------------------------------------

epwt_wage_share <- epwt %>% 
  select(one_of("Year", "Country", "wppp2011", "xppp2011")) %>%
  rename(Löhne=wppp2011, Produktivität=xppp2011) %>%
  filter(Country %in% country_set) %>%
  pivot_longer(cols=one_of("Löhne" , "Produktivität"), 
               names_to="Variable", values_to="PPP2011") %>%
  mutate(PPP2011=PPP2011/1000, 
         Country=ifelse(Country=="SouthAfrica", "South Africa", Country),
         Country=countrycode(Country, "country.name", "country.name.de"))  %>%
  pivot_wider(names_from = "Variable", values_from = "PPP2011") %>%
  mutate(Lohnquote = Löhne/Produktivität)

wage_share_plot <- ggplot(
  data = epwt_wage_share, aes(x=Year, y=Lohnquote, color=Country)
) +
  geom_line(key_glyph=draw_key_rect) + geom_point() +
  scale_y_continuous(expand = expansion(), 
                     labels = scales::percent_format(), limits = c(0,1)) +
  scale_color_viridis_d() +
  labs(title = "Lohnquote über über die Zeit", 
       caption = "Quelle: EPWT 6.0", y="Lohnquote")+
  theme_bw() +
  theme(
    legend.position = "bottom", legend.title = element_blank(), 
    axis.title.x = element_blank(), panel.border = element_blank(), 
    axis.line = element_line(), 
    strip.background = element_rect(fill="white", colour = "white")
  )
wage_share_plot

ggsave(plot = wage_share_plot, 
       filename = here("output/T6/Lohnquote_timeseries.pdf"),
       width = 5, height = 4)

# Darstellung ueber Barplot----------------------------------------------------

initial_year <- 1970
new_year <- max(epwt$Year, na.rm = T)

epwt_wage_share <- epwt %>% 
  select(one_of("Year", "Country", "wppp2011", "LabSha", "xppp2011")) %>%
  rename(Löhne=wppp2011, Produktivität=xppp2011, Lohnquote=LabSha) %>%
  filter(Country %in% country_set) %>%
  filter(Year %in% c(initial_year, new_year)) %>%
  pivot_longer(cols = -any_of(c("Year", "Country")), 
               names_to = "Variable", values_to = "Wert") %>%
  pivot_wider(names_from = "Year", values_from = "Wert") %>%
  mutate(
    Änderung = (`2014` - `1970`) / `1970` * 100
    ) %>%
  select(-any_of(c(as.character(initial_year), as.character(new_year))))

wage_share_plot_bar <- epwt_wage_share %>%
  filter(Variable == "Lohnquote") %>%
  ggplot(data = ., mapping = aes(x=reorder(Country, Änderung), y=Änderung, fill=Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Änderung Lohnquote 1970 vs. 2014\n", caption = "Quelle: EPWT 6.0") +
  scale_y_continuous(expand = expansion(add = c(0.5, 0)), labels = scales::percent_format(scale = 1)) +
  scale_fill_viridis_d() + 
  theme_bw() + theme(
    legend.position = "none", panel.border = element_blank(), 
    axis.ticks = element_blank(),
    axis.title = element_blank())

ggsave(plot = wage_share_plot_bar, 
       filename = here("output/T6/Lohnquote_barplot.pdf"),
       width = 7, height = 5)

