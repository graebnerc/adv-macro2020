library(tidyverse)
library(data.table)
library(here)
library(icaeDesign)
library(countrycode)

epwt <- data.table::fread(here("data/tidy/EPWT6.csv"))
country_set <- c("Germany", "UnitedStates", "France", "China")

# Loehne und Arbeit ueber die Zeit---------------------------------------------

epwt_wages_productivity <- epwt %>% 
  select(one_of("Year", "Country", "wppp2011", "xppp2011")) %>%
  rename(Löhne=wppp2011, Produktivität=xppp2011) %>%
  filter(Country %in% country_set) %>%
  pivot_longer(cols=one_of("Löhne" , "Produktivität"), 
               names_to="Variable", values_to="PPP2011") %>%
  mutate(PPP2011=PPP2011/1000, 
         Country=ifelse(Country=="SouthAfrica", "South Africa", Country),
         Country=countrycode(Country, "country.name", "country.name.de")) 

wage_plot <- ggplot(
  data = epwt_wages_productivity, aes(x=Year, y=PPP2011, color=Country)
  ) +
  geom_line(key_glyph=draw_key_rect) + geom_point() +
  facet_wrap(~Variable) +
  scale_y_continuous(expand = expansion()) +
  scale_color_viridis_d() +
  labs(title = "Löhne und Produktivität über die Zeit", 
       caption = "Quelle: EPWT 6.0", y="PPP2011 (in 1000)")+
  theme_bw() +
  theme(
    legend.position = "bottom", legend.title = element_blank(), 
    axis.title.x = element_blank(), panel.border = element_blank(), 
    axis.line = element_line(), 
    strip.background = element_rect(fill="white", colour = "white")
  )
wage_plot

ggsave(plot = wage_plot, 
       filename = here("output/T5/LoehneProduktivitaet.pdf"),
       width = 7, height = 5)

# Lohnquote--------------------------------------------------------------------

epwt_wage_share <- epwt_wages_productivity %>%
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
       filename = here("output/T5/Lohnquote.pdf"),
       width = 7, height = 5)
