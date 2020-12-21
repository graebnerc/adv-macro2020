library(data.table)
library(tidyverse)
library(here)
abstimmung <- fread(here("data/raw/abstimmung.csv"), 
                    sep = ";") %>%
  group_by(Abstimmung) %>%
  summarise(n=n(), .groups="drop") %>%
  mutate(Drinnen=ifelse(n>5, "1", "0"))


abstimmung_plot <- ggplot(data = abstimmung, 
       aes(x=reorder(Abstimmung, n), 
           y = n, fill=Drinnen)) +
  scale_fill_viridis_d(option = "E") +
  scale_y_continuous(
    breaks = seq(0, 9, 1), 
    expand = expansion()
    ) +
  geom_bar(stat = "identity") + theme_bw() +
  theme(panel.border = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 75, hjust = 1),
        legend.position = "none", 
        axis.line = element_line(), 
        axis.ticks = element_blank())

ggsave(plot = abstimmung_plot, filename = here("output/abstimmungsergebnis.pdf"), width = 6, height = 4)
