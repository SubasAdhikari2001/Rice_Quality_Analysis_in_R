
#### Circular bar plot with center open ####
library(tidyverse)

data_long <- data %>% 
  pivot_longer(cols = c(CL, KL, KB, KT), names_to = "Trait", values_to = "Value")

se_long <- data %>% 
  pivot_longer(cols = c(Se_CL, Se_KL, Se_KB, Se_KT), names_to = "Trait", values_to = "SE") %>% 
  mutate(Trait = str_remove(Trait, "Se_"))


data_combined <- left_join(data_long, se_long, by = c("Treatment", "Grain_size", "Trait"))

# Factoring Trait 
data_combined$Trait <- factor(data_combined$Trait, levels = c("CL", "KL", "KB", "KT"))

# Sorting treatments by Grain_size group 
data_combined <- data_combined %>%
  arrange(Grain_size, Treatment) %>%
  mutate(Treatment = factor(Treatment, levels = unique(Treatment)),
         ID = as.numeric(Treatment))

# Setting bar widths for each trait
bar_widths <- c(CL = 1, KL = 0.85, KB = 0.65, KT = 0.5)
data_combined <- data_combined %>%
  mutate(bar_width = bar_widths[as.character(Trait)])

# Defining radial breaks
radial_breaks <- pretty(range(data_combined$Value + data_combined$SE), n = 5)
max_radius <- max(radial_breaks) + 5

# y-position for custom treatment labels
label_positions <- data_combined %>%
  group_by(ID, Treatment) %>%
  summarise(y_pos = max(Value + SE) + 0.9, .groups = "drop")

# Plot
ggplot(data_combined, aes(x = as.factor(ID), y = Value, fill = Trait)) +
  geom_col(position = position_identity(),
           width = data_combined$bar_width,
           color = "black",
           size = 0.2) +
  geom_errorbar(aes(ymin = Value - SE, ymax = Value + SE),
                width = 0.25,
                size = 0.2,
                position = position_identity()) +
  geom_hline(yintercept = radial_breaks,
             color = "gray80",
             linetype = "dotted") +
  coord_polar(start = pi / 2) +
  geom_vline(data = data_combined %>%
               group_by(Grain_size) %>%
               summarise(end = max(ID)) %>%
               mutate(xintercept = end + 0.5),
             aes(xintercept = xintercept),
             color = "black",
             linetype = "dashed",
             size = 0.3) +
  # Grain size labels with KL color
  geom_text(data = data_combined %>%
              group_by(Grain_size) %>%
              summarise(start = min(ID), end = max(ID)) %>%
              mutate(center = (start + end) / 2,
                     label = case_when(
                       Grain_size == "Extra-long" ~ "Extra-long\n(>7.50 mm)",
                       Grain_size == "Long"       ~ "Long\n(6.61-7.50 mm)",
                       Grain_size == "Medium"     ~ "Medium\n(5.51-6.60 mm)",
                       Grain_size == "Short"      ~ "Short\n(<5.5 mm)",
                       TRUE ~ Grain_size
                     )),
            aes(x = center, y = max_radius - 1.5, label = label),
            inherit.aes = FALSE,
            size = 4,
            fontface = "bold",
            lineheight = 1.1,
            color = "#efa800") +  
  annotate("text",
           x = max(data_combined$ID) + 1,
           y = radial_breaks,
           label = radial_breaks,
           size = 3,
           hjust = 0) +
  # Custom Labels
  geom_text(data = label_positions,
            aes(x = as.factor(ID), y = y_pos, label = Treatment),
            inherit.aes = FALSE,
            size = 3,
            angle = 10,
            hjust = 0.5,
            vjust = 0.4) +
  scale_fill_manual(
    values = c(
      "CL" = "#f9f5e7", 
      "KL" = "#efa800",  
      "KB" = "#7FB03F",  
      "KT" = "#F37433"   
    ),
    labels = c(
      "CL" = "Cooked Length (CL)",
      "KL" = "Kernel Length (KL)",
      "KB" = "Kernel Breadth (KB)",
      "KT" = "Kernel Thickness (KT)"
    )
  ) +
  scale_y_continuous(limits = c(-2.5, max_radius),
                     breaks = radial_breaks) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  labs(
    fill = "Trait"
  )
