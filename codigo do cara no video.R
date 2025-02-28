library(tidyverse)

t_diff <- read_csv("temps.csv", skip = 1, na = "***") |>
  select(year = Year, month.abb) |>
  pivot_longer(-year, names_to="month", values_to="t_diff") %>%
  drop_na()
next_jan <- t_diff %>%
  filter(month == "Jan") %>%
  mutate(year = year - 1,
         month = "next_Jan")

t_data <- bind_rows(t_diff, next_jan) %>%
  mutate(month = factor(month, levels = c(month.abb, "next_Jan")),
         month_number = as.numeric(month))

temp_lines <- tibble(
  x = 12,
  y = c(1.5, 2.0),
  labels = c("1.5\u00B0C", "2.0\u00B0C")
)
month_labels <- tibble(
  x = 1:12,
  labels = c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago",
             "Set","Out","Nov","Dez"),
  y = 2.7
)

t_data %>% 
  ggplot(aes(x=month_number, y=t_diff, group=year, color=year)) +
  geom_col(data = month_labels, aes(x=x + 0.5, y=2.4), fill = "black",
           width  = 1,
           inherit.aes = FALSE) +
  geom_col(data = month_labels, aes(x=x + 0.5, y=-2), fill = "black",
           width  = 1,
           inherit.aes = FALSE) +
  geom_hline(yintercept = c(1.5, 2.0), color="white") +
  geom_line(size=1) +
  geom_label(data = temp_lines, aes(x=x, y=y, label=labels),
             color = "white", fill = "black", label.size = 0,
             inherit.aes=FALSE) +
  geom_text(data = month_labels, aes(x=x, y=y, label = labels),
            inherit.aes = FALSE, color="white",
            angle = seq(360 - 360/12, 0, length.out = 12)) +
  #geom_text(aes(x = 1, y=-1.3, label = year), size=6) +
  scale_x_continuous(breaks=1:12,
                     labels=month.abb, expand = c(0,0),
                     sec.axis = dup_axis(name = NULL, labels=NULL)) +
  scale_y_continuous(breaks = seq(-2, 2, 0.2),
                     limits = c(-2, 2.7), expand = c(0, -0.7), 
                     sec.axis = dup_axis(name = NULL, labels=NULL)) +
  scale_color_distiller(palette="RdYlBu",breaks = seq(1880, 2024, 20),
                        guide = "none") +
  # coord_cartesian(xlim=c(1,12)) +
  coord_polar(start = 2*pi/12) +
  labs(x = NULL,
       y = NULL,
       title = "Global temperature change (1880-2024)") +
  theme(
    panel.background = element_rect(fill="#444444", size=1),
    plot.background = element_rect(fill = "#444444", color="#444444"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(color="white", size=13),
    plot.title = element_text(color="white", hjust = 0.5,size = 15)
  ) + 
  transition_manual(frames = year, cumulative = T)