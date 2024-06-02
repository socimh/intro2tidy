# Goal: To create a word chain visualization
# Author: Minghong SHEN
# Date: 2024-02-13
# Output: word_chain_short.png, word_chain_long.png

pacman::p_load(showtext, tidyverse)
showtext_auto()

# Short word chain -----------------------
tb <- tibble(
  x = 1:11 %>% rep(8),
  y = 1:8 %>% rep(11)
) %>%
  arrange(x, y) %>%
  mutate(
    fill = case_when(
      x == 1 & y == 1 ~ 1,
      x == 4 & y == 1 ~ 2,
      x == 4 & y == 2 ~ 2,
      x == 7 & y == 2 ~ 3,
      x == 7 & y == 3 ~ 3,
      x == 10 & y == 3 ~ 4,
      TRUE ~ NA_real_
    ) %>% factor(),
    label = case_when(
      x == 1 & y == 1 ~ "胡",
      x == 2 & y == 1 ~ "作",
      x == 3 & y == 1 ~ "非",
      x == 4 & y == 1 ~ "为",
      x == 4 & y == 2 ~ "为",
      x == 5 & y == 2 ~ "富",
      x == 6 & y == 2 ~ "不",
      x == 7 & y == 2 ~ "仁",
      x == 7 & y == 3 ~ "仁",
      x == 8 & y == 3 ~ "至",
      x == 9 & y == 3 ~ "义",
      x == 10 & y == 3 ~ "尽",
      TRUE ~ NA_character_
    ),
    rev_y = 9 - y
  ) %>%
  filter(!is.na(label))

tb2 <- tibble(
  x = c(7, 4),
  y = seq(6.75, 7.75),
  xend = c(7, 4),
  yend = c(6.3, 7.3),
  color = factor(2:1)
)

gg <- tb %>%
  ggplot(aes(x, rev_y)) +
  geom_tile(
    aes(fill = fill),
    alpha = .5
  ) +
  geom_segment(
    data = tb2,
    aes(x = x, y = y, xend = xend, yend = yend, color = color),
    lwd = 1.3,
    arrow = arrow(
      length = unit(.25, "cm"),
      type = "closed"
    )
  ) +
  geom_text(
    aes(label = label),
    color = "black",
    size = 20
  ) +
  scale_color_manual(
    values = c(
      "#1d5482",
      "#1d8225"
    ),
    guide = "none"
  ) +
  scale_fill_brewer(
    palette = "Set1",
    guide = "none"
  ) +
  scale_y_continuous(
    breaks = 1:8,
    labels = paste0("第", 8:1, "轮")
  ) +
  coord_cartesian(
    xlim = c(1, 10),
    ylim = c(.85, 8.15)
  ) +
  ylab("接龙次序") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 50),
    axis.ticks.y = element_line(size = 1),
    axis.title.y = element_text(size = 70),
    panel.grid = element_blank()
  )

ggsave(
  "word_chain_short.png", gg,
  width = 6, height = 6,
  units = "in", dpi = 300,
  bg = "white"
)

# Long word chain -----------------------
tb <- tibble(
  x = 1:11 %>% rep(10),
  y = -1:8 %>% rep(11)
) %>%
  arrange(x, y) %>%
  mutate(
    fill = case_when(
      x == 5 & y == 1 ~ 1,
      x %in% c(4, 7) & y %in% 1:8 ~ 2,
      TRUE ~ NA_real_
    ) %>% factor(),
    label = case_when(
      x %in% c(4, 7) & y %in% 1:8 ~ "为",
      x == 5 & y %in% 1:8 ~ "所",
      x == 6 & y %in% 1:8 ~ "欲",
      TRUE ~ NA_character_
    ),
    alpha = x == 5 & y == 1,
    rev_y = 9 - y
  ) %>%
  filter(!is.na(label))

tb2 <- tibble(
  x = rep(6.7, 7),
  y = seq(1.75, 7.75, 1),
  xend = rep(4.3, 7),
  yend = seq(1.25, 7.25, 1)
)

gg <- tb %>%
  ggplot(aes(x, rev_y)) +
  geom_tile(
    aes(fill = fill, alpha = alpha)
  ) +
  geom_segment(
    data = tb2,
    aes(x = x, y = y, xend = xend, yend = yend),
    lwd = 1.3, color = "#1d5482",
    arrow = arrow(
      length = unit(.25, "cm"),
      type = "closed"
    )
  ) +
  geom_text(
    aes(label = label),
    color = "black",
    size = 20
  ) +
  scale_alpha_manual(
    values = c(.5, 0),
    guide = "none"
  ) +
  scale_fill_brewer(
    palette = "Set1",
    guide = "none"
  ) +
  scale_y_continuous(
    breaks = 1:8,
    labels = paste0("第", 8:1, "轮")
  ) +
  coord_cartesian(
    xlim = c(1, 10),
    ylim = c(.85, 8.15)
  ) +
  ylab("接龙次序") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 50),
    axis.ticks.y = element_line(linewidth = 1),
    axis.title.y = element_text(size = 70),
    panel.grid = element_blank()
  )

ggsave(
  "word_chain_long.png", gg,
  width = 6, height = 6,
  units = "in", dpi = 300,
  bg = "white"
)

# Long word chain (old) -----------------------
tb <- tibble(
  x = 1:11 %>% rep(8),
  y = 1:8 %>% rep(11)
) %>%
  arrange(x, y) %>%
  mutate(
    fill = case_when(
      x == 1 & y == 1 ~ 1,
      x == 4 & y == 1 ~ 2,
      x %in% c(4, 7) & y %in% 2:6 ~ 2,
      x == 4 & y == 7 ~ 2,
      x == 7 & y == 7 ~ 3,
      x == 7 & y == 8 ~ 3,
      x == 10 & y == 8 ~ 4,
      TRUE ~ NA_real_
    ) %>% factor(),
    label = case_when(
      x == 1 & y == 1 ~ "胡",
      x == 2 & y == 1 ~ "作",
      x == 3 & y == 1 ~ "非",
      x == 4 & y == 1 ~ "为",
      x %in% c(4, 7) & y %in% 2:6 ~ "为",
      x == 5 & y %in% 2:6 ~ "所",
      x == 6 & y %in% 2:6 ~ "欲",
      x == 4 & y == 7 ~ "为",
      x == 5 & y == 7 ~ "富",
      x == 6 & y == 7 ~ "不",
      x == 7 & y == 7 ~ "仁",
      x == 7 & y == 8 ~ "仁",
      x == 8 & y == 8 ~ "至",
      x == 9 & y == 8 ~ "义",
      x == 10 & y == 8 ~ "尽",
      TRUE ~ NA_character_
    ),
    rev_y = 9 - y
  ) %>%
  filter(!is.na(label))

tb2 <- tibble(
  x = c(7, rep(6.7, 5), 4),
  y = seq(1.75, 7.75, 1),
  xend = c(7, rep(4.3, 5), 4),
  yend = c(1.3, seq(2.25, 6.25, 1), 7.3),
  color = factor(c(2, rep(1, 6)))
)

gg <- tb %>%
  ggplot(aes(x, rev_y)) +
  geom_tile(
    aes(fill = fill),
    alpha = .5
  ) +
  geom_segment(
    data = tb2,
    aes(x = x, y = y, xend = xend, yend = yend, color = color),
    lwd = 1.3,
    arrow = arrow(
      length = unit(.25, "cm"),
      type = "closed"
    )
  ) +
  geom_text(
    aes(label = label),
    color = "black",
    size = 20
  ) +
  annotate(
    "rect",
    xmin = 3.5, xmax = 7.5,
    ymin = 2.5, ymax = 7.5,
    fill = NA, color = "black",
    lwd = 1.5,
    linetype = "dashed"
  ) +
  scale_color_manual(
    values = c(
      "#1d5482",
      "#1d8225"
    ),
    guide = "none"
  ) +
  scale_fill_brewer(
    palette = "Set1",
    guide = "none"
  ) +
  scale_y_continuous(
    breaks = 1:8,
    labels = paste0("第", 8:1, "轮")
  ) +
  coord_cartesian(
    xlim = c(1, 10),
    ylim = c(.85, 8.15)
  ) +
  ylab("接龙次序") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 50),
    axis.ticks.y = element_line(size = 1),
    axis.title.y = element_text(size = 70),
    panel.grid = element_blank()
  )

ggsave(
  "word_chain_long2.png", gg,
  width = 6, height = 6,
  units = "in", dpi = 300,
  bg = "white"
)
