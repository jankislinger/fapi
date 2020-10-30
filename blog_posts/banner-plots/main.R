require(tidyverse)
require(readxl)
library(ggridges)
library(ggrepel)

FONT_SIZE <- 18
WIDTH <- 16
HEIGHT <- 9
ROOT_PLOTS <- "banner-plots/plots/"

save_plot <- function(filename, plot) {
  p <- p + theme_gray(base_size = FONT_SIZE)
  ggsave(paste0(ROOT_PLOTS, filename, ".svg"), p, width = WIDTH, height = HEIGHT)
  ggsave(paste0(ROOT_PLOTS, filename, ".png"), p, width = WIDTH, height = HEIGHT)
}


# temperature -------------------------------------------------------------

data <- read_excel("banner-plots/data/PKLM_pro_portal.xlsx", sheet = "data") %>% 
  rename(year = "rok", month = "měsíc", day = "den", temp_avg = "T-AVG") %>% 
  filter(year >= 1950) %>% 
  group_by(month) %>% 
  mutate(monthly_average = mean(temp_avg))

months <- c("Leden", "Únor", "Březen", "Duben", "Květen", "Červen", "Červenec",
            "Srpen", "Září", "Říjen", "Listopad", "Prosinec")

p <- ggplot(data, aes(temp_avg, month, group = month, fill = stat(x))) + 
  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.01, show.legend = F) +
  scale_y_continuous(breaks = 1:12, labels = months, trans = "reverse") +
  scale_fill_viridis_c(option = "C") +
  xlim(c(-15, 31)) +
  labs(x = "Denní průměrná teplota", y = NULL) +
  ggtitle("Průměrné denní teploty během roku")

save_plot("temperatures", p)


# polynomial fit ----------------------------------------------------------

num_points <- 200
data <- tibble(
  x = runif(num_points,  min=-10, max=10),
  y = 0.1 * x^3 - 0.5 * x^2 - x + 10 + rnorm(num_points, sd = 20)
)

p <- ggplot(data, aes(x, y)) +
  geom_smooth(method = "lm", level = 1 - 1e-10,
              formula = y ~ x + I(x^2) + I(x^3), color = "red", se = T) +
  geom_point(color = "blue", alpha = 0.5, size = 3) +
  labs(x = NULL, y = NULL) +
  ggtitle("Polynomiální regrese")

save_plot("polynomial", p)


# gapminder ---------------------------------------------------------------

p <- gapminder::gapminder %>% 
  filter(year == 1992) %>% 
  ggplot(aes(gdpPercap, lifeExp, size = pop / 1e6, color = continent)) +
  geom_point(alpha = 0.3) +
  scale_size_continuous(range = c(1, 25), guide = NULL) +
  labs(x = "HDP na obyvatele", y = "Očekávaná doba dožití", color = "Kontinent", size = NULL) +
  ggtitle("Gapminder (1992)")

save_plot("gapminder", p)



# iris --------------------------------------------------------------------

p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + 
  geom_point(size = 3) +
  facet_grid(~Species) +
  scale_color_discrete(guide = NULL) +
  labs(x = "Délka kalichu", y = "Šířka kalichu") + 
  ggtitle("Typy kosatců a jejich velikosti")

save_plot("iris", p)


# stock markets -----------------------------------------------------------

p <- EuStockMarkets %>% 
  as_tibble() %>% 
  mutate(date = seq(as.Date("1991/05/10"), by = "day", length.out = n())) %>% 
  gather("index", "value", -date) %>% 
  filter(date <= as.Date('1996/03/01')) %>% 
  ggplot(aes(date, value, color = index)) +
  geom_line() +
  geom_label(aes(label = index), . %>% filter(date == max(date)), show.legend = F) +
  scale_color_discrete(guide = NULL) +
  labs(x = "Datum", y = "Hodnota indexu") +
  ggtitle("Vývoj cen finančních indexů")

save_plot("stock_markets", p)


p <- ggplot(diamonds, aes(cut, fill = color)) +
  geom_bar(position="dodge") +
  labs(x = "Kvalita řezu", y = "Počet", fill = "Barva") +
  ggtitle("Počty diamantů dle kvality a barvy")
save_plot("diamonds", p)



# boxplot -----------------------------------------------------------------

p <- crossing(variety = LETTERS[1:7],
         treatment = c("high","low"),
         i = 1:40) %>% 
  mutate(note = sqrt(seq(n())) * 2 + sample(1:40, n(), replace=T),
         note = note - ifelse(treatment == "high", 3, 0)) %>% 
  ggplot(aes(variety, note, fill = treatment)) + 
  geom_boxplot() +
  labs(x = "Varianta", y = NULL, fill = NULL) +
  ggtitle("Vliv úrovně léčby pro různé varianty")
p
save_plot("boxplot", p)
