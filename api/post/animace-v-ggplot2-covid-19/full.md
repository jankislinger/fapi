Knihovna `ggplot2` je v současnosti nejefektivnější nástroj pro
vizualizaci dat. V kombinaci s nástroji pro práci s daty z knihovny
`dplyr` zvládnete v rychlosti vytvořit prakticky libovolný graf. Spousta
uživatelů však neví, že cesta od statického grafu k dynamické animaci je
jen jediný příkaz. Výsledek si můžete uložit například do obrázku ve
formátu *gif* nebo do videa. V tomto článku si na velmi aktuálních
datech ukážeme, jak je to snadné.

Pro přípravu dat budeme potřebovat knihovnu `readr`, pro přípravu dat
`dplyr` a pro samotnou vizualizaci `ggplot2`. Všechny tyto knihovny jsou
obsaženy v knihovně `tidyverse`, takže postačí, když načteme pouze tu.
Pro převod grafu do animace použijeme knihovnu `gganimate`.

``` r
require(tidyverse)
require(gganimate)
```

## Příprava dat

Jako zdroj dat použijeme organizaci [Our World in
Data](https://ourworldindata.org/coronavirus). Data
obsahují pro každý stát a každý den informace o počtu provedených testů,
počtu potvrzených případů i počtu úmrtí. Obsahují i některé popisné
statistiky o jednotlivých státech, ale těmi se zabývat
nebudeme.

``` r
covid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
```

Z dat je potřeba nejprve odstranit souhrnná data za celý svět. Pro naši
vizualizaci vezmeme pouze data od začátku března. Pro každý den nám
stačí informace o 15 státech s největším počtem potvrzených případů.
Proto si zvlášť pro každý den jednotlivé státy očíslujeme a necháme si
pouze prvních 15. Dále si můžeme nechat pouze ty sloupečky, které
skutečně potřebujeme.

``` r
num_countries <- 15
data <- covid %>%
  filter(location != "World", date >= "2020-03-01") %>%
  group_by(date) %>%
  mutate(rank = rank(-total_cases, ties.method = 'first')) %>%
  ungroup() %>%
  filter(rank <= num_countries) %>%
  select(location, date, total_cases, rank)
```

Vytvořená data (prvních několik řádků) vypadá takto.

| location  | date       | total\_cases | rank |
| :-------- | :--------- | -----------: | ---: |
| Australia | 2020-03-03 |           33 |   14 |
| Australia | 2020-03-04 |           41 |   14 |
| Australia | 2020-03-05 |           52 |   14 |
| Australia | 2020-03-25 |         2423 |   15 |
| Austria   | 2020-03-17 |         1016 |   15 |
| Austria   | 2020-03-18 |         1332 |   12 |

## Statický graf

Jak bylo zmíněno v úvodu, přechod z grafu do animace se dá zvládnout
jediným příkazem. Začneme proto přípravou statického grafu do
požadovaného formátu. Ten poté jednoduše převedeme do animace. Pro
statický graf nám postačí data pro jeden den.

``` r
selected_date <- as.Date("2020-04-30")
data_day <- data %>%
  filter(date == selected_date)
```

Pro sloupcový graf bychom běžně použili vrstvu `geom_bar`, kde bychom na
ose *x* měli jednotlivé státy - tedy kategorickou proměnnou. Pro hladký
přechod na animaci potřebujeme mít na ose *x* číselnou proměnnou -
pořadí státu dle počtu případů. Provizualizaci použijeme vrstvu
`geom_tile`. U té je potřeba specifikovat střed sloupce a jeho výšku.
Jednotlivé státy rozlišíme barevně.

``` r
p <- ggplot(data_day, aes(rank)) +
  geom_tile(aes(y = total_cases / 2, height = total_cases, fill = location), width = 0.75)
plot(p)
```

![](/assets/gganimate-covid/init-plot-1.png)

V dalším kroku zaměníme osy a u osy *x* převrátíme pořadí (1 je nahoře, 15 dole).Přestože
jsme prohodili osy, značení stále *x* odpovídá pořadí státu a *y* počtu případů. Pořadí
v grafu nechceme, proto jej odstraníme pomocí `breaks=NULL`.

``` r
p <- p +
  coord_flip() +
  scale_x_reverse(breaks = NULL)
plot(p)
```

![](/assets/gganimate-covid/coord-flip-1.png)

Název státu nechceme mít v legendě, ale přímo v grafu - i text bude
pohyblivý. Ten nastavíme tak, aby pravý okraj textu (`hjust='right'`)
byl na hodnotě 0. Pro lepší čitelnost můžeme odsadit o jednu mezeru.
Název státu odstraníme z legendy pomocí `guide=F` u obou parametrů
barvy. Při vykreslování grafu `ggplot` ignoruje velikost textu a je
možné, že se text nevejde do obrázku. V tomto případě je nejjednodušší
ručně rozšířit oblast grafu o předem zvolenou hodnotu pomocí vrstvy
`expand_limits()`.

``` r
p <- p +
  geom_text(aes(y = 0, label = paste0(location, " "), color = location), hjust = 'right') +
  scale_fill_discrete(guide = F) +
  scale_color_discrete(guide = F) +
  expand_limits(y = -270000)
plot(p)
```

![](/assets/gganimate-covid/locations-1.png)

Poslední úpravy provedeme na popiskách grafu. Názvy os odstraníme a
přidáme nadpis a podnadpis. Na ose *y* zvolíme popisky po 200
tisících, které můžeme upravit do libovolného tvaru. Všimněte si, že
volba svislých čar (`breaks` a `minor_breaks`) nijak neupravuje rozsah
grafu. Někomu se líbí šedivé pozadí grafu, já preferuji jednodušší
vzhled. Motiv si snadno změníte pomocí některé z vrstev `theme_*`.

``` r
p <- p +
  labs(x = NULL, y = NULL, title = "Covid-19 potvrzené případy",
       subtitle = paste("datum:", selected_date)) +
  scale_y_continuous(
    breaks = seq(0, 2e6, 2e5),
    labels = c("0", paste0(seq(200, 800, 200), "k"), paste0(seq(1, 2, 0.2), "M")),
    minor_breaks = seq(0, 2e6, 1e5)) +
  theme_minimal()
plot(p)
```

![](/assets/gganimate-covid/labels-1.png)

Graf je samozřejmě možné vytvořit jednou posloupností příkazů.

``` r
p <- ggplot(data_day, aes(rank)) +
  geom_tile(aes(y = total_cases / 2, height = total_cases, fill = location), width = 0.75) +
  coord_flip() +
  scale_x_reverse(breaks = NULL) +
  geom_text(aes(y = 0, label = paste0(location, " "), color = location), hjust = 'right') +
  scale_fill_discrete(guide = F) +
  scale_color_discrete(guide = F) +
  expand_limits(y = -250000) +
  labs(x = NULL, y = NULL, title = "Covid-19 potvrzené případy",
       subtitle = paste("datum:", selected_date)) +
  scale_y_continuous(
    breaks = seq(0, 2e6, 2e5),
    labels = c("0", paste0(seq(200, 800, 200), "k"), paste0(seq(1, 2, 0.2), "M")),
    minor_breaks = seq(0, 2e6, 1e5)) +
  theme_minimal()
```

## Animace

Pokud máme graf poskládaný tak, jak bychom chtěli, můžeme přejít k
samotné animaci. Tu vytvoříme přidáním vrstvy `transition_time` se
správnou volbou sloupce s datem. Samozřejmě je nutné nyní použít
kompletní data namísto jednoho dne. V animaci máme možnost také
dynamicky měnit datum v nadpisu grafu. To můžeme udělat pomocí proměnné
`frame_time` ve složených závorkách. Všechny tři upravené řádky jsou v
kódu
označeny.

``` r
anim <- ggplot(data, aes(rank)) +                                          # <<<<<<<<<<<
  geom_tile(aes(y = total_cases / 2, height = total_cases, fill = location), width = 0.75) +
  coord_flip() +
  scale_x_reverse(breaks = NULL) +
  geom_text(aes(y = 0, label = paste0(location, " "), color = location), hjust = 'right') +
  scale_fill_discrete(guide = F) +
  scale_color_discrete(guide = F) +
  expand_limits(y = -300000) +
  labs(x = NULL, y = NULL, title = "Covid-19 potvrzené případy",
       subtitle = "datum: {frame_time}") +                                 # <<<<<<<<<<<
  scale_y_continuous(
    breaks = seq(0, 2e6, 2e5),
    labels = c("0", paste0(seq(200, 800, 200), "k"), paste0(seq(1, 2, 0.2), "M")),
    minor_breaks = seq(0, 2e6, 1e5)) +
  transition_time(date) +                                                  # <<<<<<<<<<<
  theme_minimal()
```

Animaci již zbývá pouze vykreslit. Takto si ji můžeme jednoduše
zobrazit. Parametr `nframes` určuje rychlost animace - čím více snímků,
tím pomalejší a delší animace bude. Aby animace neskončila hned za
posledním datem, je možné si ji chvíli pozdržet parametrem `end_pause`.

``` r
animate(anim, nframes = 500, end_pause = 25)
```

![](/assets/gganimate-covid/animate-1.gif)

Alternativně je možné animace rovnou uložit do souboru ve formátu gif,
mp4 a spousty
dalších.

``` r
animate(anim, nframes = 500, end_pause = 25, renderer = gifski_renderer("covid.gif"))
animate(anim, nframes = 500, end_pause = 25, renderer = av_renderer("covid.mp4"))
```
