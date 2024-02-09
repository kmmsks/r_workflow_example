library(data.table)
library(magrittr)
library(here)
library(pxweb)
library(geofi)
library(terra)
library(sf)
library(geojsonio)
library(ggplot2)
library(ggrepel)
library(patchwork)


# settings ---------------------------------------------------------------------

# year of satellite data
y <- 2022

theme_set(theme_void())


# 01_get_data.R nightlights, kunnat and  statfin -------------------------------

## source ----

# source("01_get_data.R")

## read data_processed ----

kunnat <- st_read(here("data_processed", "geofi_kunnat.geojson"), quiet = TRUE)

tulot <- fread(here("data_processed", "statfin_tulot_kunnittain.csv"))
mokit <- fread(here("data_processed", "statfin_mokit_kunnittain.csv"))

roi_lights_final <- rast(here("data_processed", "roi_light.tif"))
roi_lights_df    <- fread(here("data_processed", "roi_light_df.csv")) 


kunnat %<>% subset(nimi != "Närpiö")

# Region of Interest -----------------------------------------------------------
roi_sf <- kunnat %>% st_union()

# Merge StatFin data to kunnat sf data -----------------------------------------

pienituloisuusaste <- tulot[,.(Alue, vuosi = Vuosi, val = `Asuntoväestön pienituloisuusaste (raja 60 % mediaanista)`)]


kunnat <- merge(
  kunnat,
  pienituloisuusaste[vuosi == y, .(Alue, pienituloisuusaste = val)],
  by.x = "nimi",
  by.y = "Alue",
  all.x =T)

kunnat <- merge(
  kunnat,
  mokit[Vuosi == y, .(Alue, mokit_lkm = `Kesämökkejä (lkm)`)],
  by.x = "nimi",
  by.y = "Alue",
  all.x =T)



# figs -------------------------------------------------------------------------

figs <- list()

## nightlights ----

roi_sf %>% ggplot()+ geom_sf()

color <- c("#1f4762", "#FFD966", "white")

pal <- colorRampPalette(
  color, bias = 8
)(512)

### fig_nl ----

figs$nl <- roi_lights_df %>% ggplot() +
  geom_sf(
    data = roi_sf,
    fill = NA,
    color = color[[1]],
    size = .5
  ) +
  geom_tile(
    aes(
      x = x,
      y = y,
      fill = value
    )
  ) +
  scale_fill_gradientn(
    name = "",
    colors = pal
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      size = 60,
      color = "white",
      hjust = .5,
      vjust = 0
    ),
    plot.margin = unit(
      c(
        t = 0, r = 0,
        l = 0, b = 0
      ), "lines"
    )
  )

### fig_nl_kunnat ----

figs$nl_kunnat <- figs$nl + geom_sf(
  data = kunnat,
  fill = NA,
  color = "red",
  size = .3
)


### fig_nl_mean ----

figs$nl_mean <- kunnat %>%  ggplot(aes(fill = lights_mean), label = nimi) + geom_sf(colour = alpha("white", 1/3))+
  scale_fill_viridis_c( option = "B", trans = "log")

## statfgin figs ----

### fig_pienituloisuus ----

figs$pienituloisuus <- kunnat %>%  ggplot(aes(fill = pienituloisuusaste)) + geom_sf(colour = alpha("white", 1/3)) +
  scale_fill_viridis_c(option = "B", direction = -1)

### fig_mokit ----

figs$mokit <- kunnat %>%  ggplot(aes(fill = mokit_lkm)) + geom_sf(colour = alpha("white", 1/3)) +
  scale_fill_viridis_c(option = "D", direction = 1)



lapply(figs %>% names(), function(x) ggsave(here("figs", paste0(x, ".pdf")), plot = figs[[x]], device = "pdf", width =8, height = 15, unit = "cm"))

## vierekkain ----

#figs$nl_kunnat + figs$nl_mean + figs$pienituloisuus + figs$mokit

# scatter ----------------------------------------------------------------------

scttr <- list() 
p_vals <- list()

scttr$tulot_nl <-  kunnat %>%  ggplot(aes(y = pienituloisuusaste, x = lights_mean %>% log()))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  theme_classic()+
  stat_cor(method = "pearson", label.x = 3, label.y = 25)

p_vals$nl = lm(pienituloisuusaste~log(lights_mean), data = kunnat) %>% summary() %>% .$coefficients %>% .[2,4]

scttr$mokit_nl <-  kunnat %>%  ggplot(aes(y = mokit_lkm, x = lights_mean %>% log()))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  theme_classic() +
  stat_cor(method = "pearson", label.x = 3, label.y = 10000)

scttr$tulot_mokit <-  kunnat %>%  ggplot(aes(y = mokit_lkm, x = pienituloisuusaste))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  theme_classic() +
  stat_cor(method = "pearson", label.x = 20, label.y = 10000)
