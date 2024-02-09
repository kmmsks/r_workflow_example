library(data.table)
library(magrittr)
library(here)
library(pxweb)
library(geofi)
library(terra)
library(sf)

dir.create(here("data_processed"))
dir.create(here("figs"))


# nightlights ------------------------------------------------------------------

# datan haku: https://medium.com/@bagasanin/java-islands-night-light-swipe-maps-using-r-shiny-5ff666cf1a16

urls <- c(
  "https://eogdata.mines.edu/nighttime_light/annual/v22/2022/VNL_v22_npp-j01_2022_global_vcmslcfg_c202303062300.average_masked.dat.tif.gz"
)

for (url in urls) {
  download.file(url = url,
                destfile = basename(url),
                mode = "wb")
}

raster_files <- list.files(
  path = getwd(),
  pattern = "npp",
  full.names = T
)

globe_lights <- lapply(
  paste0("/vsigzip/", raster_files),
  terra::rast
)

roi_lights_list <- lapply(
  globe_lights,
  function(x){
    terra::crop(
      x,
      terra::vect(roi_sf),
      snap = "in",
      mask = T
    )
  }
)

# REMOVE ZEROS AND SUBZEROS

roi_lights_final <- lapply(
  roi_lights_list,
  function(x){
    terra::ifel(
      x <= 0,
      NA,
      x
    )
  }
)

# RASTER TO DF

roi_lights_df <- lapply(
  roi_lights_final,
  function(x){
    as.data.frame(
      x,
      xy = T,
      na.rm = T
    )
  }
)

str(roi_lights_df)

col_names <- c("x", "y", "value")

roi_lights_df <- lapply(
  roi_lights_df,
  setNames,
  col_names
)

roi_lights <- roi_lights_df[[1]]

# get municipalities (geofi) ---------------------------------------------------
kunnat <- geofi::get_municipalities(year = 2022)

kunnat %<>% st_transform(crs = 4326)


## aggregate kunnat ----

kunnat$lights_mean <- extract(roi_lights_final[[1]], kunnat, fun = "mean", na.rm =T)[,2]


# kuntien tietoja StatFin ------------------------------------------------------

## fetch  ----

# look all data
# d <- pxweb_interactive("statfin.stat.fi")

### kuntien tulot ---- 
 d <- pxweb_interactive("https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/tjt/statfin_tjt_pxt_127y.px")
# * to all

tulot <- d$data %>% setDT()
 
#' ############# CITATION #############Statistics Finland (2024). “Asuntoväestön pienituloisuus ja pitkittynyt pienituloisuus muuttujina Alue, Tiedot ja Vuosi.” [Data accessed 2024-02-02 22:32:16.788023 using
#' pxweb R package 0.16.3], <https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/tjt/statfin_tjt_pxt_127y.px>.
#' 
#' A BibTeX entry for LaTeX users is
#' 
#' @Misc{,
#'   title = {Asuntoväestön pienituloisuus ja pitkittynyt pienituloisuus muuttujina Alue, Tiedot ja Vuosi},
#'   author = {{Statistics Finland}},
#'   organization = {Statistics Finland},
#'   address = {Helsinki, Finland},
#'   year = {2024},
#'   url = {https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/tjt/statfin_tjt_pxt_127y.px},
#'   note = {[Data accessed 2024-02-02 22:32:16.788023 using pxweb R package 0.16.3]},
#' }
#' Kindly cite the pxweb R package as follows:
#'   
#'   Mans Magnusson, Markus Kainu, Janne Huovari, and Leo Lahti (rOpenGov).  pxweb: R tools for PXWEB API.  URL: http://github.com/ropengov/pxweb
#' 
#' A BibTeX entry for LaTeX users is
#' 
#' @Misc{,
#'   title = {pxweb: R tools for PX-WEB API},
#'   author = {Mans Magnusson and Markus Kainu and Janne Huovari and Leo Lahti},
#'   year = {2019},
#' }
#' ############# CITATION #############

### kuntien kesamokit ----------------------------------------------------------
d <- pxweb_interactive("https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/rakke/statfin_rakke_pxt_116j.px")
mokit <- d$data %>% setDT()

#' ############# CITATION #############Statistics Finland (2024). “Kesämökit muuttujina Alue, Vuosi ja Tiedot.” [Data accessed 2024-02-02 22:30:57.322328 using pxweb R package 0.16.3],
#' <https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/rakke/statfin_rakke_pxt_116j.px>.
#' 
#' A BibTeX entry for LaTeX users is
#' 
#' @Misc{,
#'   title = {Kesämökit muuttujina Alue, Vuosi ja Tiedot},
#'   author = {{Statistics Finland}},
#'   organization = {Statistics Finland},
#'   address = {Helsinki, Finland},
#'   year = {2024},
#'   url = {https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/rakke/statfin_rakke_pxt_116j.px},
#'   note = {[Data accessed 2024-02-02 22:30:57.322328 using pxweb R package 0.16.3]},
#' }
#' Kindly cite the pxweb R package as follows:
#'   
#'   Mans Magnusson, Markus Kainu, Janne Huovari, and Leo Lahti (rOpenGov).  pxweb: R tools for PXWEB API.  URL: http://github.com/ropengov/pxweb
#' 
#' A BibTeX entry for LaTeX users is
#' 
#' @Misc{,
#'   title = {pxweb: R tools for PX-WEB API},
#'   author = {Mans Magnusson and Markus Kainu and Janne Huovari and Leo Lahti},
#'   year = {2019},
#' }
#' ############# CITATION ############# 

# save -------------------------------------------------------------------------

roi_lights_final[[1]] %>% writeRaster(here("data_processed", "roi_light.tif"), filetype = "GTiff", overwrite = TRUE)
 
roi_lights_df[[1]] %>% fwrite(here("data_processed", "roi_light_df.csv"))

st_delete(here("data_processed", "geofi_kunnat.geojson"))
kunnat %>% st_write(here("data_processed", "geofi_kunnat.geojson"))
 
tulot %>% fwrite(here("data_processed", "statfin_tulot_kunnittain.csv"))
mokit %>% fwrite(here("data_processed", "statfin_mokit_kunnittain.csv"))




