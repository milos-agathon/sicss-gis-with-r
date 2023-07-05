###################################################
#                 GIS and spatial analysis with R
#                 Milos Popovic
#                 2023/07/06
###################################################
setwd() # please set your working directory
main_dir <- getwd()
# libraries we need
libs <- c(
    "tidyverse", "geodata",
    "sf", "ggmap", "maps",
    "ggrepel"
)

# install missing libraries

installed_libs <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libs == F)) {
    install.packages(
        libs[!installed_libs]
    )
}

# load libraries

invisible(lapply(
    libs, library,
    character.only = T
))

# 1. DATA
#-----------
url <- "https://ucdp.uu.se/downloads/ged/ged231-csv.zip"
file_name <- "ucdp-ged.zip"

download.file(
    url = url,
    destfile = file_name,
    mode = "wb"
)

unzip(file_name)

ucdp_ged <- read.csv(
    "GEDEvent_v23_1.csv",
    sep = ","
)

names(ucdp_ged)

# ukraine dataframe
unique(ucdp_ged$country)
ucdp_ged_ukraine <- ucdp_ged |>
    dplyr::filter(
        country == "Ukraine"
    ) |>
    dplyr::select(
        26, 30:31, 39,
        43
    ) |>
    dplyr::mutate(
        time = as.Date(
            date_start
        )
    ) |>
    dplyr::filter(
        time >= as.Date(
            "2022-02-24"
        )
    ) |>
    dplyr::mutate(
        month_year = as.factor(format(
            as.Date(time), "%Y-%m"
        ))
    )


summary(ucdp_ged_ukraine$time)
nrow(ucdp_ged_ukraine)
names(ucdp_ged_ukraine)

# ggplot2 theme
theme_for_the_win <- function() {
    theme_void() +
        theme(
            legend.position = "right",
            legend.text = element_text(
                size = 11, color = "grey10"
            ),
            legend.title = element_text(
                size = 12, color = "grey10"
            ),
            legend.key = element_blank(),
            plot.margin = unit(
                c(t = -5, r = 2, b = -5, l = .1),
                "lines"
            ),
            plot.title = element_text(
                face = "bold", size = 18,
                color = "darkviolet", hjust = .5
            ),
            plot.caption = element_text(
                size = 10, color = "grey30",
                hjust = .5, vjust = 0
            )
        )
}

# simple plot
ggplot(data = ucdp_ged_ukraine) +
    geom_point(
        aes(
            x = longitude,
            y = latitude,
            size = deaths_civilians
        )
    ) +
    theme_for_the_win()

# admin level 1 Ukraine
ukraine_adm1 <- geodata::gadm(
    country = "UKR",
    level = 1,
    path = main_dir
) |>
    sf::st_as_sf()

# adding Ukraine level 1 admin
p1 <- ggplot(data = subset(
    ucdp_ged_ukraine, deaths_civilians != 0
)) +
    geom_point(
        aes(
            x = longitude,
            y = latitude,
            size = deaths_civilians
        ),
        color = "darkviolet"
    ) +
    geom_sf(
        data = ukraine_adm1,
        fill = "transparent",
        color = "black",
        size = .15
    ) +
    scale_size(
        name = "",
        range = c(1, 10)
    ) +
    theme_for_the_win() +
    labs(
        title = "Civilian deaths in Ukraine in 2022",
        caption = "Data: UCDP Georeferenced Event Dataset (GED) Global version 23.1"
    )

ggsave(
    "ukraine_civilian_deaths1.png", p1,
    width = 9, height = 6,
    units = "in", bg = "white"
)

# 2. LABELS
#----------

data(world.cities)
ukraine_cities <- world.cities |>
    dplyr::filter(
        country.etc == "Ukraine"
    ) |>
    dplyr::slice_max(pop, n = 15)


p2 <- ggplot(data = subset(
    ucdp_ged_ukraine, deaths_civilians != 0
)) +
    geom_point(
        aes(
            x = longitude,
            y = latitude,
            size = deaths_civilians
        ),
        color = "darkviolet",
        alpha = .75,
        shape = 21
    ) +
    geom_sf(
        data = ukraine_adm1,
        fill = "transparent",
        color = "grey10",
        size = .15
    ) +
    ggrepel::geom_text_repel(
        data = ukraine_cities,
        aes(x = long, y = lat, label = name),
        size = 3,
        color = "grey10",
        segment.size = .25,
        nudge_y = -.75,
        direction = "x",
        force = 5,
        segment.curvature = .6
    ) +
    geom_point(
        data = ukraine_cities,
        aes(
            x = long,
            y = lat
        ),
        size = 1,
        color = "black"
    ) +
    scale_size(
        name = "",
        range = c(1, 10)
    ) +
    theme_for_the_win() +
    labs(
        title = "Civilian deaths in Ukraine in 2022",
        caption = "Data: UCDP Georeferenced Event Dataset (GED) Global version 23.1"
    )

ggsave(
    "ukraine_civilian_deaths2.png", p2,
    width = 9, height = 6,
    units = "in", bg = "white"
)

# 3. STREET LAYER
#---------------------

# Ukraine
ukraine <- sf::st_union(
    ukraine_adm1
)

ukraine_bbox <- sf::st_bbox(ukraine)

ukraine_coords <- c(
    ukraine_bbox[["xmin"]],
    ukraine_bbox[["ymin"]],
    ukraine_bbox[["xmax"]],
    ukraine_bbox[["ymax"]]
)

ukraine_layer <- ggmap::get_stamenmap(
    ukraine_coords,
    zoom = 7,
    maptype = "terrain"
)

p3 <- ggmap(ukraine_layer) +
    geom_point(
        data = subset(
            ucdp_ged_ukraine,
            deaths_civilians != 0
        ),
        aes(
            x = longitude,
            y = latitude,
            size = deaths_civilians
        ),
        color = "darkviolet",
        alpha = .75,
        shape = 21,
        inherit.aes = F
    ) +
    scale_size(
        name = "Civilian deaths",
        range = c(1, 10)
    ) +
    theme_for_the_win() +
    labs(
        title = "Civilian deaths in Ukraine in 2022",
        caption = "Data: UCDP Georeferenced Event Dataset (GED) Global version 23.1"
    )

ggsave(
    "ukraine_civilian_deaths3.png", p3,
    width = 9, height = 6,
    units = "in", bg = "white"
)

# 4. MONTHLY ANALYSIS
#--------------------

p4 <- ggmap(ukraine_layer) +
    geom_point(
        data = subset(
            ucdp_ged_ukraine,
            deaths_civilians != 0
        ),
        aes(
            x = longitude,
            y = latitude,
            size = deaths_civilians
        ),
        color = "darkviolet",
        alpha = .75,
        shape = 21,
        inherit.aes = F
    ) +
    geom_sf(
        data = ukraine_adm1,
        fill = "transparent",
        color = "grey10",
        size = .15,
        inherit.aes = F
    ) +
    scale_size(
        range = c(1, 10)
    ) +
    guides(
        size = "none"
    ) +
    facet_wrap(~month_year) +
    theme_for_the_win() +
    labs(
        title = "Civilian deaths in Ukraine in 2022",
        caption = "Data: UCDP Georeferenced Event Dataset (GED) Global version 23.1"
    )


ggsave(
    "ukraine_civilian_deaths4.png", p4,
    width = 9, height = 6,
    units = "in", bg = "white"
)


# 5. SPATIAL JOIN
#----------------
ucdp_ged_ukraine_sf <- ucdp_ged_ukraine |>
    sf::st_as_sf(
        coords = c("longitude", "latitude")
    ) |>
    sf::st_set_crs(4326) |>
    dplyr::filter(
        deaths_civilians != 0
    )

ucdp_ged_ukraine_agg_sf <- sf::st_join(
    ucdp_ged_ukraine_sf,
    ukraine_adm1,
    join = sf::st_within
) |>
    dplyr::group_by(
        GID_1
    ) |>
    dplyr::summarise(
        sum_violence = sum(
            deaths_civilians
        )
    ) |>
    dplyr::mutate(
        total_violence = sum(
            sum_violence
        )
    ) |>
    dplyr::mutate(
        perc_violence = 100 * (
            sum_violence / total_violence
        )
    ) |>
    dplyr::select(
        GID_1, perc_violence
    ) |>
    sf::st_drop_geometry() |>
    dplyr::right_join(
        ukraine_adm1,
        by = "GID_1"
    ) |>
    sf::st_as_sf()

# plot
breaks <- seq(
    from = min(
        ucdp_ged_ukraine_agg_sf$perc_violence,
        na.rm = T
    ),
    to = max(
        ucdp_ged_ukraine_agg_sf$perc_violence,
        na.rm = T
    ),
    by = 10
)

ggmap(ukraine_layer) +
    geom_sf(
        data = ucdp_ged_ukraine_agg_sf,
        aes(
            fill = perc_violence
        ),
        color = "grey10",
        inherit.aes = F
    ) +
    scale_fill_gradientn(
        name = "",
        colours = rev(hcl.colors(
            20, "Plasma",
            alpha = .8
        )),
        breaks = round(breaks, 0)
    ) +
    theme_void()
