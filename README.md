# SICSS Istanbul 2023: GIS and spatial analysis with R

The repository "GIS and Spatial Analysis with R" provides a guide to using `R` for GIS and spatial analysis. We'll analyze and visualize civilian fatalities in the Russia-Ukraine war throughout 2022. The primary dataset used in this repository is the Uppsala Conflict Data Program's Georeferenced Event Dataset - [UCDP GED](https://ucdp.uu.se/downloads/index.html#ged_global).

The UCDP Georeferenced Event Dataset (GED) Global version 23.1 is a highly detailed and comprehensive dataset that offers geospatially referenced information on armed conflicts, including their locations, types, and intensities, making it a valuable resource for analyzing and understanding patterns of political violence on a global scale. Please check the codebook [here](https://ucdp.uu.se/downloads/ged/ged231.pdf).

The repository includes code examples that demonstrate how to work with spatial data, perform geospatial analysis, and create compelling visual representations using R. It covers topics such as data acquisition, data preprocessing, spatial analysis techniques, and visualization methods.

## Examples

This is a basic bubble map of civilian deaths overlayed with GADM administrative level 1 boundary of Ukraine

![alt text](https://github.com/milos-agathon/sicss-gis-with-r/blob/main/plot/map0.png?raw=true)

In this bubble map, we show civilian deaths in Ukraine against a Stamen terrain layer.
We also colored only point edges to create more visibility.

![alt text](https://github.com/milos-agathon/sicss-gis-with-r/blob/main/plot/map-1.png?raw=true)

We leverage `facet_wrap` option in `ggplot2` to dive into the monthly trends in violence in the bubble map below.
Apart from the Stamen layer, we also add GADM administrative level 1 boundary of Ukraine for greater visibility.

![alt text](https://github.com/milos-agathon/sicss-gis-with-r/blob/main/plot/map-4.png?raw=true)

