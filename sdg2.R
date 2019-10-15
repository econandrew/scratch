library(wbgdata)
library(wbgcharts)
library(wbggeo)
library(wbgmaps)
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)
library(gtable)
library(stringr)
library(grid)
source("styles.R")

fig_sdg2_undernourish_map <- function(year = 2015) {
  indicator <- "SN.ITK.DEFC.ZS"
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = year,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg2_undernourish_map.csv"
  )
  
  df$bins <- supercut(df$SN.ITK.DEFC.ZS, c(
    "0–5" = "[0,5)",
    "5–15" = "[5,15)",
    "15 and over" = "[15,Inf)"
  ))

  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low", aspect_ratio = 1.2) {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins", aspect_ratio = aspect_ratio)
      g$theme <- style$theme()
      g
    },
    aspect_ratio = 1.2,
    title = "Globally, 1 in 10 people are undernourished and do not have enough food to meet their dietary needs. Undernourishment is most widespread in Sub-Saharan Africa, South Asia, and East Asia & Pacific.",
    subtitle = wbg_name(indicatorID = indicator, year = year),
    source = "Source: Food and Agriculture Organization. World Development Indicators (SN.ITK.DEFC.ZS)."
  )
}

imgname <- "fig_sdg2_undernourish_map"

figure_save_draft_png(
  fig_sdg2_undernourish_map(),
  style_atlas,
  paste0(imgname, ".png"),
  width = 5.5, height = 4.5
)

png(
  filename = paste0(imgname, "_twitter.png"),
  width = 1024,
  height = 512,
  res = 192
)
grid.draw(fig_sdg2_undernourish_map()$plot(style_atlas(), aspect_ratio = 2.0))
dev.off()

png(
  filename = paste0(imgname, "_facebook.png"),
  width = 1200,
  height = 630,
  res = 192
)
grid.draw(fig_sdg2_undernourish_map()$plot(style_atlas(), aspect_ratio = 2.0))
dev.off()
