library(tidyverse)
library(ggpubr)
library(rnorsk)
library(esquisse)
require(editData)
library(treemapify)
library(tmaptools)
library(waffle)

setwd("P:/15218100_barekraftig_arealbruk_innenfor_rammen_av_lokalt/Figurer_KS")

options(scipen=999)

# Her bruker vi sp?rringen get.ssb.dataset i pakken rnorsk 
# f?rst 

# get.ssb.dataset(tableid, jsoncode)

# Byggerareal
#https://www.ssb.no/statbank/table/06952

tableid <-  "06952"
jsoncode='
{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "vs:Landet",
        "values": []
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "FullFritid2"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'
  
  byggeareal <- get.ssb.dataset(tableid, jsoncode)
  byggeareal$?r <- as.integer(byggeareal$?r)
  byggeareal$value <- byggeareal$value*0.000001 #convert from m2 to km2
  byggeareal$value <- round(byggeareal$value, digits = 3)
  
  ggplot(byggeareal) +
    aes(x = ?r, y = value) +
    geom_line(size = 1L, colour = "#112446") +
    labs(
      x = "?r",
      y = "Fullf?rt bruksareal (km?)",
      title = bquote('?rlig byggeareal fritidsbygninger,'~km^2),
      subtitle = "Kilde: SSB tabell 06952",
      caption = "Caption"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 16L, face = "bold")) +
    ylim(0, 1)
  
  
  fig <- ggplot(byggeareal) +
    aes(x = ?r, y = value) +
    geom_ribbon(aes(ymin=0, ymax=value), 
                fill = "#DCDCDC")+
    geom_line(size = 1L, colour = "#112446") +
    labs(
      x = NULL,
      y = NULL,
      title = expression(paste("?rlig byggeareal fritidsboliger, km"^"2")),
      #title = bquote('?rlig byggeareal fritidsboliger,'~m^2),,
      subtitle = " ",
      caption = " "
    ) +
    theme_classic() +
    theme(plot.title = element_text(size = 16L, face = "bold")) +
    ylim(0, 1)
  
  fig
  
  # identify minimum and maximum point and labels
  min_point <- filter(byggeareal, ?r == min(byggeareal$?r))
  min_point_label <- paste0(min_point$value, " ")
  max_point <- filter(byggeareal, ?r == max(byggeareal$?r))
  max_point_label <- paste0(max_point$value, "")
  
  source <- paste("Kilde: SSB Tabell 06952")
  
  fig_a <- fig + annotate("text",  x = 2008, y = 10, label=source, size=3, , hjust=0.1, vjust=-1)+
    geom_point(data = min_point, aes(?r, value)) +
    geom_text(data = min_point, label=min_point_label, hjust=-0.4) +
    geom_point(data = max_point, aes(?r, value), shape = 62, size = 5) +
    geom_text(data = max_point, label=max_point_label, vjust=-1, hjust=0.9)
  
  fig_a
  
  png("fritidsbygninger.png", units="cm", width=12, height=10, res=300)
  fig_a
  dev.off()
  
  
  ################################################################
  
  tableid <-  "08917"
  jsoncode='
{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "vs:Landet",
        "values": []
      }
    },
    {
      "code": "ArealStatus",
      "selection": {
        "filter": "item",
        "values": [
          "11"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'
  
  tilgj_strandsone <- get.ssb.dataset(tableid, jsoncode)
  tilgj_strandsone$?r <- as.integer(tilgj_strandsone$?r)
  max(tilgj_strandsone$value)
  
  ggplot(tilgj_strandsone) +
    aes(x = ?r, y = value) +
    geom_line(size = 1L, colour = "#112446") +
    labs(
      x = "?r",
      y = "Potensielt tilgjengelig strandsoneareal",
      title = "Potensielt tilgjengelig strandsoneareal",
      subtitle = "Kilde: SSB tabell 08917",
      caption = "Caption"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 16L, face = "bold")) +
    ylim(0, 3000000)
  
  
  fig_s <- ggplot(tilgj_strandsone) +
    aes(x = ?r, y = value) +
    geom_ribbon(aes(ymin=0, ymax=value), 
                fill = "#DCDCDC")+
    geom_line(size = 1L, colour = "#112446") +
    labs(
      x = NULL,
      y = NULL,
      #title = bquote('Tilgjengelig strandsone,'~km^2),
      title = expression(paste("Tilgjengelig strandsone, km"^"2")),
      subtitle = " ",
      caption = " "
    ) +
    theme_classic() +
    theme(plot.title = element_text(size = 16L, face = "bold")) +
    ylim(0, 3500000)
  
  fig_s
  
  # identify minimum and maximum point and labels
  min_point <- filter(tilgj_strandsone, ?r == min(tilgj_strandsone$?r))
  min_point_label <- paste0(min_point$value, "")
  max_point <- filter(tilgj_strandsone, ?r == max(tilgj_strandsone$?r))
  max_point_label <- paste0(max_point$value, "")
  
  source <- paste("Kilde: SSB Tabell 08917")
  
  fig_s + annotate("text",  x = 2008, y = 10, label=source, size=3, , hjust=-0.2, vjust=-2)+
    geom_point(data = min_point, aes(?r, value)) +
    geom_text(data = min_point, label=min_point_label, hjust=0, vjust=-1.2) +
    geom_point(data = max_point, aes(?r, value), 
               shape = 62, size = 5) +
    geom_text(data = max_point, label=max_point_label, hjust=0.9, vjust=-1.2)
  
  
  png("strandsone.png", units="cm", width=12, height=10, res=300)
  fig_s + annotate("text",  x = 2008, y = 10, label=source, size=3, , hjust=-0.7, vjust=-2)+
    geom_point(data = min_point, aes(?r, value)) +
    geom_text(data = min_point, label=min_point_label, hjust=0, vjust=-1.2) +
    geom_point(data = max_point, aes(?r, value), 
               shape = 62, size = 5) +
    geom_text(data = max_point, label=max_point_label, hjust=0.9, vjust=-1.2)
  dev.off()
  
  
  
  ###############################################################
  # Bygninger i strandsonen
  ###############################################################
  
  tableid <-  "06505"
  jsoncode='
{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "vs:KommunerStrand1",
        "values": []
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'
  
  bygg_strandsone <- get.ssb.dataset(tableid, jsoncode)
  bygg_strandsone$?r <- as.integer(bygg_strandsone$?r)
  
  fig_d <- ggplot(bygg_strandsone) +
    aes(x = ?r, y = value) +
    geom_ribbon(aes(ymin=0, ymax=value), 
                fill = "#DCDCDC")+
    geom_line(size = 1L, colour = "#112446") +
    labs(
      x = NULL,
      y = NULL,
      title = expression(paste("Antall bygninger i strandsonen")),
      subtitle = " ",
      caption = " "
    ) +
    theme_classic() +
    theme(plot.title = element_text(size = 16L, face = "bold")) +
    ylim(0, 600000)
  
  fig_d
  
  min_point <- filter(bygg_strandsone, ?r == min(bygg_strandsone$?r))
  min_point_label <- paste0(min_point$value, "")
  max_point <- filter(bygg_strandsone, ?r == max(bygg_strandsone$?r))
  max_point_label <- paste0(max_point$value, "")
  
  source <- paste("Kilde: SSB Tabell 06505")
  
  
  png("strandsone_bygg.png", units="cm", width=12, height=10, res=300)
  fig_d + annotate("text",  x = 2008, y = 10, label=source, size=3, , hjust=-0.7, vjust=-2)+
    geom_point(data = min_point, aes(?r, value)) +
    geom_text(data = min_point, label=min_point_label, hjust=-0.2, vjust=-1.2) +
    geom_point(data = max_point, aes(?r, value), shape = 62, size = 5) +
    geom_text(data = max_point, label=max_point_label, hjust=0.9, vjust=-1.2)
  dev.off()
  
  
  
  fig_d
  
  ################################################################
  
  # Tettstedsareal ----------------------------------------------------------
  
  tableid <-  "04859"
  jsoncode='
{
  "query": [
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Areal"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'
  
  tettstedsareal <- get.ssb.dataset(tableid, jsoncode)
  tettstedsareal$?r <- as.integer(tettstedsareal$?r)
  max(tettstedsareal$value)
  
  fig_t <- ggplot(tettstedsareal) +
    aes(x = ?r, y = value) +
    geom_line(size = 1L, colour = "#112446") +
    geom_ribbon(aes(ymin=0, ymax=value), 
                fill = "#DCDCDC")+
    geom_line(size = 1L, colour = "#112446") +
    labs(
      x = NULL,
      y = NULL,
      title = expression(paste("Tettstedsareal, km"^"2")),
      subtitle = " ",
      caption = " "
    ) +
    theme_classic() +
    theme(plot.title = element_text(size = 16L, face = "bold")) +
    ylim(0, 2500)
  
  fig_t
  
  min_point <- filter(tettstedsareal, ?r == min(tettstedsareal$?r))
  min_point_label <- paste0(min_point$value, "")
  max_point <- filter(tettstedsareal, ?r == max(tettstedsareal$?r))
  max_point_label <- paste0(max_point$value, "")
  
  source <- paste("Kilde: SSB Tabell 04859")
  
  png("tettstedsareal.png", units="cm", width=12, height=10, res=300)
  fig_t + annotate("text",  x = 2008, y = 10, label=source, size=3, , hjust=-0.7, vjust=-2)+
    geom_point(data = min_point, aes(?r, value)) +
    geom_text(data = min_point, label=min_point_label, hjust=-0.2, vjust=-1.2) +
    geom_point(data = max_point, aes(?r, value), 
               shape = 62, size = 5) +
    geom_text(data = max_point, label=max_point_label, hjust=0.9, vjust=-1.2)
  dev.off()
  
  ###############################################################
  # Tettstedsareal, justert for ny metode
  # Tettstedsareal ----------------------------------------------------------
  
  tableid <-  "04862"
  jsoncode='{
  "query": [
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Areal"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'
  
  tett <- get.ssb.dataset(tableid, jsoncode)
  tett$?r <- as.integer(tett$?r)
  max(tett$value)
  
  ggplot(tett) +
    aes(x = ?r, y = value) +
    geom_line(size = 1L, colour = "#112446") +
    labs(
      x = "?r",
      y = "Tettstedsareal,
    Km2",
      title = "Tettstedsareal"
    ) +
    theme_minimal()
  
  tett_2001_2012 <- tett[c(1:11),]
  tett_2013_2021 <- tett[c(12:20),]
  
  min(tett_2001_2012$value)
  max(tett_2001_2012$value)
  
  diff1 <- max(tett_2001_2012$value)-min(tett_2013_2021$value)
  
  diff1
  
  tett1 <- tett_2001_2012
  names(tett1) <- c("statistikkvariabel", "?r", "VALUE")
  tett1$value <- tett1$VALUE-diff1
  
  tett1 <- tett1[,c(1,2,4)]
  tett2 <- tett_2013_2021
  
  tett3 <- rbind(tett1, tett2)
  tett3$value <- as.integer((tett3$value))
  
  ggplot(tett3) +
    aes(x = ?r, y = value) +
    geom_line(size = 1L, colour = "#112446") +
    labs(
      x = "?r",
      y = "Tettstedsareal,
    Km2",
      title = "Tettstedsareal"
    ) +
    theme_minimal()
  
  
  ###
  
  tettstedsareal <- tett3
  
  fig_t <- ggplot(tettstedsareal) +
    aes(x = ?r, y = value) +
    geom_line(size = 1L, colour = "#112446") +
    geom_ribbon(aes(ymin=0, ymax=value), 
                fill = "#DCDCDC")+
    geom_line(size = 1L, colour = "#112446") +
    labs(
      x = NULL,
      y = NULL,
      title = expression(paste("Tettstedsareal, km"^"2")),
      subtitle = " ",
      caption = " "
    ) +
    theme_classic() +
    theme(plot.title = element_text(size = 16L, face = "bold")) +
    ylim(0, 2500)
  
  fig_t
  
  min_point <- filter(tettstedsareal, ?r == min(tettstedsareal$?r))
  min_point_label <- paste0(min_point$value, "")
  max_point <- filter(tettstedsareal, ?r == max(tettstedsareal$?r))
  max_point_label <- paste0(max_point$value, "")
  
  source <- paste("Kilde: SSB Tabell 04862")
  
  png("tettstedsareal.png", units="cm", width=12, height=10, res=300)
  fig_t + annotate("text",  x = 2008, y = 10, label=source, size=3, , hjust=-0.7, vjust=-2)+
    geom_point(data = min_point, aes(?r, value)) +
    geom_text(data = min_point, label=min_point_label, hjust=-0.2, vjust=-1.2) +
    geom_point(data = max_point, aes(?r, value), 
               shape = 62, size = 5) +
    geom_text(data = max_point, label=max_point_label, hjust=0.9, vjust=-1.2)
  dev.off()
  
  ###
  
  min(tett_2001_2012$value)+ diff1
  
  mean_inc <- diff/nrow(tett_2001_2012)
  mean_inc
  
  diff2 <- max(tett_2013_2021$value)-min(tett_2013_2021$value)
  diff2
  diff2/9
  
  mean_inc2 <- diff2/nrow(tett_2013_2021)
  mean_inc2
  
  (diff1+diff2)/nrow(tett)
  max(tett_2013_2021$value)
  
  ###
  
  
  ###############################################################
  
  ggarrange(fig_a, fig_b, fig_c, fig_d, ncol = 2, nrow = 2)
  
  
  
  
  # Vernet areal ------------------------------------------------------------
  
  # https://miljostatus.miljodirektoratet.no/tema/naturomrader-pa-land/
  
  vern <- read.csv("P:/15218100_barekraftig_arealbruk_innenfor_rammen_av_lokalt/Figurer_KS/Vernet_landareal_fordelt_p?_vernekategori.csv", sep = ";")
  colnames(vern)
  colnames(vern) <- c("?r", "Nasjonalpark", "Naturreservat", "Landskapsvernomr?de", "Annet.vern" )
  vern <- vern %>%
    pivot_longer(!arealklasse, names_to = "?r")
  
  vern2 <- vern %>%
    pivot_longer(!?r, names_to = "Verneform")
  
  #esquisser(vern2)
  
  
  library(ggplot2)
  
  fig_e <- ggplot(vern2) +
    aes(x = ?r, y = value, fill = Verneform) +
    geom_area(size = 1.5) +
    scale_fill_manual(values = c(Annet.vern = "#936317", 
                                 Landskapsvernomr?de = "#ADDEA7", Nasjonalpark = "#37A055", Naturreservat = "#00441B")) +
    labs(x = "?r", y = "Vernet areal km2", title = "Vernet landareal fordelt p? vernekategori", subtitle = "Kilde: Milj?direktoratet") +
    theme_minimal()
  
  fig_e
  
  vern3 <- vern2 %>%
    group_by(?r) %>%
    summarise(value = sum(value))
  
  
  ggplot(vern3) +
    aes(x = ?r, y = value) +
    geom_line(size = 1L) +
    theme_minimal()
  
  names(vern3) <- c("?r", "value")
  
  
  
  fig_v <- ggplot(vern3) +
    aes(x = ?r, y = value) +
    geom_line(size = 1L, colour = "#112446") +
    geom_ribbon(aes(ymin=0, ymax=value), 
                fill = "#DCDCDC")+
    geom_line(size = 1L, colour = "#112446") +
    labs(
      x = NULL,
      y = NULL,
      title = expression(paste("Vernet areal, km"^"2")),
      subtitle = " ",
      caption = " "
    ) +
    theme_classic() +
    theme(plot.title = element_text(size = 16L, face = "bold")) +
    ylim(0, 60000)
  
  fig_v
  
  min_point <- filter(vern3, ?r == min(vern3$?r))
  min_point_label <- paste0(min_point$value, "")
  max_point <- filter(vern3, ?r == max(vern3$?r))
  max_point_label <- paste0(max_point$value, "")
  
  source <- paste("Kilde: Milj?direktoratet")
  
  png("vern3.png", units="cm", width=12, height=10, res=300)
  fig_v + annotate("text",  x = 2008, y = 10, label=source, size=3, , hjust=0.3, vjust=-2)+
    geom_point(data = min_point, aes(?r, value)) +
    geom_text(data = min_point, label=min_point_label, hjust=-0.2, vjust=-1.2) +
    geom_point(data = max_point, aes(?r, value), 
               shape = 62, size = 5) +
    geom_text(data = max_point, label=max_point_label, hjust=0.9, vjust=-1.2)
  dev.off()
  
  
  
  
  
  # Arealbruk
  
  
  # Bebygd areal ------------------------------------------------------------
  
  
  tableid <-  "09594"
  jsoncode='{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "vs:Landet",
        "values": []
      }
    },
    {
      "code": "ArealKlasse",
      "selection": {
        "filter": "vs:ArealHovedklasser2",
        "values": [
          "01",
          "02",
          "03",
          "04",
          "05",
          "06",
          "07",
          "08-09",
          "10-11",
          "12-13",
          "14"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'
  
  beb <- get.ssb.dataset(tableid, jsoncode)
  beb?r <- as.integer(beb$?r)
  max(beb$value)
  
  #esquisser(beb2)
  #unique(beb$arealklasse)
  #palette_explorer()
  
  mycols1 <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99")
  
  #mycols <- c("fjell", "beb", "ferskvann", "jordbruk", "skog", "sn?","v?tmark","?pen fastmark")
  
  
  ggplot(beb, aes(area = Prosent, fill = Arealklasser, 
                  label = paste(Arealklasser, Prosent, sep = "/n"))) +
    geom_treemap()+
    geom_treemap_text(colour = "black",
                      place = "centre",
                      size = 15) +
    theme(legend.position = "none")+
    scale_fill_manual(values=mycols)
  
  fig_f <- ggplot(beb) +
    aes(x = ?r) +
    geom_bar() +
    theme_minimal()
  
  fig_f
  
  beb2 <- beb %>%
    group_by(?r) %>%
    summarise(value = sum(value))
  
  beb2$?r <- as.integer(beb2$?r)
  beb2$value <- as.integer(beb2$value)
  
  ggplot(beb2) +
    aes(x = ?r, y = value) +
    geom_line(size = 1L) +
    theme_minimal()
  
  fig_f <- ggplot(beb2) +
    aes(x = ?r, y = value) +
    geom_line(size = 1L, colour = "#112446") +
    geom_ribbon(aes(ymin=0, ymax=value), 
                fill = "#DCDCDC")+
    geom_line(size = 1L, colour = "#112446") +
    labs(
      x = NULL,
      y = NULL,
      title = expression(paste("Bebygd areal, km"^"2")),
      subtitle = " ",
      caption = " "
    ) +
    theme_classic() +
    theme(plot.title = element_text(size = 16L, face = "bold")) +
    ylim(0, 6000)
  
  fig_f
  
  min_point <- filter(beb2, ?r == min(beb2$?r))
  min_point_label <- paste0(min_point$value, "")
  max_point <- filter(beb2, ?r == max(beb2$?r))
  max_point_label <- paste0(max_point$value, "")
  
  source <- paste("Kilde: SSB Tabell 09594")
  
  fig_f
  
  png("beb2.png", units="cm", width=12, height=10, res=300)
  fig_f + annotate("text",  label=source, size=3, , hjust=1.3, vjust=-2)+
    geom_point(data = min_point, aes(?r, value)) +
    geom_text(data = min_point, label=min_point_label, hjust=-0.2, vjust=-1.2) +
    geom_point(data = max_point, aes(?r, value), 
               shape = 62, size = 5) +
    geom_text(data = max_point, label=max_point_label, hjust=0.9, vjust=-1.2)
  dev.off()
  
  ###
  #setwd("P:/15218100_barekraftig_arealbruk_innenfor_rammen_av_lokalt")
  #getwd()
  #Skogsbilveier
  skogsbilvei <- read.csv("skogsbilveier.csv", header = TRUE, sep = ";")
  skogsbilvei <- skogsbilvei[,1:2]
  names(skogsbilvei) <- c("?r", "value")
  
  head(skogsbilvei)
  skogsbilvei <- skogsbilvei [-1,]
  
  max(skogsbilvei$value)
  
  
  fig <- ggplot(skogsbilvei) +
    aes(x = ?r, y = value) +
    geom_ribbon(aes(ymin=0, ymax=value), 
                fill = "#DCDCDC")+
    geom_line(size = 1L, colour = "#112446") +
    labs(
      x = NULL,
      y = NULL,
      title = "Akkumulert skogsveibygging, 1 000 km",
      #title = bquote('?rlig byggeareal fritidsboliger,'~m^2),,
      subtitle = " ",
      caption = " "
    ) +
    theme_classic() +
    theme(plot.title = element_text(size = 16L, face = "bold")) +
    ylim(0, 50000)
  
  fig
  
  
  
  # identify minimum and maximum point and labels
  min_point <- filter(skogsbilvei, ?r == min(skogsbilvei$?r))
  min_point_label <- paste0(min_point$value, "")
  max_point <- filter(skogsbilvei, ?r == max(skogsbilvei$?r))
  max_point_label <- paste0(max_point$value, "")
  
  source <- paste("Kilde: Landbruksdirektoratet")
  
  fig + annotate("text",  x = 2008, y = 10, label=source, size=3, , hjust=-0.3, vjust=-2)+
    geom_point(data = min_point, aes(?r, value)) +
    geom_text(data = min_point, label=min_point_label, hjust=-0.8) +
    geom_point(data = max_point, aes(?r, value), 
               shape = 62, size = 5) +
    geom_text(data = max_point, label=max_point_label, vjust=-0.4)
  
  fig_a <- fig + annotate("text",  x = 2008, y = 10, label=source, size=3, , hjust=0.1, vjust=-1)+
    geom_point(data = min_point, aes(?r, value)) +
    geom_text(data = min_point, label=min_point_label, hjust=-0.8) +
    geom_point(data = max_point, aes(?r, value), shape = 62, size = 5) +
    geom_text(data = max_point, label=max_point_label, vjust=-0.4, hjust=0.9)
  
  fig_a
  
  png("skogsbilveier.png", units="cm", width=12, height=10, res=300)
  fig_a
  dev.off()
  getwd()
  
  # Arealbruk og arealressurser ---------------------------------------------
  
  areal <- read.csv("P:/15218100_barekraftig_arealbruk_innenfor_rammen_av_lokalt/Figurer_KS/Arealbruk_norge.csv", sep = ";")
  areal$km2 <-  as.numeric(gsub(" ", "", areal$km2, fixed = TRUE)) #remove spaces in numbers
  norge <- areal[1,3]
  (589/norge)*100
  
  
  areal$Prosent <- as.numeric(areal$Prosent)
  areal <- areal [-1,]
  
  areal$Arealklasser <- c("Bebygd", "Jordbruk", "Skog", "?pen fastmark", "V?tmark", "Bart fjell", "Sn? og is", "Ferskvann")
  
  mycols <- c("#797D7F", "#E74C3C", "#2980B9", "#F4D03F", "#0B5345", "#a6cee3", "#C39BD3","#7DCEA0")
  
  #mycols <- c("fjell", "beb", "ferskvann", "jordbruk", "skog", "sn?","v?tmark","?pen fastmark")
  
  
  fig_g <- ggplot(areal, aes(area = Prosent, fill = Arealklasser, 
                             label = paste(Arealklasser, Prosent, sep = "/n"))) +
    geom_treemap()+
    geom_treemap_text(colour = "black",
                      place = "centre",
                      size = 15) +
    theme(legend.position = "none")+
    scale_fill_manual(values=mycols)
  
  fig_g
  
  ggplot(areal, aes(area = Prosent, fill = Arealklasser, label = paste(Arealklasser, Prosent, sep = "/n"))) +
    geom_treemap(show.legend = FALSE) +
    geom_treemap_text(colour = "black",
                      place = "centre",
                      size = 20) +
    #facet_wrap( ~continent) +
    scale_fill_manual(values=mycols) +
    #theme(legend.position = "bottom") +
    labs(
      title = "Arealressurser etter hovedklasser, prosent",
      subtitle = "Kilde: SSB tabell 09594"
    )
  
  png("Areal.png", units="cm", width=12, height=10, res=300)
  ggplot(areal, aes(area = Prosent, fill = Arealklasser, label = paste(Arealklasser, Prosent, sep = "/n"))) +
    geom_treemap(show.legend = FALSE) +
    geom_treemap_text(colour = "black",
                      place = "centre",
                      size = 20) +
    #facet_wrap( ~continent) +
    scale_fill_manual(values=mycols) +
    #theme(legend.position = "bottom") +
    labs(
      title = "Arealressurser etter hovedklasser, prosent",
      subtitle = "Kilde: SSB tabell 09594"
    )
  dev.off()
  
  # Vector
  x <- c(30, 25, 20, 5)
  
  # Waffle chart
  waffle(x, rows = 8)
  
  waffle(areal$?r)
  
  # install.packages("waffle", repos = "https://cinc.rud.is")
  library(waffle)
  
  head(areal)
  tail(areal)
  areal$Prosent
  
  # Waffle chart
  x <- c("Bebygd 1,7%" = 2, "Jordbruk 3,5%" = 4, "Skog 37,4%" = 37, 
         "?pen fastmark 37,6%" = 38, "V?tmark 5,3%" = 5, "Bart fjell 7,4%" = 7, 
         "Sn? og is 0,8%" = 1, "Ferskvann 6,2" = 6, "Kilde: SSB Tabell 09594" = 0)
  
  
  
  areal$Prosent
  # Waffle chart
  mycols <- c("#E74C3C", "#F4D03F", "#0B5345", "#7DCEA0", 
              "#C39BD3", "#797D7F", "#a6cee3","#2980B9", "white")
  coltest <- rnorm(10,1)
  hist(coltest, col = "#797D7F")# darkgrey
  hist(coltest, col = "#E74C3C")# red
  hist(coltest, col = "#2980B9")#navyblue
  hist(coltest, col = "#F4D03F")#yellow
  hist(coltest, col = "#0B5345")#forestgreen
  hist(coltest, col = "#a6cee3")#lightblue
  hist(coltest, col = "#C39BD3")#purple
  hist(coltest, col = "#7DCEA0")#lightgreen
  
  # Waffle chart
  waffle(x, rows = 10,
         colors = mycols, 
         title = "Arealtyper Fastlands-Norge") 
  
  png("arealtyper.png", units="cm", width=12, height=10, res=300)
  waffle(x, rows = 10,
         colors = mycols,
         title = "Arealtyper Fastlands-Norge") 
  dev.off()
  
  #areal$Arealklasser <- c("Bebygd", "Jordbruk", "Skog", "?pen fastmark", 
  #                        "V?tmark", "Bart fjell", "Sn? og is", "Ferskvann")
  
  # Waffle chart
  x <- c("Bebygd 1,7%" = 2, "Jordbruk 3,5%" = 4, "Skog 37,4%" = 37, 
         "?pen fastmark 37,6%" = 38, "V?tmark 5,3%" = 5, "Bart fjell 7,4%" = 7, 
         "Sn? og is 0,8%" = 1, "Ferskvann 6,2%" = 6)
  
  
  # Data
  df <- data.frame(group = LETTERS[1:3],
                   value = c(25, 20, 35))
  area <- areal[,c(1,4)]
  area$Prosent <- round(area$Prosent, digits = 0)
  
  df <- data.frame("Bebygd 1,7%" = 2, "Jordbruk 3,5%" = 4, "Skog 37,4%" = 37, 
                   "?pen fastmark 37,6%" = 38, "V?tmark 5,3%" = 5, "Bart fjell 7,4%" = 7, 
                   "Sn? og is 0,8%" = 1, "Ferskvann 6,2%" = 6)
  
  #install.packages("waffle", repos = "https://cinc.rud.is")
  
  
  # Waffle plot
  ggplot(area, aes(fill = Arealklasser, values = Prosent)) +
    geom_waffle(n_rows = 10, size = 0.33, colour = "white") +
    scale_fill_manual(name = "Arealtyper",
                      values = c("#E74C3C", "#F4D03F", "#0B5345", "#7DCEA0", 
                                 "#C39BD3", "#797D7F", "#a6cee3","#2980B9"),
                      labels = c("Bebygd 1,7%", "Jordbruk 3,5%", "Skog 37,4%", 
                                 "?pen fastmark 37,6%", "V?tmark 5,3%", "Bart fjell 7,4%", 
                                 "Sn? og is 0,8%", "Ferskvann 6,2%")) +
    coord_equal() +
    theme_void()
  
  ggplot(area, aes(fill = Arealklasser, values = Prosent)) +
    geom_waffle(n_rows = 10, size = 0.66, colour = "white") +
    scale_fill_manual(name = "Arealtyper",
                      values = c("#797D7F", "#E74C3C", "#2980B9", 
                                 "#F4D03F", "#0B5345", "#a6cee3", "#C39BD3","#7DCEA0"),
                      labels = c("Bebygd 1,7%", "Jordbruk 3,5%", "Skog 37,4%", 
                                 "?pen fastmark 37,6%", "V?tmark 5,3%", "Bart fjell 7,4%", 
                                 "Sn? og is 0,8%", "Ferskvann 6,2%")) +
    coord_equal() +
    theme_void()
  
  # Waffle plot
  ggplot(area, aes(fill = Arealklasser, values = Prosent)) +
    geom_waffle(n_rows = 10, size = 0.33, colour = "white") +
    scale_fill_manual(name = "Arealtyper",
                      values = c("#E74C3C", "#F4D03F", "#0B5345", "#7DCEA0", 
                                 "#C39BD3", "#797D7F", "#a6cee3","#2980B9"),
                      labels = c("Bebygd 1,7%", "Jordbruk 3,5%", "Skog 37,4%", 
                                 "?pen fastmark 37,6%", "V?tmark 5,3%", "Bart fjell 7,4%", 
                                 "Sn? og is 0,8%", "Ferskvann 6,2%")) +
    coord_equal() +
    theme_void()
  
  hist(coltest, col = "#797D7F")# darkgrey
  hist(coltest, col = "#E74C3C")# red
  hist(coltest, col = "#2980B9")#navyblue
  hist(coltest, col = "#F4D03F")#yellow
  hist(coltest, col = "#0B5345")#forestgreen
  hist(coltest, col = "#a6cee3")#lightblue
  hist(coltest, col = "#C39BD3")#purple
  hist(coltest, col = "#7DCEA0")#lightgreen
  
  
  fig_a
  fig_b
  fig_c
  fig_d
  fig_e
  fig_f
  fig_g
  
  ggarrange(fig_a, fig_b, fig_c, fig_d, fig_e, fig_f, fig_g, ncol = 3, nrow = 3)
  
  # Planer ------------------------------------------------------------------
  
  
  # Reguleringsplaner -------------------------------------------------------
  
  tableid <-  "12690"
  jsoncode='{
  "query": [
    {
      "code": "KOKkommuneregion0000",
      "selection": {
        "filter": "agg_single:KOGkommuneregion000005600",
        "values": [
          "EAK"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "KOShoringsfrist0000",
          "KOSinnsigelseral0000",
          "KOSomfplanbehand0001",
          "KOSomfplanbehand0000",
          "KOSklagesakerved0000",
          "KOSantkuln0000"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'
  
  reg <- get.ssb.dataset(tableid, jsoncode)
  reg$?r <- as.integer(reg$?r)
  max(reg$value)
  
  #esquisser(reg)
  
  ggplot(reg) +
    aes(x = ?r, y = value, colour = statistikkvariabel) +
    geom_line(size = 1L) +
    scale_color_brewer(palette = "Set1", direction = 1) +
    labs(
      x = "?r",
      y = "Antall reguleringsplaner",
      title = "Reguleringsplanbehandling",
      subtitle = "Kilde: SSB tabell 12690",
      color = " "
    ) +
    theme_minimal() +
    ylim(0, 2300)
  
  
  # Kommuneplaener -----------------------------------------------------------
  tableid <-  "12690"
  jsoncode='{
  "query": [
    {
      "code": "KOKkommuneregion0000",
      "selection": {
        "filter": "agg_single:KOGkommuneregion000005600",
        "values": [
          "EAK"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "KOSantreghoring0000",
          "KOSinskomplanall0000",
          "KOStemaareal0000"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'
  
  komm <- get.ssb.dataset(tableid, jsoncode)
  komm$?r <- as.integer(komm$?r)
  max(komm$value)
  
  #esquisser(komm)
  
  ggplot(komm) +
    aes(x = ?r, y = value, colour = statistikkvariabel) +
    geom_line(size = 1L) +
    scale_color_brewer(palette = "Set1", direction = 1) +
    theme_minimal() +
    ylim(0, 400)
  
  