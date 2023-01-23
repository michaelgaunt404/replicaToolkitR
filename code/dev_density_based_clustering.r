#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script [[insert brief readme here]]
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: [[insert brief readme here]]
#-------- [[insert brief readme here]]
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(tidyverse)
library(gauntlet)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library("dbscan")
set.seed(2)
n <- 400
x <- cbind(
  x = runif(4, 0, 1) + rnorm(n, sd = 0.1),
  y = runif(4, 0, 1) + rnorm(n, sd = 0.1)
)
true_clusters <- rep(1:4, time = 100)
plot(x, col = true_clusters, pch = true_clusters)

kNNdistplot(x, k = 3)
abline(h=.05, col = "red", lty=2)

res <- dbscan(x, eps = 0.05, minPts = 3)
res

plot(x, col = res$cluster + 1L, pch = res$cluster + 1L)

res <- optics(x, eps = 10, minPts = 20)

res$order

plot(res)

plot(x, col = "grey")
polygon(x[res$order,],)
res <- extractDBSCAN(res, eps_cl = .07)
plot(res)
hullplot(x, res)

dend <- as.dendrogram(res)
dend %>%  plot()
plot(dend, ylab = "Reachability dist.", leaflab = "none")
hullplot(dend)

res <- hdbscan(x, minPts = 4)

res$hc %>%
  plot()

plot(res, show_flat = T)

hullplot(x, res)

cl = res

print(cl$cluster_scores)
head(cl$membership_prob)

plot(x, col=cl$cluster+1, pch=21)
colors <- mapply(function(col, i) adjustcolor(col, alpha.f = cl$membership_prob[i]),
                 palette()[cl$cluster+1], seq_along(cl$cluster))
points(x, col=colors, pch=20)

##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data("DS3")
plot(DS3, pch=20, cex=0.25)

cl2 <- hdbscan(DS3, minPts = 10)
cl2
plot(DS3, col=cl2$cluster+1,
     pch=ifelse(cl2$cluster == 0, 8, 1), # Mark noise as star
     cex=ifelse(cl2$cluster == 0, 0.5, 0.75), # Decrease size of noise
     xlab=NA, ylab=NA)
colors <- sapply(1:length(cl2$cluster),
                 function(i) adjustcolor(palette()[(cl2$cluster+1)[i]], alpha.f = cl2$membership_prob[i]))
points(DS3, col=colors, pch=20)

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================

library(sf)



crashes =
  readxl::read_excel("C:/Users/USMG687637/Documents/051_projects/freight_bottlenecks/data/KBell_HV_ClackMultWash_2016_2020_20220519.xlsx") %>%
  st_as_sf(coords = c("LONGTD_DD", "LAT_DD"), crs = 4326)


crashes %>%
filter(NHS_FLG == 1) %>%
mutate()


crashes_hs = crashes %>%
  st_transform(2285) %>%
  sfhotspot::hotspot_gistar(bandwidth = 10000)

crashes_hs %>%
  filter(gistar > 0, pvalue < 0.05) %>%
  ggplot(aes(colour = kde, fill = kde)) +
  geom_sf() +
  scale_colour_continuous(aesthetics = c("colour", "fill")) +
  labs(title = "Density of robberies in Memphis, 2019") +
  theme_void()


library(mapedit)
library(mapview)

object = mapview() %>%
  editMap()



crashes_hs %>%
  filter(gistar > 0, pvalue < 0.05)  %>%
  mapview(zcol = "kde")

crashes_temp = crashes %>%
  filter(NHS_FLG == 1) %>%
  filter(TOT_INJ_CNT > 0) %>%
  mutate(lat = st_coordinates(geometry)[,2]
         ,lon = st_coordinates(geometry)[,1]) %>%
  select(lat, lon) %>%
  st_drop_geometry()
crashes$RDWY_NO %>%  unique()

plot = crashes_temp %>%
  ggplot() +
  geom_point(aes(lon, lat))

temp_plot %>%
  plotly::ggplotly()


crashes_cl =  crashes_temp %>%
  hdbscan(minPts = 10)

crashes_cl

temp_plot = crashes_temp %>%
  mutate(cluster = crashes_cl$cluster
         ,cluster_prob = crashes_cl$membership_prob) %>%
  filter(cluster != 0) %>%
  ggplot() +
  geom_point(aes(lon, lat, color = as.factor(cluster), alpha = cluster_prob))

temp_plot %>%
  plotly::ggplotly()


crashes_cl %>%
  plot()













