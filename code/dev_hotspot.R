library(tidyverse)
library(sf)
library(mapview)
library(SpatialKDE)
library(sfhotspot)
library(tigris)


#draw four features that samples will be drawn from 
#make one the "study area" covering the other three polys 
save = mapedit::drawFeatures() %>%  
  st_transform(crs = 4326)



sample = list(
  list(save[1,],save[2,],save[3,],save[4,])
  ,list(150, 150, 100, 1000)
) %>%  
  pmap(~{
    .x %>%  
      st_make_valid() %>% 
      st_sample(.y)
  }) 

sampled_data = sample %>%  
  map(st_as_sf) %>%  
  reduce(bind_rows) %>%  
  mutate(count = 1)

sampled_data %>%  
  # st_jitter(.051) %>% 
  mapview::mapview()

# mapview(grid_sm) + mapview(sampled_data)

crs = 32618
sampled_data_adj = sampled_data %>%  
  st_transform(crs = crs)

#point analysis - no weight=====================================================
#these two items create pretty much the same plot
#more or less given different 
{
  grid_sm = create_grid_hexagonal(sampled_data_adj, 
                                  cell_size = 500)
  kde_spkde = kde(sampled_data_adj, band_width = 5000, kernel = "quartic", grid = grid_sm)
  
  kde_hs = hotspot_kde(sampled_data_adj
                       ,grid_type = "hex"
                       ,cell_size = 500
                       ,bandwidth = 5000)
  kde_hs = kde_hs %>%  st_transform(crs = crs)
  
  mapview(kde_spkde, zcol = "kde_value") + mapview(kde_hs, zcol = "kde")
}

#weighted by bg=================================================================
#these two items create pretty much the same plot
#more or less given different 
{
  block_groups = 
    list("Maryland", "Virginia", 11) %>% 
    map(~.x %>% 
          block_groups(state = ., year = 2010) %>%  
          st_transform(crs = crs)) %>%  
    reduce(bind_rows)
  
  block_groups
  
  temp = aggregate(
    sampled_data_adj
    ,block_groups
    ,sum
  )
  
  temp = aggregate(
    sampled_data_adj
    ,block_groups
    ,count
  )
  
  block_groups$count = lengths(st_intersects(block_groups, sampled_data_adj))
  
  block_groups_cntd = block_groups %>%  
    st_centroid() %>%  
    st_filter(save[4,] %>%  
                st_transform(crs = crs))
  
  mapview(block_groups, zcol = "count") + mapview(sampled_data_adj)
  mapview(block_groups_cntd, zcol = "count")
  
  
  mapview(sampled_data_adj) + mapview(block_groups)
  
  kde_hs_wt = hotspot_kde(block_groups_cntd
                          ,grid_type = "hex"
                          ,cell_size = 500
                          ,bandwidth = 5000
                          ,weights = count)
  kde_hs_wt_gistar = hotspot_gistar(block_groups_cntd
                                    ,grid_type = "hex"
                                    ,cell_size = 500
                                    ,bandwidth = 5000
                                    ,weights = count)
  kde_hs_wt = kde_hs_wt %>%  st_transform(crs = crs)
  kde_hs_wt_gistar = kde_hs_wt_gistar %>%  
    st_transform(crs = crs) %>%  
    filter(gistar > 0, pvalue < 0.05)
  
  mapview(kde_spkde, zcol = "kde_value") + 
    mapview(kde_hs, zcol = "kde") + 
    mapview(kde_hs_wt, zcol = "kde") + 
    mapview(kde_hs_wt_gistar, zcol = "kde") 
  
  
}









