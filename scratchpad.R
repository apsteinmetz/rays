contradictions <-elev_depth %>% 
  filter(elev > 0 & depth < 0) 

# ----------------------------------------------------------------------

#look at data
elev_depth %>% 
  # filter(elev > 0 & depth < 0) %>% 
  ggplot(aes(depth)) + 
  geom_histogram(fill="blue",alpha=0.5) + 
  geom_histogram(aes(elev),fill="darkgreen",alpha=0.5) + 
  labs(title = "Combined Elevation and Bathymetric Data",
       subtitle = "Contradictions! Coordinates with elev > 0 AND depth < 0",
       x = "Meters Above Sea Level",
       y = "Point Count",
       caption = "Source: open.canada.ca")

elev_depth %>% 
  # filter(elev > 0 & depth < 0) %>% 
  filter(!is.na(depth)) %>% 
  ggplot(aes(depth,elev)) + 
  geom_point() + 
  geom_point(data=contradictions,color="red") + 
  labs(subtitle = "Combined Elevation and Bathymetric Data",
       title = "Contradictions! (Coordinates with elev > 0 AND depth < 0 in Red)",
       x = "Depth",
       y = "Elevation",
       caption = "Source: open.canada.ca")

# -------------------------------------------------------------

library(dplyr)
library(reshape2)
library(ggplot2)
library(rayshader)
# set below waterline points to zero so detect_water finds them
mont_depth <- montereybay
mont_depth[which(montereybay<0)] = 0

watermap <- detect_water(mont_depth)

# turn watermap matrix into a data frame
watermap_df <- watermap %>% 
  melt(value.name = "is_water") %>% 
  mutate(is_water = as.factor(is_water))

# turn original matrix into a data frame and merge with watermap
elev <- montereybay %>% 
  melt(value.name = "elev") %>%
  mutate(is_land = as.factor(ifelse(elev < 0 ,0,1))) %>% 
  full_join(watermap_df) %>% 
  as_tibble()

#illustrate that latitude for watermap is north/south swapped vs. original matrix
elev %>% 
  filter(elev < 0) %>% 
  ggplot() + 
  geom_tile(aes(Var1,Var2,fill=is_land),alpha=0.5) +
  geom_tile(data=watermap_df,aes(Var1,Var2,fill=is_water),alpha=0.8) +
  NULL

