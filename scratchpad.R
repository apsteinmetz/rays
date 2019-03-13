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


elev_depth_matrix[] %>% 
  melt() %>%
  filter(value < 1) %>% 
  ggplot(aes(Var1,Var2,fill=value))+geom_tile()

watermap <- detect_water(elev_matrix,min_area = 400,zscale=zscale,cutoff = 0.999)
wm <- watermap %>% 
  melt() %>% 
  rename(is_water=value)

elev <- elev_matrix %>% 
  melt() %>% 
  rename(elevation = value) %>% 
  full_join(wm) %>% 
  as_tibble()

high_water <- elev %>% filter(elevation < 10)

elev %>%   ggplot(aes(Var1,Var2,fill=as.factor(is_water)))+geom_tile() 

  geom_tile(data=high_water,aes(Var1,Var2),color="red")

