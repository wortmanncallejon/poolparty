pacman::p_load(bonn, sf, dplyr, ggplot2, here)

deciles <- function(x) {
  
  rev_quant <- function(y) {
    return(quantile(x, y))
  }
  
  return(unname(sapply(c(1:9/10), rev_quant)))
}


ausländer <- get_data(variable = 139, geography = "KRE", time = 2017) %>% 
  rename(id = Schlüssel,
         value = Wert) %>% 
  select(id, value)

match <- readxl::read_xlsx(here("data", "04-kreise.xlsx")) %>% 
  filter(!is.na(nuts))


geo <- read_sf(here("data", "shapefiles", "NUTS5000_N3.shp")) %>% 
  rename(nuts = NUTS_CODE) 

dec <- deciles(ausländer$value)  

inner_join(ausländer, match, by = "id")  %>% 
  inner_join(geo, by = "nuts") %>% 
  select(id, value, geometry) %>% 
  mutate(dec_1 = ifelse(value >= dec[1], "Included", "Excluded"),
         dec_2 = ifelse(value >= dec[2], "Included", "Excluded"),
         dec_3 = ifelse(value >= dec[3], "Included", "Excluded"),
         dec_4 = ifelse(value >= dec[4], "Included", "Excluded"),
         dec_5 = ifelse(value >= dec[5], "Included", "Excluded"),
         dec_6 = ifelse(value >= dec[6], "Included", "Excluded"),
         dec_7 = ifelse(value >= dec[7], "Included", "Excluded"),
         dec_8 = ifelse(value >= dec[8], "Included", "Excluded"),
         dec_9 = ifelse(value >= dec[9], "Included", "Excluded")) %>% 
  select(-value) %>% 
  tidyr::pivot_longer(-c(id, geometry)) %>% 
  filter(as.numeric(substr(name, 5,5)) >= 4) %>%
  mutate(name = paste0(substr(name, 5,5),"th Decile")) %>% 
ggplot(aes(fill = value, geometry = geometry)) +
  scale_fill_manual("Included in Riaz/Roemer", values = c("grey", "dodgerblue")) +
  geom_sf() +
  facet_wrap(~name) +
  theme_void() +
  theme(legend.position = "bottom")

ggsave("deciles.pdf", width = 32, height = 18, units = "cm") 


inner_join(ausländer, match, by = "id")  %>% 
  inner_join(geo, by = "nuts") %>% 
  select(id, value, geometry) %>% 
  mutate(main_spec = ifelse(value >= summary(ausländer$value)[5], "Included", "Excluded")) %>% 
  select(-value) %>% 
  ggplot(aes(fill = main_spec, geometry = geometry)) +
  scale_fill_manual("Included in Riaz/Roemer", values = c("grey", "dodgerblue")) +
  geom_sf() +
  theme_void() +
  theme(legend.position = "bottom")

ggsave("main_spec.pdf", width = 10, height = 10, units = "cm") 


  









