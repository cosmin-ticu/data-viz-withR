# install.packages("tidytuesdayR")
# library(tidytuesdayR)
rm(list = ls())
library(data.table) # for data manipulation
library(ggplot2)
library(GGally)
library(gganimate)
library(transformr) # glue type of syntax
library(animation)
library(CoordinateCleaner) # import coordinates for all countries
library(dplyr) # import %>% 
library(tmaptools) # access openstreetmap API for unmatching locations

# tuesdata <- tidytuesdayR::tt_load('2021-01-05')
# 
# transit_cost <- tuesdata$transit_cost
# 
# economic_inequality <- tidytuesdayR::tt_load('2021-02-09')
# 
# plot(transit_cost$tunnel_per, transit_cost$cost_km_millions)

movies <- read.csv("term_project/movies.csv")

# initialize empty list of countries
countries <- as.data.frame(unique(movies$country))
names(countries) <- "country"

# temp <- data.frame()
# temp <- rbind(temp, geocode_OSM(countries[1:10], as.data.frame = T, return.first.only = T, keep.unfound = T, geometry = 'point'))
# temp <- rbind(temp, geocode_OSM(countries[11:21], as.data.frame = T, return.first.only = T, keep.unfound = T, geometry = 'point'))
# temp <- rbind(temp, geocode_OSM(countries[22:33], as.data.frame = T, return.first.only = T, keep.unfound = T, geometry = 'point'))
# temp <- rbind(temp, geocode_OSM(countries[34:45], as.data.frame = T, return.first.only = T, keep.unfound = T, geometry = 'point'))
# temp <- rbind(temp, geocode_OSM(countries[46:57], as.data.frame = T, return.first.only = T, keep.unfound = T, geometry = 'point'))
# 
# for (country in countries) {
#   Sys.sleep(1)
#   temp <- rbind(temp, geocode_OSM(country, as.data.frame = T, return.first.only = T, keep.unfound = T, geometry = 'point'))
#   Sys.sleep(1)
# }
# 
# splitList(countries, 3)
# 
# install.packages('rnaturalearth')
# library(rnaturalearth)
# temp <- ne_countries(country = 'France')
# temp$long_len

# temp <- countryref
# temp <- as.data.table(temp)
# temp <- temp[, list(lon = mean(centroid.lon), lat = mean(centroid.lat)), by = list(country = name)]
# 
# temp2 <- merge(countries, temp, by = 'country', all.x = T)
# 
# # geocode_OSM('Czech Republic', as.data.frame = T)
# 
# temp2$country <- gsub('Hong Kong', 'China', temp2$country)
# 
# temp2$country <- gsub('Republic of Macedonia', 'North Macedonia', temp2$country)
# 
# temp2$country <- gsub('West Germany', 'Germany', temp2$country)
# 
# temp2$country <- gsub('Soviet Union', 'Russia', temp2$country)
# 
# temp2$country <- gsub('Federal Republic of Yugoslavia', 'Serbia', temp2$country)
# 
# temp2$country <- gsub('USA', 'United States of America', temp2$country)

movies$country <- gsub('Hong Kong', 'China', movies$country)

movies$country <- gsub('Republic of Macedonia', 'Macedonia', movies$country)

movies$country <- gsub('West Germany', 'Germany', movies$country)

movies$country <- gsub('Soviet Union', 'Russia', movies$country)

movies$country <- gsub('Federal Republic of Yugoslavia', 'Serbia', movies$country)

# movies$country <- gsub('USA', 'United States of America', movies$country)

# gigi <- c('West Germany', 'Soviet Union', 'Federal Republic of Yugoslavia', 'USA')
# for (cntry in gigi) {
#   temp2 <- temp2 %>% filter(!country ==  cntry)
# }

# for (i in 1:nrow(temp2)) {
#   if (is.na(temp2$lat[i])){
#     temp3 <- geocode_OSM(temp2$country[i])$coords
#     temp2$lat[i] <- temp3[1]
#     temp2$lon[i] <- temp3[2]
#   }
# }

worldmap <- map_data('world')

world.lab.data <- worldmap %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

movies <- as.data.table(merge(movies, world.lab.data, by.x = 'country', by.y = 'region', all.x = T))

# no missing countries
movies[, .N, by = country]
movies %>% filter(is.na(country))

movies[, `:=` ( # dplyr::mutate
  budget_percent_rev = budget / gross,
  cost_minute_runtime = budget / runtime,
  profit = gross - budget,
  year = as.integer(year)
)]

movies <- movies %>% 
  mutate(rating_econ_success = case_when(budget_percent_rev > 1 ~ "Box-Office Bomb",
                                         budget_percent_rev >= 0.9 ~ "Break-even Movie",
                                         budget_percent_rev > 0.5 ~ "Profitable Movie",
                                         budget_percent_rev >= 0.001 ~ "Box-Office Hit",
                                         budget_percent_rev < 0.001 ~ "Unknown Budget Movie"
  ))

movies <- movies %>% 
  mutate(decade = case_when(year > 2009 ~ "2010s",
                                         year > 1999 ~ "2000s",
                                         year > 1989 ~ "1990s",
                                         year <= 1989 ~ "1980s"
  ))

movie_map <- movies[, list(runtime = mean(runtime), 
                           avg_revenue = mean(gross), 
                           count = .N,
                           sum_revenue = sum(gross),
                           avg_cost_minute_runtime = mean(cost_minute_runtime)), by = list(long, lat, country, decade)]

summary(movies$budget_percent_rev)

library(glue)

ggplot() +
  geom_polygon(
    data = worldmap, 
    aes(x = long, y = lat, group = group), 
    fill = 'gray', color = 'black') +
  geom_point(
    data = movie_map, 
    aes(long, lat, size = avg_revenue)) +
  theme_void() +
  theme(legend.position = 'none') +
  xlab('') + ylab('') +
  coord_fixed(1.3) +
  transition_states(movie_map$decade) +
  labs(title = 'Global Average Movie Gross Revenue per Decade',
       subtitle = '{closest_state}')



europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Republic","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom","Switzerland")

europemap <- map_data('world', region = europeanUnion)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
europe.lab.data <- europemap %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

europe_movie_map <- movies[, list(runtime = mean(runtime), revenue = mean(gross)), by = list(country)]

test <- merge(europemap, europe_movie_map, by.x = 'region', by.y = 'country', all.x = T )

test <- test[order(test$order), ]

ggplot(test, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = runtime)) +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5) +
  geom_text(aes(label = region), data = europe.lab.data,  size = 4, hjust = 0.5, color = 'black') +
  theme_void() +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill"
  )
  
  # transition_states(decade)

# if (require("maps")) {
#   states <- map_data("state")
#   arrests <- USArrests
#   names(arrests) <- tolower(names(arrests))
#   arrests$region <- tolower(rownames(USArrests))
#   
#   choro <- merge(states, arrests, sort = FALSE, by = "region")
#   choro <- choro[order(choro$order), ]
#   ggplot(choro, aes(long, lat)) +
#     geom_polygon(aes(group = group, fill = assault)) +
#     coord_map("albers",  at0 = 45.5, lat1 = 29.5)
# }



# ggplot() +
#   geom_polygon(
#     data = europemap, 
#     aes(x = long, y = lat, group = group), 
#     fill = 'gray', color = 'black') +
#   geom_point(
#     data = movie_map_year, 
#     aes(lon.x, lat.x, size = runtime, color = runtime)) +
#   theme_bw() +
#   theme(legend.position = 'top') +
#   xlab('') + ylab('') + 
#   coord_fixed(1.3) +
#   transition_states(movie_map_year$decade)

# interactive scatterplot -------------------------------------------------


movies_scatter <- movies

movies_scatter$tooltips <- paste(movies_scatter$name, 'had a budget of', round(movies_scatter$budget), 'USD')

summary(movies_scatter$gross)

p <- ggplot(data = movies_scatter, aes(score, gross, color = rating_econ_success, tooltip = tooltips, data_id = genre)) +
  geom_point(size = 2) +
  theme_bw()+
  ylab('Gross Revenue (USD)') + xlab('IMDb Score') +
  theme(legend.position = 'top') + 
  labs(color='') +
  geom_point_interactive() +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10))
p
girafe(ggobj = p)
girafe(ggobj = p, options = list(opts_hover(css = 'fill:black;')))


# Racing lines ------------------------------------------------------------



summary_movies <- movies[, list(runtime = mean(runtime), 
                           avg_revenue = mean(gross), 
                           avg_budget = mean(budget),
                           count = .N,
                           sum_revenue = sum(gross),
                           avg_cost_minute_runtime = mean(cost_minute_runtime)), by = list(year)]

summary_movies$tooltips <- paste(summary_movies$year, 'saw an average movie budget of', round(summary_movies$avg_budget), 'USD')

colors_summary_movies <- c("Avg. Budget" = "orange3", "Avg. Gross Revenue" = "green4")

ggplot(summary_movies, aes(x = year))+
  geom_line(aes(y = avg_budget, 
                color = 'Avg. Budget'), size = 2)+
  scale_y_continuous('USD',
                     labels = scales::dollar) +
  geom_line(aes(y = avg_revenue, 
                color = 'Avg. Gross Revenue'), size = 2)+
  theme_bw()+
  scale_x_continuous('Production Year',
                     limits = c(1985, 2017), 
                     breaks = c(1985,1990,1995,2000,2005,2010,2015)) +
  labs(title = 'Global Average Budget and Revenue Trends from 1986 to 2016', 
       color = "Legend") +
  scale_color_manual(values = colors_summary_movies) +
  theme(legend.position = 'bottom') +
  transition_reveal(year, keep_last = T)



# racing bars ----------------------------------------------


baseline_genres_agg <- movies[, list(avg_score = mean(score),
                                    avg_revenue = mean(gross),
                                    avg_budget = mean(budget),
                                    avg_profit = mean(profit)),
                             by = list(year, genre)]

tidy_aproach_genres_bars <- baseline_genres_agg %>%
  group_by(year) %>%
  mutate(rank = rank(-avg_score),
         Value_rel = avg_score/avg_score[rank==1],
         Value_lbl = paste0(" ",round(avg_score, digits = 1) )) %>%
  group_by(genre) %>% 
  filter(rank <= 5) %>% 
  ungroup()

# score race on x axis

racing_bars_tidy_static <- 
  ggplot(tidy_aproach_genres_bars, aes(fill = genre)) +  
  aes(xmin = 0 ,  
      xmax = avg_score) +  
  aes(ymin = rank - .45,  
      ymax = rank + .45,  
      y = rank) +  
  facet_wrap(~ year) +  
  geom_rect(alpha = .7) +  
  scale_x_continuous(  
    limits = c(-2, 9),  
    breaks = c(0, 2, 4, 6, 8)) +  
  scale_fill_viridis_d()
  geom_text(col = "gray13",  
            hjust = "right",  
            aes(label = genre),
            x = 0) +  
  scale_y_reverse() +  
  labs(fill = NULL) +  
  labs(x = 'IMDb Score') +  
  labs(y = "")

racing_bars_tidy_dynamic <- racing_bars_tidy_static + 
  facet_null() +
  scale_x_continuous(limits = c(-1, 10))+  
  geom_text(x = 9 , y = -5,
            aes(label = as.character(year)),
            size = 20, col = "grey18")+
  aes(group = genre)+
  transition_time(year)

animate(racing_bars_tidy_dynamic, nframes = 200)

# revenue race on y axis

static_plot <- ggplot(tidy_aproach_genres_bars, aes(x = rank, group = genre, 
                                       fill = as.factor(genre), color = as.factor(genre))) +
  geom_tile(aes(y = avg_score/2,
                height = avg_score,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(genre, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=avg_score,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

dynamic_plot <- static_plot + transition_states(year) + labs(title = 'IMDb Score per Year : {closest_state}', subtitle  =  "Top Genres by Year")

animate(dynamic_plot,nframes = 300)
  


