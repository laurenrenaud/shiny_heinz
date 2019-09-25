# set up libraries -----
library(dplyr)
library(httr)
library(jsonlite)
library(lubridate)
library(readr)
library(ggplot2)
library(gganimate)
library(ggmap)
library(maps)
library(RColorBrewer)

# capital projects ----------
options(scipen = 9)

capital.projects <- read_csv("data/capital_projects.csv")

# map
capital.projects %<>% 
  dplyr::filter(latitude != 0, longitude != 0) 

map_box <- make_bbox(lat = latitude, lon = longitude, 
                     data = capital.projects)
base_map <- get_map(location = map_box, source = "stamen", maptype = "toner-lite")
# Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.
# Please cite ggmap if you use it! See citation("ggmap") for details.

ggmap(base_map) +
  geom_jitter(data = capital.projects, 
              aes(x = longitude, y = latitude, color = status, size = budgeted_amount),
              alpha = 0.7) +
  scale_color_brewer(palette = "Paired") + 
  theme_void() +
  labs(x = "", y = "", title = "Locations", 
       color = "Status", size = "Budgeted\nAmount")


cap.proj.year.dist.status <- capital.projects %>%
  dplyr::mutate(proj_year = format(start_date, "%Y")) %>%
  dplyr::group_by(proj_year, council_district, status) %>%
  dplyr::summarise(amount = sum(budgeted_amount, na.rm = T))

# bar chart by council_district
cap.proj.year.dist.status %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(council_district)) %>%
  dplyr::mutate(status = ordered(status, c("Canceled", "Planned", "In Progress", "Completed")),
                council_district = paste0("District ", council_district)) %>%
  ggplot(aes(x = proj_year, y = amount, fill = status)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_color_brewer(palette = "Paired") + 
  scale_y_continuous(labels = scales::dollar) +
  #coord_flip() +
  theme_light() +
  facet_wrap("council_district", ncol = 3) +
  labs(x = "Project Year", y = "Budgeted Amount", fill = "",
       title = "Capital Project Budgets by District, Year, Status")

# bar chart by % each category
cap.proj.year.dist.status %>%
  dplyr::group_by(council_district, proj_year) %>%
  dplyr::mutate(total_dollars =sum(amount)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(council_district)) %>%
  dplyr::mutate(
    status = ordered(status, c("Canceled", "Planned", "In Progress", "Completed")),
    council_district = paste0("District ", council_district),
    perc_dollars = amount / total_dollars
  ) %>%
  ggplot(aes(x = proj_year, y = perc_dollars, fill = status)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_color_brewer(palette = "Paired") + 
  scale_y_continuous(labels = scales::percent) +
  #coord_flip() +
  theme_light() +
  facet_wrap("council_district", ncol = 3) +
  labs(x = "Project Year", y = "Budgeted Amount", fill = "",
       title = "Capital Project Budgets by District, Year, Status")




# other datasets ----------------------

# parking data ---------------
# pull from csv --------
# squirrel.hill <- read_csv("data/squirrel_hill.csv")
# downtown <- read_csv("data/downtown.csv")
agg.parking <- read_csv("data/aggregated-transactions-2019-09-03.csv")

# pull data from WPRDC ------

# form query
search_term <- "405"
search_limit <- 50000

query <- paste0("https://data.wprdc.org/api/3/action/datastore_search?resource_id=1ad5394f-d158-46c1-9af7-90a9ef4e0ce1&q=",
                search_term, "&limit=", search_limit)

# pull JSON using query
returned.query <- jsonlite::fromJSON(query)

# metadata about the results
fields <- returned.query$result$fields
# may be data dict? : https://data.wprdc.org/dataset/parking-transactions/resource/f58a2f59-b2e8-4067-a7d9-bbedb7e119b0?inner_span=True

# total number of records
returned.query$result$total

# count of records we pulled
nrow(returned.query$result$records)

# extract the parking records from the JSON
records <- returned.query$result$records
View(records)

# viz parking by time of day -----





# payments archive -----

payments.archive <- read_csv("data/payment_historical.csv") %>%
  dplyr::filter(!is.na(in_service_utc), !is.na(latitude))

map_box <- make_bbox(lat = latitude, lon = longitude, 
                     data = payments.archive)
base_map <- get_map(location = map_box, source = "stamen", maptype = "toner-lite")
# Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.
# Please cite ggmap if you use it! See citation("ggmap") for details.

#display.brewer.pal(n = 4, name = 'Paired')

p <- ggmap(base_map) +
#p <- ggplot(data = payments.archive) +
    geom_jitter(data = payments.archive, 
                aes(x = longitude, y = latitude, color = status),
                alpha = 0.7, size = 2.75) +
    #coord_cartesian(xlim = c(-88, -87.5), ylim = c(42.01, 41.65)) +
    scale_color_brewer(palette = "Paired") + 
    theme_void() +
    # theme(axis.title.x=element_blank(),
    #       axis.text.x=element_blank(),
    #       axis.ticks.x=element_blank(),
    #       axis.title.y=element_blank(),
    #       axis.text.y=element_blank(),
    #       axis.ticks.y=element_blank()) +
    labs(x = "", y = "", title = "Locations", color = "") +
  # install.packages("gifski")
  transition_reveal(in_service_utc) +
  #ease_aes('linear')  +
  shadow_mark(past = TRUE) +
  #enter_fade() + 
  #enter_grow() +
  #exit_fly(x_loc = 7, y_loc = 40) + 
  exit_recolour(fill = 'pink')



animate(p, renderer = av_renderer())
animate(p, renderer = gifski_renderer())


