library(ggrepel)
library(tidyverse)
library(lubridate)
library(RColorBrewer)

#### Read in data ####
stop_data <- read_csv('input_data/Police_Stop_Data.csv', col_types = 'dccfffcccfffddddffc') %>%
  select(-OBJECTID)
# Population data gathered from http://www.mncompass.org/profiles/city/minneapolis
populations <- tibble(race = c('White', 'Black', 'Asian', 'Native American', 'Latino'),
                          population = c(265363,80546,25497,5834,39849)) %>%
  mutate(frc_pop = population/sum(population))

#### Fix the date times ####
stop_data$responseDate <- ymd_hms(stop_data$responseDate)
stop_data$lastUpdateDate <- ymd_hms(stop_data$lastUpdateDate)

#### Fix data types and turn yes/no into 1/0 ####
stop_data$race <- fct_explicit_na(stop_data$race)

stop_data$personSearch[stop_data$personSearch == 'YES'] <- 1
stop_data$personSearch[stop_data$personSearch == 'NO'] <- 0
stop_data$personSearch <- as.numeric(stop_data$personSearch)

stop_data$vehicleSearch[stop_data$vehicleSearch == 'YES'] <- 1
stop_data$vehicleSearch[stop_data$vehicleSearch == 'NO'] <- 0
stop_data$vehicleSearch <- as.numeric(stop_data$vehicleSearch)

stop_data$citationIssued[stop_data$citationIssued == 'YES'] <- 1
stop_data$citationIssued[stop_data$citationIssued == 'NO'] <- 0
stop_data$citationIssued <- as.numeric(stop_data$citationIssued)

#### Find average rates of searches and citations when not NA ####
table(stop_data$personSearch, useNA = 'ifany')
table(stop_data$vehicleSearch, useNA = 'ifany')

stop_data %>% filter(!is.na(personSearch) & !is.na(vehicleSearch)) %>%
  summarize(count = n(),
            person_searches = sum(personSearch),
            vehicle_searches = sum(vehicleSearch)) %>%
  mutate(person_search_rate = person_searches/count,
         vehicle_search_rate = vehicle_searches/count)

#### Bridged Race ####
stop_data <- stop_data %>%
  mutate(race_bridge = ifelse(race == 'East African' | race == 'Black', 'Black',
                              ifelse(race == 'Unknown' | race == 'Other', 'Other/Unknown',
                                     ifelse(race == 'White', 'White',
                                            ifelse(race == 'Latino', 'Latino',
                                                   ifelse(race == 'Native American', 'Native American',
                                                          ifelse(race == 'Asian', 'Asian', NA))))))) 

#### Plot fraction of population vs fraction of stops ####
inner_join(
stop_data %>% group_by(race_bridge) %>%
  filter(!is.na(race_bridge)) %>%
  filter(race_bridge != 'Other/Unknown') %>%
  summarize(count = n()) %>%
  mutate(frc_stops = count/sum(count)),
populations, by = c('race_bridge' = 'race')) %>%
  gather(category, fraction, frc_pop, frc_stops) %>%
  ggplot(aes(x = category, y = fraction, fill = race_bridge)) +
  geom_col() +
  geom_text(aes(label = round(fraction, 2)), position = position_stack(vjust = .5),
            size = 3, fontface = 'bold') +
  scale_fill_brewer(palette = 'Set2')+
  theme_classic()+
  theme(plot.caption = element_text(face = 'italic')) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1.02))+
  scale_x_discrete(labels = c('Population', 'MPD Stops')) +
  labs(y = 'Fraction of total',
       x = element_blank(),
       fill = element_blank(),
       title = 'Minneapolis population and MPD vehicular stops by race',
       subtitle = ('2016-10-31 to ????'),
       caption = 'source: opendataminneapolis Police Stop Data')
  

#### Plot rates of searches for each race ####
stop_data %>% group_by(race_bridge) %>% filter(!is.na(race_bridge)) %>%
  filter(race_bridge != 'Other/Unknown') %>%
  summarize(count = n(),
            person_search = sum(personSearch, na.rm = TRUE),
            vehicle_search = sum(vehicleSearch, na.rm = TRUE)) %>%
  mutate(person_search_rate = person_search/count,
         vehicle_search_rate = vehicle_search/count) %>%
  select(-person_search, -vehicle_search) %>%
  gather(category, rate, person_search_rate:vehicle_search_rate) %>%
  ggplot(aes(x = reorder(race_bridge,-rate), y = rate, fill = category)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(rate,2), y = rate/2),
            position = position_dodge(width = 1), fontface = 'bold', size = 3) +
  geom_hline(yintercept = c(.129, .0795), color = c('#F8766D', '#00BFC4'),
             linetype = 'dashed') +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,.35)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        legend.position = c(.8,.8),
        plot.caption = element_text(face = 'italic'))+
  labs(y = 'Search rate when stopped',
       x = element_blank(),
       fill = element_blank(),
       title = 'Fraction of MPD vehiclular stops leading to searches',
       subtitle = ('2016-10-31 to ????'),
       caption = 'source: opendataminneapolis Police Stop Data') +
  scale_fill_discrete(labels = c('Person search', 'Vehicle Search'))




#### Plot rates of searches for each race ####
stop_data %>% group_by(race_bridge, policePrecinct) %>% filter(!is.na(race_bridge)) %>%
  filter(race_bridge != 'Other/Unknown', !is.na(policePrecinct)) %>%
  summarize(count = n(),
            person_search = sum(personSearch, na.rm = TRUE),
            vehicle_search = sum(vehicleSearch, na.rm = TRUE))%>%
  mutate(person_search_rate = person_search/count,
         vehicle_search_rate = vehicle_search/count) %>%
  select(-person_search, -vehicle_search) %>%
  gather(category, rate, person_search_rate:vehicle_search_rate) %>%
  ggplot(aes(x = reorder(race_bridge,-rate), y = rate, fill = category)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(rate,2), y = rate/2),
            position = position_dodge(width = 1), fontface = 'bold', size = 3) +
  facet_grid(.~precinct)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        legend.position = c(.8,.8),
        plot.caption = element_text(face = 'italic'))+
  labs(y = 'Search rate when stopped',
       x = element_blank(),
       fill = element_blank(),
       title = 'Fraction of MPD vehiclular stops leading to searches',
       subtitle = ('2016-10-31 to ????'),
       caption = 'source: opendataminneapolis Police Stop Data') +
  scale_fill_discrete(labels = c('Person search', 'Vehicle Search'))









#### Plot of reason by race ####
stop_data %>% group_by(reason, race_bridge) %>% 
  filter(!is.na(reason)) %>%
  filter(!is.na(race_bridge)) %>%
  filter(race_bridge != 'Other/Unknown') %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(reason) %>%
  mutate(frc_of_stops = count/sum(count)) %>%
  ggplot(aes(x = reason, y = frc_of_stops, fill = race_bridge)) +
  geom_col()

#### Plot race by reason ####
stop_data %>% group_by(race_bridge, reason) %>% 
  filter(!is.na(reason)) %>%
  filter(!is.na(race_bridge)) %>%
  filter(race_bridge != 'Other/Unknown') %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(race_bridge) %>%
  mutate(frc_of_stops = count/sum(count)) %>%
  ggplot(aes(x = race_bridge, y = frc_of_stops, fill = reason)) +
  geom_col()


#### Plot of problem by race ####
stop_data %>% group_by(problem, race_bridge) %>% 
  filter(!is.na(problem)) %>%
  filter(!is.na(race_bridge)) %>%
  filter(race_bridge != 'Other/Unknown') %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(problem) %>%
  mutate(frc_of_stops = count/sum(count)) %>%
  ggplot(aes(x = problem, y = frc_of_stops, fill = race_bridge)) +
  geom_col()

#### Plot race by problem ####
stop_data %>% group_by(race_bridge, problem) %>% 
  filter(!is.na(problem)) %>%
  filter(!is.na(race_bridge)) %>%
  filter(race_bridge != 'Other/Unknown') %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(race_bridge) %>%
  mutate(frc_of_stops = count/sum(count)) %>%
  ggplot(aes(x = race_bridge, y = frc_of_stops, fill = problem)) +
  geom_col()








stop_data$race[stop_data$race == 'East African'] <- 'Black'

stop_data %>% group_by(race) %>% filter(race != '(Missing)') %>%
  summarize(count = n(),
            person_search = sum(personSearch, na.rm = TRUE),
            vehicle_search = sum(vehicleSearch, na.rm = TRUE),
            citation_issue = sum(citationIssued, na.rm = TRUE)) %>%
  mutate(person_search_rate = person_search/count,
         vehicle_search_rate = vehicle_search/count,
         citation_rate = citation_issue/count) %>%
  select(-person_search, -vehicle_search, -citation_issue) %>%
  gather(category, rate, person_search_rate:citation_rate) %>%
  ggplot(aes(x = reorder(race,-rate), y = rate, fill = category)) +
  geom_col(position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 90))