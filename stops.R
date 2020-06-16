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
p1 <- inner_join(
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
       title = 'Share of Minneapolis population and MPD stops by race',
       caption = 'source: opendataminneapolis Police Stop Data, 10/31/2016 - 6/15/2020')

#### Plot rates of searches for each race ####
p2<-stop_data %>% group_by(race_bridge) %>% filter(!is.na(race_bridge)) %>%
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
       caption = 'source: opendataminneapolis Police Stop Data 10/31/2016 - 6/15/2020') +
  scale_fill_discrete(labels = c('Person search', 'Vehicle search'))


#### Plot rates of searches for each race facet by police precinct ####
stop_data <- stop_data %>% filter(!is.na(policePrecinct))
stop_data$policePrecinct <- droplevels(stop_data$policePrecinct)
stop_data$policePrecinct <- fct_explicit_na(stop_data$policePrecinct)
stop_data$policePrecinct <- factor(stop_data$policePrecinct,
                                   levels = c('1', '2', '3', '4', '5'),
                                   labels = c('Precinct 1', 'Precinct 2', 'Precinct 3', 'Precinct 4', 'Precinct 5'))
p3 <- stop_data %>% filter(!is.na(race_bridge)) %>%
  filter(race_bridge != 'Other/Unknown') %>%
  filter(policePrecinct != '(Missing)') %>%
  group_by(race_bridge, policePrecinct) %>%
  summarize(count = n(),
            person_search = sum(personSearch, na.rm = TRUE),
            vehicle_search = sum(vehicleSearch, na.rm = TRUE)) %>%
  mutate(person_search_rate = person_search/count,
         vehicle_search_rate = vehicle_search/count) %>%
  select(-person_search, -vehicle_search) %>%
  gather(category, rate, person_search_rate, vehicle_search_rate) %>%
  ggplot(aes(x = reorder(race_bridge, -rate), y = rate, fill = category)) +
  geom_col(position = position_dodge()) +
  facet_grid(.~policePrecinct)+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,.43)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        plot.caption = element_text(face = 'italic'))+
  labs(y = 'Search rate when stopped',
       x = element_blank(),
       fill = element_blank(),
       title = 'Fraction of MPD stops leading to searches',
       caption = 'source: opendataminneapolis Police Stop Data 10/31/2016 - 6/15/2020') +
  scale_fill_discrete(labels = c('Person search', 'Vehicle search'))

#### Export Plots ####
png("outputs/p1.png", units="in", width=6, height=4, res=300)
p1
dev.off()

png("outputs/p2.png", units="in", width=6, height=4, res=300)
p2
dev.off()

png('outputs/p3.png', units = 'in', width = 8, height = 4, res = 300)
p3
dev.off()

