#PACKAGES
install.packages("PopED")
install.packages("xtable")
install.packages("network")
install.packages("tidyverse")
library(xtable)
library(tidyverse)
library(PopED)
library(network)

#SET WORKING DIRECTORY
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#READ SCRAPED DATAFRAME
df <- tibble()
for(y in 1957:2019){
  df.y <- read.table(paste0(y,".csv"),header=T)
  df.y$Year <- y
  df <- bind_rows(df,df.y)
}

# sapply(df, class) #confirm column types

#COUNTRY SHORT CODES
country_code <- read_csv("country_2_digit.csv", col_types = cols(.default = 'c'))
#Add defunct countries
historical <- data.frame(Name = c("Serbia and Montenegro", "Yugoslavia"), Code = c("CS", "YU"))
names(historical) <- c("Name", "Code")
#Merge dfs together
country_code <- rbind(country_code, historical)
country_code$Code <- tolower(country_code$Code)
country_code <- country_code %>% dplyr::rename(Voter = Name)#Rename to avoid confusion



#GEOGRAPHICAL LOCATIONS
country_regions <- read_csv("geographical_europe.csv", col_types = cols(.default = 'c'))

#MERGING COUNTRY CODE TO POINTS DF
df <- merge(df, country_code, by.x = 'Voter', by.y = 'Code', sort=FALSE) #merge both dfs
df <- select(df, Country, Points, Voter.y, Year) %>% dplyr::rename(Voter = Voter.y)
#Drop extra columns

# unique_vals <- lapply(df, unique) #Check unique values
# unique_vals["Country"]
# unique_vals["Voter"]

# Data Cleaning to match country names
df <- df %>% mutate(Points=ifelse(is.na(Points),0,Points),
                    Country=ifelse(Country=="United Kingdom","UK",Country),
                    Voter=ifelse(Voter=="United Kingdom","UK",Voter),
                    Voter=ifelse(Voter=="Macedonia, the Former Yugoslav Republic of","North Macedonia",Voter),
                    Country=ifelse(Country=="Bosnia & Herzegovina","Bosnia and Herzegovina",Country),
                    Country=ifelse(Country=="Serbia & Montenegro","Serbia and Montenegro",Country),
                    Voter=ifelse(Voter=="Moldova, Republic of", "Moldova",Voter),
                    Voter=ifelse(Voter=="Russian Federation","Russia" ,Voter))

unique_vals <- lapply(df, unique) #Check unique values
unique_vals["Country"]
unique_vals["Voter"]
setdiff(unique_vals$Voter, unique_vals$Country) #Andorra has only been a voter, never a votee - never reached a final

#Add region and subregion to dataframe for EDA
breaks <- c(floor(min(df$Year)):ceiling(max(df$Year)))

df <- dplyr::left_join(df, country_regions, by = "Country") %>% rename(Country_Region = Region, Country_Subregion = Subregion)
df <- dplyr::left_join(df, country_regions, by = c("Voter" = "Country")) %>% rename(Voter_Region = Region, Voter_Subregion = Subregion)

Points_heat_map <- df %>% group_by(Country, Voter) %>% mutate(Meetings=n()) %>% 
            group_by(Country, Voter, Meetings, Country_Region, Voter_Region) %>% 
            summarise(Total_Points = sum(Points)) %>% mutate(Mean_Points = (Total_Points/Meetings)) %>%
            group_by(Country_Region) %>% mutate(Region_Count = n_distinct(Country)) %>%
            select (Country, Country_Region, Voter, Voter_Region, Meetings, Total_Points, Mean_Points, Region_Count) %>%
            filter(Meetings >= 5)
 
Region_heat_map <- Points_heat_map %>% group_by(Country_Region, Voter_Region) %>%
  group_by(Country_Region, Voter_Region) %>% 
  summarise(Average_Region_Points = mean(Mean_Points)) %>%
  # mutate(Average_Points = (Total_Region_Points/Meetings) )%>%
  select (Country_Region, Voter_Region, Average_Region_Points) 

#Histograms
hist_points <- ggplot(Points_heat_map, aes(x=Mean_Points)) +
              geom_histogram(aes(y=..density..), binwidth=2, color="black", fill="dark green") +
              geom_vline(aes(xintercept=mean(Mean_Points)), color="blue", linetype="dashed", size=1) +
              theme(text=element_text(size=21), axis.text=element_text(size=21))
png(file=file.path(dir.path, "histogram_points.png"), title="Histogram of Points", width = 500, height = 500, units='mm', res = 250)
hist_points
dev.off()


density_points <- ggplot(Points_heat_map, aes(x=Mean_Points)) +
  geom_histogram(aes(y=..density..), binwidth=2, color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  theme(text=element_text(size=21), axis.text=element_text(size=21))

png(file=file.path(dir.path, "density_points.png"), title="Density of Points", width = 500, height = 500, units='mm', res = 250)
density_points
dev.off()

#Plot a heatmap with region voting
region.heatmap <- ggplot(data = Region_heat_map, mapping = aes(x = Country_Region,
                                                       y = Voter_Region,
                                                       fill = Average_Region_Points)) +
  geom_tile() +
  xlab(label = "Country Region") +
  ylab(label = "Voter Region") +
  scale_fill_gradient(name = "Average_Region_Points",
                      low = 'white',
                      high = "#8B0000") +
  theme(text=element_text(size=21), axis.text=element_text(size=21))


png(file=file.path(dir.path, "regionheatmap.png"), title="Regional Heat Map", width = 500, height = 500, units='mm', res = 250)
region.heatmap
dev.off()


#Plot a heatmap with country voting
country.heatmap <- ggplot(data = Points_heat_map, mapping = aes(x = Country,
                                                               y = Voter,
                                                               fill = Mean_Points)) +
  geom_tile() +
  xlab(label = "Recipient") +
  ylab(label = "Voter") +
  scale_fill_gradient(name = "Mean_Points",
                      low = "white",
                      high = "#8B0000") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), text=element_text(size=21), axis.text=element_text(size=21))
png(file=file.path(dir.path, "Countryheatmap.png"), title='Voting Heat Map', width = 500, height = 500, units='mm', res = 250)
country.heatmap
dev.off()

#Network Graph

#Create an unfiltered point_map
Country_Map_Points <- df %>% group_by(Country, Voter) %>% mutate(Meetings=n()) %>% 
  group_by(Country, Voter, Meetings, Country_Region, Voter_Region) %>% 
  summarise(Total_Points = sum(Points)) %>% mutate(Mean_Points = (Total_Points/Meetings)) %>%
  group_by(Country_Region) %>% mutate(Region_Count = n_distinct(Country)) %>%
  select (Country, Country_Region, Voter, Voter_Region, Meetings, Total_Points, Mean_Points, Region_Count)


#Create Node Table
country_network_node <- Country_Map_Points %>% dplyr::distinct(Country) %>% dplyr::select(Country, Country_Region) 
country_network_node <- bind_rows(country_network_node, c(Country = "Andorra", Country_Region = "Southern Europe")) %>% 
                        arrange(Country_Region) %>% tibble::rowid_to_column("id")
country_network_node <- transform(country_network_node, Region_ID=match(Country_Region, unique(Country_Region)))
country_network_node$Region_ID <- as.numeric(as.character(country_network_node$Region_ID))
country_network_node$id <- as.numeric(as.character(country_network_node$id))


#Create dictionaries to use ids in links 
map_country <- country_network_node %>% select(Country,id)
write.csv(map_country,"map_country.csv", row.names = FALSE)
map_region <- distinct(country_network_node %>% select(Country_Region, Region_ID))
write.csv(map_region,"map_region.csv", row.names = FALSE)


#Create Link Table
country_network_link <- Country_Map_Points %>% dplyr::distinct(Country, Voter, Country_Region, Voter_Region, Mean_Points, Meetings)
country_network_link <- transform(country_network_link, Country_ID=match(Country, map_country$Country))
country_network_link <- transform(country_network_link, Voter_ID=match(Voter, map_country$Country))
country_network_link <- subset(country_network_link, select = -c(1:4))
filtered_country_network_link <- country_network_link %>% filter(Mean_Points > quantile(Mean_Points, 0.9) & Meetings >= 10) %>% 
                          rename(from = Voter_ID, to = Country_ID, weight = Mean_Points) %>% select(from, to, weight, Meetings)