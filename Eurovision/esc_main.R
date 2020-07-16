#PACKAGES

install.packages("ggmap")
install.packages("ggthemes")
install.packages("devtools")
install.packages("PopED")
install.packages("RColorBrewer")
install.packages("tidyverse")
library(RColorBrewer)
library(tidyverse)
library(PopED)
library(network)
library(devtools)
devtools::install_github("gaborcsardi/pkgconfig")
devtools::install_github("igraph/rigraph")

#SET WORKING DIRECTORY
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir.path = getwd()
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



#MERGING COUNTRY CODE TO POINTS DF
df <- merge(df, country_code, by.x = 'Voter', by.y = 'Code', sort=FALSE) #merge both dfs
df <- select(df, Country, Points, Voter.y, Year) #Drop extra columns
df <- df %>% dplyr::rename(Voter = Voter.y) #Rename column

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

#For Gatherer Approach
#Number of countries voting in year window
countries_participating <- function(Year_St, Year_End){
  df_year_selected <- df %>% filter(Year %in% (Year_St:Year_End))
  distinct_df_year <- df_year_selected %>% distinct(Year, Voter, .keep_all = TRUE)
  # print(nrow(distinct_df_year))
  return(nrow(distinct_df_year))
}

# countries_participating(1957,1957)

#Scoring Functions

voting_standard <- function(Year, num_count){
  #Voting: 50% televoting (1,2,3,4,5,6,7,8,10,12 points) and 50% national juries (1,2,3,4,5,6,7,8,10,12 points)
  score_set_a <- c(12,10,8,7,6,5,4,3,2,1)
  #Voting: Each country's jury awarded 5, 4, 3, 2, and 1 points
  score_set_b <- c(5,4,3,2,1)
  #Jury voting. Each of the participating country awarded 3, 2 and 1 point
  score_set_c <- c(3,2,1)
  pos <- 0
  if (Year == 1962){
    scoreset <- score_set_c
    pos <- ceiling(runif(1)*num_count)
    if(pos > length(scoreset)){
      score <- 0
    }
    else score <- scoreset[pos]
  }
  else if (Year == 1963){
    scoreset <- score_set_b
    pos <- ceiling(runif(1)*num_count)
    if(pos > length(scoreset)){
      score <- 0
    } else score <- scoreset[pos]
  }
  else if (Year <= 2015 && Year >= 1975) {
    scoreset <- score_set_a
    pos <- ceiling(runif(1)*num_count)
    if(pos > length(scoreset)){
      score <- 0
    } else score <- scoreset[pos]
  }
  else if(Year >= 2016){
    scoreset <- score_set_a
    pos_jury <- ceiling(runif(1)*num_count)
    pos_telev <- ceiling(runif(1)*num_count)
    
    if(pos_jury > length(scoreset)){
      score_jury <- 0
    } else score_jury <- scoreset[pos_jury]
    
    if(pos_telev > length(scoreset)){
      score_telev <- 0
    } else score_telev <- scoreset[pos_telev]

  score <- score_jury + score_telev
  }
  # print(score)
  return(score)
}

#Set of scores with consecutive points awarded - equal chance of receiving each score
voting_sequential <- function(Year, num_count){
  score <- 0
  #Each country had 10 jury members,each jury member could award 1 pt to 1 song
  score_set_a <- ones(1, 10)
  #Each country's jury awarded 5, 3 and 1 points
  score_set_b <- c(5, 3, 1)
  
  #iterating through scores
  if((1957 <= Year && Year <= 1961) || (1967 <= Year && Year <= 1970) || Year == 1974 ){
    for(i in 1:length(score_set_a)) {
      pos <- ceiling(runif(1)*num_count)
      # print(paste0("pos_jur ", pos))
      if(pos == 1) {
        score <- score_set_a[i] + score
        # print(paste0("score_jur ", score))
      }
    }
  }
  else if(1964 <= Year && Year <= 1966){
    for(i in 1:length(score_set_b)) {
      pos <- ceiling(runif(1)*num_count)
      if(pos == 1) {
        score <- score_set_b[i] + score
      }
    }
  }
  return(score)
}

#Each country had 2 jury members and each jury member awarded 1 to 5 points for each song
voting_jury <- function(Year, num_count){
  score <- 0
  score_set <- c(1,2,3,4,5)

  #iterating through scores
  if(1971 <= Year && Year <= 1973){
    J1 <- score_set[sample(1:5, 1)]
    J2 <- score_set[sample(1:5, 1)]
    score <- J1 + J2
    return (score)
  }
}



simulation_scores <- function(Year_St, Year_End){
  simul_avg <- numeric()
  iterations <- 3000
  conf_int_5 <- max(1, floor(0.05*iterations)) #so it's never less than 1
  conf_int_10 <- max(1, floor(0.1*iterations)) #so it's never less than 1
  for(i in 1:iterations){
    current_sim <- vector()
    for(Year in Year_St:Year_End){
      num_countries <- countries_participating(Year,Year) #how many countries participating?
      if(1971 <= Year && Year <= 1973){
        score <- voting_jury(Year, num_countries)
      }
      else if( (1957 <= Year && Year <= 1961) || (1967 <= Year && Year <= 1970) || Year == 1974 || (1964 <= Year && Year <= 1966)){
        score <- voting_sequential(Year, num_countries)
      }
      else if(Year == 1962 || Year == 1963 || Year >= 2016 || Year >= 1975 || Year <= 2015 ){
        score <- voting_standard(Year, num_countries)
      }
      current_sim <- c(current_sim, score)
      # print(paste0("Score: ", score))
      # print(paste0("Current_sim a: ", current_sim))
    }
  average_sim <- mean(current_sim)
  # print(paste0("average_sim: ", average_sim))
  simul_avg <- c(simul_avg, average_sim)
  # print(paste0("simul_avg: ", simul_avg))
  }
  
  sorted_avg_sim <- sort(simul_avg, decreasing = TRUE)
  # print(paste0("sorted_avg_sim: ", sorted_avg_sim))
  conf5perc <- sorted_avg_sim[conf_int_5]
  # print(conf5perc)
  conf10perc <- sorted_avg_sim[conf_int_10]
  # print(c(conf10perc, conf5perc))
  return(c(conf10perc, conf5perc))
}

overall_score_window <- function(Year_St, Year_End, Size_Window){
  #Dictionary per window interval
  dictionary_window <- list()
  Year_Curr <- Year_St
  while ( (Year_Curr + Size_Window) <= Year_End ){
    dict_pt <- sprintf("%s-%s", Year_Curr, Year_Curr+Size_Window)
    # dictionary_window[dict_pt] <- list()
    dictionary_window[[dict_pt]]$Countries = distinct(subset(df,
                                                              Year >= Year_Curr & Year <= (Year_Curr + Size_Window),
                                                              select = c(Voter)))
    
    dictionary_window[[dict_pt]]$Scores = df %>%
      filter(Year >= Year_Curr & Year <= (Year_Curr + Size_Window)) %>%
      group_by(Voter, Country) %>%
      summarize(Sum_points = sum(Points), Avg_points = (Sum_points/(Size_Window+1)))
    
    Year_Curr = Year_Curr + Size_Window
  }
  # print(dictionary_window)
  return(dictionary_window)
}

collusion_country <- function(Year_St, Year_End, Size_Window, Window_Conf){
  #Determining collusion according to thresholds
  
  collusion_dictionary <- list()
  dictionary_window <- overall_score_window(Year_St, Year_End, Size_Window)
  Year_Curr <- Year_St
  n <- 1
  while (Year_Curr + Size_Window <= Year_End)
  {
  dict_pt <- sprintf("%s-%s", Year_Curr, Year_Curr+Size_Window)
  threshold <- Window_Conf[[dict_pt]][2]
  CountryNames <- dictionary_window[[dict_pt]]$Countries
  Points <- dictionary_window[[dict_pt]]$Scores$Sum_points
  Points_Average <- dictionary_window[[dict_pt]]$Scores$Avg_points 
  
  dict_bias <- sprintf("Bias:%s-%s", Year_Curr, Year_Curr+Size_Window)

  collusion_dictionary[[dict_bias]] <- dictionary_window[[dict_pt]]$Scores %>% filter(Avg_points > threshold) %>% mutate(Thresh_Diff = (Avg_points - threshold)) %>% select(Voter, Country, Avg_points, Thresh_Diff)
  collusion_dictionary[[dict_bias]][['Collusion']] <- ifelse(is.na(match(paste0(collusion_dictionary[[dict_bias]][['Voter']], collusion_dictionary[[dict_bias]][['Country']]), paste0(collusion_dictionary[[dict_bias]][['Country']], collusion_dictionary[[dict_bias]][['Voter']]))),"No", "Yes")
  Year_Curr <- Year_Curr + Size_Window
  n <- n+1
  }
  return(collusion_dictionary)
}

main_algorithm <- function(Year_St, Year_End, Size_Window){
  Min_Year <- 1957
  Max_Year <- 2019
  Year_Counter <- Min_Year
  Num_Countries <- list()
  while (Year_Counter <= Max_Year){
    Num_Countries[[Year_Counter]] <- countries_participating(Year_Counter, Year_Counter)
    Year_Counter <- Year_Counter + 1
    }
  #For the purpose of this project, timeframe is 1957-2019
  if(Year_St > Year_End || Year_St < Min_Year || Year_End > Max_Year){
    print(sprintf("Error - Please choose dates between %s and %s, starting with the earlier date first", Min_Year, Max_Year))
  }
  if ((Year_St + Size_Window) > Year_End){
    print("Error - Please choose a correct window size")
  } 
  
  #Collusion Significance Retrieval
  Window_Conf <- list()
  Year_Counter <- Year_St
  while ( (Year_Counter + Size_Window) <= Year_End ){
    conf_5_perc <- simulation_scores(Year_Counter, Year_Counter + Size_Window)
    print(conf_5_perc)
    Window_Conf[[sprintf("%s-%s",Year_Counter, (Year_Counter + Size_Window))]] <- conf_5_perc
    print(Window_Conf)
    Year_Counter <- Year_Counter + Size_Window
  }
  
  # dictionary_window <- overall_score_window(Year_St, Year_End, Size_Window)
  # print(dictionary_window)
  collusion_dictionary <- collusion_country(Year_St, Year_End, Size_Window, Window_Conf)
  # print(collusion_dictionary)
  
  return(collusion_dictionary)
}

#Call Algorithm to obtain Collusion Dictionaries
full_5 <- main_algorithm(1957, 2019, 5)
full_10 <- main_algorithm(1957, 2019, 10)


#Data Importing
country_regions <- read_csv("geographical_europe.csv", col_types = cols(.default = 'c'))
map_country <- read_csv("map_country.csv", col_types = cols(.default = 'c'))
map_region <- read_csv("map_region.csv", col_types = cols(.default = 'c'))

#Set Colours for Regions
library(RColorBrewer)
colrs <- brewer.pal(n=7,"Set3")

#Create Network Graphs DF (for full_5)
j=1
for(i in full_5){
nam <- paste("collusion_full_5", names(full_5)[j], sep="_")


#Full Info DF
assign(nam, i %>% filter(Collusion == 'Yes') %>% left_join(country_regions, by = "Country") 
            %>% rename(Country_Region = Region, Country_Subregion = Subregion)
            %>% left_join(country_regions, by = c("Voter"="Country"))
            %>% rename(Voter_Region = Region, Voter_Subregion = Subregion)
             %>% select(Voter, Country, Thresh_Diff, Country_Region, Country_Subregion, Voter_Region, Voter_Subregion))

#Link DF
nam2 <- paste("collusion_link_5", names(full_5)[j], sep="_")
assign(nam2, get(nam) %>% dplyr::distinct(Country, Voter, Country_Region, Voter_Region, Thresh_Diff))
assign(nam2, transform(get(nam2), Country_ID=match(Country, map_country$Country)))
assign(nam2, transform(get(nam2), Voter_ID=match(Voter, map_country$Country)))
assign(nam2, subset(get(nam2), select = -c(1:4)))
assign(nam2, get(nam2) %>% rename(from = Voter_ID, to = Country_ID, weight = Thresh_Diff) %>% select(from, to, weight))
               

#Node DF
nam3 <- paste("collusion_node_5", names(full_5)[j], sep="_")
assign(nam3, get(nam) %>% dplyr::select(Country, Country_Region, Country_Subregion, Voter))
assign(nam3, transform(get(nam3), id=match(Country, map_country$Country)))
assign(nam3, transform(get(nam3), Region_ID=match(Country_Region, map_region$Country_Region)))
assign(nam3, get(nam3) %>% distinct(id, Country, Country_Region, Region_ID, Country_Subregion) %>% select(id, Country, Country_Region, Region_ID, Country_Subregion)) 

#Generate Network Graph
library(igraph)
net <- graph_from_data_frame(d=get(nam2), vertices=get(nam3), directed=F)
V(net)$color <- colrs[as.factor(V(net)$Region_ID)]
edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]
E(net)$width <- E(net)$weight*6
l <- layout_(net, on_grid())
png(file=file.path(dir.path, paste0("coll_network_graph_5_", gsub(":", "_", names(full_5[j])), ".png")),title=names(full_5)[j], width = 500, height = 500, units='mm', res = 250)
par(mar=c(5.1, 4.1, 4.1, 18.1), xpd=TRUE)
plot(net, main = names(full_5[j]), frame = TRUE, vertex.label=V(net)$Country, layout=l, edge.curved=0, vertex.label.color="black", vertex.label.cex=1.8, vertex.label.family="Helvetica", edge.color=edge.col, rescale=TRUE, cex.main=50)
coord <- par("usr")
n <- length(V(net))
f <- factor(V(net)$Country_Region)
legend(legend=unique(f), pch=21, x = coord[2] * 1.05, y = coord[4],
       col=colrs[as.numeric(as.factor(levels(as.factor(V(net)$Country_Region))))], pt.bg = colrs, pt.cex=4, cex=2, bty="n", ncol=1)
dev.off()

j <- j+1
}

#Create Network Graphs DF (for full_10)
j=1
for(i in full_10){
  nam <- paste("collusion_full_10", names(full_10)[j], sep="_")
  

  if(dim(filter(i, Collusion == 'Yes'))[1] == 0) next
  else{
    #Full Info DF
    assign(nam, i %>% filter(Collusion == 'Yes') %>% left_join(country_regions, by = "Country") 
           %>% rename(Country_Region = Region, Country_Subregion = Subregion)
           %>% left_join(country_regions, by = c("Voter"="Country"))
           %>% rename(Voter_Region = Region, Voter_Subregion = Subregion)
           %>% select(Voter, Country, Thresh_Diff, Country_Region, Country_Subregion, Voter_Region, Voter_Subregion))
    
    #Link DF
    nam2 <- paste("collusion_link_10", names(full_10)[j], sep="_")
    assign(nam2, get(nam) %>% dplyr::distinct(Country, Voter, Country_Region, Voter_Region, Thresh_Diff))
    assign(nam2, transform(get(nam2), Country_ID=match(Country, map_country$Country)))
    assign(nam2, transform(get(nam2), Voter_ID=match(Voter, map_country$Country)))
    assign(nam2, subset(get(nam2), select = -c(1:4)))
    assign(nam2, get(nam2) %>% rename(from = Voter_ID, to = Country_ID, weight = Thresh_Diff) %>% select(from, to, weight))
    
    
    #Node DF
    nam3 <- paste("collusion_node_10", names(full_10)[j], sep="_")
    assign(nam3, get(nam) %>% dplyr::select(Country, Country_Region, Country_Subregion, Voter))
    assign(nam3, transform(get(nam3), id=match(Country, map_country$Country)))
    assign(nam3, transform(get(nam3), Region_ID=match(Country_Region, map_region$Country_Region)))
    assign(nam3, get(nam3) %>% distinct(id, Country, Country_Region, Region_ID, Country_Subregion) %>% select(id, Country, Country_Region, Region_ID, Country_Subregion)) 
  
    library(igraph)
    net <- graph_from_data_frame(d=get(nam2), vertices=get(nam3), directed=F)
    V(net)$color <- colrs[as.factor(V(net)$Region_ID)]
    edge.start <- ends(net, es=E(net), names=F)[,1]
    edge.col <- V(net)$color[edge.start]
    E(net)$width <- E(net)$weight*6
    l <- layout_(net, on_grid())
    png(file=file.path(dir.path, paste0("coll_network_graph_10_", gsub(":", "_", names(full_10[j])), ".png")),title=names(full_10)[j], width = 500, height = 500, units='mm', res = 250)
    par(mar=c(5.1, 4.1, 4.1, 18.1), xpd=TRUE)
    plot(net, main = names(full_10[j]), frame = TRUE, vertex.label=V(net)$Country, layout=l, edge.curved=0, vertex.label.color="black", vertex.label.cex=1.8, vertex.label.family="Helvetica", edge.color=edge.col, rescale=TRUE, cex.main=50)
    coord <- par("usr")
    n <- length(V(net))
    f <- factor(V(net)$Country_Region)
    legend(legend=unique(f), pch=21, x = coord[2] * 1.05, y = coord[4],
           col=colrs[as.numeric(as.factor(levels(as.factor(V(net)$Country_Region))))], pt.bg = colrs, pt.cex=4, cex=2, bty="n", ncol=1)
    dev.off()
    
    
    j <- j+1
  }
}

for(i in full_5){
  print(xtable( i %>% filter (Collusion == 'Yes') %>% select(Voter, Country, Thresh_Diff)))
}



