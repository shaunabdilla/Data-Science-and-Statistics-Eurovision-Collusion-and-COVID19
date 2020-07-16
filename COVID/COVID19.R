#Setting Path

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir.path = getwd()

#Package Installation 

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("deSolve")
install.packages("devtools")
install.packages("splusTimeDate")
install.packages("optimr")
install.packages("miscset")
install.packages("Metrics")
install.packages("directlabels")
install.packages("car")

#Package Usage

library(tidyverse)
library(ggthemes)
library(reshape2)
library(grid)
library(deSolve)
library(devtools)
library(splusTimeDate)
library(optimr)
library(miscset)

#Read in Malta data

data_Malta <- read.csv(url("https://raw.githubusercontent.com/Lobeslab-Ltd/covid-19-MT/master/malta_time_series.csv"), header=T, colClasses = c("character", "Date", "integer","integer", "integer", "integer", "integer"))
# write.csv(data_Malta,"Data_Malta_added.csv", row.names = FALSE)
# data <- read.csv("Data_Malta.csv", header=T, colClasses = c("character", "Date", "integer",
# "integer", "integer", "integer", "integer"))

#Read in swab figures
swabs <- read.csv("Swabs_Malta.csv", header=T, colClasses = c("integer", "Date", "numeric")) %>% na.omit(swabs)
avg_daily_tests <- ceiling(swabs[[3]])

# Combine to dataframe
data_Malta$Tests <- c(avg_daily_tests)
data_Malta$Date <- factor(data_Malta$Date, ordered = T) %>% as.Date(data_Malta$Date, format = "%d-%m-%y")

#Correct a typo in the data
data_Malta$Date[data_Malta$Date == "2020-08-09"] <- "2020-06-09" 

#Global Data Processing and Cleaning
data_confirmed <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"), header=T)
data_recovered <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"), header=T)
data_dead <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"), header=T)

#Remove excess columns
data_dead <- data_dead %>% group_by(Country.Region) %>% summarise_if(is.numeric, funs(sum)) %>% select(-c(Lat, Long))
data_confirmed <- data_confirmed %>% group_by(Country.Region) %>% summarise_if(is.numeric, funs(sum)) %>% select(-c(Lat, Long))
data_recovered <- data_recovered %>% group_by(Country.Region) %>% summarise_if(is.numeric, funs(sum)) %>% select(-c(Lat, Long))

#Fix Date Columns
data_dead <- data_dead %>% gather("Date", "Deaths", 2:ncol(data_dead)) %>% arrange(Country.Region)
data_dead$Date <- gsub('X','0',data_dead$Date)
data_dead$Date <- lubridate::mdy(data_dead$Date)

#Fix Date Columns
data_confirmed <- data_confirmed %>% gather("Date", "Confirmed", 2:ncol(data_confirmed)) %>% arrange(Country.Region)
data_confirmed$Date <- gsub('X','0',data_confirmed$Date)
data_confirmed$Date <- lubridate::mdy(data_confirmed$Date)

#Fix Date Columns
data_recovered <- data_recovered %>% gather("Date", "Recovered", 2:ncol(data_recovered)) %>% arrange(Country.Region)
data_recovered$Date <- gsub('X','0',data_recovered$Date)
data_recovered$Date <- lubridate::mdy(data_recovered$Date)

#Combine Dataframes
data_global <- list(data_dead, data_confirmed, data_recovered) %>% reduce(left_join, by = c("Date", "Country.Region")) %>% rename(Country = Country.Region)

#Look at Rates of Change and growth
data_global <- data_global %>% mutate(Active = Confirmed - Deaths - Recovered,
                                          Mortality_rate = Deaths/Confirmed, 
                                          Recovery_rate = Recovered/Confirmed,
                                          Confirmed_change = Confirmed - lag(Confirmed),
                                          Previous_daily = lag(Confirmed_change),
                                          Growth_rate = Confirmed_change/Confirmed,
                                          Growth_rate_change = Growth_rate - lag(Growth_rate),
                                          Growth_rate_accel = Growth_rate_change/Growth_rate) %>%
                                          filter_at(vars("Mortality_rate", "Recovery_rate", "Growth_rate"), any_vars(!is.na(.)))

#Keep Malta dataframe separate for ease of reference
data.Malta <- subset(data_Malta, select =-Country) %>% mutate(Mortality_rate = Deaths/Confirmed, 
                                                        Recovery_rate = Recovered/Confirmed,
                                                        Confirmed_change = Confirmed - lag(Confirmed),
                                                        Previous_daily = lag(Confirmed_change),
                                                        Growth_rate = Confirmed_change/Confirmed,
                                                        Growth_rate_change = Growth_rate - lag(Growth_rate),
                                                        Growth_rate_accel = Growth_rate_change/Growth_rate,
                                                        Confirmed_per_test = Confirmed/Tests) %>%
                                                        filter_at(vars("Mortality_rate", "Recovery_rate", "Growth_rate"), any_vars(!is.na(.)))

#Vectors for graph splitting
absolutes <- c("Confirmed", "Active", "Deaths", "ITU", "Recovered")
rates <- c("Mortality_rate", "Recovery_rate", "Growth_rate")
growth_rate <- c("Growth_rate_accel")
changes <- c("Growth_rate_change", "Confirmed_change")

#Melt dataframes for ggplotting
mdf.Malta.abso <- reshape2::melt(data.Malta,id.vars="Date") %>% filter(variable %in% absolutes)
#mdf.Malta.abso$value <- as.numeric(mdf.Malta.abso$value)
mdf.Malta.rat <- reshape2::melt(data.Malta,id.vars="Date") %>% filter(variable %in% rates) %>% arrange(variable, Date)
mdf.Malta.acc <- reshape2::melt(data.Malta,id.vars="Date") %>% filter(variable %in% growth_rate) %>% arrange(variable, Date)
mdf.Malta.chg <- reshape2::melt(data.Malta,id.vars="Date") %>% filter(variable %in% changes) %>% arrange(variable, Date)

#aggregating over a week
mdf.Malta.rat_week <- mdf.Malta.rat %>% group_by(week = lubridate::week(Date), variable) %>% summarise(value = mean(value))

#Melt Dataframes for ggplotting - Global
mdf.Global.abso <- reshape2::melt(data_global,id.vars=c("Date","Country")) %>% filter(variable %in% absolutes)
mdf.Global.abso$Country <- gsub("\\*","",mdf.Global.abso$Country)
mdf.Global.rat <- reshape2::melt(data_global,id.vars=c("Date","Country")) %>% filter(variable %in% rates) %>% arrange(variable, Date)
mdf.Global.rat <- mdf.Global.rat[Reduce(`&`, lapply(mdf.Global.rat, is.finite)),]
mdf.Global.acc <- reshape2::melt(data_global,id.vars=c("Date","Country")) %>% filter(variable %in% growth_rate) %>% arrange(variable, Date)
mdf.Global.acc <- mdf.Global.acc[Reduce(`&`, lapply(mdf.Global.acc, is.finite)),]
mdf.Global.acc %>% group_by(Country, week = lubridate::week(Date), variable) %>% summarise(value = mean(value))
mdf.Global.chg <- reshape2::melt(data_global,id.vars=c("Date","Country")) %>% filter(variable %in% changes) %>% arrange(variable, Date)
mdf.Global.chg <- mdf.Global.chg[Reduce(`&`, lapply(mdf.Global.chg, is.finite)),]

#Aggregating over a week
mdf.Global.rat_week <- mdf.Global.rat %>% group_by(Country, week = lubridate::week(Date), variable) %>% summarise(value = mean(value))

#Subsetting top 10 countries globally 
mdf.Global.rates.top10 <- mdf.Global.chg %>% 
  filter(variable == 'Confirmed_change') %>%
  group_by(Country) %>% 
  summarise(Most_Growth = mean(value)) %>% 
  arrange(desc(Most_Growth)) %>% 
  slice(1:10)

#Make vector with lost top_countries
top_countries <- unique(mdf.Global.rates.top10$Country)
top_countries <- as.vector.factor(droplevels(top_countries))
top_countries <- c(top_countries, 'Malta')

#Subset countries for graph
mdf.Global.abso.top <- mdf.Global.abso %>% filter(Country %in% top_countries)

#ASBOLUTE FIGURES
#absolute figures graph
abs <- ggplot(mdf.Malta.abso, aes(x=Date, y=value, colour=variable, group=variable)) + 
  geom_line(size=2) +
  scale_color_stata() +
  ggtitle("Malta - Absolute Metrics") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), legend.title = element_text(size = 14),
        legend.text = element_text( size = 20)) +
  guides(colour = guide_legend(override.aes = list(size=10)))

# png(file=file.path(dir.path, paste0("absolute_Malta.png")),title="Malta, Absolute", width = 800, height = 500, units='mm', res = 250)
# print(abs)
# dev.off()

#plotting individual plots
p <- ggplot(mdf.Global.abso.top, aes(x=Date, y=value)) +
  geom_line(size=1.1) +
  scale_color_stata() +
  facet_grid(rows = vars(Country), cols = vars(variable), scales = "free") +
  ggtitle("Top Countries - Absolute Metrics") +
  scale_y_continuous(labels = scales::comma) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  theme(text = element_text(size=15), strip.text.y = element_text(size = 15, colour = "black", angle = 90))

# png(file=file.path(dir.path, paste0("absolute_glob.png")),title="Comparison, Absolute", width = 800, height = 500, units='mm', res = 250)
# print(p)
# dev.off()

#RATES OF CHANGE
#rates graph
rat <- ggplot(mdf.Malta.rat, aes(x=Date, y=value, colour=variable, group=variable)) + 
  geom_line(size=2) +
  ggtitle("Malta - Rate Metrics") +
  scale_color_manual(values = c("Mortality_rate" = "dark red", "Recovery_rate" = "dark green", "Growth_rate" = "dark blue")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  scale_x_date(date_breaks = "1 week", 
               limits = as.Date(c(min(mdf.Malta.rat$Date),max(mdf.Malta.rat$Date)))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), text = element_text(size=20))
# png(file=file.path(dir.path, paste0("rates_Malta.png")),title="Malta, Rates", width = 800, height = 500, units='mm', res = 250)
# print(rat)
# dev.off()

#rates graph - weekly aggregation
rat_week <- ggplot(mdf.Malta.rat_week, aes(x=week, y=value, colour=variable, group=variable)) + 
  geom_line(size=2) +
  ggtitle("Malta - Rate Metrics(weekly aggregation)") +
  scale_colour_manual(values = c("Mortality_rate" = "dark red", "Recovery_rate" = "dark green", "Growth_rate" = "dark blue")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  scale_x_continuous(limits =(c(min(mdf.Malta.rat_week$week),max(mdf.Malta.rat_week$week)))) +
  theme(text = element_text(size=20))
# png(file=file.path(dir.path, paste0("weekly_rates_Malta.png")),title="Malta, Weekly Rates", width = 800, height = 500, units='mm', res = 250)
# print(rat_week)
# dev.off()

#Filter by Top Countries
mdf.Global.rat_week.top <- mdf.Global.rat_week %>% filter(Country %in% top_countries)

#Rates graph for global
rat_week <- ggplot(mdf.Global.rat_week.top, aes(x=week, y=value)) + 
  geom_line(aes(color = variable, linetype = Country)) +
  scale_colour_manual(values = c("Mortality_rate" = "dark red", "Recovery_rate" = "dark green", "Growth_rate" = "blue")) + 
  #scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + 
  ylim(0,0.75)

#plotting individual plots
p <- ggplot(mdf.Global.rat_week.top, aes(x=week, y=value)) +
  ylim(0, 1) +
  geom_line() +
  facet_grid(rows = vars(Country), cols = vars(variable), scales = "fixed")
# png(file=file.path(dir.path, paste0("weekly_rates_glob.png")),title="Global, Weekly Rates", width = 800, height = 500, units='mm', res = 250)
# print(p)
# dev.off()

#Acceleration graph
acc <- ggplot(mdf.Malta.acc, aes(x=Date, y=value, colour=variable, group=variable)) + 
  geom_line(size=1.2) +
  scale_color_stata() +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), text = element_text(size=20))
# png(file=file.path(dir.path, paste0("acceleration_malta.png")),title="Malta Acceleration", width = 800, height = 500, units='mm', res = 250)
# print(acc)
# dev.off()

#Filter by Top Countries
mdf.Global.acc.top <- mdf.Global.acc %>% filter(Country %in% top_countries)

acc <- ggplot(mdf.Global.acc.top, aes(x=Date, y=value)) + 
  geom_line() +
  scale_color_stata() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits=c(-5, 5)) +
  scale_x_date(date_breaks = "2 weeks") +
  facet_grid(rows = vars(Country), cols = vars(variable), scales = "fixed")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), text = element_text(size=20))
# png(file=file.path(dir.path, paste0("acceleration_global.png")),title="Global Acceleration", width = 600, height = 500, units='mm', res = 400)
# print(acc)
# dev.off()

#SIR Fitting
#For Malta, initial uninfected population at beginning of data: according to 
# https://countrymeters.info/en/Malta, https://tradingeconomics.com/malta/population, https://www.worldometers.info/world-population/malta-population/
N <- ceiling(mean(c(425556, 441516, 490000)))

#Now we compare the daily incidence with the predicted incidence from the SIR model
Infected_abs <- subset(mdf.Malta.abso, variable == 'Confirmed')

#Infected <- filter(Infected_abs, Date >= SIR_start_date & Date <= SIR_end_date) %>% mutate(active_cum = cumsum(value)) %>% select(active_cum)
SIR_start_date <- lubridate::ymd("2020-03-07")
SIR_end_date <- tail(Infected_abs$Date, n=1)

Infected <- filter(Infected_abs, Date >= SIR_start_date & Date <= SIR_end_date) %>% select(c("Date", "value"))
Infected <- pull(Infected, value)

Day <- 1:(length(Infected))

#Initializing the values
init <- c(
  S <- N - Infected[1],
  I <- Infected[1],
  R <- 0
)


SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I
    list(c(dS, dI))
  })
}

RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fitInfected <- N-out[,2] 
  sum((Infected - fitInfected)^2)
}
K = 0.16
R0 = 1.04
SIR2 <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- I * K * (-S/N *  R0/(R0-1))
    dI <- I * K * ( S/N *  R0/(R0-1) - 1/(R0-1))  
    list(c(dS, dI))
  })
}

RSS2 <- function(parameters) {
  names(parameters) <- c("K", "R0")
  out <- ode(y = init, times = Day, func = SIR2, parms = parameters)
  fit <- out[,3]
  sum((Infected - fit)^2)
}

Infected_MC <- Infected
SIRMC2 <- function(R0,K) {
  parameters <- c(K=K, R0=R0)
  out <- ode(y = init, times = Day, func = SIR2, parms = parameters)
  fit <- out[,3]
  RSS <- sum((Infected_MC - fit)^2)
  return(RSS)  
}

SIRMC <- function(K) {
  optimize(SIRMC2, lower=1,upper=10^5,K=K, tol = .Machine$double.eps)$objective
}

#Return Estimated Values
estimated_values <- function() {
  opt1 <- optimize(SIRMC,lower=0,upper=1, tol = .Machine$double.eps)
  opt2 <- optimize(SIRMC2, lower=1, upper=10^5, K=opt1$minimum, tol = .Machine$double.eps)
  return(list(RSS=opt2$objective,K=opt1$minimum,R0=opt2$minimum))
}

# Starting Conditions
init <- c(S = N-Infected[1], I = Infected[1])

Optim_3 <- estimated_values()

Opt_par3 <- setNames(Optim_3[2:3], c("K", "R0"))

# plotting the result
t <- 1:as.integer(SIR_end_date + 1 - SIR_start_date) # time in days
fit3 <- data.frame(ode(y = init, times = t, func = SIR2, parms = Opt_par3))

# png(file=file.path(dir.path, paste0("SIRModel.png")), width = 500, height = 500, units='mm', res = 250)
# plot(Day,Infected, cex.lab=1.5, cex.main = 1.5, cex.axis=1.4,
#      main = "Infected including Recovered and Dead", xlab = "Days", ylab = "Infected")
# lines(t, fit3[,3], col = 1)
# dev.off()

#Modelling the curve with Logistic Regression Curve
library("car")
library("ggplot2")
full_rmse_matrix <- matrix(, nrow = 0, ncol = 2)
for (nation in unique(mdf.Global.abso$Country)) {
  Infected_abs <- subset(mdf.Global.abso, variable == 'Confirmed' & Country == nation )

  A <- function(a) a > 0
  test <- sapply(Infected_abs$value, A)
  start_pos <- ifelse(is.na(which(test == FALSE)[1]),1,which(test == FALSE)[1])

  SIR_start_date <- Infected_abs$Date[start_pos]
  SIR_end_date <- tail(Infected_abs$Date, n=1)

  Infected <- filter(Infected_abs, Date >= SIR_start_date & Date <= SIR_end_date) %>% select(c("Date", "value"))
  Infected <- pull(Infected, value)

  Day <- 1:(length(Infected))

  Cases_day <- data.frame(Infected,Day) #create the data frame
  # plot(Infected~Day, data=Cases_day)
  simulations = 100
  nam <- paste("val_rmse", nation, sep="_")

  assign(nam, matrix(0, nrow = simulations))

  Cases_day <- data.frame(Infected, Day)
  rmse_list <- vector()
  for (i in 1:simulations) {

    # set aside validation set
    train_split  <- sample(1:nrow(Cases_day), 0.8*length(Infected))
    training_set <- Cases_day[train_split, ]
    training_set <- training_set[order(training_set$Day), ]
    validation_set <- Cases_day[-train_split, ]
    validation_set <- validation_set[order(validation_set$Day), ]
    est_asym <- runif(1, min = ceiling(max(Infected)+sd(Infected)), max = ceiling(max(Infected)+(1.3*sd(Infected))))

    # fit models and store RMSEs
    coeffi <- try(coef(lm(qlogis(Infected / est_asym)~Day, data = training_set)), TRUE)
    if(class(coeffi)=="try-error"){
    next
    }

    log_growth <-try(nls(Infected~phi1/(1+exp(-(phi2+phi3*Day))),
                     start=list(phi1=est_asym,phi2=coeffi[1],phi3=coeffi[2]),data=training_set,trace=TRUE),TRUE)

    if(class(log_growth)=="try-error"){
      next
    }

    #set final parameters
    phi1<-coef(log_growth)[1]
    phi2<-coef(log_growth)[2]
    phi3<-coef(log_growth)[3]
    #fit model
    x<-c(min(training_set$Day):max(Cases_day$Day)) #construct a range of x values bounded by the data
    y<-phi1/(1+exp(-( phi2 + phi3 * x)))
    predict <- data.frame(x,y)

    #Validation Model
    calculation_rmse <- function(actual, predicted) {
    sqrt(mean((actual - predicted)^2))
    }
    #Error Calculation
    predict_calc <- predict %>% filter(x %in% validation_set$Day)
    validation_set <- validation_set %>% filter(Day %in% predict_calc$x)
    # validation_set <- validation_set %>% filter(x %in% predict_calc$x)
    rmse_list <- c(rmse_list, calculation_rmse(actual = validation_set$Infected, predicted = predict_calc$y))
  }
  assign(nam, median(rmse_list))
  full_rmse_matrix <- rbind(c(nam, get(nam)), full_rmse_matrix)
  # Plot Model Fit
  p <- ggplot(data=Cases_day,aes(x=Day,y=Infected))+
    geom_point(color='dark red',size=2)+theme_bw()+
    labs(x='Day',y='Infected')+
    scale_x_continuous()+
    scale_y_continuous()+
    theme(axis.text=element_text(size=18),axis.title=element_text(size=24))+
    geom_line(data=predict,aes(x=x,y=y), size=1)
  # png(file=file.path(dir.path, paste0("model_", nation, ".png")),title=nation, width = 500, height = 500, units='mm', res = 250)
  # print(p)
  # dev.off()

}


# ggplot(data=Cases_day,aes(x=Day,y=Infected))+
#   geom_point(color='blue',size=2)+ theme_bw()+
#   labs(x='Day',y='Infected')+
#   scale_x_continuous()+
#   scale_y_log10()+
#   theme(axis.text=element_text(size=18),axis.title=element_text(size=24))+
#   geom_line(data=predict,aes(x=x,y=y), size=1)

# startdate <- data_global$Date[1]
# library(directlabels)
# library(lubridate)

# #New Cases vs Existing Cases
# global_top_log <- data_global %>% filter(Country %in% top_countries) %>% mutate(Day=difftime(Date, startdate, units="days")) %>% select(Country, Day, Confirmed, Confirmed_change, Previous_daily, Deaths,Date)
# global_top_log_weekly <- global_top_log %>% group_by(Country, week = week(Date)) %>% summarise(Confirmed_week = mean(Confirmed), Confirmed_change_week = mean(Confirmed_change))
# png(file=file.path(dir.path, paste0("growth_vs_existing.png")), width = 400, height = 400, units='mm', res = 350)
# ggplot(global_top_log_weekly, aes(x = Confirmed_week, y = Confirmed_change_week, col = Country, group = Country)) + 
#   geom_line(size=1.1) +
#   scale_y_log10(labels = scales::comma) +
#   scale_x_log10(labels = scales::comma) +
#   guides(colour = guide_legend(override.aes = list(size=10))) +
#   theme(axis.text=element_text(size=18), text = element_text(size=20))+
#   labs(title ="New Confirmed Cases vs Existing Cases as at 28th June 2020", x = "Cumulative Cases", y = "New Cases") +
#   geom_dl(aes(label = Country), method = list(dl.combine("last.points")), cex = 1.4)
# dev.off()
