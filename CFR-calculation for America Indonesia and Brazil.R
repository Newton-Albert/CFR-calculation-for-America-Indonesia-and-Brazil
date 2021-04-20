mydata <- read.csv("C:\\Users\\Laplace\\Documents\\R work in CityU\\SIR epidemics\\WHO-COVID-19-global-data.csv", sep = ",")
names(mydata)[1] <- "Date_reported"
mydata$Date_reported <- as.Date(as.character(mydata$Date_reported), format = "%Y/%m/%d")
library(dplyr)
America <- mydata %>% 
  filter(Date_reported >= as.Date("20200201", format = "%Y%m%d")) %>%
  filter(Country == "United States of America")

Indonesia <- mydata %>%
  filter(Date_reported >= as.Date("20200201", format = "%Y%m%d")) %>%
  filter(Country == "Indonesia")

Brazil <- mydata %>%
  filter(Date_reported >= as.Date("20200201", format = "%Y%m%d")) %>%
  filter(Country == "Brazil")

UK <- mydata %>%
  filter(Date_reported >= as.Date("20200201", format = "%Y%m%d")) %>%
  filter(Country == "The United Kingdom")

India <- mydata %>%
  filter(Date_reported >= as.Date("20200201", format = "%Y%m%d")) %>%
  filter(Country == "India")


CFR_America <- data.frame(Date_reported = America$Date_reported, CFR = America$New_deaths/America$New_cases)
CFR_Indonesia <- data.frame(Date_reported = Indonesia$Date_reported, CFR = Indonesia$New_deaths/Indonesia$New_cases)
CFR_Brazil <- data.frame(Date_reported = Brazil$Date_reported, CFR = Brazil$New_deaths/Brazil$New_cases)
CFR_UK <- data.frame(Date_reported = UK$Date_reported, CFR = UK$New_deaths/UK$New_cases)
CFR_India <- data.frame(Date_reported = India$Date_reported, CFR = India$New_deaths/India$New_cases)
CFR_America$Country <- "America"
CFR_Indonesia$Country <- "Indonesia"
CFR_Brazil$Country <- "Brazil"
CFR_UK$Country <- "The United Kingdom"
CFR_India$Country <- "India"


CFR_selected_country <- rbind(CFR_America, CFR_Indonesia, CFR_Brazil,CFR_UK, CFR_India)

raw_data_selected_country <- rbind(America, Indonesia,Brazil,UK,India)
write.csv(raw_data_selected_country, "C:\\Users\\Laplace\\Desktop\\raw_data_selected_country.csv")


datebreaks <- seq(as.Date("2020-02-01"), as.Date("2021-03-31"),by = "15 day")
ggplot(data = CFR_AAB, aes(x = Date_reported, y = CFR, color = Country)) +
  geom_point()+
  scale_x_date(breaks = datebreaks) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  labs(x = "Date",y = "Cumulative Fatality Rate(CFR)",title = "Daily CFR (without delay, from 2020-02-01 to 2021-03-31)") 

mav <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=1)}

#mav(CFR_America$CFR,5)
#mav(CFR_Indonesia$CFR,5)

#plot(mav(CFR_Indonesia$CFR,5), col = "red", ylim = c(0,0.14))
#lines(mav(CFR_America$CFR,5), col = "green")
#lines(mav(CFR_Brazil$CFR,5), col = "pink")


CFR_America$moving_average <- as.character(mav(CFR_America$CFR,5))
CFR_Indonesia$moving_average <- as.character(mav(CFR_Indonesia$CFR,5))
CFR_Brazil$moving_average <- as.character(mav(CFR_Brazil$CFR,5))

CFR_moving_average <- rbind(CFR_America, CFR_Indonesia, CFR_Brazil)
CFR_moving_average$moving_average <- as.numeric(CFR_moving_average$moving_average)

ggplot(data = CFR_moving_average, aes(x = Date_reported, y = moving_average, color = Country)) +
  geom_line()+
  theme_bw()+
  scale_x_date(breaks = datebreaks) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  labs(x = "Date",y = "Moving average CFR of 5 days",title = "5-day moving average CFR (without delay, from 2020-02-01 to 2021-03-31)") 

