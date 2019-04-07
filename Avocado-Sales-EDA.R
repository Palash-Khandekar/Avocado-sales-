library(ggplot2)
library(dplyr)
library(tidyr)
install.packages("ggthemes")
library(ggthemes)
install.packages("plotly")
library(plotly)
##data prep
### READING THE DATA

#If I don't use the stringsAsFactors = FALSE argument in read.csv below, 
#the date column is read as a factor and not a string of characters
#that would it harder to split the date in year, month, day
data = read.csv("sales.csv", stringsAsFactors = FALSE)
str(data)

### PREPARING THE DATA


data$Date <- as.Date(data$Date, "%m/%d/%Y")
class(data$Date)
str(data)


# regions and types (conventional/organic) really should be factors 
# not a character because they're names
data$type = as.factor(data$type)
data$region = as.factor(data$region)
str(data)

#rename column names
colnames(data)[colnames(data)=="X4046"] <- "PLU4046"
colnames(data)[colnames(data)=="X4225"] <- "PLU4225"
colnames(data)[colnames(data)=="X4770"] <- "PLU4770"

#save new data to csv
#write.csv(data, file = "MyData.csv")

#reading data
#data <- read.csv("Mydata.csv")
#str(data)
#Avocados sold- codes
sale_codes <- data %>% 
  select(PLU4046, PLU4225, PLU4770)

avocado_type <- gather(sale_codes,type,tot_sale)
avocado_type

sale_graph <- ggplot(avocado_type, aes(x = type, y = tot_sale))+
  geom_bar(stat = "identity",fill = "#bbdd5d")+
  xlab("Hass Avocado Codes") + ylab("Total Avocado sold") + 
  ggtitle("Total number of Avocados sold in USA")

ggplotly(sale_graph)

#comparing consumption of organic and conventional type
type <- ggplot(data, aes(x = year,y = Total.Volume, fill = type)) + 
  geom_bar(position = "dodge",stat = "identity")
type

#density plot to look at the average price of Avocado by type
avgprice_type <- ggplot(data, aes(x=AveragePrice, fill=type)) + geom_density() + 
  facet_wrap(~type) + 
  theme(plot.title=element_text(hjust=0.5), legend.position="bottom") + 
  labs(title="Avocado Price by Type") + scale_fill_brewer(palette="Set2")
ggplotly(avgprice_type)



## Filter by type
organic <- data %>% 
  select(Date, AveragePrice, type, Total.Volume) %>% 
  filter(type == "organic")

conventional <- data %>% 
  select(Date, AveragePrice, type, Total.Volume) %>% 
  filter(type == "conventional")

#monthly changes of Avocado prices
df <- data
df$month_year <- format(as.Date(data$Date), "%m-%Y")
df$month <- format(as.Date(data$Date), "%m")
df$year <- format(as.Date(data$Date), "%Y")
str(df)

df$month_name <- sapply(df$month, function(x) month.abb[as.numeric(x)])
df$month_name = factor(df$month_name, levels = month.abb)

#monthly average price of conventional type
conv_monthly_pattern <- df %>% 
  select(month_name, AveragePrice, type) %>% 
  filter(type == "conventional") %>%
  group_by(month_name) %>% 
  summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=month_name, y=avg)) + geom_point(color="#1fc27c", aes(size=avg)) + 
  geom_line(group=1, color="#3d7630") + 
  theme_economist() + theme(legend.position="none", plot.title=element_text(hjust=0.5), 
                            plot.background=element_rect(fill="#fdfe9e")) + 
  labs(title="Conventional Avocados", x="Month", y="Average Price")
ggplotly(conv_monthly_pattern)

#monthly average price of conventional type
org_monthly_pattern <- df %>% 
  select(month_name, AveragePrice, type) %>% 
  filter(type == "organic") %>%
  group_by(month_name) %>% 
  summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=month_name, y=avg)) + geom_point(color="#1fc27c", aes(size=avg)) + 
  geom_line(group=1, color="#3d7630") + 
  theme_economist() + 
  theme(legend.position="none", plot.title=element_text(hjust=0.5), 
        plot.background=element_rect(fill="#fdfe9e")) + 
  labs(title="Organic Avocados", x="Month", y="Average Price")
ggplotly(org_monthly_pattern)

#####
vol_monthly <- df %>% select(month_name, Total.Volume) %>% 
  group_by(month_name) %>% 
  summarize(avg.volume=mean(Total.Volume)) %>%
  ggplot(aes(x=month_name, y=avg.volume)) + geom_bar(stat="identity",  fill="#b2df50") + 
  coord_flip() + theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"))
vol_monthly

price_monthly <- df %>% select(month_name, AveragePrice) %>% 
  group_by(month_name) %>% group_by(avg.price=mean(AveragePrice)) %>%
  ggplot(aes(x=month_name, y=avg.price))  + geom_bar(stat="identity", fill="#ffc71b") + 
  coord_flip() + theme_minimal() + 
  theme(legend.position="none", plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"))
price_monthly

###area wise sale
area_monthly <- df %>% select(region, Total.Volume) %>% 
  group_by(region) %>% summarize(avg.volume=mean(Total.Volume)) %>%
  ggplot(aes(x=region, y=avg.volume))  + geom_bar(stat="identity", fill="#ffc71b") + 
  coord_flip() + theme_minimal() + 
  theme(legend.position="none", plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"))
area_monthly

#area wise avg price
area_avg_price <- df %>% select(region, AveragePrice) %>% 
  group_by(region) %>% summarize(avg.price=mean(AveragePrice)) %>%
  ggplot(aes(x=region, y=avg.price))  + geom_bar(stat="identity", fill="#ffc71b") + 
  coord_flip() + theme_minimal() + 
  theme(legend.position="none", plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"))
area_avg_price



