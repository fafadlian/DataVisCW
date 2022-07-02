#Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(WDI)
library(corrplot)
library(GGally)
library(factoextra)
library(cluster)
library(fpc)
library(ggpubr)
library(MASS)
library(lubridate)
library(ggstatsplot)
library(grid)
library(ggrepel)
library(colorblindr)
library(psych)
library(ggpattern)

#WDI Loading
new_wdi_cache<-WDIcache()

#Overall Data and Line Chart

line_chart<-WDI(country = "all",
         indicator = c("NY.GDP.MKTP.KD.ZG",
                       "NE.TRD.GNFS.ZS",
                       "NY.GDP.PCAP.KN",
                       "NY.GDP.MKTP.KN"),
         start = 2000,
         end = 2020,
         extra = TRUE,
         cache = new_wdi_cache)

names(line_chart)[4]<-paste("GDP_growth_rate")
names(line_chart)[5]<-paste("Trade")
names(line_chart)[6]<-paste("GDP_per_capita")
names(line_chart)[7]<-paste("GDP")

line_chart<-filter(line_chart, region == "Aggregates")
line_chart<-filter(line_chart, reg)

line_chart_income<-filter(line_chart,country %in% c("High income", "Low income", "Lower middle income", "Upper middle income"))

satu<-ggplot(line_chart_income, aes(year, GDP_growth_rate, group = country, col = country))+
  geom_line( aes(linetype = country),size = 1.2)+
  geom_vline(xintercept = c(2008,2019), col = "red")+
  geom_text(aes(x = 2008), label = "2008 Economic Crisis", y = 9, angle = 90, vjust = -1, col = 'red')+
  geom_text(aes(x = 2019), label = "First Case of COVID-19", y = 9, angle = 90, vjust = -1, col = 'red')+
  geom_label_repel(data = subset(line_chart_income, year == "2020"), 
                   aes(label = country),
                   nudge_x = 1,
                   show.legend = FALSE)+
  # scale_x_continuous(breaks = scales::pretty_breaks(10))+
  xlim(2000, 2022)+
  ylim(-6, 12)+
  ggtitle("Annual GDP Growth Rate (%)")+
  xlab("Year")+
  ylab("GDP Growth Rate (%)")+
  theme_gray()+
  scale_color_OkabeIto()+
  theme(plot.title = element_text(hjust = 0.5))
  # guides(colour = guide_legend(override.aes = list(size = 10)))+
  # theme(legend.key=element_rect(fill=NA))





#Detailed Data Preparation
covid_period_chart<-WDI(country = "all",
                        indicator = c("NY.GDP.MKTP.KD.ZG",
                                      "NV.SRV.TOTL.ZS", 
                                      "NV.IND.TOTL.ZS", 
                                      "NV.AGR.TOTL.ZS",
                                      "NE.TRD.GNFS.ZS",
                                      "NY.GDP.PCAP.KN",
                                      "NY.GDP.MKTP.KN"),
                        start = 2020,
                        end = 2020,
                        extra = TRUE,
                        cache = new_wdi_cache)

health_expenditure<-WDI(country = "all",
                        indicator = c("SH.XPD.CHEX.GD.ZS"),
                        start = 2018,
                        end = 2018,
                        extra = FALSE,
                        cache = new_wdi_cache)

covid_period_chart<-filter(covid_period_chart, region != "Aggregates")

names(health_expenditure)[3]<-paste("health_expenditure")
health_expenditure<-subset(health_expenditure, select = -c(year, country))

names(covid_period_chart)[4]<-paste("GDP_growth_annual")
names(covid_period_chart)[5]<-paste("service_sector")
names(covid_period_chart)[6]<-paste("industry_sector")
names(covid_period_chart)[7]<-paste("agriculture_sector")
names(covid_period_chart)[8]<-paste("trade")
names(covid_period_chart)[9]<-paste("GDP_per_capita")
names(covid_period_chart)[10]<-paste("GDP")
names(covid_period_chart)[11]<-paste("iso_code")
names(covid_period_chart)[12]<-paste("region")

covid_period_chart<-merge(covid_period_chart, health_expenditure, by = "iso2c")

covid_period_chart<-covid_period_chart %>% relocate(health_expenditure, .before = iso_code)
#COVID19 Data Loading
covidData<-read.csv("covidData.csv")
names(covidData)[1]<-paste("iso_code")
covidDF<-data.frame(covidData[,(colnames(covidData) %in% c('iso_code',
                                                           'date',
                                                           'total_cases_per_million',
                                                           'total_deaths_per_million',
                                                           'total_vaccinated_per_hundred',
                                                           'life_expectancy',
                                                           'human_development_index',
                                                           'population_density',
                                                           'hospital_beds_per_thousand',
                                                           'total_vaccinations_per_hundred'))])







covidDF<-filter(covidDF, date=="2020-12-31")
covidDF<-merge(covidDF, covid_period_chart,by = "iso_code")


covidDF<-mutate_at(covidDF, c("total_vaccinations_per_hundred"), ~replace(., is.na(.),0.0))

covidDF<-subset(covidDF, select = -c(year))
covidDF<-filter(covidDF, income != "Not classified")

Q<-quantile(covidDF$GDP_growth_annual, probs = c(.25, .75), na.rm = TRUE)
covidDF<-na.omit(covidDF)
iqr<-IQR(covidDF$GDP_growth_annual)
up<-Q[2]+1.5*iqr #upper interquartile 
low<-Q[2]-1.5*iqr #lower interquartile

covidDF<-subset(covidDF, 
                           covidDF$GDP_growth_annual>low &
                             covidDF$GDP_growth_annual<up)

covidDF$income<-factor(covidDF$income, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))

covidDF<-covidDF %>% relocate(GDP_growth_annual, .before = total_cases_per_million)
covidDF<-covidDF %>% relocate(health_expenditure, .before = service_sector)

summ_covidDF<-covidDF %>% 
  group_by(income) %>%
  summarise(median_gdp = median(GDP_growth_annual),
            median_service = median(service_sector),
            iqrGDP = IQR(GDP_growth_annual),
            iqrServ = IQR(service_sector))

title<-"Heatmap Correlation Matrix"
cor_data1<-select_if(covidDF, is.numeric)
cor_data1<-na.omit(cor_data1)
cor = cor(cor_data1)
colnames(cor)<-c("GDP Growth Rate", 
                 "Total Cases", 
                 "Total Deaths", 
                 "Total Vaccinations", 
                 "Population Density", 
                 "Hospital Beds",
                 "Life Expectancy",
                 "HDI",
                 "Health Expenditure",
                 "Service Sector",
                 "Industry Sector",
                 "Agriculture Sector",
                 "Trade",
                 "GDP per Capita",
                 "GDP")
rownames(cor)<-c("GDP Growth Rate", 
                 "Total Cases", 
                 "Total Deaths", 
                 "Total Vaccinations", 
                 "Population Density", 
                 "Hospital Beds",
                 "Life Expectancy",
                 "HDI",
                 "Health Expenditure",
                 "Service Sector",
                 "Industry Sector",
                 "Agriculture Sector",
                 "Trade",
                 "GDP per Capita",
                 "GDP")
corrplot(cor, method = 'color', diag = FALSE, type = "upper", addCoef.col = "black", title = title, mar=c(0,0,1,0))

colnames(cor)<-c("GDP_Growth_Rate", 
                 "Total Cases", 
                 "Total Deaths", 
                 "Total Vaccinations", 
                 "Population Density", 
                 "Hospital Beds",
                 "Life Expectancy",
                 "HDI",
                 "Health Expenditure",
                 "Service Sector",
                 "Industry Sector",
                 "Agriculture Sector",
                 "Trade",
                 "GDP per Capita",
                 "GDP")
cor_df = data.frame(cor)
cor_df$var<-row.names(cor_df)
cor_df$abs_gdp<-abs(cor_df$GDP_Growth_Rate)
cor_df$Correlation_Value<-c("P", "N", "N", "P", "N", "N", "N", "N", "N", "N", "P", "P", "N", "P", "P")
cor_df<-cor_df[-1,]
cor_df$Correlation_Value<-c("Negative", "Negative", "Positive", "Negative", "Negative", "Negative", "Negative", "Negative", "Negative", "Positive", "Positive", "Negative", "Positive", "Positive")


ggplot(cor_df)+
  geom_bar_pattern(aes(y = reorder(var, abs_gdp),x = abs_gdp, fill = Correlation_Value, pattern = Correlation_Value), stat = "identity")+
  ggtitle("Absolute Correlation Value with GDP Growth Rate")+
  xlab("Absolute Correlation Value")+
  ylab("Variables")+
  geom_text(aes(abs_gdp, var, label = round(abs_gdp,2)), hjust = -0.3)+
  scale_fill_OkabeIto()+
  guides(guide_legend("Correlation Value"))+
  theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(face = "bold"))

ggplot(covidDF,aes(GDP_growth_annual, service_sector))+
  geom_point(colour = "black", size = 4.5, aes(shape = income))+
  geom_point(aes(col=income, shape = income), size = 3)+
  scale_shape_manual(values=c(15,16,17,18))+
  geom_smooth()+
  ggtitle("Relationship of GDP Growth Rate 2020 and Service Sector")+
  xlab("GDP Growth Rate 2020")+
  ylab("Service Sector (% of GDP)")+
  scale_colour_OkabeIto()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(covidDF,aes(GDP_growth_annual, agriculture_sector))+
  geom_point(aes(col=income), size = 3)+
  geom_smooth()+
  ggtitle("Relationship of GDP Growth Rate 2020 and Service Sector")+
  xlab("GDP Growth Rate 2020")+
  ylab("Agriculture Sector (% of GDP)")+
  labs(col = "Income")

ggplot(covidDF,aes(GDP_growth_annual, health_expenditure))+
  geom_point(aes(col=income, shape = income), size = 3)+
  geom_smooth()+
  ggtitle("Relationship of GDP Growth Rate 2020 and Health Expenditure")+
  xlab("GDP Growth Rate 2020")+
  ylab("Health Expenditure (% of GDP)")+
  labs(col = "Income")

ggplot(covidDF,aes(income))+
  geom_bar(aes(fill=income))+
  ggtitle("Countries Classification")+
  xlab("Income")+
  ylab("Count")

ggplot(covidDF, aes(income, GDP_growth_annual))+
  geom_boxplot()+
  ggtitle("Income vs GDP Growth Rate 2020")+
  xlab("Income")+ylab("GDP Growth Rate 2020 (%)")

ggplot(covidDF, aes(income, service_sector, fill = income))+
  geom_violin()+
  geom_boxplot(width = 0.1, fill = 'white')+
  geom_text(data = summ_covidDF, aes(x = income, y = median_service, label = round(median_service, digits = 2)), vjust = -1.5)+
  geom_point(data = summ_covidDF, aes(x = income, y = median_service, col = income), size = 5)+
  ggtitle("Cluster vs Servie Sector")+
  xlab("Income")+ylab("Service Sector % of GDP")+
  scale_fill_OkabeIto()+
  scale_colour_OkabeIto()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(covidDF, aes(income, GDP_growth_annual, fill = income))+
  geom_violin()+
  geom_boxplot(width = 0.1, fill = 'white')+
  geom_text(data = summ_covidDF, aes(x = income, y = median_gdp, label = round(median_gdp, digits = 2)), vjust = -1.5, size = 4)+
  geom_point(data = summ_covidDF, aes(x = income, y = median_gdp, col = income), size = 5)+
  ggtitle("Cluster vs GDP Growth Rate 2020")+
  xlab("Income")+ylab("GDP Growth Rate 2020")+
  scale_fill_OkabeIto()+
  scale_colour_OkabeIto()+
  theme(plot.title = element_text(hjust = 0.5))
  

ggplot(covidDF, aes(income, health_expenditure))+
  geom_boxplot()+
  ggtitle("Income vs Health Expenditure")+
  xlab("Income")+ylab("Health Expenditure (% of GDP)")

ggplot(covidDF,aes(GDP_growth_annual, service))+
  geom_point(aes(col=income), size = 3)+
  geom_smooth()+
  ggtitle("Relationship of GDP Growth Rate 2020 and Service Sector")+
  xlab("GDP Growth Rate 2020")+
  ylab("Service Sector (% of GDP)")+
  labs(col = "Income")

ggplot(covidDF,aes(GDP_growth_annual, agriculture))+
  geom_point(aes(col=income), size = 3)+
  geom_smooth()+
  ggtitle("Relationship of GDP Growth Rate 2020 and Service Sector")+
  xlab("GDP Growth Rate 2020")+
  ylab("Agriculture Sector (% of GDP)")+
  labs(col = "Income")

