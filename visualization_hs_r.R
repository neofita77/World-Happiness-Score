##specification of a working directory
setwd('/Users/michalprzybysz/Desktop/SGH-Akademia Analityka z SAS, R & Python/Zaliczenie studiów/world-happiness-report')
getwd()

##instaling or adding new packages
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("PBImisc")
library(PBImisc)
install.packages("lattice")
library(lattice)
library(reshape)
library(reshape2)
library(scales)
install.packages("extrafont")
library(extrafont)
font_import()
y #yes
library(tidyr)
library(grDevices)
install.packages("graphics")
library(graphics)
install.packages("ellipse")
library(ellipse)
install.packages("corrgram")
library(corrgram)
install.packages("corrplot")
library(corrplot)
library(RColorBrewer)
install.packages("rworldmap")
library(rworldmap)
library(knitr)
library(data.table)


##import data
dat_2015 = read.csv("2015.csv", header = TRUE)
dat_2016 = read.csv("2016.csv", header = TRUE)
dat_2017 = read.csv("2017.csv", header = TRUE)

##adding region to data from 2017 year
##adding yer to data
dat_2015 <- mutate(dat_2015, Year = 2015)
dat_2016 <- mutate(dat_2016, Year = 2016)
dat_2017 <- mutate(dat_2017, Year = 2017)

##creating data frame with selecting only 'Country' and 'Region' from tables
dat_2015_new <- data.frame('Country' = dat_2015$Country, 
                           'Region' = dat_2015$Region)
dat_2016_new <- data.frame('Country' = dat_2016$Country, 
                           'Region' = dat_2016$Region)

##merging two data frames
dat_1516 <- merge.data.frame(dat_2015_new, dat_2016_new, by = 'Country',
                             all = TRUE)

##filling empty observations fom one column with another column
dat_1516$Region.x[is.na(dat_1516$Region.x)] <- 
  dat_1516$Region.y[is.na(dat_1516$Region.x)]

##finally dat for merging with dat_2017  
dat_1516 <- select(dat_1516, Country, 'Region' = Region.x)

##Region to data
dat_2017 <- merge(dat_2017, dat_1516, by = 'Country')

##all data together
dat_all <- rbind(select(dat_2015,
                        Country,Region,Happiness.Score,Economy..GDP.per.Capita.,Family,
                        Health..Life.Expectancy.,Freedom,Trust..Government.Corruption.,
                        Generosity,Dystopia.Residual), 
                 select(dat_2016,
                        Country,Region,Happiness.Score,Economy..GDP.per.Capita.,Family,
                        Health..Life.Expectancy.,Freedom,Trust..Government.Corruption.,
                        Generosity,Dystopia.Residual),
                 select(dat_2017,
                        Country,Region,Happiness.Score,Economy..GDP.per.Capita.,Family,
                        Health..Life.Expectancy.,Freedom,Trust..Government.Corruption.,
                        Generosity,Dystopia.Residual))

##average per region plus sorting and rounding
dat_avg_reg <- dat_all %>%
  group_by(Region) %>%
  summarise(score = mean(Happiness.Score),
            gdp = mean(Economy..GDP.per.Capita.),
            family = mean(Family),
            health = mean(Health..Life.Expectancy.),
            freedom = mean(Freedom),
            corruption = mean(Trust..Government.Corruption.),
            generosity = mean(Generosity),
            dystopia = mean(Dystopia.Residual)) %>%
  arrange(score) %>% mutate_if(is.numeric,round,3)

##average per country plus sorting and rounding
dat_avg_cou <- dat_all %>%
  group_by(Country) %>%
  summarise(score = mean(Happiness.Score),
            gdp = mean(Economy..GDP.per.Capita.),
            family = mean(Family),
            health = mean(Health..Life.Expectancy.),
            freedom = mean(Freedom),
            corruption = mean(Trust..Government.Corruption.),
            generosity = mean(Generosity),
            dystopia = mean(Dystopia.Residual)) %>%
  arrange(score) %>% mutate_if(is.numeric,round,3)

##data for bar plot 
##region
dat_bpr <- dat_avg_reg %>% arrange(desc(score)) %>% 
  mutate(observation = 1:n()) %>% 
  unite(Region, observation, Region, sep = '.') %>%
  unite(Region, Region, score, sep = '/')

dat_bp_tr <- melt(dat_bpr, category = c('Region'))
##country
dat_bpc <- dat_avg_cou %>% arrange(desc(score)) %>% 
  mutate(observation = 1:n()) %>% 
  unite(Country, observation, Country, sep = '.') %>%
  unite(Country, Country, score, sep = '/')

dat_bp_tc1 <- melt(dat_bpc[1:41, ], category = c('Country'))
dat_bp_tc2 <- melt(dat_bpc[42:83, ], category = c('Country'))
dat_bp_tc3 <- melt(dat_bpc[84:125,  ], category = c('Country'))
dat_bp_tc4 <- melt(dat_bpc[126:164, ], category = c('Country'))

##data for dynamics plot
##regions
dat_avg_reg16 <- dat_2016 %>%
  group_by(Region) %>%
  summarise(score = mean(Happiness.Score)) %>%
  arrange(score) %>% mutate_if(is.numeric,round,4)

dat_avg_reg17 <- dat_2017 %>%
  group_by(Region) %>%
  summarise(score = mean(Happiness.Score)) %>%
  arrange(score) %>% mutate_if(is.numeric,round,4)

dat_dyn_mrg <- merge(dat_avg_reg17, dat_avg_reg16, by = 'Region') %>%
  mutate(Happiness.Score.avg.dyn = score.x - score.y) %>%
  arrange(desc(Happiness.Score.avg.dyn)) %>% 
  mutate(observation = 1:n()) %>% 
  unite(Region, observation, Region, sep = '.')

data_dynr <- 
  data.frame ('Region' = dat_dyn_mrg$Region, 
              'Happiness.Score.avg.dyn' = 
                dat_dyn_mrg$Happiness.Score.avg.dyn) %>% 
  mutate_if(is.numeric,round,3)
##countries
dat_cou16 <- select(dat_2016, 'Country', 'Happiness.Score') %>%
  arrange(Happiness.Score) %>% mutate_if(is.numeric,round,4)

dat_cou17 <- select(dat_2017, 'Country', 'Happiness.Score') %>%
  arrange(Happiness.Score) %>% mutate_if(is.numeric,round,4)

dat_dyn_mcu <- merge(dat_cou17, dat_cou16, by = 'Country') %>%
  mutate(Happiness.Score.dyn = 
           Happiness.Score.x - Happiness.Score.y) %>%
  arrange(desc(Happiness.Score.dyn)) %>% 
  mutate(observation = 1:n()) %>% 
  unite(Country, observation, Country, sep = '.')

data_dync <- 
  data.frame ('Country' = dat_dyn_mcu$Country, 
              'Happiness.Score.dyn' = 
                dat_dyn_mcu$Happiness.Score.dyn) %>% 
  mutate_if(is.numeric,round,3)

data_dync1 = data_dync[1:38, ]
data_dync2 = data_dync[39:76, ]
data_dync3 = data_dync[77:115, ]
data_dync4 = data_dync[116:150, ]

##data for correlation matrix
kor <- cor(dat_avg_cou[
  c('score', 'gdp', 'family', 'health',
    'freedom', 'generosity', 'corruption', 'dystopia')], 
  method = 'spearman')
colnames(kor) <- c('1', '2', '3', '4', '5', '6', '7', '8')
row.names(kor) <- c('1.Happiness score', '2.GDP per capita',
                    '3.Social support', '4.Healthy life expectancy',
                    '5.Freedom to make life choices',
                    '6.Perceptions of corruption', '7.Generosity',
                    '8.Dystopia')

##data for maps
countries <- data.frame('country' = dat_avg_cou$Country, 
                        'value' = dat_avg_cou$score)
colnames(countries) <- c('country', 'value')
matched <- joinCountryData2Map(countries, joinCode="NAME", 
                               nameJoinColumn="country")
#160 codes from your data successfully matched countries in the map
#4 codes from your data failed to match with a country code in the map
#83 codes from the map weren't represented in your data


##bar chart for regions
png(filename = "barchart_regions.png", width = 920, height = 600)
chart1 <- ggplot(dat_bp_tr, aes(reorder(Region, value, sum), value, 
                                fill = variable)) +
  labs(x = "Region", y = "Score") +
  geom_bar(position = "stack", stat = "identity", width = 0.25) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,8)) +
  ggtitle("Average Ranking of Happiness 2015–2017-Regions") +
  coord_flip() + theme_grey() + scale_fill_brewer(palette = "Paired",       name = "Variable", labels = c("GDP per capita",
                                                                                                          "Social support",
                                                                                                          "Healthy life expectancy",
                                                                                                          "Freedom to make life choices",
                                                                                                          "Perceptions of corruption",
                                                                                                          "Generosity",
                                                                                                          "Dystopia (1.92) + residual")) +
  theme(text = element_text(size = 18, family = "Arial Rounded MT Bold",
                            color = "dodgerblue4")) +
  theme(legend.position = "right", legend.text = element_text(size = 10))
chart1
dev.off()

##bar charts for countries
png(filename = "barchart_country1.png", width = 920, height = 800)
chart2 <- ggplot(dat_bp_tc1, aes(reorder(Country, value, sum), value, 
                                 fill = variable)) +
  labs(x = "Country", y = "Score") +
  geom_bar(position = "stack", stat = "identity", width = 0.25) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,8)) +
  ggtitle("Average Ranking of Happiness 2015–2017-Country(Part1)") +
  coord_flip() + theme_grey() + scale_fill_brewer(palette = "Paired",       name = "Variable", labels = c("GDP per capita",
                                                                                                          "Social support",
                                                                                                          "Healthy life expectancy",
                                                                                                          "Freedom to make life choices",
                                                                                                          "Perceptions of corruption",
                                                                                                          "Generosity",
                                                                                                          "Dystopia (1.92) + residual")) +
  theme(text = element_text(size = 14, family = "Arial Rounded MT Bold",
                            color = "dodgerblue4")) +
  theme(legend.position = "right", legend.text = element_text(size = 10))
chart2
dev.off()

png(filename = "barchart_country2.png", width = 920, height = 800)
chart3 <- ggplot(dat_bp_tc2, aes(reorder(Country, value, sum), value, 
                                 fill = variable)) +
  labs(x = "Country", y = "Score") +
  geom_bar(position = "stack", stat = "identity", width = 0.25) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,8)) +
  ggtitle("Average Ranking of Happiness 2015–2017-Country(Part2)") +
  coord_flip() + theme_grey() + scale_fill_brewer(palette = "Paired",       name = "Variable", labels = c("GDP per capita",
                                                                                                          "Social support",
                                                                                                          "Healthy life expectancy",
                                                                                                          "Freedom to make life choices",
                                                                                                          "Perceptions of corruption",
                                                                                                          "Generosity",
                                                                                                          "Dystopia (1.92) + residual")) +
  theme(text = element_text(size = 14, family = "Arial Rounded MT Bold",
                            color = "dodgerblue4")) +
  theme(legend.position = "right", legend.text = element_text(size = 11))
chart3
dev.off()

png(filename = "barchart_country3.png", width = 920, height = 800)
chart4 <- ggplot(dat_bp_tc3, aes(reorder(Country, value, sum), value, 
                                 fill = variable)) +
  labs(x = "Country", y = "Score") +
  geom_bar(position = "stack", stat = "identity", width = 0.25) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,8)) +
  ggtitle("Average Ranking of Happiness 2015–2017-Country(Part3)") +
  coord_flip() + theme_grey() + scale_fill_brewer(palette = "Paired",       name = "Variable", labels = c("GDP per capita",
                                                                                                          "Social support",
                                                                                                          "Healthy life expectancy",
                                                                                                          "Freedom to make life choices",
                                                                                                          "Perceptions of corruption",
                                                                                                          "Generosity",
                                                                                                          "Dystopia (1.92) + residual")) +
  theme(text = element_text(size = 14, family = "Arial Rounded MT Bold",
                            color = "dodgerblue4")) +
  theme(legend.position = "right", legend.text = element_text(size = 11))
chart4
dev.off()

png(filename = "barchart_country4.png", width = 920, height = 800)
chart5 <- ggplot(dat_bp_tc4, aes(reorder(Country, value, sum), value, 
                                 fill = variable)) +
  labs(x = "Country", y = "Score") +
  geom_bar(position = "stack", stat = "identity", width = 0.25) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,8)) +
  ggtitle("Average Ranking of Happiness 2015–2017-Country(Part4)") +
  coord_flip() + theme_grey() + scale_fill_brewer(palette = "Paired",       name = "Variable", labels = c("GDP per capita",
                                                                                                          "Social support",
                                                                                                          "Healthy life expectancy",
                                                                                                          "Freedom to make life choices",
                                                                                                          "Perceptions of corruption",
                                                                                                          "Generosity",
                                                                                                          "Dystopia (1.92) + residual")) +
  theme(text = element_text(size = 14, family = "Arial Rounded MT Bold",
                            color = "dodgerblue4")) +
  theme(legend.position = "right", legend.text = element_text(size = 10))
chart5
dev.off()

##correlation matrix
png(filename = "correlation_matrix_lower.png", width = 700, height = 400)
corrplot(kor, method = "ellipse", type = "lower", 
         tl.col="dodgerblue4", tl.cex=1.6,
         col = brewer.pal(n = 9, name = "RdYlGn"))
dev.off()

png(filename = "correlation_matrix_upper.png", width = 700, height = 400)
corrplot(kor, method = "number", type = "upper",
         tl.col="dodgerblue4", tl.cex=1.6, 
         col = brewer.pal(n = 9, name = "RdYlGn"))
dev.off()

##histograms for all variables
png(filename = "histograms1.png", width = 920, height = 600)
par(mfrow=c(2,2))
hist(dat_avg_cou$score, 
     main="Happiness score (Average 2015-2017:Countries)",
     xlab="Happiness score",
     col.axis = "black",
     col.main = "dodgerblue4",
     col.lab = "dodgerblue4",
     border="white", 
     col="gray81",
     cex.main=1.5, 
     cex.lab=1.5,
     cex.axis = 1.2,
     las=1,
     breaks=15,
     prob = TRUE)
lines(density(dat_avg_cou$score), col = "orangered4")

hist(dat_avg_cou$gdp,
     main="GDP per capita (Average 2015-2017:Countries)",
     xlab="GDP per capita",
     col.axis = "black",
     col.main = "dodgerblue4",
     col.lab = "dodgerblue4",
     border="white", 
     col="gray81",
     cex.main=1.5, 
     cex.lab=1.5,
     cex.axis = 1.2,
     xlim=c(0.0,2),
     las=1,
     breaks=15,
     prob = TRUE)
lines(density(dat_avg_cou$gdp), col = "orangered4")

hist(dat_avg_cou$generosity,
     main="Generosity (Average 2015-2017:Countries)",
     xlab="Generosity",
     col.axis = "black",
     col.main = "dodgerblue4",
     col.lab = "dodgerblue4",
     border="white", 
     col="gray81",
     cex.main=1.5, 
     cex.lab=1.5,
     cex.axis = 1.2,
     las=1,
     breaks=15,
     prob = TRUE)
lines(density(dat_avg_cou$generosity), col = "orangered4")

hist(dat_avg_cou$health,
     main="Healthy life expectancy (Average 2015-2017:Countries)",
     xlab="Healthy life expectancy",
     col.axis = "black",
     col.main = "dodgerblue4",
     col.lab = "dodgerblue4",
     border="white", 
     col="gray81",
     cex.main=1.5, 
     cex.lab=1.5,
     cex.axis = 1.2,
     las=1,
     breaks=15,
     prob = TRUE)
lines(density(dat_avg_cou$health), col = "orangered4")
dev.off()

png(filename = "histograms2.png", width = 920, height = 600)
par(mfrow=c(2,2))
hist(dat_avg_cou$freedom,
     main="Freedom to make life choices (Average 2015-2017:Countries)",
     xlab="Freedom to make life choices",
     col.axis = "black",
     col.main = "dodgerblue4",
     col.lab = "dodgerblue4",
     border="white", 
     col="gray81",
     cex.main=1.5, 
     cex.lab=1.5,
     cex.axis = 1.2,
     xlim=c(0.0,0.7),
     las=1,
     breaks=15,
     prob = TRUE)
lines(density(dat_avg_cou$freedom), 
      col = "orangered4")

hist(dat_avg_cou$family,
     main="Social support (Average 2015-2017:Countries)",
     xlab="Social support",
     col.axis = "black",
     col.main = "dodgerblue4",
     col.lab = "dodgerblue4",
     border="white", 
     col="gray81",
     cex.main=1.5, 
     cex.lab=1.5,
     cex.axis = 1.2,
     xlim=c(0.0,1.4),
     las=1,
     breaks=15,
     prob = TRUE)
lines(density(dat_avg_cou$family), col = "orangered4")

hist(dat_avg_cou$corruption,
     main="Perceptions of corruption(Average 2015-2017:Countries)",
     xlab="Perceptions of corruption",
     col.axis = "black",
     col.main = "dodgerblue4",
     col.lab = "dodgerblue4",
     border="white", 
     col="gray81",
     cex.main=1.5, 
     cex.lab=1.5,
     cex.axis = 1.2,
     xlim=c(0.0,0.6),
     las=1,
     breaks=15,
     prob = TRUE)
lines(density(dat_avg_cou$corruption), col = "orangered4")

hist(dat_avg_cou$dystopia,
     main="Dystopia (1.92) + residual (Average 2015-2017:Countries)",
     xlab="Dystopia (1.92) + residual",
     col.axis = "black",
     col.main = "dodgerblue4",
     col.lab = "dodgerblue4",
     border="white", 
     col="gray81",
     cex.main=1.5, 
     cex.lab=1.5,
     cex.axis = 1.2,
     xlim=c(0.5,4),
     las=1,
     breaks=15,
     prob = TRUE)
lines(density(dat_avg_cou$dystopia), col = "orangered4")
dev.off()

##bar chart with dynamic of happiness score
##regions
png(filename = "barchart_dyn_regions.png", width = 920, height = 600)
chart5 <- ggplot(data_dynr, aes(reorder(Region, Happiness.Score.avg.dyn),
                                Happiness.Score.avg.dyn, 
                                fill = Happiness.Score.avg.dyn)) + 
  labs(x = "Region", y = "Score") + 
  geom_bar(stat = "identity", width = 0.25) + 
  scale_fill_gradient2(low = 'red', mid = 'snow3', high = 'green', 
                       space = 'Lab', name = "Dynamic-avg", 
                       labels = c("", -0.15, -0.1, 0, 0.1, 0.15, ""),
                       limits = c(-0.15, 0.15)) + 
  scale_y_continuous(breaks = c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15),
                     limits = c(-0.15, 0.15)) +
  theme_grey() + coord_flip() + 
  ggtitle("Average dynamic of Happiness Score (2016-2017)") + 
  theme(text = element_text(size = 15, family = "Arial Rounded MT Bold",
                            color = "dodgerblue4")) + 
  geom_text(aes(label = Happiness.Score.avg.dyn), vjust = 2, 
            color = "dodgerblue4", family = "Arial Rounded MT Bold", 
            size = 4)
chart5
dev.off()
##countries
png(filename = "barchart_dyn_countries1.png", width = 920, height = 600)
chart6 <- ggplot(data_dync1, aes(reorder(Country, Happiness.Score.dyn),
                                 Happiness.Score.dyn, 
                                 fill = Happiness.Score.dyn)) + 
  labs(x = "Country", y = "Score") + 
  geom_bar(stat = "identity", width = 0.25) + 
  scale_fill_gradient2(low = 'green', mid = 'green', high = 'green', 
                       space = 'Lab', name = "Dynamic", 
                       labels = c("", 0, 0.1, 0.3, 0.5, ""),
                       limits = c(0, 0.5)) + 
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
                     limits = c(0, 0.5)) +
  theme_grey() + coord_flip() + 
  ggtitle("Dynamic of Happiness Score (2016-2017)-Part1") + 
  theme(text = element_text(size = 15, family = "Arial Rounded MT Bold",
                            color = "dodgerblue4")) + 
  geom_text(aes(label = Happiness.Score.dyn), vjust = 0, 
            color = "dodgerblue4", family = "Arial Rounded MT Bold", 
            size = 3.5)
chart6
dev.off()

png(filename = "barchart_dyn_countries2.png", width = 920, height = 600)
chart7 <- ggplot(data_dync2, aes(reorder(Country, Happiness.Score.dyn),
                                 Happiness.Score.dyn, 
                                 fill = Happiness.Score.dyn)) + 
  labs(x = "Country", y = "Score") + 
  geom_bar(stat = "identity", width = 0.25) + 
  scale_fill_gradient2(low = 'snow3', mid = 'snow3', high = 'green', 
                       space = 'Lab', name = "Dynamic", 
                       labels = c("", 0, 0.05, 0.1, ""),
                       limits = c(0, 0.1)) + 
  scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1),
                     limits = c(0, 0.1)) +
  theme_grey() + coord_flip() + 
  ggtitle("Dynamic of Happiness Score (2016-2017)-Part2") + 
  theme(text = element_text(size = 15, family = "Arial Rounded MT Bold",
                            color = "dodgerblue4")) + 
  geom_text(aes(label = Happiness.Score.dyn), vjust = 0, 
            color = "dodgerblue4", family = "Arial Rounded MT Bold", 
            size = 3.5)
chart7
dev.off()

png(filename = "barchart_dyn_countries3.png", width = 920, height = 600)
chart8 <- ggplot(data_dync3, aes(reorder(Country, Happiness.Score.dyn),
                                 Happiness.Score.dyn, 
                                 fill = Happiness.Score.dyn)) + 
  labs(x = "Country", y = "Score") + 
  geom_bar(stat = "identity", width = 0.25) + 
  scale_fill_gradient2(low = 'red', mid = 'snow3', high = 'snow3', 
                       space = 'Lab', name = "Dynamic", 
                       labels = c("", -0.09, -0.05, 0, ""),
                       limits = c(-0.09, 0)) + 
  scale_y_continuous(breaks = c(-0.09, -0.075, -0.05, -0.025, 0),
                     limits = c(-0.09, 0)) +
  theme_grey() + coord_flip() + 
  ggtitle("Dynamic of Happiness Score (2016-2017)-Part3") + 
  theme(text = element_text(size = 15, family = "Arial Rounded MT Bold",
                            color = "dodgerblue4")) + 
  geom_text(aes(label = Happiness.Score.dyn), vjust = 0, 
            color = "dodgerblue4", family = "Arial Rounded MT Bold", 
            size = 3.5)
chart8
dev.off()

png(filename = "barchart_dyn_countries4.png", width = 920, height = 600)
chart9 <- ggplot(data_dync4, aes(reorder(Country, Happiness.Score.dyn),
                                 Happiness.Score.dyn, 
                                 fill = Happiness.Score.dyn)) + 
  labs(x = "Country", y = "Score") + 
  geom_bar(stat = "identity", width = 0.25) + 
  scale_fill_gradient2(low = 'red', mid = 'red', high = 'snow3', 
                       space = 'Lab', name = "Dynamic", 
                       labels = c("", -0.9, -0.5, -0.08, ""),
                       limits = c(-0.9, 0)) + 
  scale_y_continuous(breaks = c(-0.9, -0.7, -0.5, -0.3, -0.08),
                     limits = c(-0.9, 0)) +
  theme_grey() + coord_flip() + 
  ggtitle("Dynamic of Happiness Score (2016-2017)-Part4") + 
  theme(text = element_text(size = 15, family = "Arial Rounded MT Bold",
                            color = "dodgerblue4")) + 
  geom_text(aes(label = Happiness.Score.dyn), vjust = 1, 
            color = "dodgerblue4", family = "Arial Rounded MT Bold", 
            size = 3.5)
chart9
dev.off()

##visualization of map wit happiness score per country
#all world
png(filename = "all_world_map.png", width = 920, height = 600)
colourPalette <- brewer.pal(11, "RdYlBu")
finally_map_wr <- mapCountryData(matched, nameColumnToPlot="value", 
                                 mapTitle = "Happiness score all World (Average 2015-2017)",
                                 mapRegion = "World", catMethod = "pretty", 
                                 colourPalette = colourPalette, oceanCol = "white", 
                                 borderCol = "dodgerblue4", missingCountryCol = "gray81")
do.call(addMapLegend, c(finally_map_wr, legendLabels = "all"))
dev.off()

#europe
png(filename = "all_world_map1.png", width = 920, height = 600)
colourPalette <- brewer.pal(11, "RdYlBu")
finally_map_eu <- mapCountryData(matched, nameColumnToPlot="value", 
                                 mapTitle = "Happiness score in Europe (Average 2015-2017)",
                                 mapRegion = "Europe", catMethod = "pretty", 
                                 colourPalette = colourPalette, oceanCol = "white", 
                                 borderCol = "black", missingCountryCol = "gray81")
do.call(addMapLegend, c(finally_map_eu, legendLabels = "all"))
dev.off()