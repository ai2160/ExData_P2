
#Part 1
#From the line graph PM2.5 have decreased gradually in USA
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

PMbyYear <- tapply(NEI$Emissions, NEI$year, sum)

PMbyYear <- data.frame(PMbyYear)
PMbyYear$Year <- rownames(PMbyYear)
colnames(PMbyYear) <- c('PM2.5', 'Year')

png('1.png')
with(PMbyYear, plot(Year, PM2.5, type = 'n'))
with(PMbyYear, lines(Year, PM2.5, col = 'blue'))
title(main = 'PM 2.5 Emissions over years in USA')
dev.off()

#Part 2
#From the line graph PM 2.5 in Baltimore started decreasing initially
#then shot up in 2005 but eventually ended up way below 1999 levels
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

BTMR <- subset(NEI, fips == '24510')

BTMRPMbyYear <- tapply(BTMR$Emissions, BTMR$year, sum)

BTMRPMbyYear <- data.frame(BTMRPMbyYear)
BTMRPMbyYear$Year <- rownames(BTMRPMbyYear)
colnames(BTMRPMbyYear) <- c('PM2.5', 'Year')

png('2.png')
with(BTMRPMbyYear, plot(Year, PM2.5, type = 'n', ylab = 'Baltimore PM2.5'))
with(BTMRPMbyYear, lines(Year, PM2.5, col = 'blue'))
title(main = 'Baltimore PM 2.5 Emissions over years')
dev.off()

#Part 3
#From the graph all of the source types gradually decreased over time except
#point source type which increased from 1999 to 2005 and sharply decrased in 2008
#with emissions in 2008 close to 1999 levels despite the drastic increase upto 2005
library(plyr)
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

BTMR <- subset(NEI, fips == '24510')

GroupedSum <- ddply(BTMR, .(year, type), summarize, GroupedSum = sum(Emissions))

plot <- ggplot(GroupedSum, aes(x=year, y=GroupedSum, col=type))

png('3.png')
plot+geom_line()+labs(y='PM2.5 Emissions', title='Baltimore PM2.5 Emissions by Source')
dev.off()

#Part 4
#I have defined Emissions from Coal fuel combustion as emissions that have both
#words 'fuel comb' AND 'coal' in EI.Sector field in SCC
#Emissions for Coal Fuel combustions stayed almost flat from 1999 to 2005 and have
#drastically tanked from 2005 to 2008
library(plyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

unq <- unique(SCC$EI.Sector)

sources <- intersect(unq[grep('.*fuel comb.*', unq, ignore.case = T)], 
                     unq[grep('.*coal.*', unq, ignore.case = T)])

SCCids <- as.character(SCC[SCC$EI.Sector %in% sources,"SCC"])

data <- NEI[NEI$SCC %in% SCCids,]

CoalPMbyYear <- ddply(data, .(year), summarize, YearSum = sum(Emissions))

png('4.png')
with(CoalPMbyYear, plot(year, YearSum, type = 'n',
                        ylab = 'PM 2.5 related to Coal Fuel Combustion'))
with(CoalPMbyYear, lines(year, YearSum, col = 'blue'))
title(main = 'Coal fuel combution related emissions over years in USA')
dev.off()


#Part 5
#I have defined emissions from Vehicles as having 'Onroad' in Data.Category field of SCC
#There are several options for defining a 'Vehicle' and this is the assumption I made after
#looking at SCC table

library(plyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

BTMR <- subset(NEI, fips == '24510')

unq <- unique(SCC$Data.Category)

sources <- unq[grep('Onroad', unq, ignore.case = F)]

SCCids <- as.character(SCC[SCC$Data.Category %in% sources,"SCC"])

data <- BTMR[BTMR$SCC %in% SCCids,]

VehiclePMbyYear <- ddply(data, .(year), summarize, YearSum = sum(Emissions))

png('5.png')
with(VehiclePMbyYear, plot(year, YearSum, type = 'n',
                        ylab = 'Vehicle Emission PM 2.5'))
with(VehiclePMbyYear, lines(year, YearSum, col = 'blue'))
title(main = 'Vehicle Emission PM 2.5 in Baltimore City')
dev.off()

#Part 6
#I have defined emissions from Vehicles as having 'Onroad' in Data.Category field of SCC
#There are several options for defining a 'Vehicle' and this is the assumption I made after
#looking at SCC table
#LA topped Baltimore by multiple folds. Surprise Surprise. Atleast it is trending down ward since 2005
#Baltimore has a greater change and the PM 2.5 have decreased more in Baltimore than they have increased
#in LA

library(plyr)
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

data <- NEI[NEI$fips %in% c('24510', '06037'),]
data$City <- NA
data$City[data$fips == '24510'] <- 'Baltimore City'
data$City[data$fips == '06037'] <- 'LA County'

unq <- unique(SCC$Data.Category)

sources <- unq[grep('Onroad', unq, ignore.case = F)]

SCCids <- as.character(SCC[SCC$Data.Category %in% sources,"SCC"])

clean_data <- data[data$SCC %in% SCCids,]

VehiclePMbyYear <- ddply(clean_data, .(year,City), summarize, YearSum = sum(Emissions))

plot <- ggplot(VehiclePMbyYear, aes(x=year, y=YearSum, col=City)) +
  labs(y='Vehicle PM2.5 Emissions', title='Vehicle PM2.5 Emissions over years')
  
plot <- plot + annotate("rect", xmin = 2004, xmax=2008, ymin=3600, ymax=3900, alpha = .2)
plot <- plot + annotate("rect", xmin = 2004, xmax=2008, ymin=300, ymax=600, alpha = .2)
plot <- plot + annotate("text", x = 2006, y = 3750, label = "Change = +170.2", col = 'cyan')
plot <- plot + annotate("text", x = 2006, y = 450, label = "Change = -258.5", col = 'red')

png('6.png')
plot+ geom_line()
dev.off()
