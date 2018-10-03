#Read in data files

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#There are considerably different numbers of measurements between these years.
#It is therefore not reasonable to simply sum all the measurement locations. 
#We will sum only the SCC values that are common to all 4 years, and use that to
#plot the relative accumulated emissions. 

#Create subsets by year
NEI99 = NEI[NEI$year == 1999,]
NEI02 = NEI[NEI$year == 2002,]
NEI05 = NEI[NEI$year == 2005,]
NEI08 = NEI[NEI$year == 2008,]

#Determine which measurement locations (SCC values) were common to all years
commonSCC <- Reduce(intersect, list(NEI99$SCC, NEI02$SCC, NEI05$SCC, NEI08$SCC)) 

#There are no NAs in the emission column, so we can simply sum up the totals by year:
totalemissions <- tapply(NEI$Emissions[NEI$SCC %in% commonSCC], NEI$year[NEI$SCC %in% commonSCC], sum)

#Create png device
png("plot1.png", width=480, height=480)

#Plot total emissions by year
plot(names(totalemissions), totalemissions, ylim = c(0, 6500000), xlab = "Year", ylab = "Relative Total Emissions (PM25)", xlim = c(1998,2008), main = "Relative Total Emissions by Year (All US)")

#Close plot device
dev.off()
