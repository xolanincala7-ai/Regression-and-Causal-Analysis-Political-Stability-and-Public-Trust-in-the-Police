knitr::opts_chunk$set(echo = FALSE)

IIAG.PTPdata <- read.csv("iiag_cleaned_trust_police.csv")

# subset PTP data from 2022

IIAG.PTP.22 <- IIAG.PTPdata[IIAG.PTPdata$Year == 2022,]

# now i am adding the country code variable for easy merge
library("countrycode")
IIAG.PTP.22$Code <- countrycode(IIAG.PTP.22$Country, origin = "country.name", destination = "iso3c")

#Now that i have my dependent variable i want to make sure now that i have the independent. 

library(readxl)

WGI.PVdata <- read_excel("WGI.PV.xlsx", sheet = 1)

WGI.PVdt <- WGI.PVdata[,c("Series Code", "Country Name", "Country Code", "2022 [YR2022]")]
colnames(WGI.PVdt)[colnames(WGI.PVdt) == "Series Code"] <- "Year"
colnames(WGI.PVdt)[colnames(WGI.PVdt) == "2022 [YR2022]"] <- "PV.est"

WGI.PVdt$Year <- 2022

WGI.PVdt <- WGI.PVdt[complete.cases(WGI.PVdt), ]

# Remove columns that contain any NA and then make the estimate 3 decimal places



WGI.PVdt$PV.est[WGI.PVdt$PV.est == ".."] <- NA
WGI.PVdt$PV.est <- as.numeric(as.character(WGI.PVdt$PV.est))
WGI.PVdt <- WGI.PVdt[!is.na(WGI.PVdt$PV.est), ]
WGI.PVdt$PV.est <- round(WGI.PVdt$PV.est, 3)

# I want to merge the wgi,PVdt with the IIAG.PTP.22 

Merged.ptp.pv <- merge(IIAG.PTP.22, WGI.PVdt, by.x = "Code", by.y = "Country Code")

# Now i am going to add the data that has the rule of law control

WGI.RLdata <- read_excel("WGI.RLdata.xlsx")

WGI.RLdt <- WGI.RLdata[,c("Series Code", "Country Name", "Country Code", "2022 [YR2022]")]
colnames(WGI.RLdt)[colnames(WGI.RLdt) == "Series Code"] <- "Year"
colnames(WGI.RLdt)[colnames(WGI.RLdt) == "2022 [YR2022]"] <- "RL.est"

WGI.RLdt$Year <- 2022

WGI.RLdt <- WGI.RLdt[complete.cases(WGI.RLdt), ]

#Remove all columns with NA and round estimate to three decimal.


WGI.RLdt$RL.est[WGI.RLdt$RL.est == ".."] <- NA
WGI.RLdt$RL.est <- as.numeric(as.character(WGI.RLdt$RL.est))
WGI.RLdt <- WGI.RLdt[!is.na(WGI.RLdt$RL.est), ]
WGI.RLdt$RL.est <- round(WGI.RLdt$RL.est, 3)

# I want to merge the RL variable to the data 
Merge2.rl <- merge(Merged.ptp.pv, WGI.RLdt, by.x = "Code", by.y = "Country Code")

# Now I am Going to add the data that has gpd per capita

WGI.GDPdata <- read_excel("WGI.GDPdta.xlsx")

WGI.GDPdt <- WGI.GDPdata[,c("Series Code", "Country Name", "Country Code", "2022 [YR2022]")]
colnames(WGI.GDPdt)[colnames(WGI.GDPdt) == "Series Code"] <- "Year"
colnames(WGI.GDPdt)[colnames(WGI.GDPdt) == "2022 [YR2022]"] <- "GDP.est"

WGI.GDPdt$Year <- 2022

WGI.GDPdt <- WGI.GDPdt[complete.cases(WGI.GDPdt), ]

# Now I remove all columns with NA 


WGI.GDPdt$GDP.est[WGI.GDPdt$GDP.est == ".."] <- NA
WGI.GDPdt$GDP.est <- as.numeric(as.character(WGI.GDPdt$GDP.est))
WGI.GDPdt <- WGI.GDPdt[!is.na(WGI.GDPdt$GDP.est), ]


# now we do the final merge 
FMerge.rl <- merge(Merge2.rl, WGI.GDPdt, by.x = "Code", by.y = "Country Code")

# After the final merge we remove extra columns 

colums.to.remove <- c(
  "Year.y",
  "Country Name.x",
  "Year.x.1",
  "Country Name.y",
  "Year.y.1",
  "Country Name"
)

Report3.data <- FMerge.rl[ , !(names(FMerge.rl) %in% colums.to.remove)]

Report3.Data <- Report3.data[,!(names(Report3.data) %in% c("Year.x.1")) ]

colnames(Report3.Data)[colnames(Report3.Data) == "Year.x"] <- "Year"
colnames(Report3.Data)[colnames(Report3.Data) == "Public.Trust.in.Police"] <- "PTP.est"
colnames(Report3.Data)[colnames(Report3.Data) == "PV.est"] <- "PS.est"
colnames(Report3.Data)[colnames(Report3.Data) == "GDP.est"] <- "GDP.est ($)"

Report3.Data <- Report3.Data[, c("Code", "Country", "Year", "PS.est", "PTP.est", "RL.est", "GDP.est ($)")]
#Table 1 Publication.
library(knitr)
kable(head(Report3.Data), caption = "Table 1: Preview of key varibales and controls", align = 'c')




library(ggplot2)

ggplot(Report3.Data, aes(x = Report3.Data$PS.est)) + 
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "skyblue",
                 color = "black",
                 alpha = 0.7)+
  geom_density(color = "red", size = 1.2) + 
  labs (
    title = "Figure 1: Distribution of PS.est",
    x = "Political Stability and Absence of Violence.est",
    y = "Density")+ 
  theme_minimal()
  


ggplot(Report3.Data, aes(x = Report3.Data$PTP.est)) + 
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "skyblue",
                 color = "black",
                 alpha = 0.7)+
  geom_density(color = "red", size = 1.2) + labs (title = "Figure 2: Distribution of PTP.est",
                                                  x = "Public Trust in Police.est",
                                                  y = "Density")+ theme_minimal()


ggplot(Report3.Data, aes(x = Report3.Data$RL.est)) + 
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "skyblue",
                 color = "black",
                 alpha = 0.7)+
  geom_density(color = "red", size = 1.2) + labs (title = "Figure 3: Distribution of RL.est",
                                                  x = "Rule of Law.est",
                                                  y = "Density")+ theme_minimal()



ggplot(Report3.Data, aes(x = Report3.Data$GDP.est)) + 
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "skyblue",
                 color = "black",
                 alpha = 0.7)+
  geom_density(color = "red", size = 1.2) + labs (title = "Figure 4: Distribution of GDP.est",
                                                  x = "Gross Domestic Product per person.est ($)",
                                                  y = "Density")+ theme_minimal()

ggplot(Report3.Data, aes(x = log(Report3.Data$GDP.est))) + 
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "skyblue",
                 color = "black",
                 alpha = 0.7)+
  geom_density(color = "red", size = 1.2) + labs (title = "Figure 5: Distribution of Log GDP.est",
                                                  x = "Gross Domestic Product per person.est ($)",
                                                  y = "Density")+ theme_minimal()

Report3.Data$LogGDP.est <- log(Report3.Data$`GDP.est ($)`)



kable(head(Report3.Data), caption = "Table 2: Preview of key varibales and controls + LogGDP", align = 'c')

The.Data.Append <- Report3.Data

Report3.Data <- Report3.Data[, !(names(Report3.Data) %in% "GDP.est ($)")]


# Correlation Matrix to check for Multicollinearity 
Cor.Data <- Report3.Data[, c("PS.est", "PTP.est", "RL.est", "LogGDP.est")]

Cor.Matrix <- round(cor(Cor.Data, use = "complete.obs"), 3)



library(knitr)

kable(Cor.Matrix, caption = "Table 3: Correlation matrix of the Variables and Controls")


Model.XY <- lm(PTP.est ~ PS.est, data = Report3.Data)

Model.XYc1 <- lm(PTP.est ~ PS.est + RL.est, data = Report3.Data)

Model.XYc2 <- lm(PTP.est ~ PS.est + LogGDP.est, data = Report3.Data)

Model.XYc1c2 <- lm(PTP.est ~ PS.est + RL.est + LogGDP.est, data = Report3.Data)

# Publication of Regression results on stargazer 
library(modelsummary)


models <- list(
  "Model(X)(Y)" = Model.XY,
  "Model(X)(Y)(c1)" = Model.XYc1,
  "Model(X)(Y)(c2)" = Model.XYc2,
  "Model(X)(Y)(c1)(c2)" = Model.XYc1c2
)


coef_names <- c(
  "PS.est"      = "Political Stability",
  "RL.est"      = "Rule of Law",
  "LogGDP.est"  = "Log GDP"
)

# Generate the HTML table
modelsummary(models,
  coef_map = coef_names,
  statistic = "std.error",  
  stars = c('*' = .05, '**' = .01, '***' = .001),
  gof_omit = "IC|Log.Lik",
  output = "html"
)


par(mfrow = c(2,2))
plot(Model.XYc1c2)



kable((The.Data.Append), caption = "Data Table: The Key varibales and controls + LogGDP", align = 'c')

