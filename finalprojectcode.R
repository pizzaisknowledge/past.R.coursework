#read in csv
data1 <- read.csv(file.choose("C:\\Users\\chris\\Documents\\Fall_2019\\DA460\\edited_movie_data.csv"))
#print first row and column names
data1[1,]

#remove outliers
exclude.outliers <- subset(data1, Title != "Krrish 3" & Title !="Eliza on the Ice" & Title != "Devilman")
data1[3,]
data1[29,]
data1[36,]
exclude.outliers2 <- subset(exclude.outliers, Title != "Avengers: Endgame" & Title != "Deadpool" & Title != "The Lone Ranger")
exclude.outliers[11,]
exclude.outliers[21,]
exclude.outliers[31,]

#exclude.outliers3 <- subset(exclude.outliers2, Title !="Glass" & Title !="Watchmen" & Title != "Batman Begins")
#running the model with this 3rd set of outliers did not affect R-square enough to justify dropping them
#so final model will use exclude.outliers2
#after accepting model4, delete more outliers
exclude.outliers2[290,]
exclude.outliers2[66,]
exclude.outliers2[123,]
exclude.outliers4 <- subset(exclude.outliers2, Title != "Special" & Title != "Wanted" & Title != "Teenage Mutant Ninja Turtles: Out of the Shadows")
#try another round of outliers
exclude.outliers4[16,]
exclude.outliers4[219,]
exclude.outliers4[278,]
exclude.outliers5 <- subset(exclude.outliers4, Title != "Deadpool 2" & Title != "The Toxic Avenger" & Title != "Super Capers: The Origins of Ed and the Missing Bullion")
#create dummy variable for release month
DV_month <- as.factor(exclude.outliers2$Month)
#run first model predicting opening weekend with budget and release month
model_data <- lm(exclude.outliers2$Opening.Weekend ~ exclude.outliers2$Budget + DV_month)
model2_data <-lm(exclude.outliers2$Opening.Weekend ~ exclude.outliers2$Budget)
#print summary statistics
summary(model_data)
summary(model2_data)
#plot residuals
par(mfrow=c(1,1))
plot(model_data)

#try a logarithmic model
#model4 has highest adj R squared
exclude.outliers5$LnOpening.Weekend <- log(exclude.outliers5$Opening.Weekend)
exclude.outliers5$LnBudget <- log(exclude.outliers5$Budget)
model3_data <- lm(exclude.outliers2$LnOpening.Weekend ~ exclude.outliers2$LnBudget)
model4_data <- lm(exclude.outliers5$LnOpening.Weekend ~ exclude.outliers5$LnBudget +DV_month)
summary(model3_data)
summary(model4_data)
plot(model3_data)
plot(model4_data)

#try adding exponential variables
#reduces adj R squared
model5_data <- lm(exclude.outliers2$LnOpening.Weekend ~ exclude.outliers2$Budget + I(exclude.outliers2$Budget^2) + I(exclude.outliers2$Budget^3))
summary(model5_data)
plot(model5_data)

#First Visualization: Regression
#color code months on scatter plot, add regression line
#need to add month legend and adjust y-axis for scale
month = cut(exclude.outliers5$Month, breaks = 12)
cols = rainbow(12)[as.numeric(month)]
plot(exclude.outliers2$Opening.Weekend ~ exclude.outliers2$Budget, 
     main = "A Linear Model for Opening Weekend using Budget with Release Month", 
     cex.main = 0.9,
     col=cols,
     axes=F,
     las =1, pch = 20,
     xlab = "Budget",
     ylab = "Opening Weekend")
axis(1, at=c(0,100000000,200000000,300000000,400000000), labels = c(0,"100M","200M","300M","400M"))
axis(2, at=c(0,100000000,200000000,300000000,400000000), labels = c(0,"100M","200M","300M","400M"))
abline(model_data)
#legend(1, 1, legend, col=cols, title = "Month")

#Second Visualization: Barplot
#compare average amount for opening weekend by month
#would like to improve the y-axis labeling and reorder data
plot.new()
barplot.data <-aggregate(Opening.Weekend ~ Month, data = exclude.outliers2, mean, na.rm = TRUE)
as.data.frame(barplot.data)

barplot(barplot.data$Opening.Weekend,
        las=1, border=NA, xlab="$", ylab = "Month", horiz = TRUE,
        main = "April and October Boast Highest Opening Weekend Averages", col.main= "royalblue", axes = F, 
        col = c("gray","gray","gray","royalblue","gray","gray","gray","gray","gray","royalblue","gray","gray"))
axis(2, at=c(1,2,3,4,5,6,7,8,9,10,11,12),tick = 0, labels = c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug", "Sept", "Oct","Nov","Dec"), las = 1)
axis(1, at=c(0,30000000,60000000,90000000,120000000), labels = c("0","30M","60M","90M","120M"))

#Third Visualization: Histogram
#histogram of opening weekend

hist(exclude.outliers2$Opening.Weekend, 
     xlab = "Opening Weekend Revenue", ylab = "Number of Movies", 
     main = "How many superhero movies bring in less than $100M in opening weekend?", 
     cex.main = 1, col.main = "royalblue", axes = F,
     col = c("royalblue", "royalblue", "gray","gray","gray","gray","gray","gray"))
axis(1, at=c(0,100000000,200000000,300000000,400000000), labels = c("0","100M","200M","300M","400M"))
axis(2, at=c(0, 20, 40, 60, 80, 100, 120))

#Fourth Visualization: Boxplot
#compare Opening Weekend by MPAArating buckets

boxplot(Opening.Weekend ~ MPAA, data = exclude.outliers2, subset = (MPAA =="G"| MPAA == "PG"| MPAA == "PG-13"| MPAA == "R"),
        drop = TRUE, axes=F, 
        xlab = "Movie Rating", ylab = "Opening Weekend Revenue",
        main = "PG-13 Movies Yield Higher Opening Weekend Revenues", col.main= "royalblue",
        col = c("gray","gray","royalblue","gray"))
axis(1, at=c(1,2,3,4), labels = c("G", "PG", "PG-13", "R"), tick = 0)
axis(2, at=c(0,100000000,200000000,300000000,400000000), labels = c("0", "100M", "200M","300M","400M"))

#Fifth Visualization: Histogram of Budget and BudgetLn
hist(exclude.outliers2$Budget, axes = F)
axis(1, at=c(0, 100000000, 200000000,300000000,400000000,500000000,600000000), labels = c("0","100M","200M","300M","400M","500M","600M"))
axis(2, at=c(0, 50, 100, 150, 200, 250, 300))
hist(exclude.outliers2$LnBudget)
