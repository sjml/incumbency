library(readxl)
library(hexbin)

color_all <- "#36C800"
color_dem <- "#0055C8"
color_rep <- "#C80000"

senate_data <- read_excel("data/processed/senate_data.xlsx")
governor_data <- read_excel("data/processed/governor_data.xlsx")
president_data <- read_excel("data/processed/president_data.xlsx")


# First we calculate total votes and the percentage of votes against the incumbent.
senate_data$`Total Votes` =  senate_data$`Votes For Incumbent` + senate_data$`Votes Against Incumbent`
senate_data$`Percent Against` = senate_data$`Votes Against Incumbent` / senate_data$`Total Votes` * 100

# Build a linear model of straight unemployment.
senate_unemployment <- data.frame(
  "AGAINST" = senate_data$`Percent Against`, 
  "UNEMP" = senate_data$`Unemployment Rate`,
  "INC" = senate_data$`Incumbent Party`
)
senate_unemployment <- na.omit(senate_unemployment)
senate_unemployment <- senate_unemployment[senate_unemployment$AGAINST > 0.0,]
senate_unemployment <- senate_unemployment[senate_unemployment$AGAINST < 100.0,]

senate_rep_unemp <- senate_unemployment[senate_unemployment$INC == "Republican",]
senate_dem_unemp <- senate_unemployment[senate_unemployment$INC == "Democratic",]

senateModel <- lm(AGAINST ~ UNEMP, data=senate_unemployment)
summary(senateModel)

senateModelR <- lm(AGAINST ~ UNEMP, data=senate_rep_unemp)
senateModelD <- lm(AGAINST ~ UNEMP, data=senate_dem_unemp)

# graph it
pdf(file="reports/images/senate_unemployment-against-all.pdf", width=5, height=4)
bins <- hexbin(senate_unemployment$UNEMP, senate_unemployment$AGAINST, xbins=100)
hexbinplot(AGAINST~UNEMP, senate_unemployment, 
                      xbins=100,
                      aspect=1,
                      xlim=range(senate_unemployment$UNEMP),
                      ylim=range(senate_unemployment$AGAINST),
                      main="Unemployment Effects on Votes Against Incumbents\n(Senate, All Candidates)", 
                      ylab="Votes Against Incumbent (Percentage)", 
                      xlab="Unemployment Rate (Percentage)",
                      colorkey = FALSE,
                      panel = function(x, y, ...) {
                         panel.hexbinplot(x, y, ...)
                         lattice::panel.abline(senateModel, col=color_all, lwd=3)
                         lattice::panel.abline(senateModelR, col=color_rep, lwd=3)
                         lattice::panel.abline(senateModelD, col=color_dem, lwd=3)
                      }
            )
dev.off()


# But let's look at it when considering the change in unemployment over time, looking up to 10 years in the past.
deltaList = list()
for (i in 1:10) {
  colname <- paste("Unemployment Delta_", i, sep="")
  deltaModel <- lm(`Percent Against` ~ get(colname), data=senate_data)
  coeffs <- summary(deltaModel)[["coefficients"]]
  deltaList[[i]] <- c(
    i,
    coeffs[,"Estimate"][-1][1],
    coeffs[,"Pr(>|t|)"][-1][1]
  )
}
deltaFrame <- do.call("rbind", deltaList)
colnames(deltaFrame) <- c("Unemployment Interval", "Coefficient Estimate", "Pr(>|t|)")

coeffs <- summary(deltaModel)[["coefficients"]]
estimates <- deltaFrame[,"Coefficient Estimate"]
pvals <- deltaFrame[,"Pr(>|t|)"]

pdf(file="reports/images/senate_regression-trends-all.pdf", width=5, height=4)
bp <- barplot(estimates, 
              col="#3F97D0", 
              xaxt='n', 
              main="Unemployment Trends vs Anti-incumbency\n(Senate, All)", 
              xlab="Unemployment Interval (Years)", 
              ylab="Regression Coefficient",
      )
text(bp, par("usr")[3], labels = deltaFrame[, 1], adj = c(0.5,1.0), xpd = TRUE, cex=1.0)
dev.off()
