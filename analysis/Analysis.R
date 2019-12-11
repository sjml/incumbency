library(R.utils)
library(readxl)
library(hexbin)
library(jtools)

color_all <- "#36C800"
color_dem <- "#0055C8"
color_rep <- "#C80000"

senate_data <- read_excel("data/processed/senate_data.xlsx")
governor_data <- read_excel("data/processed/governor_data.xlsx")
president_data <- read_excel("data/processed/president_data.xlsx")


unemployment_regressions <- function(input_data, data_name) {
  # First we calculate total votes and the percentage of votes against the incumbent.
  input_data$`Total Votes` = input_data$`Votes For Incumbent` + input_data$`Votes Against Incumbent`
  input_data$`Percent Against` = input_data$`Votes Against Incumbent` / input_data$`Total Votes` * 100

  # Make data subsets
  unemployment <- data.frame(
    "AGAINST" = input_data$`Percent Against`, 
    "UNEMP" = input_data$`Unemployment Rate`,
    "INC" = input_data$`Incumbent Party`
  )
  unemployment <- na.omit(unemployment)
  unemployment <- unemployment[unemployment$AGAINST > 0.0,]
  unemployment <- unemployment[unemployment$AGAINST < 100.0,]
  
  rep_unemp <- unemployment[unemployment$INC == "Republican",]
  dem_unemp <- unemployment[unemployment$INC == "Democratic" | unemployment$INC == "Democrat",]
  
  # build the models
  modelAll <- lm(AGAINST ~ UNEMP, data=unemployment)
  modelR <- lm(AGAINST ~ UNEMP, data=rep_unemp)
  modelD <- lm(AGAINST ~ UNEMP, data=dem_unemp)
  
  # Output the datums in word format
  export_summs(modelAll, modelR, modelD,
               model.names = c("All", "Rep", "Dem"),
               to.file="docx", file.name=paste("reports/docs/", data_name, "_unemployment.docx", sep=""))
  
  # Output the heatmaps
  pdf(file=paste("reports/images/", data_name, "_unemployment-against-all-no-regression.pdf", sep=""), width=5, height=4)
  hbpA <- hexbinplot(AGAINST~UNEMP, unemployment, 
                     xbins=100,
                     aspect=1,
                     xlim=range(unemployment$UNEMP),
                     ylim=range(unemployment$AGAINST),
                     main=paste("Unemployment Effects on Votes Against Incumbents\n(", capitalize(data_name), ")", sep=""), 
                     ylab="Votes Against Incumbent (Percentage)", 
                     xlab="Unemployment Rate (Percentage)",
                     colorkey = FALSE,
  )
  print(hbpA)
  dev.off()
  
  pdf(file=paste("reports/images/", data_name, "_unemployment-against-all.pdf", sep=""), width=5, height=4)
  hbpA <- hexbinplot(AGAINST~UNEMP, unemployment, 
             xbins=100,
             aspect=1,
             xlim=range(unemployment$UNEMP),
             ylim=range(unemployment$AGAINST),
             main=paste("Unemployment Effects on Votes Against Incumbents\n(", capitalize(data_name), ", All Candidates)", sep=""), 
             ylab="Votes Against Incumbent (Percentage)", 
             xlab="Unemployment Rate (Percentage)",
             colorkey = FALSE,
             panel = function(x, y, ...) {
               panel.hexbinplot(x, y, ...)
               lattice::panel.abline(modelAll, col=color_all, lwd=3)
             }
  )
  print(hbpA)
  dev.off()
  
  pdf(file=paste("reports/images/", data_name, "_unemployment-against-D+R.pdf", sep=""), width=5, height=4)
  hbpDR <- hexbinplot(AGAINST~UNEMP, unemployment, 
             xbins=100,
             aspect=1,
             xlim=range(unemployment$UNEMP),
             ylim=range(unemployment$AGAINST),
             main=paste("Unemployment Effects on Votes Against Incumbents\n(", capitalize(data_name), ", Republicans v Democrats)", sep=""), 
             ylab="Votes Against Incumbent (Percentage)", 
             xlab="Unemployment Rate (Percentage)",
             colorkey = FALSE,
             panel = function(x, y, ...) {
               panel.hexbinplot(x, y, ...)
               lattice::panel.abline(modelR, col=color_rep, lwd=3)
               lattice::panel.abline(modelD, col=color_dem, lwd=3)
             }
  )
  print(hbpDR)
  dev.off()
  
  # But let's look at it when considering the change in unemployment over time, 
  #  looking up to 10 years in the past.
  deltaList = list()
  for (i in 1:10) {
    colname <- paste("Unemployment Delta_", i, sep="")
    deltaModel <- lm(`Percent Against` ~ get(colname), data=input_data)
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
  
  pdf(file=paste("reports/images/", data_name, "_regression-trends-all.pdf", sep=""), width=5, height=4)
  bp <- barplot(estimates, 
                col=color_all, 
                xaxt='n', 
                main=paste("Unemployment Trends vs Anti-incumbency\n(", capitalize(data_name), ", All)", sep=""), 
                xlab="Unemployment Interval (Years)", 
                ylab="Regression Coefficient",
  )
  text(bp, par("usr")[3], labels = deltaFrame[, 1], adj = c(0.5,1.0), xpd = TRUE, cex=1.0)
  dev.off()
}

unemployment_regressions(senate_data, "senate")
unemployment_regressions(governor_data, "governor")
unemployment_regressions(president_data, "president")
