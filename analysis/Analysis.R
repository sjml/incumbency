library(R.utils)
library(readxl)
library(hexbin)
library(jtools)
library(lmtest)

color_all <- "#36C800"
color_dem <- "#0055C8"
color_rep <- "#C80000"

generate_regressions <- function(input_data, data_name) {
  # First we calculate total votes and the percentage of votes against the incumbent.
  input_data$`Total Votes` = input_data$`Votes For Incumbent` + input_data$`Votes Against Incumbent`
  input_data$`Percent Against` = input_data$`Votes Against Incumbent` / input_data$`Total Votes` * 100

  # Make data subsets
  datums <- data.frame(
    "AGAINST" = input_data$`Percent Against`,
    "UNEMP" = input_data$`Unemployment Rate`,
    "GDP" = input_data$`GDP Growth`,
    "INC" = input_data$`Incumbent Party`
  )
  datums <- na.omit(datums)
  datums <- datums[datums$AGAINST > 0.0,]
  datums <- datums[datums$AGAINST < 100.0,]

  rep_datums <- datums[datums$INC == "Republican",]
  dem_datums <- datums[datums$INC == "Democratic" | datums$INC == "Democrat",]

  # build the models
  modelUnempAll <- lm(AGAINST ~ UNEMP, data=datums)
  modelUnempR <- lm(AGAINST ~ UNEMP, data=rep_datums)
  modelUnempD <- lm(AGAINST ~ UNEMP, data=dem_datums)

  modelGrowthAll <- lm(AGAINST ~ GDP, data=datums)
  modelGrowthR <- lm(AGAINST ~ GDP, data=rep_datums)
  modelGrowthD <- lm(AGAINST ~ GDP, data=dem_datums)

  # dump the summary info to text
  outfile <- paste("reports/txt/", data_name, "_unemployment-all.txt", sep="")
  capture.output(summary(modelUnempAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelUnempAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelUnempAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelUnempAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelUnempAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)
  
  outfile <- paste("reports/txt/", data_name, "_unemployment-R.txt", sep="")
  capture.output(summary(modelUnempR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelUnempR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelUnempR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelUnempR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelUnempR, vcov. = hccm)[,4]), file=outfile, append=TRUE)
  
  outfile <- paste("reports/txt/", data_name, "_unemployment-D.txt", sep="")
  capture.output(summary(modelUnempD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelUnempD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelUnempD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelUnempD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelUnempD, vcov. = hccm)[,4]), file=outfile, append=TRUE)
  
  outfile <- paste("reports/txt/", data_name, "_gdp-all.txt", sep="")
  capture.output(summary(modelGrowthAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelGrowthAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelGrowthAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelGrowthAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelGrowthAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)
  
  outfile <- paste("reports/txt/", data_name, "_gdp-R.txt", sep="")
  capture.output(summary(modelGrowthR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelGrowthR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelGrowthR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelGrowthR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelGrowthR, vcov. = hccm)[,4]), file=outfile, append=TRUE)
  
  outfile <- paste("reports/txt/", data_name, "_gdp-D.txt", sep="")
  capture.output(summary(modelGrowthD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelGrowthD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelGrowthD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelGrowthD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelGrowthD, vcov. = hccm)[,4]), file=outfile, append=TRUE)
  
  # Output the datums in word format
  export_summs(modelUnempAll, modelUnempR, modelUnempD,
               model.names = c("All", "Rep", "Dem"),
               to.file="docx", file.name=paste("reports/docs/", data_name, "_unemployment.docx", sep=""))

  export_summs(modelGrowthAll, modelGrowthR, modelGrowthD,
               model.names = c("All", "Rep", "Dem"),
               to.file="docx", file.name=paste("reports/docs/", data_name, "_gdp_growth.docx", sep=""))

  # Output the heatmaps
  pdf(file=paste("reports/images/", data_name, "_unemployment-against-all-no-regression.pdf", sep=""), width=5, height=4)
  hbpA <- hexbinplot(AGAINST~UNEMP, datums,
                     xbins=100,
                     aspect=1,
                     xlim=range(datums$UNEMP),
                     ylim=range(datums$AGAINST),
                     main=paste("Unemployment Effects on Votes Against Incumbents\n(", capitalize(data_name), ")", sep=""),
                     ylab="Votes Against Incumbent (Percentage)",
                     xlab="Unemployment Rate (Percentage)",
                     colorkey = FALSE,
  )
  print(hbpA)
  dev.off()

  pdf(file=paste("reports/images/", data_name, "_gdp_growth-against-all-no-regression.pdf", sep=""), width=5, height=4)
  hbpA <- hexbinplot(AGAINST~GDP, datums,
                     xbins=100,
                     aspect=1,
                     xlim=range(datums$GDP),
                     ylim=range(datums$AGAINST),
                     main=paste("GDP Growth Effects on Votes Against Incumbents\n(", capitalize(data_name), ")", sep=""),
                     ylab="Votes Against Incumbent (Percentage)",
                     xlab="GDP Growth (Percentage)",
                     colorkey = FALSE,
  )
  print(hbpA)
  dev.off()


  pdf(file=paste("reports/images/", data_name, "_unemployment-against-all.pdf", sep=""), width=5, height=4)
  hbpA <- hexbinplot(AGAINST~UNEMP, datums,
             xbins=100,
             aspect=1,
             xlim=range(datums$UNEMP),
             ylim=range(datums$AGAINST),
             main=paste("Unemployment Effects on Votes Against Incumbents\n(", capitalize(data_name), ", All Candidates)", sep=""),
             ylab="Votes Against Incumbent (Percentage)",
             xlab="Unemployment Rate (Percentage)",
             colorkey = FALSE,
             panel = function(x, y, ...) {
               panel.hexbinplot(x, y, ...)
               lattice::panel.abline(modelUnempAll, col=color_all, lwd=3)
             }
  )
  print(hbpA)
  dev.off()

  pdf(file=paste("reports/images/", data_name, "_gdp_growth-against-all.pdf", sep=""), width=5, height=4)
  hbpA <- hexbinplot(AGAINST~GDP, datums,
             xbins=100,
             aspect=1,
             xlim=range(datums$GDP),
             ylim=range(datums$AGAINST),
             main=paste("GDP Growth Effects on Votes Against Incumbents\n(", capitalize(data_name), ", All Candidates)", sep=""),
             ylab="Votes Against Incumbent (Percentage)",
             xlab="GDP Growth (Percentage)",
             colorkey = FALSE,
             panel = function(x, y, ...) {
               panel.hexbinplot(x, y, ...)
               lattice::panel.abline(modelGrowthAll, col=color_all, lwd=3)
             }
  )
  print(hbpA)
  dev.off()


  pdf(file=paste("reports/images/", data_name, "_unemployment-against-D+R.pdf", sep=""), width=5, height=4)
  hbpDR <- hexbinplot(AGAINST~UNEMP, datums,
             xbins=100,
             aspect=1,
             xlim=range(datums$UNEMP),
             ylim=range(datums$AGAINST),
             main=paste("Unemployment Effects on Votes Against Incumbents\n(", capitalize(data_name), ", Republicans v Democrats)", sep=""),
             ylab="Votes Against Incumbent (Percentage)",
             xlab="Unemployment Rate (Percentage)",
             colorkey = FALSE,
             panel = function(x, y, ...) {
               panel.hexbinplot(x, y, ...)
               lattice::panel.abline(modelUnempR, col=color_rep, lwd=3)
               lattice::panel.abline(modelUnempD, col=color_dem, lwd=3)
             }
  )
  print(hbpDR)
  dev.off()
  print(summary(modelUnempR))
  
  pdf(file=paste("reports/images/", data_name, "_gdp_growth-against-D+R.pdf", sep=""), width=5, height=4)
  hbpDR <- hexbinplot(AGAINST~GDP, datums,
             xbins=100,
             aspect=1,
             xlim=range(datums$GDP),
             ylim=range(datums$AGAINST),
             main=paste("GDP Growth Effects on Votes Against Incumbents\n(", capitalize(data_name), ", Republicans v Democrats)", sep=""),
             ylab="Votes Against Incumbent (Percentage)",
             xlab="GDP Growth (Percentage)",
             colorkey = FALSE,
             panel = function(x, y, ...) {
               panel.hexbinplot(x, y, ...)
               lattice::panel.abline(modelGrowthR, col=color_rep, lwd=3)
               lattice::panel.abline(modelGrowthD, col=color_dem, lwd=3)
             }
  )
  print(hbpDR)
  dev.off()

  # But let's look at it when considering the changes over time,
  #  looking up to 10 years in the past.
  unempDeltaListAll = list()
  unempDeltaListR = list()
  unempDeltaListD = list()
  for (i in 1:10) {
    colname <- paste("Unemployment Delta_", i, sep="")
    
    deltaDatums <- data.frame(
      "AGAINST" = input_data$`Percent Against`,
      "UNEMP" = get(colname, input_data),
      "INC" = input_data$`Incumbent Party`
    )
    deltaDatums <- na.omit(deltaDatums)
    deltaDatums <- deltaDatums[deltaDatums$AGAINST > 0.0,]
    deltaDatums <- deltaDatums[deltaDatums$AGAINST < 100.0,]
    
    rep_deltaDatums <- deltaDatums[deltaDatums$INC == "Republican",]
    dem_deltaDatums <- deltaDatums[deltaDatums$INC == "Democratic" | deltaDatums$INC == "Democrat",]
    
    deltaModelAll <- lm(AGAINST ~ UNEMP, data=deltaDatums)
    coeffsAll <- summary(deltaModelAll)[["coefficients"]]
    unempDeltaListAll[[i]] <- c(
      i,
      coeffsAll[,"Estimate"][-1][1],
      coeffsAll[,"Pr(>|t|)"][-1][1]
    )
    
    deltaModelR <- lm(AGAINST ~ UNEMP, data=rep_deltaDatums)
    coeffsR <- summary(deltaModelR)[["coefficients"]]
    unempDeltaListR[[i]] <- c(
      i,
      coeffsR[,"Estimate"][-1][1],
      coeffsR[,"Pr(>|t|)"][-1][1]
    )
    
    deltaModelD <- lm(AGAINST ~ UNEMP, data=dem_deltaDatums)
    coeffsD <- summary(deltaModelD)[["coefficients"]]
    unempDeltaListD[[i]] <- c(
      i,
      coeffsD[,"Estimate"][-1][1],
      coeffsD[,"Pr(>|t|)"][-1][1]
    )
    
    
    pdf(file=paste("reports/images/", data_name, "_unemployment-minus-", i, "-against-all.pdf", sep=""), width=5, height=4)
    hbpA <- hexbinplot(`Percent Against` ~ get(colname), input_data,
                       xbins=100,
                       aspect=1,
                       xlim=range(get(colname, input_data)),
                       ylim=range(input_data$`Percent Against`),
                       main=paste("Unemployment Effects on Votes Against Incumbents\n(", capitalize(data_name), ", All Candidates, ", i, "-year offset)", sep=""),
                       ylab="Votes Against Incumbent (Percentage)",
                       xlab="Change in Unemployment Rate (Percentage Points)",
                       colorkey = FALSE,
                       panel = function(x, y, ...) {
                         panel.hexbinplot(x, y, ...)
                         lattice::panel.abline(deltaModelAll, col=color_all, lwd=3)
                         lattice::panel.abline(deltaModelR, col=color_rep, lwd=3)
                         lattice::panel.abline(deltaModelD, col=color_dem, lwd=3)
                       }
    )
    print(hbpA)
    dev.off()
    
  }
  unempDeltaFrameAll <- do.call("rbind", unempDeltaListAll)
  colnames(unempDeltaFrameAll) <- c("Unemployment Interval", "Coefficient Estimate", "Pr(>|t|)")
  estimatesAll <- unempDeltaFrameAll[,"Coefficient Estimate"]
  pvalsAll <- unempDeltaFrameAll[,"Pr(>|t|)"]

  
  unempDeltaFrameR <- do.call("rbind", unempDeltaListR)
  colnames(unempDeltaFrameR) <- c("Unemployment Interval", "Coefficient Estimate", "Pr(>|t|)")
  estimatesR <- unempDeltaFrameR[,"Coefficient Estimate"]
  pvalsR <- unempDeltaFrameR[,"Pr(>|t|)"]

  
  unempDeltaFrameD <- do.call("rbind", unempDeltaListD)
  colnames(unempDeltaFrameD) <- c("Unemployment Interval", "Coefficient Estimate", "Pr(>|t|)")
  estimatesD <- unempDeltaFrameD[,"Coefficient Estimate"]
  pvalsD <- unempDeltaFrameD[,"Pr(>|t|)"]
  
  
  pdf(file=paste("reports/images/", data_name, "_unemployment-regression-trends-all.pdf", sep=""), width=5, height=4)
  bp <- barplot(estimatesAll,
                col=color_all,
                xaxt='n',
                main=paste("Unemployment Trends vs Anti-incumbency\n(", capitalize(data_name), ", All)", sep=""),
                xlab="Unemployment Interval (Years)",
                ylab="Regression Coefficient",
  )
  text(bp, par("usr")[3], labels = unempDeltaFrameAll[, 1], adj = c(0.5,1.0), xpd = TRUE, cex=1.0)
  dev.off()
  
  pdf(file=paste("reports/images/", data_name, "_unemployment-regression-trends-R.pdf", sep=""), width=5, height=4)
  bp <- barplot(estimatesR,
                col=color_rep,
                xaxt='n',
                main=paste("Unemployment Trends vs Anti-incumbency\n(", capitalize(data_name), ", Republicans)", sep=""),
                xlab="Unemployment Interval (Years)",
                ylab="Regression Coefficient",
  )
  text(bp, par("usr")[3], labels = unempDeltaFrameR[, 1], adj = c(0.5,1.0), xpd = TRUE, cex=1.0)
  dev.off()
  
  pdf(file=paste("reports/images/", data_name, "_unemployment-regression-trends-D.pdf", sep=""), width=5, height=4)
  bp <- barplot(estimatesD,
                col=color_dem,
                xaxt='n',
                main=paste("Unemployment Trends vs Anti-incumbency\n(", capitalize(data_name), ", Democrats)", sep=""),
                xlab="Unemployment Interval (Years)",
                ylab="Regression Coefficient",
  )
  text(bp, par("usr")[3], labels = unempDeltaFrameD[, 1], adj = c(0.5,1.0), xpd = TRUE, cex=1.0)
  dev.off()
}


senate_data <- read_excel("data/processed/senate_data.xlsx", 
                          col_types = c("numeric", "numeric", "text", 
                                        "text", "text", "text", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric"))

governor_data <- read_excel("data/processed/governor_data.xlsx",
                            col_types = c("numeric", "numeric", "text",
                                          "text", "text", "text", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric"))

president_data <- read_excel("data/processed/president_data.xlsx",
                             col_types = c("numeric", "numeric", "text",
                                           "text", "text", "text", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric"))

generate_regressions(senate_data, "senate")
generate_regressions(governor_data, "governor")
generate_regressions(president_data, "president")
