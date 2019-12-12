# I've never really done anything with R before, but I
#   liked the control it gave over graphing output.
# This is almost certainly non-paradigmatic, inefficient,
#   and likely even wrong in some places.

library(R.utils)
library(readxl)
library(hexbin)
library(jtools)
library(lmtest)
library(car)

color_all <- "#36C800"
color_dem <- "#0055C8"
color_rep <- "#C80000"

output_regression <- function(model, filename) {
  capture.output(summary(model), file=filename)
  capture.output(print("p values:"), file=filename, append=TRUE)
  capture.output(summary(model)[["coefficients"]][,4], file=filename, append=TRUE)
  capture.output(bptest(model), file=filename, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=filename, append=TRUE)
  capture.output(print(coeftest(model, vcov. = hccm)[,2]), file=filename, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=filename, append=TRUE)
  capture.output(print(coeftest(model, vcov. = hccm)[,4]), file=filename, append=TRUE)
}

generate_regressions <- function(input_data, data_name) {
  # First we calculate total votes and the percentage of votes against the incumbent.
  input_data$`Total Votes` = input_data$`Votes For Incumbent` + input_data$`Votes Against Incumbent`
  input_data$`Percent Against` = input_data$`Votes Against Incumbent` / input_data$`Total Votes` * 100

  # Make data subsets
  datums <- data.frame(
    "AGAINST" = input_data$`Percent Against`,
    "UNEMP" = input_data$`Unemployment Rate`,
    "GDP" = input_data$`GDP Growth`,
    "DOW3M" = input_data$`Dow-3M` * 100,
    "DOW6M" = input_data$`Dow-6M` * 100,
    "DOW1Y" = input_data$`Dow-1Y` * 100,
    "DOW2Y" = input_data$`Dow-2Y` * 100,
    "DOW4Y" = input_data$`Dow-4Y` * 100,
    "SP5003M" = input_data$`SP500-3M` * 100,
    "SP5006M" = input_data$`SP500-6M` * 100,
    "SP5001Y" = input_data$`SP500-1Y` * 100,
    "SP5002Y" = input_data$`SP500-2Y` * 100,
    "SP5004Y" = input_data$`SP500-4Y` * 100,
    "NASDAQ3M" = input_data$`Nasdaq-3M` * 100,
    "NASDAQ6M" = input_data$`Nasdaq-6M` * 100,
    "NASDAQ1Y" = input_data$`Nasdaq-1Y` * 100,
    "NASDAQ2Y" = input_data$`Nasdaq-2Y` * 100,
    "NASDAQ4Y" = input_data$`Nasdaq-4Y` * 100,
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

  modelDow3MAll <- lm(AGAINST ~ DOW3M, data=datums)
  modelDow6MAll <- lm(AGAINST ~ DOW6M, data=datums)
  modelDow1YAll <- lm(AGAINST ~ DOW1Y, data=datums)
  modelDow2YAll <- lm(AGAINST ~ DOW2Y, data=datums)
  modelDow4YAll <- lm(AGAINST ~ DOW4Y, data=datums)

  modelSP5003MAll <- lm(AGAINST ~ SP5003M, data=datums)
  modelSP5006MAll <- lm(AGAINST ~ SP5006M, data=datums)
  modelSP5001YAll <- lm(AGAINST ~ SP5001Y, data=datums)
  modelSP5002YAll <- lm(AGAINST ~ SP5002Y, data=datums)
  modelSP5004YAll <- lm(AGAINST ~ SP5004Y, data=datums)

  modelNasdaq3MAll <- lm(AGAINST ~ NASDAQ3M, data=datums)
  modelNasdaq6MAll <- lm(AGAINST ~ NASDAQ6M, data=datums)
  modelNasdaq1YAll <- lm(AGAINST ~ NASDAQ1Y, data=datums)
  modelNasdaq2YAll <- lm(AGAINST ~ NASDAQ2Y, data=datums)
  modelNasdaq4YAll <- lm(AGAINST ~ NASDAQ4Y, data=datums)

  modelDow3MR <- lm(AGAINST ~ DOW3M, data=rep_datums)
  modelDow6MR <- lm(AGAINST ~ DOW6M, data=rep_datums)
  modelDow1YR <- lm(AGAINST ~ DOW1Y, data=rep_datums)
  modelDow2YR <- lm(AGAINST ~ DOW2Y, data=rep_datums)
  modelDow4YR <- lm(AGAINST ~ DOW4Y, data=rep_datums)

  modelSP5003MR <- lm(AGAINST ~ SP5003M, data=rep_datums)
  modelSP5006MR <- lm(AGAINST ~ SP5006M, data=rep_datums)
  modelSP5001YR <- lm(AGAINST ~ SP5001Y, data=rep_datums)
  modelSP5002YR <- lm(AGAINST ~ SP5002Y, data=rep_datums)
  modelSP5004YR <- lm(AGAINST ~ SP5004Y, data=rep_datums)

  modelNasdaq3MR <- lm(AGAINST ~ NASDAQ3M, data=rep_datums)
  modelNasdaq6MR <- lm(AGAINST ~ NASDAQ6M, data=rep_datums)
  modelNasdaq1YR <- lm(AGAINST ~ NASDAQ1Y, data=rep_datums)
  modelNasdaq2YR <- lm(AGAINST ~ NASDAQ2Y, data=rep_datums)
  modelNasdaq4YR <- lm(AGAINST ~ NASDAQ4Y, data=rep_datums)

  modelDow3MD <- lm(AGAINST ~ DOW3M, data=dem_datums)
  modelDow6MD <- lm(AGAINST ~ DOW6M, data=dem_datums)
  modelDow1YD <- lm(AGAINST ~ DOW1Y, data=dem_datums)
  modelDow2YD <- lm(AGAINST ~ DOW2Y, data=dem_datums)
  modelDow4YD <- lm(AGAINST ~ DOW4Y, data=dem_datums)

  modelSP5003MD <- lm(AGAINST ~ SP5003M, data=dem_datums)
  modelSP5006MD <- lm(AGAINST ~ SP5006M, data=dem_datums)
  modelSP5001YD <- lm(AGAINST ~ SP5001Y, data=dem_datums)
  modelSP5002YD <- lm(AGAINST ~ SP5002Y, data=dem_datums)
  modelSP5004YD <- lm(AGAINST ~ SP5004Y, data=dem_datums)

  modelNasdaq3MD <- lm(AGAINST ~ NASDAQ3M, data=dem_datums)
  modelNasdaq6MD <- lm(AGAINST ~ NASDAQ6M, data=dem_datums)
  modelNasdaq1YD <- lm(AGAINST ~ NASDAQ1Y, data=dem_datums)
  modelNasdaq2YD <- lm(AGAINST ~ NASDAQ2Y, data=dem_datums)
  modelNasdaq4YD <- lm(AGAINST ~ NASDAQ4Y, data=dem_datums)

  modelStock3MAll <- lm(AGAINST ~ DOW3M + SP5003M + NASDAQ3M, data=datums)
  modelStock6MAll <- lm(AGAINST ~ DOW6M + SP5006M + NASDAQ6M, data=datums)
  modelStock1YAll <- lm(AGAINST ~ DOW1Y + SP5001Y + NASDAQ1Y, data=datums)
  modelStock2YAll <- lm(AGAINST ~ DOW2Y + SP5002Y + NASDAQ2Y, data=datums)
  modelStock4YAll <- lm(AGAINST ~ DOW4Y + SP5004Y + NASDAQ4Y, data=datums)

  modelStock3MR <- lm(AGAINST ~ DOW3M + SP5003M + NASDAQ3M, data=rep_datums)
  modelStock6MR <- lm(AGAINST ~ DOW6M + SP5006M + NASDAQ6M, data=rep_datums)
  modelStock1YR <- lm(AGAINST ~ DOW1Y + SP5001Y + NASDAQ1Y, data=rep_datums)
  modelStock2YR <- lm(AGAINST ~ DOW2Y + SP5002Y + NASDAQ2Y, data=rep_datums)
  modelStock4YR <- lm(AGAINST ~ DOW4Y + SP5004Y + NASDAQ4Y, data=rep_datums)

  modelStock3MD <- lm(AGAINST ~ DOW3M + SP5003M + NASDAQ3M, data=dem_datums)
  modelStock6MD <- lm(AGAINST ~ DOW6M + SP5006M + NASDAQ6M, data=dem_datums)
  modelStock1YD <- lm(AGAINST ~ DOW1Y + SP5001Y + NASDAQ1Y, data=dem_datums)
  modelStock2YD <- lm(AGAINST ~ DOW2Y + SP5002Y + NASDAQ2Y, data=dem_datums)
  modelStock4YD <- lm(AGAINST ~ DOW4Y + SP5004Y + NASDAQ4Y, data=dem_datums)

  # dump the summary info to text
  output_regression(modelUnempAll, paste("reports/txt/", data_name, "_unemployment-all.txt", sep=""))
  output_regression(modelUnempR, paste("reports/txt/", data_name, "_unemployment-R.txt", sep=""))
  output_regression(modelUnempD, paste("reports/txt/", data_name, "_unemployment-D.txt", sep=""))

  output_regression(modelGrowthAll, paste("reports/txt/", data_name, "_gdp-all.txt", sep=""))
  output_regression(modelGrowthR, paste("reports/txt/", data_name, "_gdp-R.txt", sep=""))
  output_regression(modelGrowthD, paste("reports/txt/", data_name, "_gdp-D.txt", sep=""))

  output_regression(modelStock3MAll, paste("reports/txt/", data_name, "_stock-3M-all.txt", sep=""))
  output_regression(modelStock3MR, paste("reports/txt/", data_name, "_stock-3M-R.txt", sep=""))
  output_regression(modelStock3MD, paste("reports/txt/", data_name, "_stock-3M-D.txt", sep=""))

  output_regression(modelStock6MAll, paste("reports/txt/", data_name, "_stock-6M-all.txt", sep=""))
  output_regression(modelStock6MR, paste("reports/txt/", data_name, "_stock-6M-R.txt", sep=""))
  output_regression(modelStock6MD, paste("reports/txt/", data_name, "_stock-6M-D.txt", sep=""))

  output_regression(modelStock1YAll, paste("reports/txt/", data_name, "_stock-1Y-all.txt", sep=""))
  output_regression(modelStock1YR, paste("reports/txt/", data_name, "_stock-1Y-R.txt", sep=""))
  output_regression(modelStock1YD, paste("reports/txt/", data_name, "_stock-1Y-D.txt", sep=""))

  output_regression(modelStock2YAll, paste("reports/txt/", data_name, "_stock-2Y-all.txt", sep=""))
  output_regression(modelStock2YR, paste("reports/txt/", data_name, "_stock-2Y-R.txt", sep=""))
  output_regression(modelStock2YD, paste("reports/txt/", data_name, "_stock-2Y-D.txt", sep=""))

  output_regression(modelStock4YAll, paste("reports/txt/", data_name, "_stock-4Y-all.txt", sep=""))
  output_regression(modelStock4YR, paste("reports/txt/", data_name, "_stock-4Y-R.txt", sep=""))
  output_regression(modelStock4YD, paste("reports/txt/", data_name, "_stock-4Y-D.txt", sep=""))

  output_regression(modelDow3MAll, paste("reports/txt/", data_name, "_dow-3M-all.txt", sep=""))
  output_regression(modelDow3MR, paste("reports/txt/", data_name, "_dow-3M-R.txt", sep=""))
  output_regression(modelDow3MD, paste("reports/txt/", data_name, "_dow-3M-D.txt", sep=""))

  output_regression(modelDow6MAll, paste("reports/txt/", data_name, "_dow-6M-all.txt", sep=""))
  output_regression(modelDow6MR, paste("reports/txt/", data_name, "_dow-6M-R.txt", sep=""))
  output_regression(modelDow6MD, paste("reports/txt/", data_name, "_dow-6M-D.txt", sep=""))

  output_regression(modelDow1YAll, paste("reports/txt/", data_name, "_dow-1Y-all.txt", sep=""))
  output_regression(modelDow1YR, paste("reports/txt/", data_name, "_dow-1Y-R.txt", sep=""))
  output_regression(modelDow1YD, paste("reports/txt/", data_name, "_dow-1Y-D.txt", sep=""))

  output_regression(modelDow2YAll, paste("reports/txt/", data_name, "_dow-2Y-all.txt", sep=""))
  output_regression(modelDow2YR, paste("reports/txt/", data_name, "_dow-2Y-R.txt", sep=""))
  output_regression(modelDow2YD, paste("reports/txt/", data_name, "_dow-2Y-D.txt", sep=""))

  output_regression(modelDow4YAll, paste("reports/txt/", data_name, "_dow-4Y-all.txt", sep=""))
  output_regression(modelDow4YR, paste("reports/txt/", data_name, "_dow-4Y-R.txt", sep=""))
  output_regression(modelDow4YD, paste("reports/txt/", data_name, "_dow-4Y-D.txt", sep=""))

  output_regression(modelSP5003MAll, paste("reports/txt/", data_name, "_sp500-3M-all.txt", sep=""))
  output_regression(modelSP5003MR, paste("reports/txt/", data_name, "_sp500-3M-R.txt", sep=""))
  output_regression(modelSP5003MD, paste("reports/txt/", data_name, "_sp500-3M-D.txt", sep=""))

  output_regression(modelSP5006MAll, paste("reports/txt/", data_name, "_sp500-6M-all.txt", sep=""))
  output_regression(modelSP5006MR, paste("reports/txt/", data_name, "_sp500-6M-R.txt", sep=""))
  output_regression(modelSP5006MD, paste("reports/txt/", data_name, "_sp500-6M-D.txt", sep=""))

  output_regression(modelSP5001YAll, paste("reports/txt/", data_name, "_sp500-1Y-all.txt", sep=""))
  output_regression(modelSP5001YR, paste("reports/txt/", data_name, "_sp500-1Y-R.txt", sep=""))
  output_regression(modelSP5001YD, paste("reports/txt/", data_name, "_sp500-1Y-D.txt", sep=""))

  output_regression(modelSP5002YAll, paste("reports/txt/", data_name, "_sp500-2Y-all.txt", sep=""))
  output_regression(modelSP5002YR, paste("reports/txt/", data_name, "_sp500-2Y-R.txt", sep=""))
  output_regression(modelSP5002YD, paste("reports/txt/", data_name, "_sp500-2Y-D.txt", sep=""))

  output_regression(modelSP5004YAll, paste("reports/txt/", data_name, "_sp500-4Y-all.txt", sep=""))
  output_regression(modelSP5004YR, paste("reports/txt/", data_name, "_sp500-4Y-R.txt", sep=""))
  output_regression(modelSP5004YD, paste("reports/txt/", data_name, "_sp500-4Y-D.txt", sep=""))

  output_regression(modelNasdaq3MAll, paste("reports/txt/", data_name, "_nasdaq-3M-all.txt", sep=""))
  output_regression(modelNasdaq3MR, paste("reports/txt/", data_name, "_nasdaq-3M-R.txt", sep=""))
  output_regression(modelNasdaq3MD, paste("reports/txt/", data_name, "_nasdaq-3M-D.txt", sep=""))

  output_regression(modelNasdaq6MAll, paste("reports/txt/", data_name, "_nasdaq-6M-all.txt", sep=""))
  output_regression(modelNasdaq6MR, paste("reports/txt/", data_name, "_nasdaq-6M-R.txt", sep=""))
  output_regression(modelNasdaq6MD, paste("reports/txt/", data_name, "_nasdaq-6M-D.txt", sep=""))

  output_regression(modelNasdaq1YAll, paste("reports/txt/", data_name, "_nasdaq-1Y-all.txt", sep=""))
  output_regression(modelNasdaq1YR, paste("reports/txt/", data_name, "_nasdaq-1Y-R.txt", sep=""))
  output_regression(modelNasdaq1YD, paste("reports/txt/", data_name, "_nasdaq-1Y-D.txt", sep=""))

  output_regression(modelNasdaq2YAll, paste("reports/txt/", data_name, "_nasdaq-2Y-all.txt", sep=""))
  output_regression(modelNasdaq2YR, paste("reports/txt/", data_name, "_nasdaq-2Y-R.txt", sep=""))
  output_regression(modelNasdaq2YD, paste("reports/txt/", data_name, "_nasdaq-2Y-D.txt", sep=""))

  output_regression(modelNasdaq4YAll, paste("reports/txt/", data_name, "_nasdaq-4Y-all.txt", sep=""))
  output_regression(modelNasdaq4YR, paste("reports/txt/", data_name, "_nasdaq-4Y-R.txt", sep=""))
  output_regression(modelNasdaq4YD, paste("reports/txt/", data_name, "_nasdaq-4Y-D.txt", sep=""))



  # Output the datums in word format
  export_summs(modelUnempAll, modelUnempR, modelUnempD,
               model.names = c("All", "Rep", "Dem"),
               to.file="docx", file.name=paste("reports/docs/", data_name, "_unemployment.docx", sep=""))

  export_summs(modelGrowthAll, modelGrowthR, modelGrowthD,
               model.names = c("All", "Rep", "Dem"),
               to.file="docx", file.name=paste("reports/docs/", data_name, "_gdp_growth.docx", sep=""))

  ## Some kind of output error with the stock models that I don't feel like debugging at the moment.

  # export_summs(modelStock3MAll, modelStock3MR, modelStock3MD,
  #              model.names = c("All", "Rep", "Dem"),
  #              to.file="docx", file.name=paste("reports/docs/", data_name, "_stock_3M.docx", sep=""))
  #
  # export_summs(modelStock6MAll, modelStock6MR, modelStock6MD,
  #              model.names = c("All", "Rep", "Dem"),
  #              to.file="docx", file.name=paste("reports/docs/", data_name, "_stock_6M.docx", sep=""))
  #
  # export_summs(modelStock1YAll, modelStock1YR, modelStock1YD,
  #              model.names = c("All", "Rep", "Dem"),
  #              to.file="docx", file.name=paste("reports/docs/", data_name, "_stock_1Y.docx", sep=""))
  #
  # export_summs(modelStock2YAll, modelStock2YR, modelStock2YD,
  #              model.names = c("All", "Rep", "Dem"),
  #              to.file="docx", file.name=paste("reports/docs/", data_name, "_stock_2Y.docx", sep=""))
  #
  # export_summs(modelStock4YAll, modelStock4YR, modelStock4YD,
  #              model.names = c("All", "Rep", "Dem"),
  #              to.file="docx", file.name=paste("reports/docs/", data_name, "_stock_4Y.docx", sep=""))

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
