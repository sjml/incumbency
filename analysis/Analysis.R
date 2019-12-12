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
  outfile <- paste("reports/txt/", data_name, "_unemployment-all.txt", sep="")
  # output_regression(modelUnempAll, outfile)
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



  outfile <- paste("reports/txt/", data_name, "_stock-3M-all.txt", sep="")
  capture.output(summary(modelStock3MAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelStock3MAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelStock3MAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock3MAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock3MAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_stock-3M-R.txt", sep="")
  capture.output(summary(modelStock3MR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelStock3MR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelStock3MR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock3MR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock3MR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_stock-3M-D.txt", sep="")
  capture.output(summary(modelStock3MD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelStock3MD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelStock3MD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock3MD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock3MD, vcov. = hccm)[,4]), file=outfile, append=TRUE)


  outfile <- paste("reports/txt/", data_name, "_stock-6M-all.txt", sep="")
  capture.output(summary(modelStock6MAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelStock6MAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelStock6MAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock6MAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock6MAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_stock-6M-R.txt", sep="")
  capture.output(summary(modelStock6MR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelStock6MR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelStock6MR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock6MR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock6MR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_stock-6M-D.txt", sep="")
  capture.output(summary(modelStock6MD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelStock6MD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelStock6MD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock6MD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock6MD, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_stock-1Y-all.txt", sep="")
  capture.output(summary(modelStock1YAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelStock1YAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelStock1YAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock1YAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock1YAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_stock-1Y-R.txt", sep="")
  capture.output(summary(modelStock1YR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelStock1YR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelStock1YR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock1YR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock1YR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_stock-1Y-D.txt", sep="")
  capture.output(summary(modelStock1YD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelStock1YD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelStock1YD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock1YD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock1YD, vcov. = hccm)[,4]), file=outfile, append=TRUE)


  outfile <- paste("reports/txt/", data_name, "_stock-2Y-all.txt", sep="")
  capture.output(summary(modelStock2YAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelStock2YAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelStock2YAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock2YAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock2YAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_stock-2Y-R.txt", sep="")
  capture.output(summary(modelStock2YR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelStock2YR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelStock2YR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock2YR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock2YR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_stock-2Y-D.txt", sep="")
  capture.output(summary(modelStock2YD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelStock2YD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelStock2YD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock2YD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock2YD, vcov. = hccm)[,4]), file=outfile, append=TRUE)


  outfile <- paste("reports/txt/", data_name, "_stock-4Y-all.txt", sep="")
  capture.output(summary(modelStock4YAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelStock4YAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelStock4YAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock4YAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock4YAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_stock-4Y-R.txt", sep="")
  capture.output(summary(modelStock4YR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelStock4YR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelStock4YR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock4YR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock4YR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_stock-4Y-D.txt", sep="")
  capture.output(summary(modelStock4YD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelStock4YD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelStock4YD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock4YD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelStock4YD, vcov. = hccm)[,4]), file=outfile, append=TRUE)



  outfile <- paste("reports/txt/", data_name, "_dow-3M-all.txt", sep="")
  capture.output(summary(modelDow3MAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelDow3MAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelDow3MAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow3MAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow3MAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_dow-3M-R.txt", sep="")
  capture.output(summary(modelDow3MR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelDow3MR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelDow3MR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow3MR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow3MR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_dow-3M-D.txt", sep="")
  capture.output(summary(modelDow3MD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelDow3MD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelDow3MD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow3MD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow3MD, vcov. = hccm)[,4]), file=outfile, append=TRUE)


  outfile <- paste("reports/txt/", data_name, "_dow-6M-all.txt", sep="")
  capture.output(summary(modelDow6MAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelDow6MAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelDow6MAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow6MAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow6MAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_dow-6M-R.txt", sep="")
  capture.output(summary(modelDow6MR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelDow6MR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelDow6MR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow6MR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow6MR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_dow-6M-D.txt", sep="")
  capture.output(summary(modelDow6MD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelDow6MD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelDow6MD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow6MD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow6MD, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_dow-1Y-all.txt", sep="")
  capture.output(summary(modelDow1YAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelDow1YAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelDow1YAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow1YAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow1YAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_dow-1Y-R.txt", sep="")
  capture.output(summary(modelDow1YR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelDow1YR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelDow1YR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow1YR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow1YR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_dow-1Y-D.txt", sep="")
  capture.output(summary(modelDow1YD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelDow1YD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelDow1YD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow1YD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow1YD, vcov. = hccm)[,4]), file=outfile, append=TRUE)


  outfile <- paste("reports/txt/", data_name, "_dow-2Y-all.txt", sep="")
  capture.output(summary(modelDow2YAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelDow2YAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelDow2YAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow2YAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow2YAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_dow-2Y-R.txt", sep="")
  capture.output(summary(modelDow2YR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelDow2YR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelDow2YR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow2YR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow2YR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_dow-2Y-D.txt", sep="")
  capture.output(summary(modelDow2YD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelDow2YD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelDow2YD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow2YD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow2YD, vcov. = hccm)[,4]), file=outfile, append=TRUE)


  outfile <- paste("reports/txt/", data_name, "_dow-4Y-all.txt", sep="")
  capture.output(summary(modelDow4YAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelDow4YAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelDow4YAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow4YAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow4YAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_dow-4Y-R.txt", sep="")
  capture.output(summary(modelDow4YR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelDow4YR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelDow4YR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow4YR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow4YR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_dow-4Y-D.txt", sep="")
  capture.output(summary(modelDow4YD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelDow4YD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelDow4YD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow4YD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelDow4YD, vcov. = hccm)[,4]), file=outfile, append=TRUE)


  outfile <- paste("reports/txt/", data_name, "_sp500-3M-all.txt", sep="")
  capture.output(summary(modelSP5003MAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelSP5003MAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelSP5003MAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5003MAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5003MAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_sp500-3M-R.txt", sep="")
  capture.output(summary(modelSP5003MR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelSP5003MR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelSP5003MR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5003MR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5003MR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_sp500-3M-D.txt", sep="")
  capture.output(summary(modelSP5003MD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelSP5003MD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelSP5003MD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5003MD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5003MD, vcov. = hccm)[,4]), file=outfile, append=TRUE)


  outfile <- paste("reports/txt/", data_name, "_sp500-6M-all.txt", sep="")
  capture.output(summary(modelSP5006MAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelSP5006MAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelSP5006MAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5006MAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5006MAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_sp500-6M-R.txt", sep="")
  capture.output(summary(modelSP5006MR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelSP5006MR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelSP5006MR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5006MR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5006MR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_sp500-6M-D.txt", sep="")
  capture.output(summary(modelSP5006MD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelSP5006MD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelSP5006MD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5006MD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5006MD, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_sp500-1Y-all.txt", sep="")
  capture.output(summary(modelSP5001YAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelSP5001YAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelSP5001YAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5001YAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5001YAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_sp500-1Y-R.txt", sep="")
  capture.output(summary(modelSP5001YR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelSP5001YR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelSP5001YR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5001YR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5001YR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_sp500-1Y-D.txt", sep="")
  capture.output(summary(modelSP5001YD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelSP5001YD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelSP5001YD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5001YD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5001YD, vcov. = hccm)[,4]), file=outfile, append=TRUE)


  outfile <- paste("reports/txt/", data_name, "_sp500-2Y-all.txt", sep="")
  capture.output(summary(modelSP5002YAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelSP5002YAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelSP5002YAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5002YAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5002YAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_sp500-2Y-R.txt", sep="")
  capture.output(summary(modelSP5002YR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelSP5002YR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelSP5002YR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5002YR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5002YR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_sp500-2Y-D.txt", sep="")
  capture.output(summary(modelSP5002YD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelSP5002YD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelSP5002YD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5002YD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5002YD, vcov. = hccm)[,4]), file=outfile, append=TRUE)


  outfile <- paste("reports/txt/", data_name, "_sp500-4Y-all.txt", sep="")
  capture.output(summary(modelSP5004YAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelSP5004YAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelSP5004YAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5004YAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5004YAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_sp500-4Y-R.txt", sep="")
  capture.output(summary(modelSP5004YR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelSP5004YR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelSP5004YR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5004YR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5004YR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_sp500-4Y-D.txt", sep="")
  capture.output(summary(modelSP5004YD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelSP5004YD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelSP5004YD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5004YD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelSP5004YD, vcov. = hccm)[,4]), file=outfile, append=TRUE)


  outfile <- paste("reports/txt/", data_name, "_nasdaq-3M-all.txt", sep="")
  capture.output(summary(modelNasdaq3MAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelNasdaq3MAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelNasdaq3MAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq3MAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq3MAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_nasdaq-3M-R.txt", sep="")
  capture.output(summary(modelNasdaq3MR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelNasdaq3MR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelNasdaq3MR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq3MR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq3MR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_nasdaq-3M-D.txt", sep="")
  capture.output(summary(modelNasdaq3MD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelNasdaq3MD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelNasdaq3MD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq3MD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq3MD, vcov. = hccm)[,4]), file=outfile, append=TRUE)


  outfile <- paste("reports/txt/", data_name, "_nasdaq-6M-all.txt", sep="")
  capture.output(summary(modelNasdaq6MAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelNasdaq6MAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelNasdaq6MAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq6MAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq6MAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_nasdaq-6M-R.txt", sep="")
  capture.output(summary(modelNasdaq6MR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelNasdaq6MR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelNasdaq6MR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq6MR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq6MR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_nasdaq-6M-D.txt", sep="")
  capture.output(summary(modelNasdaq6MD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelNasdaq6MD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelNasdaq6MD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq6MD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq6MD, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_nasdaq-1Y-all.txt", sep="")
  capture.output(summary(modelNasdaq1YAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelNasdaq1YAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelNasdaq1YAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq1YAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq1YAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_nasdaq-1Y-R.txt", sep="")
  capture.output(summary(modelNasdaq1YR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelNasdaq1YR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelNasdaq1YR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq1YR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq1YR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_nasdaq-1Y-D.txt", sep="")
  capture.output(summary(modelNasdaq1YD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelNasdaq1YD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelNasdaq1YD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq1YD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq1YD, vcov. = hccm)[,4]), file=outfile, append=TRUE)


  outfile <- paste("reports/txt/", data_name, "_nasdaq-2Y-all.txt", sep="")
  capture.output(summary(modelNasdaq2YAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelNasdaq2YAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelNasdaq2YAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq2YAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq2YAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_nasdaq-2Y-R.txt", sep="")
  capture.output(summary(modelNasdaq2YR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelNasdaq2YR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelNasdaq2YR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq2YR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq2YR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_nasdaq-2Y-D.txt", sep="")
  capture.output(summary(modelNasdaq2YD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelNasdaq2YD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelNasdaq2YD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq2YD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq2YD, vcov. = hccm)[,4]), file=outfile, append=TRUE)


  outfile <- paste("reports/txt/", data_name, "_nasdaq-4Y-all.txt", sep="")
  capture.output(summary(modelNasdaq4YAll), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelNasdaq4YAll)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelNasdaq4YAll), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq4YAll, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq4YAll, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_nasdaq-4Y-R.txt", sep="")
  capture.output(summary(modelNasdaq4YR), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelNasdaq4YR)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelNasdaq4YR), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq4YR, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq4YR, vcov. = hccm)[,4]), file=outfile, append=TRUE)

  outfile <- paste("reports/txt/", data_name, "_nasdaq-4Y-D.txt", sep="")
  capture.output(summary(modelNasdaq4YD), file=outfile)
  capture.output(print("p values:"), file=outfile, append=TRUE)
  capture.output(summary(modelNasdaq4YD)[["coefficients"]][,4], file=outfile, append=TRUE)
  capture.output(bptest(modelNasdaq4YD), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected standard errors:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq4YD, vcov. = hccm)[,2]), file=outfile, append=TRUE)
  capture.output(print("heteroskedasticity-corrected p values:"), file=outfile, append=TRUE)
  capture.output(print(coeftest(modelNasdaq4YD, vcov. = hccm)[,4]), file=outfile, append=TRUE)


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
