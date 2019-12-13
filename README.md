# Economy vs Incumbency

## Round 1: FIGHT

This is the source code I used for an applied econometrics project on how various economic indicators correlate with voting behaviors. Specifically, it was measuring the effects of unemployment, GDP, and the stock market on percentage of votes against an incumbent. 

County-level data was used as much as possible, which yielded a pretty large set of data (~30k observations) so doing it with code made a lot of sense. 

Unfortunately I don't have a license to redistribute any of the raw data I used, but where it came from and how it should be plugged into this code is in the [data description file](./data/data_description.txt). 

The output regressions and graphs are all in the [reports directory](./reports/).

To run the Python parts, install the packages from `requirements.txt`, then run the `src/_process.sh` script. This will take all the raw data, do some intermediate steps, and eventually output a nicely formatted spreadsheet. 

The actual analysis is done in R and the file sits in the [analysis directory](./analysis/). I used RStudio to do all of this, and have no idea how to run it from the command line, how to make sure you have the right libraries installed, etc. Apologies. ¯\\_(ツ)\_/¯ 
