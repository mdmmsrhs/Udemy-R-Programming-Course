# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2011-10-08 16:52:52 Graham Williams>
#
# WEATHER DATASET
#
# Copyright (c) 2009-2010 Togaware Pty Ltd
#
# This file is part of Rattle.
#
# Rattle is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# Rattle is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Rattle. If not, see <http://www.gnu.org/licenses/>.
#
########################################################################
#
# Generate the weather dataset that is used to illustrate typcial
# modelling in Rattle.

acquireWeatherData <- function(write.to.file=FALSE)
{
  
  # 081217 Generate the weather dataset and write each of the
  # processed datasets to file if requested.
  
  # These sample datasets come from the Australian Bureau of
  # Meteorology and are used with permission from Cathy Toby
  # <C.Toby@bom.gov.au> Wed, 17 Dec 2008 14:01:03 +1100. We download
  # the data direct from their web site and then transform it into a
  # simple dataset to illustrate data mining. I am collecting the data
  # over time, as only 13 months is available at any time.
  
  sites <- read.csv("weather/site-numbers.csv")
  names(sites) <- c("stnID", "stnNum", "DWOname", "latitude", "longitude")
  
  
  locations <- matrix(c(
    2002,	"Albury",
    2005,   "BadgerysCreek",
    2029,   "Cobar",
    2030,   "CoffsHarbour",
    2084,   "Moree",
    2098,	"Newcastle",
    2099,   "NorahHead",
    2100,   "NorfolkIsland",
    2111,	"Penrith",
    2119,   "Richmond",
    2124, 	"Sydney",
    2125,   "SydneyAirport",
    2139,   "WaggaWagga",
    2145,   "Williamtown",
    2146,	"Wollongong",
    2801, 	"Canberra",
    2802,	"Tuggeranong",
    2804, 	"MountGinini",
    3005,	"Ballarat",
    3008,	"Bendigo",
    3022,   "Sale",
    3049,   "MelbourneAirport",
    3050, 	"Melbourne",
    3051,   "Mildura",
    3068,   "Portland",
    3079,   "Watsonia",
    3101,   "Dartmoor",
    4019, 	"Brisbane",
    4024,	"Cairns",
    4050, 	"GoldCoast",
    4128, 	"Townsville",
    5002, 	"Adelaide",
    5041,	"MountGambier",
    5049,   "Nuriootpa",
    5072,   "Woomera",
    6001,	"Albany",
    6081,   "Witchcliffe",
    6108,   "PearceRAAF",
    6111, 	"Perth",
    6110,   "PerthAirport",
    6119,   "SalmonGums",
    6138,   "Walpole",
    7021, 	"Hobart",
    7025,	"Launceston",
    8002,	"AliceSprings",
    8014, 	"Darwin"),
    byrow=TRUE, ncol=2)
  locations <- as.data.frame(locations)
  names(locations) <- c("stnID", "name")
  locations <- merge(locations, sites)
  locations <- locations[c("stnID", "name", "stnNum", "latitude", "longitude")]
  
  write.csv(locations, "weather/locations.csv")
  locationsAUS <- locations
  save(locationsAUS, file="locationsAUS.RData")
  
  # 090327 We download each dataset, and keep one constant for the
  # book. The "weatherCanberra", "weatherSydney", ... datasets are
  # created each time. We use backto=2 since some of the old data has
  # errors that is manually fixed by me, and so I want to avoid
  # overwriting them with the copy from the website.
  
  # 100131 Go back only 6 months to limit time checking old data.
  
  for (l in 1:nrow(locations))
    updateWeatherLocation(locations[l, "name"], locations[l, "stnID"],
                          write.to.file, backto=2)
  
  # 100123 Create the combined weather dataset, which is more like
  # what we might expect as the first attempt to data mine - large and
  # not prepared. This dataset will be used for exercises.
  
  weatherAUS <- createWeatherAUS(locations)
  
  # 090329 Now take a specific subset create the sample weather
  # dataset - which will always be the same dataset - require
  # stability for the book.
  
  weather <- subset(weatherAUS, Date <= "2008/10/31" & Location == "Canberra")
  rownames(weather) <- NULL
  
  # Save the dataset to a CSV file for inclusion with rattle.
  
  fname <- "weather.csv"
  write.csv(weather, fname, row.names=FALSE)
  
  # 100213 We might not actually include this as it adds about 400K to
  # the package, taking it from 900K to 1.3M
  
  fname <- "weatherAUS.csv"
  write.csv(weatherAUS, fname, row.names=FALSE)
  
  # 090628 Should really now convert all of the variables to their
  # appropraite data types, with Date as a Date, numeric variables as
  # numeric, and the target as a factor with levels "Yes" and
  # "No". Currently everything seems to be a character now.
  
  save(weather, file="weather.RData", compress=TRUE)
  
  library(foreign)
  write.arff(weather, "weather.arff")
  system('perl -pi -e "s|@attribute Date numeric|@attribute Date date|" weather.arff')
  
  # Create a dataset with many missing values.
  
  set.seed(123)
  mr <- sample(1:nrow(weather), nrow(weather)/4, replace=TRUE)
  mc <- sample(2:(ncol(weather)-1), nrow(weather)/4, replace=TRUE)
  
  for (i in 1:(nrow(weather)/4))
  {
    weather[mr[i], mc[i]] <- NA
  }
  write.table(weather, "weather_missing.csv", sep=",", row.names=FALSE)
  
  # 100123 Check the data looks okay
  
  checkDataOkay(locations[["name"]])
  
  # 100124 Report size.
  
  cat(sprintf("\nweatherAUS: %d rows by %d columns and %d weather stations.\n",
              nrow(weatherAUS), ncol(weatherAUS), length(unique(weatherAUS$Location))))
  
  # ToDo - extract info for each weather station
  
  dfc <- aggregate(weatherAUS$Location, weatherAUS["Location"], length)
  dfd <- aggregate(weatherAUS$Date, weatherAUS["Location"], min)
}

updateWeatherLocation <- function(name, stnID, write.to.file, backto=14)
{
  
  # 090329 Download all available data files from BOM and archive into
  # the weather directory.
  
  months <- format(seq(from=(Sys.Date()-backto*30), by="months", length=backto), "%Y%m")
  BOM <- "http://www.bom.gov.au/climate/dwo/"
  curwd <- setwd("weather")
  lapply(months, function(m)
    system(sprintf("wget -N -m -nd --backup-converted %s%s/text/IDCJDW%s.%s.csv",
                   BOM, m, stnID, m)))
  setwd(curwd)
  
  # Now work only with he downloaded CSV files. We read the archived
  # files and process into datasets for Rattle.
  
  weather <- data.frame()
  
  # Use a for loop rather than lappy - simpler to debug!
  
  for (f in dir("weather", pattern=sprintf("^IDCJDW%s.*.csv$", as.character(stnID))))
  {
    # 100125 Calculate the amount of skip.
    
    skip <- as.integer(system(sprintf("grep -v '^,' weather/%s | wc -l", f), intern=TRUE))
    
    # Read the CSV file.
    
    x <- read.csv(paste("weather/", f, sep=""), skip=skip, check.names=FALSE)
    
    weather <- rbind(weather,
                     data.frame(Date = as.Date(x[["Date"]]),
                                Location = rep(name, nrow(x)),
                                MinTemp = x[["Minimum temperature (\xb0C)"]],
                                MaxTemp = x[["Maximum temperature (\xb0C)"]],
                                Rainfall = x[["Rainfall (mm)"]],
                                Evaporation = x[["Evaporation (mm)"]],
                                Sunshine = x[["Sunshine (hours)"]],
                                WindGustDir = x[["Direction of maximum wind gust "]],
                                WindGustSpeed = x[["Speed of maximum wind gust (km/h)"]],
                                WindDir9am = x[["9am wind direction"]],
                                WindDir3pm = x[["3pm wind direction"]],
                                WindSpeed9am=as.character(x[["9am wind speed (km/h)"]]),
                                WindSpeed3pm=as.character(x[["3pm wind speed (km/h)"]]),
                                Humidity9am = x[["9am relative humidity (%)"]],
                                Humidity3pm = x[["3pm relative humidity (%)"]],
                                Pressure9am = x[["9am MSL pressure (hPa)"]],
                                Pressure3pm = x[["3pm MSL pressure (hPa)"]],
                                Cloud9am = x[["9am cloud amount (oktas)"]],
                                Cloud3pm = x[["3pm cloud amount (oktas)"]],
                                Temp9am = x[["9am Temperature (\xb0C)"]],
                                Temp3pm = x[["3pm Temperature (\xb0C)"]]
                     ))
    # 100123 Some of the CSV files were being corrupted.
    #
    # 100124 Not sure if all was the fault of "wget -c", but it was
    # certainly the fault for some. So no that I use "wget -N -m -nd"
    # all seems okay again.
    #
    # I'm seeing some
    # repeated ",,,,," in some files, that then have the actual values
    # following.
    #
    # I'm also seeing the last number like: 1017.91017.9. Seems to be
    # repeated.
    #
    # I then manually fix these up in the downloaded CSV and hope I
    # don't override it. This is why backto is now set to 2. To debug,
    # go through this look and see which CSV causes the change of
    # datatype. Then edit the file.
    #
    # Alternatively go into the weather folder and do:
    #   egrep '[0-9]+\.[0-9]+\.[0-9]+' * | cut -f1 -d":"
    # to list the files with a problem number, and then
    #   perl -pi.bak -e 's|([0-9]+\.[0-9])[0-9]*\.[0-9]+|\1|' XXX.csv
    # to each file. Or all in one go:
    #   egrep '[0-9]+\.[0-9]+\.[0-9]+' * | cut -f1 -d":" | xargs perl -pi.bak -e 's|([0-9]+\.[0-9])[0-9]*\.[0-9]+|\1|'
    #
    # Some useful commands:
    # sapply(x, class)
    # sapply(weather, class)
    
    #print(f)
    #print(class(weather$WindDir9am))
  }
  
  # Clean up some of the columns
  
  ordDirs <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
               "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  
  weather <- within(weather,
                    {
                      # 9am
                      
                      WindSpeed9am <- as.character(WindSpeed9am)
                      WindSpeed9am[WindSpeed9am == ""] <- NA
                      WindSpeed9am[WindSpeed9am == "Calm"] <- "0"
                      WindSpeed9am <- as.numeric(WindSpeed9am)
                      
                      WindDir9am[WindDir9am == ""] <- NA
                      WindDir9am[is.na(WindSpeed9am) | (WindSpeed9am == 0)] <- NA
                      WindDir9am <- factor(WindDir9am, levels = ordDirs, ordered = TRUE)
                      
                      # 3pm
                      
                      WindSpeed3pm <- as.character(WindSpeed3pm)
                      WindSpeed3pm[WindSpeed3pm == ""] <- NA
                      WindSpeed3pm[WindSpeed3pm == "Calm"] <- "0"
                      WindSpeed3pm <- as.numeric(WindSpeed3pm)
                      
                      WindDir3pm[WindDir3pm == ""] <- NA
                      WindDir3pm[is.na(WindSpeed3pm) | (WindSpeed3pm == 0)] <- NA
                      WindDir3pm <- factor(WindDir3pm, levels = ordDirs, ordered = TRUE)
                      
                      # Gust
                      
                      WindGustSpeed[WindGustSpeed == ""] <- NA
                      WindGustSpeed[WindGustSpeed == "Calm"] <- "0"
                      WindGustSpeed <- as.numeric(WindGustSpeed)
                      WindGustDir[WindGustDir == ""] <- NA
                      WindGustDir[is.na(WindGustSpeed) | (WindGustSpeed == 0)] <- NA
                      WindGustDir <- factor(WindGustDir, levels=ordDirs, ordered = TRUE)
                    })
  
  
  # Did it rain today, and how much rain tomorrow (RISK) and did it
  # rain tomorrow (TARGET). Rain less than 1mm is no rain!
  
  weather$RainToday <- as.integer(weather$Rainfall > 1)
  weather$RainToday <- as.factor(weather$RainToday)
  levels(weather$RainToday) <- c("No", "Yes")
  
  weather$RISK_MM <- c(weather$Rainfall[2:nrow(weather)], NA)
  weather$RainTomorrow <- c(as.integer(weather$Rainfall > 1)[2:nrow(weather)], NA)
  weather$RainTomorrow <- as.factor(weather$RainTomorrow)
  levels(weather$RainTomorrow) <- c("No", "Yes")
  
  # 090229 Let's keep the original data.
  
  ## # Change in Temp
  
  ## weather$ChangeTemp <- weather$Temp3pm - weather$Temp9am
  
  ## weather$ChangeTempDir <- as.factor(weather$ChangeTemp > 0)
  ## levels(weather$ChangeTempDir) <- c("down", "up")
  
  ## weather$ChangeTempMag <- "small"
  ## weather$ChangeTempMag[weather$ChangeTemp >
  ##                       median(weather$ChangeTemp[weather$ChangeTemp>0])] <- "large"
  ## weather$ChangeTempMag[weather$ChangeTemp <
  ##                       median(weather$ChangeTemp[weather$ChangeTemp<0])] <- "large"
  ## weather$ChangeTempMag <- as.factor(weather$ChangeTempMag)
  
  ## # Note change of wind directions
  
  ## weather$WindDirect9am[weather$WindDirect9am == " "] <- "NONE"
  ## weather$WindDirect9am[weather$WindDirect9am == ""] <- "NONE"
  ## weather$WindDirect3pm[weather$WindDirect3pm == " "] <- "NONE"
  ## weather$WindDirect3pm[weather$WindDirect3pm == ""] <- "NONE"
  
  ## wind.changes <- matrix(c("n", "c", "c",   "c",  "c",   "c",  "c",  "c",  "c",
  ##                          "c",  "c",  "c",   "c",  "c",  "c",  "c",  "c",
  ##                          "c", "n", "s",   "s",  "s",   "s",  "l",  "l",  "l",
  ##                          "l",  "l",  "l",   "l",  "s",  "s",  "s",  "s",
  ##                          "c", "s", "n",   "s",  "s",   "s",  "s",  "l",  "l",
  ##                          "l",  "l",  "l",   "l",  "l",  "s",  "s",  "s",
  ##                          "c", "s", "s",   "n",  "s",   "s",  "s",  "s",  "l",
  ##                          "l",  "l",  "l",   "l",  "l",  "l",  "s",  "s",
  ##                          "c", "s", "s",   "s",  "n",   "s",  "s",  "s",  "s",
  ##                          "l",  "l",  "l",   "l",  "l",  "l",  "l",  "s",
  ##                          "c", "s", "s",   "s",  "s",   "n",  "s",  "s",  "s",
  ##                          "s",  "l",  "l",   "l",  "l",  "l",  "l",  "l",
  ##                          "c", "l", "s",   "s",  "s",   "s",  "n",  "s",  "s",
  ##                          "s",  "s",  "l",   "l",  "l",  "l",  "l",  "l",
  ##                          "c", "l", "l",   "s",  "s",   "s",  "s",  "n",  "s",
  ##                          "s",  "s",  "s",   "l",  "l",  "l",  "l",  "l",
  ##                          "c", "l", "l",   "l",  "s",   "s",   "s",  "s", "n",
  ##                          "s",  "s",  "s",   "s",  "l",  "l",  "l",  "l",
  ##                          "c", "l", "l",   "l",  "l",   "s",   "s",  "s", "s",
  ##                          "n",  "s",  "s",   "s",  "s",  "l",  "l",  "l",
  ##                          "c", "l", "l",   "l",  "l",   "l",   "s",  "s", "s",
  ##                          "s",  "n",  "s",   "s",  "s",  "s",  "l",  "l",
  ##                          "c", "l", "l",   "l",  "l",   "l",   "l",  "s", "s",
  ##                          "s",  "s",  "n",   "s",  "s",  "s",  "s",  "l",
  ##                          "c", "l", "l",   "l",  "l",   "l",   "l",  "l", "s",
  ##                          "s",  "s",  "s",   "n",  "s",  "s",  "s",  "s",
  ##                          "c", "s", "l",   "l",  "l",   "l",   "l",  "l", "l",
  ##                          "s",  "s",  "s",   "s",  "n",  "s",  "s",  "s",
  ##                          "c", "s", "s",   "l",  "l",   "l",   "l",  "l", "l",
  ##                          "l",  "s",  "s",   "s",  "s",  "n",  "s",  "s",
  ##                          "c", "s", "s",   "s",  "l",   "l",   "l",  "l", "l",
  ##                          "l",  "l",  "s",   "s",  "s",  "s",  "n",  "s",
  ##                          "c", "s", "s",   "s",  "s",   "l",   "l",  "l", "l",
  ##                          "l",  "l",  "l",   "s",  "s",  "s",  "s",  "n"),
  ##                        nrow=17, byrow=TRUE)
  
  ## colnames(wind.changes) <- c("NONE", "N", "NNE", "NE", "ENE", "E", "ESE", "SE",
  ##                             "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  ## rownames(wind.changes) <- c("NONE", "N", "NNE", "NE", "ENE", "E", "ESE", "SE",
  ##                             "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  
  
  ## # changes <- c(seq(0, 180, 22.5), seq(-157.5, -22.5, 22.5))
  
  ## weather$ChangeWindDirect <- NA
  ## for (i in 1:nrow(weather))
  ##   weather$ChangeWindDirect[i] <- wind.changes[weather$WindDirect9am[i],
  ##                                               weather$WindDirect3pm[i]]
  ## weather$ChangeWindDirect <- as.factor(weather$ChangeWindDirect)
  
  ## weather$WindDirect9am <- NULL
  ## weather$WindDirect3pm <- NULL
  
  ## # When was the MaxWindTime: am or pm?
  
  ## weather$MaxWindTime <- as.integer(sub(":.*$", "", weather$MaxWindTime))
  ## weather$MaxWindPeriod <- NA
  ## weather$MaxWindPeriod[weather$MaxWindTime < 6] <- "earlyAM"
  ## weather$MaxWindPeriod[weather$MaxWindTime >= 6 & weather$MaxWindTime < 12 ] <- "lateAM"
  ## weather$MaxWindPeriod[weather$MaxWindTime >= 12 & weather$MaxWindTime < 18 ] <- "earlyPM"
  ## weather$MaxWindPeriod[weather$MaxWindTime >= 18] <- "latePM"
  
  ## weather$MaxWindTime <- NULL
  
  ## # Did it rain today
  
  ## weather$RainToday <- as.integer(weather$RainMM > 1)
  
  ## # What was the average rainfull in the past week
  
  ## # Did the wind change today
  
  ## # Temperature range
  
  ## weather$TempRange <- weather$MaxTempC - weather$MinTempC
  
  ## # Change in pressure
  
  ## weather$PressureChange <- NA
  ## weather$PressureChange[weather$Pressure3pm > weather$Pressure9am] <- "up" 
  ## weather$PressureChange[weather$Pressure3pm < weather$Pressure9am] <- "down" 
  ## weather$PressureChange[weather$Pressure3pm == weather$Pressure9am] <- "steady" 
  
  ## # TODO  Turn original pressures into High/Medium/Low
  
  
  
  
  ## # Generate the "risk" and target variables. Let's say it has to be
  ## # greate than 1 for it to be siad to have rained!
  
  ## weather$RISK_MM <- c(weather$RainMM[2:nrow(weather)], NA)
  ## weather$RainTomorrow <- c(as.integer(weather$RainMM > 1)[2:nrow(weather)], NA)
  
  ## # Remove unwanted columns
  
  ## weather$Remove <- NULL
  ## weather$RainMM <- NULL
  ## weather$MaxTempC <- NULL
  ## weather$MinTempC <- NULL
  ## weather$MaxWindDirection <- NULL
  
  # Resort the columns to get some categorics near the front for
  # explanation purposes.
  
  # Remove the last row, as it won't contain a prediction for
  # tomorrow.
  
  weather <- weather[1:(nrow(weather)-1),]
  weather.orig <- weather
  
  if (write.to.file)
  {
    # Save the combined dataset to a CSV file for inclusion with rattle.
    
    # 100123 No longer generate individual csv/arff/missing files.
    
    ## fname <- sprintf("weather%s.csv", name)
    ## write.table(weather[1:(nrow(weather)-1),], fname, sep=",", row.names=FALSE)
    eval(parse(text=sprintf("weather%s <- weather", name)))
    eval(parse(text=sprintf('save(weather%s, file="weather%s.RData", compress=TRUE)',
                            name, name)))
    
    ## library(foreign)
    ## arff <- weather
    ## #arff$RainTomorrow <- as.factor(arff$RainTomorrow)
    ## write.arff(arff, sprintf("weather%s.arff", name))
    
    ## # Create a dataset with many missing values.
    
    ## set.seed(123)
    ## mr <- sample(1:nrow(weather), nrow(weather)/4, replace=TRUE)
    ## mc <- sample(2:(ncol(weather)-1), nrow(weather)/4, replace=TRUE)
    
    ## for (i in 1:(nrow(weather)/4))
    ## {
    ##   weather[mr[i], mc[i]] <- NA
    ## }
    ## write.table(weather, sprintf("weather%s_missing.csv", name),
    ##             sep=",", row.names=FALSE)
  }
  
  if (write.to.file)
    invisible(weather.orig)
  else
    return(weather.orig)
}

createWeatherAUS <- function(locations)
{
  weatherAUS <- NULL
  for (i in locations[["name"]])
  {
    dsname <- load(sprintf("weather%s.RData", i))
    ds <- eval(parse(text=dsname))
    weatherAUS <- rbind(weatherAUS, ds)
    #print(locations[i])
    #print(class(weather[["MinTemp"]]))
  }
  #  write.csv(weatherAUS, "weatherAUS.csv")
  #  require(foreign)
  #  write.arff(weatherAUS, "weather.arff")
  save(weatherAUS, file="weatherAUS.RData")
  print("weatherAUS.RData written.")
  
  return(weatherAUS)
}

checkDataOkay <- function(locations)
{
  
  expect <- c("Date", "factor", "numeric", "numeric", "numeric",
              "numeric", "numeric", "ordered factor", "numeric", "ordered factor",
              "ordered factor", "numeric", "numeric", "integer", "integer",
              "numeric", "numeric", "integer", "integer", "numeric",
              "numeric", "factor", "numeric", "factor")
  
  names(expect) <- c("Date", "Location", "MinTemp", "MaxTemp",
                     "Rainfall", "Evaporation", "Sunshine",
                     "WindGustDir", "WindGustSpeed", "WindDir9am",
                     "WindDir3pm", "WindSpeed9am", "WindSpeed3pm",
                     "Humidity9am", "Humidity3pm", "Pressure9am",
                     "Pressure3pm", "Cloud9am", "Cloud3pm", "Temp9am",
                     "Temp3pm", "RainToday", "RISK_MM", "RainTomorrow")
  
  for (i in seq_along(locations))
  {
    dsname <- load(sprintf("weather%s.RData", locations[i]))
    ds <- eval(parse(text=dsname))
    for (v in names(expect))
      if (paste(class(ds[[v]]), collapse=" ") != expect[v] && ! all(is.na(ds[[v]])))
        print(sprintf("Problem in %s: %s is %s but expected %s.\n",
                      locations[i], v, paste(class(ds[[v]]), collapse=" "), expect[v]))
    #else
    #  print(sprintf("Okay in %s: %s is %s and expected %s.\n",
    #                locations[i], v, class(ds[[v]]), expect[v]))
  }
  print("Data integrity checked.")
}

readJsonBom <- function(path)
{
  require(RJSONIO)
  path <- "http://www.bom.gov.au/fwo/IDN60801/IDN60801.95753.json"
  path <- "http://www.bom.gov.au/fwo/IDN60801/IDN60801.95753.axf"
  obs <- fromJSON()
}