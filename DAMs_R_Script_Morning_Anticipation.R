#Librarys to be used, load these first
library(devtools)
library(damr)
library(ggetho)
library(sleepr)
library(zeitgebr)
library(behavr)
library(ggplot2)
library(ggsignif)
library(ggpubr)
library(wesanderson)
library(tidyr)
library(R.utils)
library(openxlsx)
library(extrafont)
library(grid)
library(svMisc)

#Where the Data is
DATA_DIR <- "Z:/PRJ-NeelyLab/Current staff files/Josie/Thangs paper 06.05.2020/ppkXTrpA1-IR"

#Check data is correctly loaded
list.files(DATA_DIR, pattern= "*.txt|*csv")

#set working directory
setwd(DATA_DIR)

#Load metadata
metadata <- fread("metadata.csv")

#remove # to check if metadata is loaded correctly
metadata

#connect metadata to raw data
metadata <- link_dam_metadata(metadata, result_dir = DATA_DIR)
metadata

metadata$Condition=factor(metadata$Condition, levels = c("Intact", "Injured"))
metadata$genotype=factor(metadata$genotype)
str(metadata)


#Define new loading functions

DAM2_COLS <-  as.list(c("i", "c", "c","i", rep("_",5), "i", rep("i",32)))
names(DAM2_COLS) <- c("idx", "date", "time","status", sprintf("no_data_%02d", 1:5), "light_sensor", sprintf("channel_%02d", 1:32))

DAM5_COLS <-  DAM2_COLS
DAM5_COLS[[8]] <- "c"
names(DAM5_COLS)[[8]] <- "data_type"

DATA_TYPE_NAMES <- c("C1"= "C1",
                     "C2" = "C2",
                     "C3" = "C3",
                     "C4" = "C4",
                     "CT" = "activity",
                     "D1" = "D1",
                     "D2" = "D2",
                     "D3" = "D3",
                     "D4" = "D4",
                     "Pn" = "Pn",
                     "0" = "activity",
                     "PnF" = "Pn",
                     "Ct" = "activity")

load_dam3 = function(metadata, date_format="%d %b %y", FUN=NULL, ...){
  . = regions = start_datetime =  stop_datetime =  data = diff_t = NULL
  region_id = path = file_info =NULL
  
  
  tz="UTC"
  # TODO check for uniqueness in query!!
  q <- data.table::copy(metadata)
  q[, path:=sapply(file_info, function(x) x$path)]
  to_read <- q[,.(regions = list(region_id)),by=c("path","start_datetime","stop_datetime")]
  
  s <- to_read[,
               list(data=list(read_dam_file3(path,
                                             regions[[1]],
                                             start_datetime,
                                             stop_datetime,
                                             tz=tz,
                                             date_format=date_format)
               )
               ),
               by="path,start_datetime,stop_datetime"]
  
  d <- behavr::bind_behavr_list(s[,data])
  
  # new metadata had the columns in metadata that are not in d[metadata] already
  met <- d[meta=T]
  met <- met[
    metadata[,
             c("id", setdiff(colnames(metadata), colnames(met))),
             with=F],
    on="id"]
  
  data.table::setkeyv(met,"id")
  # replace metadata
  
  behavr::setmeta(d, met)
  if(!is.null(FUN))
    d <- d[,FUN(.SD, ...),by="id"]
  d
}

clean_dam_data2 <- function(df, regions, experiment_id, t0){
  . = channel = datetime = id = region_id = t = value = light_sensor = NULL
  
  df <- unique(df, by="datetime")
  df <- df[, (colnames(df) %like% "(channel)|(datetime)|(light_sensor)"), with=F]
  setnames(df,
           grep("channel_", colnames(df), value = T),
           gsub("channel_", "0", grep("channel_", colnames(df), value = T)))
  
  df <- melt(df, id=c("datetime", "light_sensor"), measure.vars = patterns("0"), variable.name = "channel", value.name = "value")
  
  dt <- df[ ,. (id = as.factor(sprintf("%s|%02d",experiment_id, as.integer(channel))),
                region_id = as.integer(channel),
                t = as.numeric(datetime-t0, units = "secs"),
                light_sensor = as.integer(light_sensor),
                value = value)]
  
  setkeyv(dt, "id")
  dt <- dt[region_id %in% regions]
  dt
}

read_dam_file3 <- function(path,
                           region_id=1:32,
                           start_datetime=-Inf,
                           stop_datetime=+Inf,
                           tz="UTC",
                           date_format="%d %b %y"){
  
  DAM2_COLS <-  as.list(c("i", "c", "c","i", rep("_",5), "i", rep("i",32)))
  names(DAM2_COLS) <- c("idx", "date", "time","status", sprintf("no_data_%02d", 1:5), "light_sensor", sprintf("channel_%02d", 1:32))
  
  DAM5_COLS <-  DAM2_COLS
  DAM5_COLS[[8]] <- "c"
  names(DAM5_COLS)[[8]] <- "data_type"
  
  DATA_TYPE_NAMES <- c("C1"= "C1",
                       "C2" = "C2",
                       "C3" = "C3",
                       "C4" = "C4",
                       "CT" = "activity",
                       "D1" = "D1",
                       "D2" = "D2",
                       "D3" = "D3",
                       "D4" = "D4",
                       "Pn" = "Pn",
                       "0" = "activity",
                       "PnF" = "Pn",
                       "Ct" = "activity")
  
  . =  datetime =  time =  datetime_posix = data_type = status = light_sensor = NULL
  # todo check whether region has duplicates/ is in range
  start_datetime <- parse_datetime(start_datetime,tz=tz)
  stop_datetime <- parse_datetime(stop_datetime,tz=tz)
  # print(str(start_datetime))
  # print(str(stop_datetime))
  first_last_lines <- find_dam_first_last_lines(path,
                                                start_datetime,
                                                stop_datetime,
                                                tz)
  first_line = first_last_lines$id[1]
  last_line = first_last_lines$id[2]
  # col_types=do.call(readr::cols_only, DAM5_COLS)
  
  
  col_names =  names(DAM5_COLS)
  col_class = c(i="integer", c="character", "_"="NULL")[as.character(DAM5_COLS)]
  #col_class <- col_class[which(DAM5_COLS != "_")]
  
  possible_classes <- unique(col_class)
  col_class <- lapply(possible_classes, function(x)which( col_class %in% x))
  names(col_class) <- possible_classes
  
  df <- fread(path,
              #header = F,
              col.names = col_names[which(DAM5_COLS != "_")],
              colClasses = col_class,
              #select = which(DAM5_COLS != "_"),
              skip = first_line - 1,
              nrows =  last_line - first_line + 1,
              showProgress = FALSE,
              drop=col_class$`NULL`)
  
  
  df <- df[, datetime := paste(date,time, sep=" ")]
  format <- paste(date_format,"%H:%M:%S", sep=" ")
  suppressWarnings(
    df <- df[, datetime_posix  := as.POSIXct(strptime(datetime,format,tz=tz))]
  )
  df[, datetime := NULL]
  setnames(df, "datetime_posix", "datetime")
  
  df[, data_type := DATA_TYPE_NAMES[as.character(data_type)]]
  # if start date is not defined, t0 is the first read available, whether or not is is valid!
  if(is.infinite(start_datetime)) {
    t0 = df$datetime[1]
  } else {
    t0 = df[min(which(df$light_sensor == 1)),]$datetime
  }
  
  if(is.infinite(stop_datetime)){
    t1 = df$datetime[nrow(df)]
  } else {
    t1 = stop_datetime
  }
  
  experiment_id <- paste(format(start_datetime, format = "%F %T"), basename(path),sep="|")
  df <- df[status == 1 & data_type != "TA"]
  dt <- df[, clean_dam_data2(.SD, region_id, experiment_id, t0), by="data_type"]
  
  setkeyv(dt, "id")
  
  meta <- unique(dt[, c("id","region_id")],by="id")
  
  meta[,experiment_id := experiment_id]
  meta[, start_datetime := start_datetime]
  meta[,stop_datetime := t1]
  
  file_info <- meta[,.(file_info =  list(list(path = path, file = basename(path)))), by="id"]
  meta <- file_info[meta]
  
  #meta <- met[,file:=basename(path)]
  dt <- dcast(dt, id + t + light_sensor ~ data_type,value.var="value")
  setkeyv(dt, "id")
  
  behavr::behavr(dt,meta)
  
}

parse_datetime <- function(x, tz="UTC"){
  stopifnot(length(x)==1) # scalar only
  
  if(is.factor(x))
    x <-  as.character(x)
  
  if("Date" %in% class(x))
    x <- as.POSIXct(as.IDate(x, tz=tz))
  
  
  
  if(is.infinite(x) | "POSIXct" %in% class(x))
    return(x)
  
  if(is.character(x)){
    out <- readr::parse_datetime(x, locale = readr::locale("en", tz = tz))
    readr::stop_for_problems(out)
    return(out)
  }
  stop("Unexpected type for x")
}

find_dam_first_last_lines <- function(file,
                                      start_datetime = -Inf,
                                      stop_datetime = +Inf,
                                      tz = "UTC"){
  . = data_type = status = datetime =  time =  datetime_posix = diff_t = NULL
  id = read_id = NULL
  
  
  start_datetime <- parse_datetime(start_datetime,tz=tz)
  
  has_time <- TRUE
  
  if(is.character(stop_datetime)){
    if(any(grep("^\\s*[0-9]{4}-[0-9]{2}-[0-9]{2}\\s*$", stop_datetime)))
      has_time <- FALSE
  }
  
  
  stop_datetime <- parse_datetime(stop_datetime,tz=tz)
  
  # we stop_date should be exclusive
  if(!has_time)
    stop_datetime <- stop_datetime + days(1) - .01
  
  if(start_datetime > stop_datetime)
    stop("start_datetime is greater than stop_datetime. Cannot fetch any data!")
  
  if(tools::file_ext(file) == "zip")
    fun <- fread_zip
  else
    fun <- fread
  
  cols_to_keep <- which(names(DAM5_COLS) %in% c("date", "time", "status", "data_type"))
  datetimes_dt <- fun(file,
                      col.names = names(DAM5_COLS)[cols_to_keep],
                      select =cols_to_keep)
  
  datetimes_dt[, id := 1:.N]
  datetimes_dt[, read_id := cumsum(data_type == "CT" | data_type == "0")]
  datetimes_dt <- datetimes_dt[status == 1]
  datetimes_dt <- datetimes_dt[, datetime := paste(date,time, sep=" ")]
  suppressWarnings(
    datetimes_dt <- datetimes_dt[, datetime_posix  := as.POSIXct(strptime(datetime,"%d %b %y %H:%M:%S",tz=tz))]
  )
  datetimes_dt <- datetimes_dt[datetime_posix >= start_datetime & datetime_posix <= stop_datetime]
  datetimes_dt[, diff_t := as.numeric(datetime_posix - shift(datetime_posix), unit="secs")]
  
  
  ## duplicated time stamps, clock changes?
  n_dups <- sum(duplicated(unique(datetimes_dt, by = "read_id")$datetime_posix))
  if(n_dups > 50){
    stop("More than 50 duplicated dates entries in the metadata file.
         This is a likely instance of the recording computer changing time
         (e.g. between winter and summer time)")
  }
  
  if(n_dups > 0){
    warning(sprintf("Some of the dates are repeated between successive measuments in %s.",
                    file))
  }
  
  sampling_periods <- unique(na.omit(unique(datetimes_dt, by = "read_id")$diff_t))
  if(any(abs(sampling_periods) >= 3600))
    stop("Time has jumped for an hour or more!
          No valid data during this time.
          Possibly, device was disconected or maybe a change from summer to winter time")
  
  ## irregular time stamps, possible missing reads
  n_sampling_periods <- length(sampling_periods)
  
  if(n_sampling_periods > 1)
    warning(sprintf("The sampling period is not always regular in %s.
                    Some reads must have been skipped.",file))
  
  first_and_last <- datetimes_dt[c(1, .N)]
  
  if(nrow(first_and_last) !=2)
    stop("No data in selected date range")
  first_and_last[, datetime := NULL]
  setnames(first_and_last, "datetime_posix", "datetime")
  first_and_last
}




# We find and load the matching data
dt <- load_dam3(metadata)
summary(dt)
head(dt[meta=T])

dt[, moving := activity>0]

dt[, sleeping := activity == 0]

###################################################

#quality control
  ##detecting anomolies

  ###Replicate 1
ggetho(dt[xmv(replicate) == 1 ], aes(z=activity)) +
 stat_tile_etho() +
 stat_ld_annotations()

  ###Replicate 2
ggetho(dt[xmv(replicate) == 2 ], aes(z=activity)) +
  stat_tile_etho() +
  stat_ld_annotations()

###Replicate 3
ggetho(dt[xmv(replicate) == 3 ], aes(z=activity)) +
  stat_tile_etho() +
  stat_ld_annotations()

  ###all replicates
ggetho(dt, aes(z=activity)) +
  stat_tile_etho() +
  stat_ld_annotations()

#sleep data
dts <- load_dam3(metadata, FUN = sleepr::sleep_dam_annotation)

#visualising all sleep data, look for death
 #replicate 1
#ggetho(dts[xmv(replicate) == 1], aes(z=asleep)) +
 #stat_ld_annotations(height = 1) +
 #stat_tile_etho()

 #replicate 2
#ggetho(dts[xmv(replicate) == 2], aes(z=asleep)) +
  #stat_ld_annotations(height = 1) +
  #stat_tile_etho()

 #replicate 3
#ggetho(dts[xmv(replicate) == 3], aes(z=asleep)) +
  #stat_ld_annotations(height = 1) +
  #stat_tile_etho()


 #remove flies that died early

dts_curated = curate_dead_animals(dts)

#make a summary table of lifespan for all flies
lifespan_dts <- dts_curated[, .(lifespan = max(t)), by=id]
#filter this table for lifespan>9 and keep id
valid_ids <- lifespan_dts[lifespan > days(9), id]
#now apply filter
dts_curated <- dts_curated[id %in% valid_ids]
summary(dts_curated)

setdiff(dts[, id, meta=T], dts_curated[, id, meta=T])

#lifespan_dts_2 = dts[t %between% c(days(0), days(9))]
#lifespan_dts_2 = dts[, .(lifespan = max(t)), by=id]
dts_dead = lifespan_dts[which(dts$id %in% setdiff(dts[, id, meta=T], dts_curated[, id, meta=T])),]

dts_dead = rejoin(lifespan_dts)
dts_dead$lifespan = max(dts_dead$lifespan) - dts_dead$lifespan



#curate time to only 9 days
dts_curated = dts_curated[t %between% c(days(0), days(9))]

#check dead are removed
ggetho(dts_curated, aes(z=activity)) +
  stat_tile_etho() +
  stat_ld_annotations()

head(dts_curated[meta=T])

#Summarise data per animal
summary_dt <- rejoin(dts_curated[, .(sleep_fraction = mean(asleep),
                                     activity = activity)
                                 , by=id])
summary_dt

dts_morning = dts_curated[t %between% c(days(0.5), days(8.5))]
head(dts_morning[meta=T])

Lights_on = (which((dts_morning$t/86400)%%1 == 0))

set_creation = function(first, bin_size){
  a = 0
  a = a[-1]
  for (i in c(1,(bin_size/5))){
    a = cbind((first - i),a)
  }
  return(a)
}

bin_size = 60 #in minutes
bm1 = set_creation(Lights_on, bin_size)
bm2 = set_creation((bm1[,1]), bin_size)
bm3 = set_creation((bm2[,1]), bin_size)
bp1 = set_creation((Lights_on+(bin_size/5)), bin_size)
bm3h = bm1 = set_creation(Lights_on, 180)
bm6h = bm1 = set_creation(Lights_on, 180*2)

dts_morning_cut = dts_morning[Lights_on,]
head(dts_morning_cut[meta=T])

summing = function(x,  first, last) {
  a = 0
  a = a[-1]
  for (i in 1:length(first)){
    a = c(a,sum(x[c(first[i]:last[i])]))
  }
  return(a)
}


dt_bm1 = summing(dts_morning$activity, bm1[,1], bm1[,2])
dt_bm2 = summing(dts_morning$activity, bm2[,1], bm2[,2])
dt_bm3 = summing(dts_morning$activity, bm3[,1], bm3[,2])
dt_bp1 = summing(dts_morning$activity, bp1[,1], bp1[,2])
dt_bm3h = summing(dts_morning$activity, bm3h[,1], bm3h[,2])
dt_bm6h = summing(dts_morning$activity, bm6h[,1], bm6h[,2])

dts_morning_cut$bm1 = dt_bm1
dts_morning_cut$bm2 = dt_bm2
dts_morning_cut$bm3 = dt_bm3
dts_morning_cut$bp1 = dt_bp1
dts_morning_cut$bm3h = dt_bm3h
dts_morning_cut$bm6h = dt_bm6h

dts_morning_cut$AI = (dts_morning_cut$bm1*(dts_morning_cut$bm1-dts_morning_cut$bm2)*(dts_morning_cut$bm2-dts_morning_cut$bm3))/dts_morning_cut$bp1

dts_morning_cut$AI_2 = dts_morning_cut$bm3h/dts_morning_cut$bm6h
head(dts_morning_cut[meta=T])
#dts_morning_cut = dts_morning_cut[is.finite(dts_morning_cut$AI),]
dts_morning_cut = dts_morning_cut[is.finite(dts_morning_cut$AI_2),]
#dts_morning_cut = dts_morning_cut[!is.na(dts_morning_cut$AI),]
dts_morning_cut = dts_morning_cut[!is.na(dts_morning_cut$AI_2),]

dts_morning_cut= rejoin(dts_morning_cut[,.(AI = mean(AI), AI_2 = mean(AI_2)), by = id])
dts_morning_cut$file_info = NULL

#NIGHT AND DAY
dts_curated[, phase  :=ifelse(t %% hours(24) < hours(12), "L", "D")]
summary_dt <- rejoin(dts_curated[, .(
  sleep_fraction_all = mean(asleep), 
  sleep_fraction_l = mean(asleep[phase == "L"]), 
  sleep_fraction_d = mean(asleep[phase == "D"]),
  activity_all = sum(activity),
  activity_day = sum(activity[phase == "L"]),
  activity_night = sum(activity[phase == "D"])),, 
  by=id])
summary_dt

#two at once
summary_dt_melted <- melt(summary_dt , measure.vars  = patterns("sleep_fraction_"),
                          variable.name = "phase", value.name = "sleep_fraction")

##STATS

#two way anova
model <- aov(sleep_fraction_all ~ Condition * genotype, summary_dt)
summary(model)

#SLEEP Architecture
#bout analysis
bout_dt <- bout_analysis(asleep, dts_curated)
bout_dt <- bout_dt[asleep == TRUE, -"asleep"]

bout_dt_daytime <- bout_analysis(asleep, dts_curated[phase == "L"])
bout_dt_daytime <- bout_dt_daytime[asleep == TRUE, -"asleep"]

bout_dt_nighttime <- bout_analysis(asleep, dts_curated[phase == "D"])
bout_dt_nighttime <- bout_dt_nighttime[asleep == TRUE, -"asleep"]


#architecture description
bout_dt[, .(n_bouts = .N, mean_bout_length = mean(duration)), by=id]
bout_dt_latency = bout_dt[,.(
  latency = t[1],
  first_bout_length = duration[1],
  latency_to_longest_bout = t[which.max(duration)],
  length_longest_bout = max(duration)
), by = id]

boutdt = bout_dt[, .(n_bouts = .N, mean_bout_length = mean(duration)), by=id]
boutnrdaytime = bout_dt_daytime[, .(n_bouts = .N, mean_bout_length = mean(duration)), by=id]
boutnrnightime = bout_dt_nighttime[, .(n_bouts = .N, mean_bout_length = mean(duration)), by=id]

bout_summary = as.data.frame(cbind(boutdt,
                                   latency = bout_dt_latency$latency,
                                   first_bout_length = bout_dt_latency$first_bout_length,
                                   latency_to_longest_bout = bout_dt_latency$latency_to_longest_bout,
                                   length_longest_bout = bout_dt_latency$length_longest_bout,
                                   daytime_boutlength = boutnrdaytime$mean_bout_length,
                                   nighttime_boutlength = boutnrnightime$mean_bout_length,
                                   daytime_boutnr = boutnrdaytime$n_bouts,
                                   nighttime_boutnr = boutnrnightime$n_bouts))

#merging all statistics
overall_summary <- summary_dt[bout_summary]
overall_summary

#periodicity modelling
per_xsq_dt <- periodogram(activity, 
                          dts_curated,
                          FUN = ac_periodogram)
per_xsq_dt

per_xsq_dt <- find_peaks(per_xsq_dt)

summary_dt <- rejoin(per_xsq_dt[peak==1])


dt_days <- dts_curated
dt_days [,day := floor(t/days(1))]
dt_days

summary_days <- rejoin(dt_days[, .
                               (sleep_day1 = mean(asleep[day == 0]), 
                                 sleep_day2 = mean(asleep[day == 1]), 
                                 sleep_day3 = mean(asleep[day == 2]), 
                                 sleep_day4 = mean(asleep[day == 3]), 
                                 sleep_day5 = mean(asleep[day == 4]), 
                                 sleep_day6 = mean(asleep[day == 5]), 
                                 sleep_day7 = mean(asleep[day == 6]), 
                                 sleep_day8 = mean(asleep[day == 7]), 
                                 sleep_day9 = mean(asleep[day == 8])),
                               
                               by=id])
summary_days_melted <- melt(summary_days, measure.vars = patterns("sleep_day"),
                            variable.name = "Sleep_per_day",
                            value.name= "avrg_sleep")
summary_days_melted

dt_daytime <- dts_curated
dt_daytime [,day := floor(t/days(1))]
dt_daytime

summary_daytime <- rejoin(dt_daytime[, .
                               (sleep_day1 = mean(asleep[day == 0 & phase == "L"]), 
                                 sleep_day2 = mean(asleep[day == 1 & phase == "L"]), 
                                 sleep_day3 = mean(asleep[day == 2 & phase == "L"]), 
                                 sleep_day4 = mean(asleep[day == 3 & phase == "L"]), 
                                 sleep_day5 = mean(asleep[day == 4 & phase == "L"]), 
                                 sleep_day6 = mean(asleep[day == 5 & phase == "L"]), 
                                 sleep_day7 = mean(asleep[day == 6 & phase == "L"]), 
                                 sleep_day8 = mean(asleep[day == 7 & phase == "L"]), 
                                 sleep_day9 = mean(asleep[day == 8 & phase == "L"])),
                               
                               by=id])
summary_daytime_melted <- melt(summary_daytime, measure.vars = patterns("sleep_day"),
                            variable.name = "Sleep_per_day",
                            value.name= "avrg_sleep")
summary_daytime_melted

dt_nighttime <- dts_curated
dt_nighttime [,day := floor(t/days(1))]
dt_nighttime

summary_nighttime <- rejoin(dt_nighttime[, .
                                     (sleep_day1 = mean(asleep[day == 0 & phase == "D"]), 
                                       sleep_day2 = mean(asleep[day == 1 & phase == "D"]), 
                                       sleep_day3 = mean(asleep[day == 2 & phase == "D"]), 
                                       sleep_day4 = mean(asleep[day == 3 & phase == "D"]), 
                                       sleep_day5 = mean(asleep[day == 4 & phase == "D"]), 
                                       sleep_day6 = mean(asleep[day == 5 & phase == "D"]), 
                                       sleep_day7 = mean(asleep[day == 6 & phase == "D"]), 
                                       sleep_day8 = mean(asleep[day == 7 & phase == "D"]), 
                                       sleep_day9 = mean(asleep[day == 8 & phase == "D"])),
                                     
                                     by=id])
summary_nighttime_melted <- melt(summary_nighttime, measure.vars = patterns("sleep_day"),
                               variable.name = "Sleep_per_day",
                               value.name= "avrg_sleep")
summary_nighttime_melted


summary_actdays = rejoin(dt_days[, .
                                 (act_day1 = sum(activity[day == 0]), 
                                   act_day2 = sum(activity[day == 1]), 
                                   act_day3 = sum(activity[day == 2]), 
                                   act_day4 = sum(activity[day == 3]), 
                                   act_day5 = sum(activity[day == 4]), 
                                   act_day6 = sum(activity[day == 5]), 
                                   act_day7 = sum(activity[day == 6]), 
                                   act_day8 = sum(activity[day == 7]), 
                                   act_day9 = sum(activity[day == 8])),
                                 by = id])
summary_actdays_melted = melt(summary_actdays, measure.vars = patterns("act_day"), 
                              variable.name = "Day", 
                              value.name = "avrg_act")
summary_actdays


dt_bout_days <- bout_dt
dt_bout_days [,day := floor(t/days(1))]
summary_bout_days <- rejoin(dt_bout_days[, . 
                                         (bout_day1 = mean(duration[day == 0]), 
                                           bout_day2 = mean(duration[day == 1]), 
                                           bout_day3 = mean(duration[day == 2]), 
                                           bout_day4 = mean(duration[day == 3]), 
                                           bout_day5 = mean(duration[day == 4]), 
                                           bout_day6 = mean(duration[day == 5]), 
                                           bout_day7 = mean(duration[day == 6]), 
                                           bout_day8 = mean(duration[day == 7]),
                                           bout_day9 = mean(duration[day == 8])), 
                                         by=id])
summary_bout_days_melted <- melt(summary_bout_days, measure.vars = patterns("bout_day"), 
                                 variable.name =  "Bout_per_day", 
                                 value.name = "mean_bout_length")
summary_bout_days_melted

dt_boutdays_daytime <- bout_dt_daytime
dt_boutdays_daytime [,day := floor(t/days(1))]
summary_boutdays_daytime <- rejoin(dt_boutdays_daytime[, . 
                                         (bout_day1 = mean(duration[day == 0]), 
                                           bout_day2 = mean(duration[day == 1]), 
                                           bout_day3 = mean(duration[day == 2]), 
                                           bout_day4 = mean(duration[day == 3]), 
                                           bout_day5 = mean(duration[day == 4]), 
                                           bout_day6 = mean(duration[day == 5]), 
                                           bout_day7 = mean(duration[day == 6]), 
                                           bout_day8 = mean(duration[day == 7]),
                                           bout_day9 = mean(duration[day == 8])), 
                                         by=id])
summary_boutdays_daytime_melted <- melt(summary_boutdays_daytime, measure.vars = patterns("bout_day"), 
                                 variable.name =  "Bout_per_day", 
                                 value.name = "mean_bout_length")
summary_boutdays_daytime_melted

dt_boutdays_nighttime <- bout_dt_nighttime
dt_boutdays_nighttime [,day := floor(t/days(1))]
summary_boutdays_nighttime <- rejoin(dt_boutdays_nighttime[, . 
                                                           (bout_day1 = mean(duration[day == 0]), 
                                                             bout_day2 = mean(duration[day == 1]), 
                                                             bout_day3 = mean(duration[day == 2]), 
                                                             bout_day4 = mean(duration[day == 3]), 
                                                             bout_day5 = mean(duration[day == 4]), 
                                                             bout_day6 = mean(duration[day == 5]), 
                                                             bout_day7 = mean(duration[day == 6]), 
                                                             bout_day8 = mean(duration[day == 7]),
                                                             bout_day9 = mean(duration[day == 8])), 
                                                           by=id])
summary_boutdays_nighttime_melted <- melt(summary_boutdays_nighttime, measure.vars = patterns("bout_day"), 
                                          variable.name =  "Bout_per_day", 
                                          value.name = "mean_bout_length")
summary_boutdays_nighttime_melted



summary_boutnr_days <- rejoin(dt_bout_days[, . 
                                           (bout_day1 = length(duration[day == 0]), 
                                             bout_day2 = length(duration[day == 1]), 
                                             bout_day3 = length(duration[day == 2]), 
                                             bout_day4 = length(duration[day == 3]), 
                                             bout_day5 = length(duration[day == 4]), 
                                             bout_day6 = length(duration[day == 5]), 
                                             bout_day7 = length(duration[day == 6]), 
                                             bout_day8 = length(duration[day == 7]),
                                             bout_day9 = length(duration[day == 8])), 
                                           by=id])
summary_boutnr_days_melted <- melt(summary_boutnr_days, measure.vars = patterns("bout_day"), 
                                   variable.name =  "Boutnr_per_day", 
                                   value.name = "boutnr")
summary_boutnr_days_melted

summary_boutnrdays_nighttime <- rejoin(dt_boutdays_nighttime[, . 
                                                             (bout_day1 = length(duration[day == 0]), 
                                                               bout_day2 = length(duration[day == 1]), 
                                                               bout_day3 = length(duration[day == 2]), 
                                                               bout_day4 = length(duration[day == 3]), 
                                                               bout_day5 = length(duration[day == 4]), 
                                                               bout_day6 = length(duration[day == 5]), 
                                                               bout_day7 = length(duration[day == 6]), 
                                                               bout_day8 = length(duration[day == 7]),
                                                               bout_day9 = length(duration[day == 8])), 
                                                             by=id])
summary_boutnrdays_nighttime_melted <- melt(summary_boutnrdays_nighttime, measure.vars = patterns("bout_day"), 
                                            variable.name =  "Boutnr_per_day", 
                                            value.name = "boutnr")
summary_boutnrdays_nighttime_melted

summary_boutnrdays_daytime <- rejoin(dt_boutdays_daytime[, . 
                                                         (bout_day1 = length(duration[day == 0]), 
                                                           bout_day2 = length(duration[day == 1]), 
                                                           bout_day3 = length(duration[day == 2]), 
                                                           bout_day4 = length(duration[day == 3]), 
                                                           bout_day5 = length(duration[day == 4]), 
                                                           bout_day6 = length(duration[day == 5]), 
                                                           bout_day7 = length(duration[day == 6]), 
                                                           bout_day8 = length(duration[day == 7]),
                                                           bout_day9 = length(duration[day == 8])), 
                                                         by=id])
summary_boutnrdays_daytime_melted <- melt(summary_boutnrdays_daytime, measure.vars = patterns("bout_day"), 
                                          variable.name =  "Boutnr_per_day", 
                                          value.name = "boutnr")
summary_boutnrdays_daytime_melted

##############################################################################################

summary_789days <- rejoin(dt_days[, .
                               (sleep = mean(asleep[day %in% c(7,8,9)])),
                               by=id])

summary_789days_melted <- melt(summary_789days, measure.vars = patterns("sleep"),
                            value.name= "avrg_sleep")
summary_789days_melted

summary_789days_daytime <- rejoin(dt_daytime[, .
                                  (sleep = mean(asleep[day %in% c(7,8,9)])),
                                  by=id])

summary_789days_daytime_melted <- melt(summary_789days_daytime, measure.vars = patterns("sleep"),
                               value.name= "avrg_sleep")
summary_789days_daytime_melted

summary_789days_nighttime <- rejoin(dt_nighttime[, .
                                             (sleep = mean(asleep[day %in% c(7,8,9)])),
                                             by=id])

summary_789days_nighttime_melted <- melt(summary_789days_nighttime, measure.vars = patterns("sleep"),
                                       value.name= "avrg_sleep")
summary_789days_nighttime_melted

summary_act789days = rejoin(dt_days[, .
                                 (act = sum(activity[day == 8])),
                                 by = id])
summary_act789days_melted = melt(summary_act789days, measure.vars = patterns("act"), 
                              value.name = "avrg_act")
summary_act789days

summary_bout_789days <- rejoin(dt_bout_days[, . 
                                         (bout_length = mean(duration[day  %in% c(7,8,9)])), 
                                         by=id])
summary_bout_789days_melted <- melt(summary_bout_789days, measure.vars = patterns("bout_length"),  
                                 value.name = "bout_length")
summary_bout_789days_melted

summary_boutdaytime_789days <- rejoin(dt_boutdays_daytime[, . 
                                            (bout_length = mean(duration[day  %in% c(7,8,9)])), 
                                            by=id])
summary_boutdaytime_789days_melted <- melt(summary_boutdaytime_789days, measure.vars = patterns("bout_length"),  
                                    value.name = "bout_length")
summary_boutdaytime_789days_melted

summary_boutnighttime_789days <- rejoin(dt_boutdays_nighttime[, . 
                                                          (bout_length = mean(duration[day  %in% c(7,8,9)])), 
                                                          by=id])
summary_boutnighttime_789days_melted <- melt(summary_boutnighttime_789days, measure.vars = patterns("bout_length"),  
                                           value.name = "bout_length")
summary_boutnighttime_789days_melted

summary_boutnr_789days <- rejoin(dt_bout_days[, . 
                                           (boutnr = length(duration[day %in% c(7,8,9)])), 
                                           by=id])
summary_boutnr_789days_melted <- melt(summary_boutnr_789days, measure.vars = patterns("boutnr"),  
                                   value.name = "boutnr")
summary_boutnr_789days_melted


summary_boutnrdaytime_789days <- rejoin(dt_boutdays_daytime[, . 
                                              (boutnr = length(duration[day %in% c(7,8,9)])), 
                                              by=id])
summary_boutnrdaytime_789days_melted <- melt(summary_boutnrdaytime_789days, measure.vars = patterns("boutnr"),  
                                      value.name = "boutnr")
summary_boutnrdaytime_789days_melted

summary_boutnrnighttime_789days <- rejoin(dt_boutdays_nighttime[, . 
                                                            (boutnr = length(duration[day %in% c(7,8,9)])), 
                                                            by=id])
summary_boutnrnighttime_789days_melted <- melt(summary_boutnrnighttime_789days, measure.vars = patterns("boutnr"),  
                                             value.name = "boutnr")
summary_boutnrnighttime_789days_melted

overall_789days_summary = cbind(summary_789days_melted,
                                avrgsleep_daytime = summary_789days_daytime_melted$avrg_sleep,
                                avrgsleep_nighttime = summary_789days_nighttime_melted$avrg_sleep,
                                avrg_act = summary_act789days_melted$avrg_act, 
                                bout_length = summary_bout_789days_melted$bout_length, 
                                bout_length_daytime = summary_boutdaytime_789days_melted$bout_length,
                                bout_length_nighttime = summary_boutnighttime_789days_melted$bout_length,
                                boutnr = summary_boutnr_789days_melted$boutnr,
                                boutnr_daytime = summary_boutnrdaytime_789days_melted$boutnr,
                                boutnr_nighttime = summary_boutnrnighttime_789days_melted$boutnr)

#######################################################################################################
#######################################################################################################

overall_days_summary = cbind(summary_days_melted,
                             daytime_slptme = summary_daytime_melted$avrg_sleep,
                             nighttime_slptme = summary_nighttime_melted$avrg_sleep,
                             avrg_act = summary_actdays_melted$avrg_act,
                             bout_length = summary_bout_days_melted$mean_bout_length,
                             bout_daytime_length = summary_boutdays_daytime_melted$mean_bout_length,
                             bout_nighttime_length = summary_boutdays_nighttime_melted$mean_bout_length,
                             boutnr = summary_boutnr_days_melted$boutnr,
                             boutnr_daytime = summary_boutnrdays_daytime_melted$boutnr,
                             boutnr_nighttime = summary_boutnrdays_nighttime_melted$boutnr)


list = unique(overall_summary$genotype)

Foldchange = function(overall_summary) {
  intact = overall_summary[Condition == "Intact" & genotype == name]$sleep_fraction_all
  injured = as.data.frame(
    cbind(id = overall_summary[Condition == "Injured" & genotype == name]$id, 
                                sleeptime = overall_summary[Condition == "Injured" & genotype == name]$sleep_fraction_all))
  name = as.data.frame(cbind(Foldchange = injured$sleeptime/(mean(intact))*100, id = 1:length(injured$id), name = name))
}

all_foldchanges = as.data.frame(cbind(Foldchange = 0, id = 0, name = 0))

for (i in 1:length(list)) {
  name = list[i]
  foldchange = Foldchange(overall_sum)
  write.csv(foldchange, paste0("Foldchange_", list[i], ".csv"))
  set = foldchange
  all_foldchanges = rbind(all_foldchanges, set)
}

write.csv(all_foldchanges, "all_foldchanges.csv")

FoldchangeBtlength = function(overall_summary) {
  intact = overall_summary[Condition == "Intact" & genotype == name]$mean_bout_length
  injured = as.data.frame(
    cbind(id = overall_summary[Condition == "Injured" & genotype == name]$id, 
          boutlength = overall_summary[Condition == "Injured" & genotype == name]$mean_bout_length))
  name = as.data.frame(cbind(Foldchange = injured$boutlength/(mean(intact))*100, id = 1:length(injured$id), name = name))
}

all_foldchangesbtlength = as.data.frame(cbind(Foldchange = 0, id = 0, name = 0))

for (i in 1:length(list)) {
  name = list[i]
  foldchange = FoldchangeBtlength(overall_summary)
  set = foldchange
  all_foldchangesbtlength = rbind(all_foldchangesbtlength, set)
}

write.csv(all_foldchangesbtlength, "all_foldchangesbtlength.csv")

FoldchangeBtNr = function(overall_summary) {
  intact = overall_summary[Condition == "Intact" & genotype == name]$n_bouts
  injured = as.data.frame(
    cbind(id = overall_summary[Condition == "Injured" & genotype == name]$id, 
          boutlength = overall_summary[Condition == "Injured" & genotype == name]$n_bouts))
  name = as.data.frame(cbind(Foldchange = injured$boutlength/(mean(intact))*100, id = 1:length(injured$id), name = name))
}

all_foldchangesbtnr = as.data.frame(cbind(Foldchange = 0, id = 0, name = 0))

for (i in 1:length(list)) {
  name = list[i]
  foldchange = FoldchangeBtNr(overall_summary)
  set = foldchange
  all_foldchangesbtnr = rbind(all_foldchangesbtnr, set)
}

write.csv(all_foldchangesbtnr, "all_foldchangesbtnr.csv")


final_days <- rejoin(dt_days[, .
                             (sleep = mean(asleep[day %in% c(6,7,8)])),
                             by=id])
final_days_melted = melt(final_days, measure.vars = "sleep",
                         variable.name =  "FinalDay", 
                         value.name = "SleepTime")

Foldchange789 = function(overall_summary) {
  intact = final_days_melted[Condition == "Intact" & genotype == name]$SleepTime
  injured = as.data.frame(
    cbind(id = final_days_melted[Condition == "Injured" & genotype == name]$id, 
          sleeptime = final_days_melted[Condition == "Injured" & genotype == name]$SleepTime))
  name = as.data.frame(cbind(Foldchange = injured$sleeptime/(mean(intact))*100, id = 1:length(injured$id), name = name))
}

all_foldchanges = as.data.frame(cbind(Foldchange = 0, id = 0, name = 0))

for (i in 1:length(list)) {
  name = list[i]
  foldchange = Foldchange789(final_days_melted)
  write.csv(foldchange, paste0("Foldchange789_", list[i], ".csv"))
  set = foldchange
  all_foldchanges = rbind(all_foldchanges, set)
}

write.csv(all_foldchanges, "all_foldchanges789.csv")

list2 = list.files(pattern = "Foldchange789_")

for (filename in list2)
 filename = read.csv(filename)
 
temp = overall_summary
temp$file_info = NULL
write.csv(temp, "overallsummary.csv")

temp = overall_days_summary
temp$file_info = NULL
write.csv(temp, "overall_days_summary.csv")

temp = overall_789days_summary
temp$file_info = NULL
write.csv(temp, "overall_789days_summary.csv")

temp = summary_dt
temp$file_info = NULL
write.csv(temp, "summary_dt.csv")

temp = dts_dead
temp$file_info = NULL
write.csv(temp, "dead.csv")

temp = dts_morning_cut
temp$file_info = NULL
write.csv(temp, "morning_ai.csv")

######################################################
          #Making Fragmentation Plots
#####################################################

bout_dt_graphing = dts_curated
bout_dt_graphing$activity = 0
bout_dt_graphing$activity = as.double(bout_dt_graphing$activity)

for(i in 1:length(unique(bout_dt_graphing$id))) {
  progress((i/length(unique(bout_dt_graphing$id)))*100)
  Sys.sleep(0.01)
  flush.console()
  rows = which(bout_dt$id == unique(bout_dt$id)[i])
  sets = cbind(which(bout_dt_graphing$id %in% unique(bout_dt_graphing$id)[i] &
                       bout_dt_graphing$t %in% bout_dt[rows,]$t),
               (which(bout_dt_graphing$id %in% unique(bout_dt_graphing$id)[i] & 
                        bout_dt_graphing$t %in% bout_dt[rows,]$t) + ((bout_dt[rows,]$duration/300)-1)))
  for (j in 1:length(rows)) {
    bout_dt_graphing[c(sets[j,1]:sets[j,2]),]$activity = bout_dt[rows[j],]$duration
  }
}


bout_dt <- bout_analysis(asleep, dts_curated)
bout_dt <- bout_dt[asleep == TRUE, -"asleep"]

mean_bout_dt <- bout_dt[, .(n_bouts = .N),  by=id]
new_meta <- dts_curated[mean_bout_dt,  meta=T]
setmeta(bout_dt_graphing, new_meta)

setmeta(bout_dt_graphing, metadata)

dt_test = new_meta[, list(
  id = bout_dt_graphing$id,
  t = bout_dt_graphing$t,
  activity = bout_dt_graphing$activity,
  phase = bout_dt_graphing$phase,
  day = bout_dt_graphing$day
)]

setmeta(dt_test, new_meta)

#######################################################
                   # PLOTS
#######################################################

#####Fragmentation##########

p = theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          legend.title=element_blank(),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(colour = "black", size = 2),
          axis.text.x = element_text(size = 10, face = "bold", colour = "black", angle = 90),
          axis.text.y = element_blank(),
          legend.text = element_text(size = 14, face = "bold", colour = "black"),
          axis.line= element_line(colour = "white", size = 0)) #to make it pretty



ggetho(bout_dt_graphing[xmv(Condition) == "Intact" & day == 5],
       aes(y = interaction(id, n_bouts, sep=" : "),z=scales::rescale(activity, to = c(0,1),
                                                                     from = c(0, 12*3600)))) +
  scale_fill_gradientn(colours = c("white", "#00BFC4", "skyblue4", "skyblue4"),
                       values = c(0, 1/12, 2/12, 1),
                       limits = c(0,1)) +
  stat_tile_etho(method = max) + p

ggetho(bout_dt_graphing[xmv(Condition) == "Injured" & day == 5], 
       aes(y = interaction(id, n_bouts, sep=" : "),z=scales::rescale(activity, to = c(0,1),
                                                                     from = c(0, 12*3600)))) +
  scale_fill_gradientn(colours = c("white", "#F8766D", "pink4", "pink4"),
                      values = c(0, 1/12, 2/12, 1),
                      limits = c(0,1)) +
  stat_tile_etho(method = max) + p

list = unique(metadata$genotype)

i = 3


ggetho(dt_test[xmv(Condition) == "Intact" & xmv(genotype) == list[i]],
       aes(y = interaction(id, -n_bouts, sep=" : "),z=scales::rescale(activity, to = c(0,1),
                                                                      from = c(0, 12*3600)))) +
  scale_fill_gradientn(colours = c("white", "#00BFC4", "skyblue4", "skyblue4"),
                       values = c(0, 1/12, 2/12, 1),
                       limits = c(0,1),
                       na.value = "skyblue4") +
  scale_x_continuous(breaks = seq(0,86400*9,43200)) +
  ylab(paste0(list[i])) + 
  stat_tile_etho(method = max) + p

ggetho(dt_test[xmv(Condition) == "Injured" & xmv(genotype) == list[i]], 
       aes(y = interaction(id, -n_bouts, sep=" : "),z=scales::rescale(activity, to = c(0,1),
                                                                      from = c(0, 12*3600)))) +
  scale_fill_gradientn(colours = c("white", "#F8766D", "pink4", "pink4"),
                       values = c(0, 1/12, 2/12, 1),
                       limits = c(0,1),
                       na.value = "pink4") +
  scale_x_continuous(breaks = seq(0,86400*9,43200)) +
  ylab(paste0(list[i])) + 
  stat_tile_etho(method = max) + p


######OTHER##########

  
ggplot(overall_summary, aes(x=(mean_bout_length/3600), y=n_bouts, colour = Condition)) + 
  geom_jitter(size = 6) +  
  facet_grid( genotype ~ .) + 
  scale_color_manual(values = c( "#00BFC4", "#F8766D")) + 
  scale_fill_manual(values = c( "#00BFC4", "#F8766D")) + 
  scale_y_continuous(name= "bout number") + 
  scale_x_continuous(name = "bout duration (hrs)") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.title=element_blank(), 
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.ticks.y = element_line(colour = "black", size = 2),
        axis.ticks.x = element_line(colour = "black", size = 2),
        axis.text.x = element_text(size = 28, face = "bold", colour = "black", family ="Arial"),
        axis.text.y = element_text(size = 28, face = "bold", colour = "black", family ="Arial"),
        legend.text = element_text(size = 14, face = "bold", colour = "black", family ="Arial"),
        axis.line= element_line(colour = "black", size = 2))

#wrap Plot Sleep



ggetho(dts_curated, aes(x=t, y=asleep, colour=Condition), time_wrap = hours(24)) + 
  stat_pop_etho() + 
  stat_ld_annotations() +
  ylab("Sleep Time (%)") +
  scale_color_manual(values = c( "#00BFC4", "#F8766D")) + 
  scale_fill_manual(values = c( "#00BFC4", "#F8766D")) + 
  scale_y_continuous(name= "Sleep Time (%)", labels = c("0", "25", "50", "75", "100")) +
  facet_grid(genotype ~ .) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.title=element_blank(), 
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.ticks.y = element_line(colour = "black", size = 2),
        axis.ticks.x = element_line(colour = "black", size = 2),
        axis.text.x = element_text(size = 28, face = "bold", colour = "black", family ="Arial"),
        axis.text.y = element_text(size = 28, face = "bold", colour = "black", family ="Arial"),
        legend.text = element_text(size = 14, face = "bold", colour = "black", family ="Arial"),
        axis.line= element_line(colour = "black", size = 2))



#Wrap Plot Activity

ggetho(dts_curated, aes(x=t, y=activity, colour=Condition), time_wrap = hours(24), time_offset = hours(6)) + 
  stat_pop_etho(size=2) + 
  stat_ld_annotations(height=1, alpha=0.3, outline = NA) +
  stat_ld_annotations(size = 2) +
  ylab("Activity") +
  scale_color_manual(values = c( "#00BFC4", "#F8766D")) + 
  scale_fill_manual(values = c( "#00BFC4", "#F8766D")) + 
  facet_grid(genotype ~ .) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.title=element_blank(), 
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.ticks.y = element_line(colour = "black", size = 2),
        axis.ticks.x = element_line(colour = "black", size = 2),
        axis.text.x = element_text(size = 28, face = "bold", colour = "black", family ="Arial"),
        axis.text.y = element_text(size = 28, face = "bold", colour = "black", family ="Arial"),
        legend.text = element_text(size = 14, face = "bold", colour = "black", family ="Arial"),
        axis.line= element_line(colour = "black", size = 2))

ggetho(dts_curated, aes(x=t, y=activity, colour=Condition)) + 
  stat_pop_etho(size=2) + 
  stat_ld_annotations(size = 2) +
  ylab("Activity") +
  scale_color_manual(values = c( "#00BFC4", "#F8766D")) + 
  scale_fill_manual(values = c( "#00BFC4", "#F8766D")) + 
  facet_grid(genotype ~ .) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.title=element_blank(), 
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.ticks.y = element_line(colour = "black", size = 2),
        axis.ticks.x = element_line(colour = "black", size = 2),
        axis.text.x = element_text(size = 28, face = "bold", colour = "black", family ="Arial"),
        axis.text.y = element_text(size = 28, face = "bold", colour = "black", family ="Arial"),
        legend.text = element_text(size = 14, face = "bold", colour = "black", family ="Arial"),
        axis.line= element_line(colour = "black", size = 2))



#plot sleep latency
filename = paste("sleep_latency.pdf")
pdf(filename)
ggplot(overall_summary, aes(n_bouts, mean_bout_length, colour=Condition)) +
  geom_point() +
  scale_x_continuous(name="Number of bouts") +
  scale_y_continuous(name="Average bout duration (s)") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line= element_line(colour = "black"))
dev.off()

filename = paste("Sleep_latency2.pdf")
pdf(filename)
ggplot(overall_summary, aes(latency / 60, sleep_fraction_all, colour=Condition)) +
  geom_point() +
  geom_smooth(method="lm", level = 0.95, alpha=0.5)+
  facet_grid(genotype ~ .) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line= element_line(colour = "black"))
dev.off()

#coordinate plot
filename = paste("Coordinate_plot.pdf")
pdf(filename)
pl <- ggetho(dts_curated, aes(x=t, y=moving, colour=Condition), time_wrap = days(1)) + 
  stat_ld_annotations(height=1, 
                      alpha=.2, 
                      x_limits = c(0, days(1)), 
                      outline = NA) + 
  facet_grid( genotype ~ .) + 
  geom_smooth(level = .95) + 
  scale_color_manual(values = wes_palette(n=2, name="GrandBudapest1")) +
  scale_fill_manual(values = wes_palette(n=2, name="GrandBudapest1")) +
  stat_pop_etho(geom = "polygon", fill=NA)
pl + coord_polar()
dev.off()

#Periodograms

ggperio(per_xsq_dt, aes(
  y = power-signif_threshold,
  colour=Condition)) +
  stat_pop_etho(size=2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 2) +
  geom_vline(xintercept = 86400, linetype = "dashed", alpha = 0.3, size = 2) +
  scale_color_manual(values = c( "#00BFC4", "#F8766D")) + 
  scale_fill_manual(values = c( "#00BFC4", "#F8766D")) + 
  scale_y_continuous(name= "Power") + 
  facet_grid( genotype ~ .) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.title=element_blank(), 
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.ticks.y = element_line(colour = "black", size = 2),
        axis.ticks.x = element_line(colour = "black", size = 2),
        axis.text.x = element_text(size = 28, face = "bold", colour = "black", family ="Arial"),
        axis.text.y = element_text(size = 28, face = "bold", colour = "black", family ="Arial"),
        legend.text = element_text(size = 14, face = "bold", colour = "black", family ="Arial"),
        axis.line= element_line(colour = "black", size = 2))


