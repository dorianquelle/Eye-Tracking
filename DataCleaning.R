#libraries to include
library(readxlsx)
library(tidyverse)
library(janitor)
library(data.table)


#Data outside of Function
aois <- readxl::read_xlsx("data/export/AOI_neu2.xlsx")

#Fragebögen
fragebogen2 <-fread("data/part2_data_clean.csv")
fragebogen1 <- fread("data/part1_data_clean.csv")


#get all paths to folders
paths <- paste0("data/fulldata/",list.files("data/fulldata")[grepl(x = list.files("data/fulldata"), pattern = "^Data")])

#get path to specific teil2
for(i in 1:length(paths)){
  teil2_paths <- list.files(paths[i])[grep("teil2.xlsx",list.files(paths[i]))]
  if(length(teil2_paths) != 0){
    teil2_path[i] <- teil2_paths[!grepl(x = teil2_paths, pattern = "^~")]
  }else{
    teil2_path[i] <- NA
  }
}


path_df <- data.frame("path" = paths, "teil2_path" = teil2_path, "name" = sub(x = paths,pattern = ".* - ",replacement = ""))
path_df <- path_df[complete.cases(path_df), ]


# Function needs to start here
data_extractor <- function(path, teil2_path, name, fragebogen2_path = "part2_data.csv",fragebogen1_path = "part1_data.csv"){
  # create a data.frame to append to
  df <- 
    data.frame("time_to_first_fixation" = numeric(),
               "number_of_fixations_aoi" = numeric(),
               "number_of_fixations_total" = numeric(),
               "mean_duration_fixation_aoi" = numeric(),
               "avg_pupil_dilation_aoi" = numeric(),
               "avg_pupil_dilation_total" = numeric(),
               "salience" = character(),
               "disc" = character(),
               "name" = character())
  
  #Loop of files needs to begin here
  
  
  #Import Fragebogen
  fragebogen2_data <- fragebogen2[fragebogen2$name == name,]
  fragebogen1_data <- fragebogen1[fragebogen1$name == name,]
  
  pictureid <-
    fragebogen2_data$FinalList %>% 
    as.character() %>%
    strsplit(",") %>% 
    unlist
  
  disclosureid <-
    fragebogen2_data$FinalListdisc %>% 
    as.character() %>%
    strsplit(",") %>% 
    unlist
  
  #import specific teil2
  teil2_data <- readxl::read_xlsx(paste0(path,"/",teil2_path))
  

  #clean colnames teil2
  teil2_data <- teil2_data %>% clean_names()
  
  #remove chunk of nonsensical data
  teil2_data <- teil2_data[,-c(grep(x = colnames(teil2_data),pattern = "^average"))]
  
  #Clean the damn names
  if(colnames(teil2_data)[1] == "recording_timestamp_ms"){
    colnames(teil2_data) <- sub(x = colnames(teil2_data),pattern = "_ms$",replacement = "")
    colnames(teil2_data) <- sub(x = colnames(teil2_data),pattern = "_dacs_px$",replacement = "")
    colnames(teil2_data) <- sub(x = colnames(teil2_data),pattern = "_mm$",replacement = "")
  }
  
  indexofnewurls <- which(teil2_data$event == "URLStart")
  indexofstopurls <- which(teil2_data$event == "URLEnd")
  indexofurls <- cbind(indexofnewurls,indexofstopurls)[4+4*c(1:8),]

  for(i in 1:8){
    #group by dataset per image
    teil2_data_bild <- teil2_data[c(indexofurls[i,1]:indexofurls[i,2]),]
    
    #exclude all non fixations
    
    teil2_data_bild <- teil2_data_bild[which(teil2_data_bild$eye_movement_type == "Fixation"),]
    
    #get first timestamp for time to first fixation
    start_time <- teil2_data_bild$recording_timestamp[1]
    end_time <- teil2_data_bild$recording_timestamp[nrow(teil2_data_bild)]
    total_time <- end_time - start_time
    
    #remove all duplicates
    teil2_data_bild <- 
      teil2_data_bild %>%
      group_by(eye_movement_type_index) %>%
      slice(1)
    
    #AOI check
    aoidata <- aois[which(aois$pictureid == pictureid[i]),]
    aoidatadis <- aois[which(aois$pictureid == disclosureid[i]),]
    
    x <- aoidata$target_x
    y <- 1080-aoidata$target_y
    w <- aoidata$target_w
    h <- aoidata$target_h
    sal <- aoidata$saliency
    pixels.x <- (x):(x+w)
    pixels.y <- (y):(y-h)
    
    #AOI hit check
    teil2_data_bild$aoihit <- ifelse((teil2_data_bild$fixation_point_x %in% pixels.x & teil2_data_bild$fixation_point_y %in% pixels.y),1,0) 
    
    if(1 %in% teil2_data_bild$aoihit){
      time_to_first_fixation <- min(teil2_data_bild[which(teil2_data_bild$aoihit == 1),"recording_timestamp"])-start_time
      number_of_fixations_aoi <- nrow(teil2_data_bild[which(teil2_data_bild$aoihit == 1),])
      number_of_fixations_total <- nrow(teil2_data_bild)
      mean_duration_fixation_aoi <- mean(teil2_data_bild[which(teil2_data_bild$aoihit == 1),]$gaze_event_duration)
      avg_pupil_dilation_aoi <- mean(teil2_data_bild$pupil_diameter_left[which(teil2_data_bild$aoihit == 1)] + teil2_data_bild$pupil_diameter_right[which(teil2_data_bild$aoihit == 1)],na.rm = T)
      avg_pupil_dilation_total <- mean(teil2_data_bild$pupil_diameter_left + teil2_data_bild$pupil_diameter_right,na.rm = T)
    }else{
      print(teil2_data_bild$aoihit)
      time_to_first_fixation <- mean_duration_fixation_aoi <- avg_pupil_dilation_aoi  <- NA
      number_of_fixations_aoi <- 0
      number_of_fixations_total <- nrow(teil2_data_bild)
      avg_pupil_dilation_total <-  mean(teil2_data_bild$pupil_diameter_left + teil2_data_bild$pupil_diameter_right,na.rm = T)
    }
    temp <- 
      data.frame(
        "total_time" = total_time,
        "time_to_first_fixation" = time_to_first_fixation,
        "number_of_fixations_aoi" = number_of_fixations_aoi,
        "number_of_fixations_total" = number_of_fixations_total,
        "mean_duration_fixation_aoi" = mean_duration_fixation_aoi,
        "avg_pupil_dilation_aoi" = avg_pupil_dilation_aoi,
        "avg_pupil_dilation_total" = avg_pupil_dilation_total,
        "salience" = sal,
        "disc" = disclosureid[i],
        "name" = name)
    df <- rbind(df,temp)
  }
  write.csv(x = df,file = paste0("data/cleaneddata/",sub(".*- ", "", path),".csv"))
}

