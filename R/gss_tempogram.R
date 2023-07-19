gss_tempogram <- function(df, path_to_activity_codes = "data/GSS_codes.csv", w = NULL, granularity = "full", method = "first"){
  start.time <- Sys.time()

  data = df

  # checking if all activities summing up to 1440
  data <- data %>% group_by(PUMFID) %>% mutate(sum_dur = sum(DURATION))

  if(names(table(data$sum_dur)) != c("1440")){
    stop("Your subsample contains sequences that do not add up to 1440 minutes, or 24 hours")
  }

  # upload activity codes file
  act_codes <- read.csv(path_to_activity_codes)

  ## check if there are activities missing:
  if(length(setdiff(unique(data$TUI_01), unique(act_codes$TUI_01))) != 0){
    print(setdiff(unique(data$TUI_01), unique(act_codes$TUI_01)))
    stop("Your activity codes file does not contain all possible activity codes.")
  }

  ## attach the Codes and Alphabet
  total <- merge(data, act_codes,by="ACTIVITY", all.x = TRUE)

  total <- total[order(total$PUMFID, total$EPINO),]

  ## create sequences of activities per activity (result: long list of combined sequences)
  sequences = rep(total$Alphabet, total$DURATION)

  ## separate the long list into sequences (1440 min in each sequence)
  seq = matrix(sequences, nrow=length(sequences)/1440, ncol=1440, byrow=T)

  ## transform matrix to dataframe
  seq <- as.data.frame(seq, row.names = unique(total$PUMFID))

  ## create id
  seq$PUMFID <- unique(total$PUMFID)

  if(!is.null(w)){
    ## weights only + remove duplicates
    weights <- data %>% select(c(PUMFID, all_of(w)))
    weights <- weights[!duplicated(weights), ]
    weights[[w]] <- weights[[w]]*length(weights[[w]])/sum(weights[[w]])

    ## merge sequences with weights
    seq <- merge(seq, weights, by="PUMFID", all.x = TRUE)

    w = "WT06"
  }

  # dictionary
  act <- unique(act_codes$Name)
  names(act) <- unique(act_codes$Alphabet)

  tempo <- data.frame(key = unique(act_codes$Name),
                      values=rep(NA, length(unique(act_codes$Name))),
                      row.names = unique(act_codes$Alphabet))

  tempogram <- data.frame(matrix(ncol = 1440, nrow = length(names(act))),
                          row.names = unique(act_codes$Alphabet))

  colnames(tempogram) <- names(seq)[!names(seq) %in% c("PUMFID", w)]


  pb = txtProgressBar(min = 0, max = 1440, initial = 0)
  if(is.null(w)){
    for(i in 1:1440){
      temp <- seq %>% count((!!sym(paste("V", i, sep = ""))))
      rownames(temp) <- temp[[paste("V", i, sep = "")]]
      tempogram[[paste("V", i, sep = "")]] <- temp$n
      setTxtProgressBar(pb,i)
    }
  } else {
    for(i in 1:1440){
      temp <- seq %>% count((!!sym(paste("V", i, sep = ""))), wt = !!sym(w))
      rownames(temp) <- temp[[paste("V", i, sep = "")]]
      tempogram[[paste("V", i, sep = "")]] <- temp$n
      setTxtProgressBar(pb,i)
    }
  }
  close(pb)

  if(granularity != "full" & is.numeric(granularity)){
    if(method == "first"){
      repetitions = seq(240, 1679, granularity)
      positions = seq(240, 1679, granularity) - 239
    } else if(method == "last"){
      repetitions = seq(240, 1679, granularity)
      positions = seq(240, 1679, granularity) - 239 + granularity - 1
      repetitions <- repetitions[positions < 1441]
      positions <- positions[positions < 1441]
      if(length(repetitions) != length(positions)){
        stop("Not equal length resulted in calculations.")
      }
    }
    if(1679 %in% repetitions){
      break
    } else {
      repetitions <- c(repetitions, 1679)
      positions <- c(positions, 1440)
    }
    for(i in 1:length(names(act))){
      tempo$values[i] <- list(cbind(repetitions, unname(unlist(tempogram[i, positions]))))
    }
  } else {
    for(i in 1:length(names(act))){
      tempo$values[i] <- list(cbind(240:1679, unname(unlist(tempogram[i, ]))))
    }
  }

  end.time <- Sys.time()

  t = end.time - start.time

  print(t)

  return(tempo)

}
