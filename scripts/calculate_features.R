calculate_features <- function(dataset){
    
    # FIND WINDOW NUMBERS FOR THIS DATA SET
    win_nums <- unique(dataset$num_window)
    
    # NAMES OF EULER ANGLES
    Eulers <- c("roll_belt", "pitch_belt", "yaw_belt",
                "roll_arm", "pitch_arm", "yaw_arm",
                "roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell",
                "roll_forearm", "pitch_forearm", "yaw_forearm")
    
    # ESTABLISH PARALLEL PROCESSING CLUSTER FOR MARGINALLY FASTER COMPUTATION
    ncore <- detectCores() - 1
    c1 <- makePSOCKcluster(ncore)
    registerDoParallel(c1)
    
    # LOOP OVER EACH WINDOW NUMBER, COMBINING OUTPUT BY ROW INTO MATRIX
    features <- foreach (i = 1:length(win_nums), .combine = 'rbind') %dopar% {
        # PACKAGE REQUIRED FOR KURTOSIS AND SKEWNESS
        require(e1071)
        
        n <- win_nums[i]
        dataset_i <- dataset[dataset$num_window == n,]
        Eulers_i <- dataset_i[,Eulers]
        
        if (length(unique(dataset_i$classe)) == 1 & 
            dim(dataset_i)[1] > 2){
            
            # FIND MAX MIN FOR EACH EULER ANGLE IN WINDOW
            maxs <- apply(Eulers_i, 2, max)
            mins <- apply(Eulers_i, 2, min)
            
            # CALCULATE MEAN VAR STDDEV FOR EACH EULER ANGLE IN WINDOW
            avgs <- apply(Eulers_i, 2, mean)
            vars <- apply(Eulers_i, 2, var)
            sdvs <- apply(Eulers_i, 2, sd)
            
            # CALCULATE SKEWNESS AND KURTOSIS FOR EACH EULER ANGLE IN WINDOW
            # USING MODIFIED DATA TO ACCOUNT FOR WINDOWS WITH ZERO VARIANCE
            mod_Eulers_i <- rbind(Eulers_i,
                                  avgs + 1, 
                                  avgs - 1)
            krts <- apply(mod_Eulers_i, 2, kurtosis)
            skws <- apply(mod_Eulers_i, 2, skewness, type = 3)
            
            c(maxs, mins, 
              avgs, vars, sdvs, 
              krts, skws,
              as.numeric(dataset_i[1,"classe"]))
            
        }
    }
    
    # END PARALLEL PROCESSING CLUSTER
    stopCluster(c1)
    
    # CREATE DESCRIPTIVE NAMES FOR FEATURES
    new_names <- rep("",85)
    new_names[1:12] <- paste("max", Eulers, sep = "_")
    new_names[13:24] <- paste("min", Eulers, sep = "_")
    new_names[25:36] <- paste("avg", Eulers, sep = "_")
    new_names[37:48] <- paste("var", Eulers, sep = "_")
    new_names[49:60] <- paste("sdv", Eulers, sep = "_")
    new_names[61:72] <- paste("krt", Eulers, sep = "_")
    new_names[73:84] <- paste("skw", Eulers, sep = "_")
    new_names[85] <- "classe"
    colnames(features) <- new_names
    
    features <- data.frame(features)
    features$classe <- factor(features$classe, labels = c("A","B","C","D","E"))
    features
    
}