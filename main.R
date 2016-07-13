
# stage 1
pollutantmean <- function(directory, pollutant, id) {
    data <- data.frame()
    file_directory <- list.files(directory, full.names = TRUE)
    for (index in id) {
        data <- rbind(data, read.csv(file_directory[index]))
    }
    pm <- mean(data[[pollutant]], na.rm = TRUE)
    pm
}


# stage 2
complete <- function(directory, id = 1 : length(list.files(directory))) {
    data <- data.frame(matrix(data = NA, nrow = length(id), ncol = 2))
    colnames(data) <- c("id","nobs")
    #names(data) <- c("id", "nobs")
    file_directory <- list.files(directory, full.names = TRUE)
    
    for (index in id) {
        data$id <- as.integer(id)
        data$nobs[index] <- sum(complete.cases(read.csv(file_directory[index])))
    }
    data
}

#stage rem
complete <- function(directory, id = 1: length(list.files(directory))) {
    data <- data.frame(matrix(data = NA, nrow = length(id), ncol = 2))
    colnames(data) <- c("id", "nobs")
    file_directory <- list.files(directory, full.names = TRUE)
    
    for (variable in vector) {
        data@id <- as.integer(id)
        data@nobs[index] <- sum(complete.cases(read.csv(file_directory[index])))
        
    
    }
}

complete <- function(directory, id = 1: length(list.files(directory))) {
    data <- data.frame(matrix(data = NA, nrow = length(id), ncol = 2))
}

# stage 3
corr <- function(directory, threshold = 0) {
    dir <- list.files(directory, full.names = TRUE)
    dir.n <- length(dir)
    core.data <- data.frame()
    correlations <- c()
    
    for (index in 1:dir.n) {
        if (sum(complete.cases(read.csv(dir[index]))) > threshold) {
            cache.data <- read.csv(dir[index])
            logic <- complete.cases(cache.data)
            correlations[index] <- cor(cache.data$sulfate[logic], cache.data$nitrate[logic])
            cache.data <- NULL
        }
    }
    if (sum(is.na(correlations)) == 0) {
        correlations <- c()
    }
    correlations <- na.exclude(correlations)
}