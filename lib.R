# plotting functions
bar_plot <- function(data, xvar, group, 
                     position = "stack", 
                     color = "black",
                     palette = "Spectral") {
  g <- ggplot(data, aes_string(xvar, fill = group), na.rm = TRUE) +
    geom_bar(position = position, color = color, na.rm = TRUE) + 
    scale_fill_brewer(palette = palette)
  if(position == "fill") {
    g <- g + ylab("Percentage")
  }
  return(g)
}


heat_map <- function(data, xvar, yvar, colvar, 
                     low = "white",
                     high = "black") {
  ggplot(data, aes_string(xvar, yvar)) + 
    geom_tile(aes_string(fill = colvar)) +
    scale_fill_gradient(low = low, high = high) +
    theme_classic()
}

# date converting functions
toDay <- function(x) {
  sapply(x, function(y) {
    age <- unlist(strsplit(y, split = " "))
    if(grepl("day", age[2])) 
      return(as.numeric(age[1]))
    else 
      return(NA)
  })
}

toWeek <- function(x) {
  sapply(x, function(x) {
    age <- unlist(strsplit(x, split = " "))
    if(grepl("day", age[2])) 
      return(0)
    else if(grepl("week", age[2])) 
      return(as.numeric(age[1]))
    else 
      return(NA)
  })
}

toMonth <- function(x) {
  sapply(x, function(x) {
    age <- unlist(strsplit(x, split = " "))
    if(grepl("day", age[2]) | grepl("week", age[2])) 
      return(0)
    else if(grepl("month", age[2])) 
      return(as.numeric(age[1]))
    else return(NA)
  })
}

toYear <- function(x) {
  sapply(x, function(x) {
    age <- unlist(strsplit(x, split = " "))
    if(grepl("day", age[2]) | grepl("week", age[2]) | grepl("month", age[2])) 
      return(0)
    else if(grepl("year", age[2])) 
      return(as.numeric(age[1]))
    else return(NA)
  })
}


toYear <- function(x) {
  Map(function(x) {
    age <- unlist(strsplit(x, split = " "))
    if(grepl("day", age[2]) | grepl("week", age[2]) | grepl("month", age[2])) 
      return(0)
    else if(grepl("year", age[2])) 
      return(as.numeric(age[1]))
    else return(-1)
  }, x)
}

featEng <- function(.data) {
  transmute(.data, 
            AnimalType = AnimalType,
            hasName = ifelse(is.na(Name), 0, 1),
            sex = ifelse(grepl("Female", SexuponOutcome), 0,
                         ifelse(grepl("Male", SexuponOutcome), 1, NA)),
            isNeutered = ifelse(grepl("Unknown", SexuponOutcome), NA, 
                                ifelse(grepl("Intact", SexuponOutcome), 0, 1)),
            isMix = ifelse(grepl(" Mix", Breed), 1, 0),
            breed_pri = ifelse(grepl("/", Breed), 
                               gsub("/.*", "", Breed), 
                               sub(" Mix", "", Breed)),
            breed_sec = ifelse(grepl("/", Breed), 
                               gsub(".*/", "", sub(" Mix", "", Breed)), 
                               sub(" Mix", "", Breed)),
            pattern_pri = ifelse(grepl("/", Color), 
                                 gsub("/.*", "", Color), 
                                 Color),
            pattern_sec = ifelse(grepl("/", Color), 
                                 gsub(".*/", "", Color), 
                                 Color),
            ageInDay = toDay(AgeuponOutcome),
            ageInWeek = toWeek(AgeuponOutcome),
            ageInMonth = toMonth(AgeuponOutcome),
            ageInYear = toYear(AgeuponOutcome))
}