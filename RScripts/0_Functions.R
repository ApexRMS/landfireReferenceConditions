## Funtions

# Function to ensure sum of rounded percentages is equal 100%
sum.100 <- function(df, cols) {
  # Get row sums
  totals <- rowSums(df[cols], na.rm = T)

  # Force each row to sum to 100
  sapply(1:nrow(df), function(x) {
    row <- df[x, cols] # Get row

    if ((!is.na(totals[x])) && (!totals[x] == 100)) {
      # If total not equal to 100

      if (totals[x] > 100) {
        # If total > 100
        dif <- totals[x] - 100 # Size of excess = dif
        row[which.max(row)] <- row[which.max(row)] - dif # Remove dif from maximum value
      } else {
        # If total < 100
        dif <- 100 - totals[x] # Size of deficiency = dif
        row[which.min(row)] <- row[which.min(row)] + dif # Add dif to minimum value
      }

      df[x, cols] <<- row # Assign row
    }
  })
  # Return df
  return(df)
}


# Function to compute, for each FRG, min and max FRI_AllFire
allFireFRI.minMax <- function(x) {
  x <- substr(x, start = 1, stop = gregexpr("years", x, fixed = T)[[1]][1] - 2)
  min <- ifelse(
    grepl("-", x, fixed = T),
    as.integer(substr(
      x,
      start = 1,
      stop = gregexpr("-", x, fixed = T)[[1]][1] - 1
    )),
    ifelse(
      grepl("to", x, fixed = T),
      as.integer(substr(
        x,
        start = 1,
        stop = gregexpr("to", x, fixed = T)[[1]][1] - 1
      )),
      501
    )
  )
  max <- ifelse(
    grepl("-", x, fixed = T),
    as.integer(substr(
      x,
      start = gregexpr("-", x, fixed = T)[[1]][1] + 1,
      stop = nchar(x)
    )),
    ifelse(
      grepl("to", x, fixed = T),
      as.integer(substr(
        x,
        start = gregexpr("to", x, fixed = T)[[1]][1] + 2,
        stop = nchar(x)
      )),
      max(table$FRI_AllFire, na.rm = T)
    )
  ) +
    1 # Add 1 because otherwise there are gaps between intervals

  y <- as.vector(c(min, max))
  return(y)
}

# Function to compute, for each FRG, min and max PercentOfFire_ReplacementFire
PercentOfFire.ReplacementFire.minMax <- function(x) {
  min <- ifelse(
    x == "Less than 66.7%",
    0,
    ifelse(
      x == "66.7% or greater",
      66.7,
      ifelse(x == "Less than 80%", 0, ifelse(x == "80% or greater", 80, 0))
    )
  )

  max <- ifelse(
    x == "Less than 66.7%",
    66.7,
    ifelse(
      x == "66.7% or greater",
      101,
      ifelse(x == "Less than 80%", 80, ifelse(x == "80% or greater", 101, 101))
    )
  )
  y <- as.vector(c(min, max))
  return(y)
}


# Function to compute the correct FRG for each model
get.FRG <- function(x) {
  if (is.na(table$FRI_AllFire[x])) {
    old <- new <- NA # NA if FRI_AllFire is NA
  } else {
    # If PercentOfFires_ReplacementFire is NA, use 0%
    if (is.na(table$PercentOfFires_ReplacementFire[x])) {
      replFire <- 0
    } else {
      replFire <- table$PercentOfFires_ReplacementFire[x]
    }

    # Get record corresponding to correct FRG
    rules <- FRG_rules %>%
      filter(FRI_AllFire_min <= table$FRI_AllFire[x]) %>% # FRI_AllFire must be equal to or greater than min for FRG
      filter(FRI_AllFire_max > table$FRI_AllFire[x]) %>% # FRI_AllFire must be strictly lesser than max for FRG
      filter(PercentOfFire_ReplacementFire_min <= replFire) %>% # PercentOfFires_ReplacementFire must be equal to or greater than min for FRG
      filter(PercentOfFire_ReplacementFire_max > replFire) # PercentOfFires_ReplacementFire must be strictly lesser than max for FRG

    # Get FRG names
    old <- as.character(rules$Original.Fire.Regime.Group)
    new <- as.character(rules$New.Group.Designation)
  }
  final <- as.vector(c(old, new))
  return(final)
}
