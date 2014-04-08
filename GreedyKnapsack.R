# Created by Daniel Hadley
# A greedy approximation algorithm to solve the 0/1 knapsack problem in R. 
# Not optimal, but close enough for my purposes.
# It's important to inspect the final values added before the napsack fills up
# E.g., http://oucsace.cs.ohiou.edu/~razvan/courses/cs4040/lecture16.pdf

# Create data to test the algo
weight <- c(1, 3, 5, 7, 7, 8, 2, 1)
value <- c(7, 2, 4, 3, 6, 4, 1, 1)



# Write the algo as a function
knapsack <- function(value, weight, limit){
  benefit.to.cost <- value / weight #Create ratio
  df = data.frame(value, weight, benefit.to.cost) # turn it into a DF
  df <- df[with(df, order(-benefit.to.cost)), ] # Sort by benefit.to.cost
  rownames(df) <- NULL # Reset the row names for easier indexing
  df$total.weight <- ifelse(cumsum(df$weight) <= limit, cumsum(df$weight), 0) # Add first items that fit
  # I need to add a break here if nothing fits in the bag on the first pass
  for(i in 2:nrow(df)){ #Start in row 2 because some values have been added above
    df$total.weight[i] <- ifelse(df$weight[i] + df$total.weight[i-1] <= limit, # If adding won't go over limit
                                 df$weight[i] + df$total.weight[i-1], df$total.weight[i-1]) # If it will, keep Weight the same
  }
  df$add <- 0
  df$add[1] <- ifelse(df$total.weight[1] > 0, 1, 0)
  for(i in 2:nrow(df)){ #Start in row 2 
    df$add[i] <- ifelse(df$total.weight[i] > df$total.weight[i-1], 1, 0) # 1 if it has been added
  }
  return(df)
}

# Test it
knapsack(value, weight, 14)


