# Following code is for Part A of the assignment

probs <- c(.97, .68, .89, .93)
comps <- c(1, 5, 3, 2)

sys_rel <- function(probs, comps){
  tot_rel <- 1 # Initial statement
  
  for (i in 1:length(probs)) {
    p <- probs[i] # The individual probabilities
    c <- comps[i] # The individual number of components
    
    tot_rel <- tot_rel * (1 - (1 - p) ^ c) # Formula to calculate the reliability
  }
  return(tot_rel) # Returning the final reliability
}

sprintf("Probability of system working =  %f", sys_rel(probs, comps))

### End of Part A answer

# The following is for Part B of assignment

num_sim <- 100 # Total number of simulations
probs <- c(.97, .68, .89, .93) # The probabilities
comps <- c(1, 5, 3, 2) # The parallel components

sim_sys_rel <- function(num_sim, probs, comps){
  tot_count <- 0 # The final count of successful simulations
  
  for (i in 1:num_sim){
    count <- 0 # This count is used to keep track if all components pass (for the system to work it has to be 4/4)
    
    for (c in 1:length(comps)){ # This loop goes through all components one by one
      temp_count <- 0 # Used to check if at least one components passes
      
      for (j in 1:comps[c]){ # This loop goes through all components per each probability
        
        if (runif(1) <= probs[c]){ # If the component in parallel passes 
          temp_count <- 1 # temp_count is set to 1, to signify that at least one component in that parallel has passed
          break # Exits the loop since at least one component has passed
        }
      }
      if (temp_count == 1){ # This if statement will change the count, if at least one component has passed (i.e if the temp_count is 1)
        count <- count + 1
      }
    }
    if (count == length(probs)){ # If at least one parallel component has passed for each component, then it should equal to the number of comps, meaning that this simulation has passed
      tot_count <- tot_count + 1
    }
  }
  system_reliability_probability <- tot_count / num_sim # This is the total passed simulations over the total number of simulations attempted
  return(system_reliability_probability)
}  

sprintf("This is the result after 100 iterations: %f", sim_sys_rel(num_sim, probs, comps))

### End of part B answer

# The following is for part C of assignment

iteration_intervals <- seq(1, 10000, 50) # Setting a sequence for number of iterations
reliabilities <- c(0) # Starting the variable

for (i in 1:length(iteration_intervals)){ # Setting the result to its corresponding position in the reliabilities list
  reliabilities[i] <- sim_sys_rel(iteration_intervals[i], probs, comps) # Calling function for each iteration interval
}

# The following is plotting the values
plot(iteration_intervals, reliabilities, xlab = "Number of iterations in simulations", ylab = "System Reliability", main = "The simulated systems reliability changing over more iterations")

rel <- 0.960728 # This is the reliability from part A
abline(h=rel, col='red') # Adds a red line on reliability given

