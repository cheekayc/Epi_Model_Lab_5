# Question 1: Write a program in R to display the pattern like right angle triangle with a number.

## We use two nested loops to print the numbers in the desired pattern. The outer loop iterates over the rows, and the inner loop iterates over the columns in each row. We use the `cat` function to print the numbers, and then a newline character at the end of each row.

for (i in 1:9) { # number of rows 
  for (j in 1:i) { # how many numbers in each row (1st row 1 number, 2nd row 2 numbers, ...)
    cat(j)
  }
  cat("\n") # print newline character after each row
}


# Question 2: Write a program in R to make such a pattern like right angle triangle with a number which will repeat a number in a row.  

## In this problem, instead of using j to print the numbers, we use i to print the same number repeatedly in each row.
for (i in 1:9) {
  for (j in 1:i) {
    cat(i)
  }
  cat("\n") # print newline character after each row
}


# Question 3: Suppose you have daily outputs from a simulation as shown in the out.daily.csv file. Write a program in R to aggregate the daily outputs to weekly outputs. 

## We compute the week number for each day using the formula (day - start_day + 1) / 7, which takes the difference between the day number and the starting day, adds 1 to account for the starting day itself, and divides the result by 7 to get the number of full weeks that have elapsed since the starting day. We use the `ceiling` function to round up the week number to the nearest integer, so that we get a new week number at the beginning of each week.

## We then use the `aggregate` function to group the data by week and compute the weekly totals using the `sum function`. The resulting `out.weekly` data frame contains the weekly totals of outputs for the 100 days, grouped by week number. Or we can do `group_by()` and `summarize()`.

library(tidyverse)
out.daily = read_csv("./out.daily.csv")

# set the starting day for week numbering
start_day = 1

# compute the week number for each day
out.daily$Week = ceiling((out.daily$Day - start_day + 1) / 7)

# Option 1: use `aggregate` function to compute weekly totals
out.weekly = aggregate(Cases ~ Week, data = out.daily, sum)

# Option 2: use `group_by()` and then summarize
out.daily %>% 
  group_by(Week) %>% 
  summarize(Cases = sum(Cases))


# Question 4: Write a program in R to create a 5 by 5 matrix with 'x' in the diagonal and 'o' in the off-diagonal and empty cells for the rest.

# create a 5 by 5 matrix that fills with 'o'
M = matrix('o', 5, 5)

# fill the diagonal with 'x'
for (i in 1:5) {
  M[i, i] = 'x'
}

# print the matrix
print(M)


# Question 5: Suppose you want to use an SIR model to simulate how R0 would affect the epidemic size. One way to do so is to simulate the epidemic dynamics using the SIR model under a fixed recovery rate (say, 0.8) and different transmission rates (say, 0.5 to 2, with an increment of 0.1).  You can then compare the final epidemic size, say at the end of 1 year, under these different parameters (and R0’s).  Use a for-loop to implement these simulations and plot to show the final epidemic size vs. R0.  (1 pt)

library(deSolve)

# Define the SIR model equations
SIR = function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS = -beta*S*I/N
    dI = beta*S*I/N - gamma*I
    dR = gamma*I
    
    return(list(c(dS, dI, dR)))
  })
}

# Initial conditions: 
N = 1e5;  # population size
I0 = 10;   # initial number of infectious
S0 = N - I0;  # initial number of susceptibles

# Parameters:
beta_vec = seq(0.5, 2, by = 0.1);  # all the beta values (transmission rates)
gamma = 0.8 # Recovery rate

# Simulation time steps
times = seq(1, 365, by = 1);  

# Create a vector to store beta values
final_sizes = numeric(length(beta_vec))

# Simulate epidemics and calculate final sizes
for (i in seq_along(beta_vec)) {
  beta = beta_vec[i]
  R0 = beta/gamma
  parameters = c(beta = beta, gamma = gamma, N = N)
  state = c(S = S0, I = I0, R = 0)
  sim = ode(y = state, times = times, func = SIR, parms = parameters)
  final_size = sim[sim[, 'time'] == 365, 'R'] 
  final_sizes[i] = final_size
}

# Create a data frame that stores each beta values and its corresponding final epidemic size.
beta_size = 
  tibble(
  R0 = beta_vec/gamma, 
  final_size = final_sizes)

# Plot final epidemic size vs. R0
ggplot(beta_size, aes(x = R0, y = final_size)) +
  geom_line() +
  labs(
    title = "Final Epidemic Size vs. R0",
    x = "R0",
    y = "Final Epidemic Size") +
  theme(plot.title = element_text(hjust = 0.5))

