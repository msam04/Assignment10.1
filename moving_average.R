find_k_moving_average <- function(list_numbers, k){
  avg_list <- list()
  u <- length(list_numbers) - (k-1)
  u1 <- k - 1
  for (i in (1:u)) {
    sum <- 0
    for (j in 0:u1) {
      sum <- sum + as.integer(list_numbers[i+j])
    }
    average <- sum / k
    avg_list <- c(avg_list, as.numeric(average))
  }
  return(avg_list)
}

input <- strsplit(readline((prompt="Enter the list of numbers: ")), " ")
numbers <- list()
for (num in input) {
  numbers <- c(numbers, as.integer(num))
}
k <- as.integer(readline(prompt="Enter the value of k:"))
a_list <- find_k_moving_average(numbers, k)
print(a_list)
