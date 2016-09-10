library(knitr)
# 2. Implement a function that computes the log of the factorial value of an integer using a for loop. 
# Note that implementing it using log(A)+log(B)+ · · · avoids overflow while implementing it as log(A · B · · · · ) 
# creates an overflow early on.

logFactorialForLoop = function(n){
  if (n<=0){
    print("Error: Out of Range")
  } else{
    # log(n!) = log(1) + log(2) + log(3) + … + log(n)
    sum_log = 0
    for(i in 1:n){
      sum_log = sum_log + log(i)
    }
    return(sum_log)
  }
}

# 3. Implement a function that computes the log of the factorial value of an integer using recursion.
logFactorialRecursion = function(n){
  if (n<=0){
    print("Error: Out of Range")
  } else if (n==1){
    return(log(1))
  } else{
    sum_log = log(n) + sum(log(seq(n-1)))
    return(sum_log)
  }
}

# 4. Using your two implementations of log-factorial in (2) and (3) above, compute the sum of the log-factorials 
# of the integers 1, 2, . . . , N for various N values.

sumLFForLoop = function(N){
  sumLFForLoop = logFactorialForLoop(N) + sum(unlist(lapply(seq(N-1), logFactorialForLoop)))
  return(sumLFForLoop)
}

sumLFRecursion = function(N){
  sumLFRecursion = logFactorialRecursion(N) + sum(unlist(lapply(seq(N-1), logFactorialRecursion)))
  return(sumLFRecursion)
}

sumLFForLoop(100) # Sum of Log-Factorials for N=100 using For Loop
sumLFRecursion(100) # Sum of Log-Factorials for N=100 using Recursion

# 5. Compare the execution times of your two implementations for (4) with an implementation based on the official 
# R function lfactorial(n). You may use the function system.time() to measure execution time. 
# What are the growth rates of the three implementations as N increases? Use the command options(expressions=500000) 
# to increase the number of nested recursions allowed. Compare the timing of the recursion implementation as much as 
# possible, and continue beyond that for the other two implementations.

integer_value = c(1000, 10000, 100000, 500000, 1000000)

time = NULL
for(i in 1:length(integer_value)){
  LFForLoopTime = system.time(logFactorialForLoop(integer_value[i]))
  LFForLoopTimeRow = data.frame(method="For Loop", 
                                integer=integer_value[i], 
                                logfactorial=logFactorialForLoop(integer_value[i]), 
                                user=LFForLoopTime[1], system= LFForLoopTime[2], elapsed=LFForLoopTime[3])
  
  LFRecursionTime = system.time(logFactorialRecursion(integer_value[i]))
  LFRecursionTimeRow = data.frame(method="Recursion", 
                                  integer=integer_value[i], 
                                  logfactorial=logFactorialRecursion(integer_value[i]), 
                                  user=LFRecursionTime[1], system= LFRecursionTime[2], elapsed=LFRecursionTime[3])
  
  LFTime = system.time(lfactorial(integer_value[i]))
  LFTimeRow = data.frame(method="lfactorial", 
                         integer=integer_value[i], 
                         logfactorial=lfactorial(integer_value[i]), 
                         user=LFTime[1], system=LFTime[2], elapsed=LFTime[3])
  
  time[[i]] = do.call(rbind, list(LFForLoopTimeRow, LFRecursionTimeRow, LFTimeRow))
}
time_table = do.call(rbind, time)
row.names(time_table) = NULL
time_table = time_table[order(time_table$method),]
kable(time_table)

sumLF = function(N){
  sumLF = logFactorialRecursion(N) + sum(unlist(lapply(seq(N-1), lfactorial)))
  return(sumLF)
}

integer_value = c(10, 100, 500, 1000)

sum_time = NULL
for(i in 1:length(integer_value)){
  print(i)
  LFForLoopTime = system.time(sumLFForLoop(integer_value[i]))
  LFForLoopTimeRow = data.frame(method="For Loop", 
                                integer=integer_value[i], 
                                logfactorial=sumLFForLoop(integer_value[i]), 
                                user=LFForLoopTime[1], 
                                system= LFForLoopTime[2], 
                                elapsed=LFForLoopTime[3])
  
  LFRecursionTime = system.time(sumLFRecursion(integer_value[i]))
  LFRecursionTimeRow = data.frame(method="Recursion", 
                                  integer=integer_value[i], 
                                  logfactorial=sumLFRecursion(integer_value[i]), 
                                  user=LFRecursionTime[1], 
                                  system= LFRecursionTime[2], 
                                  elapsed=LFRecursionTime[3])
  
  LFTime = system.time(sumLF(integer_value[i]))
  LFTimeRow = data.frame(method="lfactorial", 
                         integer=integer_value[i], 
                         logfactorial=sumLF(integer_value[i]), 
                         user=LFTime[1], 
                         system=LFTime[2], 
                         elapsed=LFTime[3])
  
  sum_time[[i]] = do.call(rbind, list(LFForLoopTimeRow, LFRecursionTimeRow, LFTimeRow))
}
sum_time_table = do.call(rbind, sum_time)
row.names(sum_time_table) = NULL
sum_time_table = sum_time_table[order(sum_time_table$method),]
kable(sum_time_table)
