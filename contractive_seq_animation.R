contr.seq = function(x0, x1, lambda, steps = 15, delay = 0.1) {
  x = numeric(length = steps)
  x[1] = x0
  x[2] = x1
  
  for(i in 2:(steps - 1)) {
    x[i + 1] = (lambda * x[i]) + ((1 - lambda) * x[i - 1])
  }
  
  ylim = range(x)
  
  plot(x = 1:steps, y = rep(NA, steps), main = paste("x_0 =", x0, ", x_1 =", x1, ", lambda =", lambda), type = "o", xlab = "n", ylab = "X_n", ylim = ylim)
  
  x[1] = x0
  x[2] = x1
  
  for(i in 2:(steps - 1)) {
    x[i + 1] = (lambda * x[i]) + ((1 - lambda) * x[i - 1])
    points(x = i + 1, y = x[i + 1], type ='o')
    Sys.sleep(delay)
  }
}

# Changing initial points but keeping the contractive constant same
contr.seq(x0 = 1, x1 = 1.5, lambda = 0.5, steps = 15, delay = 0.2)
Sys.sleep(2)
contr.seq(x0 = 1, x1 = 99, lambda = 0.5, steps = 15, delay = 0.1)
Sys.sleep(2)

contr.seq = function(x0, x1, lambda, steps = 100, delay = 0.01) {
  x = numeric(length = steps)
  x[1] = x0
  x[2] = x1
  
  for(i in 2:(steps - 1)) {
    x[i + 1] = (lambda * x[i]) + ((1 - lambda) * x[i - 1])
  }
  
  ylim = range(x)
  
  plot(x = 1:steps, y = rep(NA, steps), main = paste("x_0 =", x0, ", x_1 =", x1, ", lambda =", lambda), type = "l", col = "blue", xlab = "n", ylab = "X_n", ylim = ylim)
  
  x[1] = x0
  x[2] = x1
  
  for(i in 2:(steps - 1)) {
    x[i + 1] = (lambda * x[i]) + ((1 - lambda) * x[i - 1])
    lines(x = c(i, i + 1), y = c(x[i], x[i + 1]), col = "blue")
    Sys.sleep(delay)
  }
}

# Changing the values of contractive constant but keeping the initial values same
contr.seq(x0 = 1, x1 = 1.5, lambda = 0.005, steps = 1000, delay = 0.)
Sys.sleep(2)
contr.seq(x0 = 1, x1 = 99, lambda = 0.005, steps = 1000, delay = 0.09)
