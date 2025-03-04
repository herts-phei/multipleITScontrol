#### plot test

plot(tibble_data[["Date"]], tibble_data[["score"]], type = "l", main = "Numeric Value Over Time", xlab = "Date", ylab = "Value")

plot(tibble_data[["Date"]][tibble_data[["group_var"]] == "treatment"], tibble_data[["score"]][tibble_data[["group_var"]] == "treatment"], type = "l", col = "blue",
     main = "Numeric Value Over Time by Category", xlab = "Date", ylab = "Value")
lines(tibble_data[["Date"]][tibble_data[["group_var"]] == "control"], tibble_data[["score"]][tibble_data[["group_var"]] == "control"], col = "red")
abline(v = as.Date(c("2025-09-01", "2026-03-02")), col = "black", lty = 2)
legend("topleft", legend = c("Treatment Group", "Control Group"), col = c("blue", "red"), lty = 1)



########

plot.myclass <- function(obj, ...){
  plot(x = obj[[1]], y = obj[[2]], main='my title', type = 'b', col = 'red', ...)
}

obj <- myfun(5,6)
plot(obj)
