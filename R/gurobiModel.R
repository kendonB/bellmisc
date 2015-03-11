gurobiModel <- function(A, obj, sense, rhs){
  model <- list()
  model$A <- A
  model$obj <- obj
  model$sense <- "<="
  model$rhs <- matrix(1)
  model$Q <- matrix(1)
  gurobi(model = model)
}
