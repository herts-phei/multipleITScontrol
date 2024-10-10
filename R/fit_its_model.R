#' @title fit_its_model
#'
#' @description Fits an interrupted time series model using the nlme package, defaulting to an autocorrelation-moving average correlation structure of order (p, q)
#'
#' @param transformed_df Am unmodified data frame created from `transform_data()`.
#' @param impact_model The hypothesized impact model from interventions. Available options are 'level', 'slope', or 'levelscope'.
#' @param num_interventions The number of interventions in your transformed data. Should be the vector length of `intervention_dates` passed in `transform_data()`.
#' @param method The estimation method for `gls()`, either "REML" (default) or "ML".
#' @param p The order of the autoregressive component. Defaults to `NULL`.
#' @param q The order of the moving average component. Defaults to `NULL`.
#' @param ... Additional arguments passed to `nlme::gls()`.
#' @return A `gls` object representing the fitted model.
#' @examples
#' output_table <- fit_its_model(transformed_df = df, impact_model)
#' @export
#' @importFrom nlme gls corARMA
#' @importFrom dplyr ungroup group_by arrange mutate case_match sym recode
#' @importFrom rlang !! !!! :=

fit_its_model <- function(transformed_data,
                          impact_model,
                          num_interventions,
                          method = "REML",
                          p = NULL,
                          q = NULL,
                          ...) {

  level_intervention_cols <- paste0("level_", seq_len(num_interventions), "_intervention")
  slope_intervention_cols <- paste0("slope_", seq_len(num_interventions), "_intervention")

  termlabels <- switch(impact_model,
                       "pre_intervention" = "x * time_index",
                       "level" = c("x * time_index", paste0("x * ", level_intervention_cols)),
                       "slope" = c("x * time_index", paste0("x * ", slope_intervention_cols)),
                       "levelscope" = c("x * time_index", paste0("x * ", level_intervention_cols), paste0("x * ", slope_intervention_cols))
                       )

  gls_object <- nlme::gls(
    reformulate(
      termlabels = termlabels,
      response = "outcome"
    ),
    data = transformed_data,
    method = method,
    correlation = (
      if (is.null(p) && is.null(q)) {
        NULL
      } else {
        nlme::corARMA(
          p = p,
          q = q,
          form = ~ time_index | x
        )
      }
    )
    ,
    ...
  )


#
# name_map <- c(
#   "(Intercept)" = "Control y-axis intercept",
#   "x" = "Pilot y-axis intercept difference to control",
#   "time_index" = "Control pre-intervention slope",
#   "x:time_index" = "Pilot pre-intervention slope difference to control"
# )
#
# # Add slopes and interventions for up to 9 interventions
# for (i in 1:9) {
#   name_map <- c(
#     name_map,
#     setNames(
#       c(sprintf("Control intervention %d slope", i), sprintf("Pilot intervention %d slope", i)),
#       c(sprintf("slope_%d_intervention", i), sprintf("x:slope_%d_intervention", i))
#     )
#   )
# }
#
#
# new_names_coeffs <- recode(
#   names(coef(gls_object)),
#   !!!name_map
# )
#
# new_names_row_matrix <- recode(
#   rownames(gls_object$varBeta),
#   !!!name_map
# )
#
# new_names_col_matrix <- recode(
#   colnames(gls_object$varBeta),
#   !!!name_map
# )
#
# new_names_parAssign <- recode(names(gls_object$parAssign), !!!name_map)
#
# new_names_row_varBetaFact <- recode(rownames(attr(gls_object$parAssign, "varBetaFact")), !!!name_map)
#
# new_names_col_varBetaFact <- recode(colnames(attr(gls_object$parAssign, "varBetaFact")), !!!name_map)
#
#
# names(gls_object$coefficients) <- new_names_coeffs
# rownames(gls_object$varBeta) <- new_names_row_matrix
# colnames(gls_object$varBeta) <- new_names_col_matrix
# names(gls_object$parAssign) <- new_names_parAssign
#
# rownames(attr(gls_object$parAssign, "varBetaFact")) <- new_names_row_varBetaFact
# colnames(attr(gls_object$parAssign, "varBetaFact")) <- new_names_col_varBetaFact
#


return(gls_object)

}

fit_its_model(moo, "slope", num_interventions = 2) -> model



terms_obj <- terms(model)

variables <- attr(terms_obj, "variables")
variables <- as.list(variables)

replace_names <- function(var) {
  var_name <- as.character(var)
  if (var_name %in% names(name_map)) {
    return(as.symbol(name_map[var_name]))  # Replace with the new name
  } else {
    return(var)  # Keep the original name if not found in the vector
  }
}

# Apply the renaming function to each variable in the list
variables <- lapply(variables, replace_names)

# Convert the list back to a call and reassign it
attr(terms_obj, "variables") <- as.call(variables)


rownames(attr(terms_obj, "factors")) <- recode(rownames(attr(terms_obj, "factors")), !!!name_map)
colnames(attr(terms_obj, "factors")) <- recode(colnames(attr(terms_obj, "factors")), !!!name_map)


attr(terms_obj, "term.labels") <- recode(attr(terms_obj, "term.labels"), !!!name_map)


predvars <- attr(terms_obj, "predvars")
predvars <- as.list(predvars)
predvars <- lapply(predvars, replace_names)
attr(terms_obj, "predvars") <- as.call(predvars)


names(attr(terms_obj, "dataClasses")) <- recode(names(attr(terms_obj, "dataClasses")), !!!name_map)



# Define a recursive function using purrr to replace old names with new ones
replace_recursive <- function(obj, name_map) {
  # Check if the object is a symbol and replace if necessary
  if (is.symbol(obj)) {
    obj_name <- as.character(obj)
    if (obj_name %in% names(name_map)) {
      return(as.symbol(name_map[[obj_name]]))  # Replace with new name
    }
  }

  # If the object is a call, replace its arguments
  if (is.call(obj)) {
    updated_args <- map(as.list(obj[-1]), ~ replace_recursive(.x, name_map))
    return(as.call(c(obj[[1]], updated_args)))
  }

  # If it's a list, apply the replacement function to each element
  if (is.list(obj)) {
    return(map(obj, ~ replace_recursive(.x, name_map)))
  }

  # If it's none of the above, return the object unchanged
  return(obj)
}

# Access the unnamed sub-object at index 3
unnamed_subobj <- as.list(terms_obj[[3]])

# Recursively replace the old variable names with new ones in the unnamed sub-object
updated_subobj <- replace_recursive(unnamed_subobj, name_map)

terms_obj[[3]] <- as.call(updated_subobj)

model$terms <- terms_obj

model[["call"]][["correlation"]][[2]][[4]][[2]][["form"]][[2]][[3]]
