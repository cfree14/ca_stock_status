
# Function validate model
################################################################################

parse_sanity_check <- function(x){
  
  hess_out <- ifelse(x$hessian_ok==T, 
                     "Hessian matrix is positive definite", 
                     "***Non-positive-definite Hessian matrix: model may not have converged***")
  eigen_out <- ifelse(x$eigen_values_ok==T, 
                      "No extreme or very small eigenvalues detected", 
                      "Extreme or very small eigenvalues detected: model may not have converged")
  nlminb_out <- ifelse(x$nlminb_ok==T, 
                       "Non-linear minimizer suggests successful convergence", 
                       "***Non-linear minimizer did not converge: do not trust this model***")
  range_out <- ifelse(x$range_ok==T, 
                      "Range parameters don't look unreasonably large", 
                      "A `range` parameter looks fairly large (> 1.5 the greatest distance in data)")
  gradients_out <- ifelse(x$gradients_ok==T, 
                          "No gradients with respect to fixed effects are large", 
                          "***See ?run_extra_optimization(), standardize covariates, and/or simplify the model***")
  se_mag_out <- ifelse(x$se_magnitude_ok==T, 
                       "No standard errors look unreasonably large", 
                       "***Some standard errors may be large***")
  se_na_out <- ifelse(x$se_na_ok==T, 
                      "No fixed-effect standard errors are NA", 
                      "***Some fixed-effect standard errors are NA***")
  sigmas_out <- ifelse(x$sigmas_ok==T, 
                       "Sigmas look okay", 
                       "***Sigmas are messed up")
  
  out <- paste(hess_out, eigen_out, nlminb_out, range_out, 
               gradients_out, se_mag_out, se_na_out, sigmas_out, sep="\n")
  return(out)
}

