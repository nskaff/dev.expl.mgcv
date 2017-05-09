#' Create a list of all possible models from a set of covariates and a response variable
#'
#' Create a list of all possible models from a set of covariates and a response variable.
#'Generally, this function is only useful when the resulting object is passed to
#'dev.expl.mgcv::dev.expl.calcs.
#' @param regressors Vector of strings representing each covariate in a gam model
#' including k value and outter wraper (s, ti, te etc.)
#' @param response A string with the response variable followed by a ~ and the intercept
#' (usually 1), e.g. "Mean_Abundance_perTrapnight~1"
#' @return A list of formulas for all possible combinations of models and a matrix
#' using logical values to describe which terms are included in each of the model formulas.
#' @export model.list
#' @examples
#' #from mgcv documentation
#' library(mgcv)
#' library(dev.expl.mgcv)
#' set.seed(2) ## simulate some data...
#' dat <- gamSim(1,n=400,dist="normal",scale=2)
#' b <- gam(y~1+x0+s(x0, k=4)+ti(x2, x3, k=4),data=dat)
#' model.list.output<-model.list(covariates = c("x0","s(x0, k=4)",
#' "ti(x2, x3, k=4)"), response="y~1")


#calculates a dev. expl for individual covariates in a mgcv gam model in a way that isn't order of removal dependent.


model.list<-function (covariates, response){

  #create a list with 1 T/F element
  logica <- list(c(T,F))

  #repeating that list for each variable included in "covariates"
  rep_logical <-  rep(logica, length(covariates))

  #generating a matrix of all combiantions of models, each covariate is represented with true or false
  regres_logical<-expand.grid(rep_logical)

  colnames(regres_logical)<-covariates
  #creating a string with a list of all the covariates the full models, then coverting it to a formula and generating a list of reduced models based on the true false values in regres_logical
  allModelsList <- apply(regres_logical, 1, function(x) as.formula(paste(c(response, covariates[x]),collapse=" + ")) )

  #returning a list composed of the list of all the models, and the logical dataframe. This allows me to refer to both in the dev.expl.calcs function. to refer just to the list of models, has to be allModelsList[[1]] for just logical dataframe allModelsList[[2]].
  return(list(allModelsList, regres_logical))
}

#model.list.output<-model.list(covariates = c("Prop_Gravid_Trap","s(prop_PEM_5000.y, k=4)", "s(prop_PFO_Ever.x_3000, k=4)", "s(prop_PFO_Decid.x_3000, k=4)","s(prop_PSS_4000,k=4)", "s(PFO_mean_stream_cnt_1500, k=4)","ti(prop_PFO_F_5000,prev_phdi_0,k=4)", "s(impvMN_200, k=4)", "s(month_num,k=4)"), response="ln_Mean_Abundance_perTrapnight~1")
