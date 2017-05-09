#' Calculates order-independent deviation explained values for individual response variables in a mgcv gam model
#'
#' Calculates the deviation explained for individual covariates in a generalized additive model (gam) from the 'mgcv' package such that the calculated value isn't dependent on order of covariate removal. This is accomplished by calculating the deviation explained for all potential models built from the supplied covariates and response, then calculating the mean difference in deviance explained between models with and without each covariate. The sum of the deviation explained for each covariate may not equal the deviation explained for a model that includes all covariates due to rounding error. The runtime may be very long for models with many covariates.
#' @param model.list.output Object created from model.list function
#' @param data The data to be used in the GAM model. Must be a data frame.
#' @param select Logical indicating whether to use MGCV automatic smoothness selection.
#'See: https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gam.selection.html
#' @param family An object of class "family".
#' See: https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/family.mgcv.html
#' @return A vector of numeric values representing the partial deviation explained for
#' each covariate. Each value is determined by calculating the mean difference between
#' all full and reduced models for each covariate.
#' @import dplyr
#' @import mgcv
#' @export dev.expl.calcs
#' @examples
#' library(mgcv)
#' library(dev.expl.mgcv)
#' #adapted from mgcv documentation
#' set.seed(2) ## simulate some data...
#' dat <- gamSim(1,n=400,dist="normal",scale=2)
#' b <- gam(y~1+x0+s(x0, k=4)+ti(x2, x3, k=4),data=dat)
#' model.list.output<-model.list(covariates = c("x0","s(x0, k=4)",
#' "ti(x2, x3, k=4)"), response="y~1")
#' dev.expl.calcs(model.list.output, dat, select = TRUE,family = gaussian())


#only allows you to use the default "method" for the Gam
dev.expl.calcs<-function(model.list.output, data, select, family){

#accounting for a situation where someone enters only 1 regressor
  if(length(model.list.output[[1]])==2){return(summary(gam(model.list.output[[1]][[1]], select=select, data=data, family=family))$dev.expl)}
  else
  {

  ##calculating the gam model for each combination of covariates and populating a list with each model. The sp stuff is keeping the sp consistent between the original full model and each of the reduced models generated here so that the dev.expl will be consistent
  allModelsResults<-list()


  fullmodel<-gam(model.list.output[[1]][[1]], select=select, data=data, family=family)



  gam_results<-lapply(model.list.output[[1]], function(model.list.output){

    red_model_nosp<- gam(model.list.output,data=data,select=select,family=family)

    #sp for the reduced model is handled by looking for matches between a reduced model with no sp specific and the fullmodel
    sp_vals<-if(summary(red_model_nosp)$m>0){
      fullmodel$sp[c(pmatch(names(red_model_nosp$sp),names(fullmodel$sp), duplicates.ok = F ))]
    } else  {NULL}


    red_model<- gam(model.list.output,data=data,select=select,family=family, sp=sp_vals)


  })

  dev.expl<-lapply(1:length(gam_results),function(x) summary(gam_results[[x]])$dev.expl)


  #making it not a list so I'll be able to bind it with the regMat dataframe
  dev.expl_frame<-data.frame(unlist(dev.expl))

  #matching up each model listed in regMat with the dev.expl for the model

  dev.expl.var<-bind_cols(data.frame(model.list.output[[2]]), dev.expl_frame)


  #comparing the deviation explained for each model, with the reduced model that is identical except that 1 covariate has been removed. The function below is first identifying row,column combinations that include a covariate, then it's identifying the deviation explained for that model and subtracting from that the deviation explained for a model that is identical except tha the focal covariate has been removed.
  reduced_model_diff<-sapply(1:(length(dev.expl.var)-1),function(i) sapply(1:nrow(dev.expl.var), function(index) if(dev.expl.var[index,i]==T){

    dev.expl.var[index, "unlist.dev.expl."] - (dev.expl.var[names(which(apply(dev.expl.var[-index,c(-i,-length(dev.expl.var)),drop=F],1,function(x) identical(unname(unlist(x)),unname(unlist(dev.expl.var[index,c(-i,-length(dev.expl.var))])))))), "unlist.dev.expl."])

  } else {NA}

  ))

  #converting output matrix to dataframe
  reduced_model_diff<-data.frame(reduced_model_diff)
  #naming the column names after each covariate name
  colnames(reduced_model_diff)<-colnames(model.list.output[[2]])

  #calculating the mean difference between all full and reduced models for each covariate
  return(colMeans(reduced_model_diff, na.rm=T))

}
}

#dev.expl.calcs(model.list.output, fulldata, select = T,family = gaussian())

