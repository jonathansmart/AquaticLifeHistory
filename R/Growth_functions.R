#' Estimate length-at-age parameters and growth curves for Elasmobranchs
#' @description A multi-model growth estimation approach is applied to length-at-age data. Three models can be applied which include the von Bertalanffy (VB), logistic (Log) and Gompertz (Gom) models. AIC values and weights are calculated. The outputs will return a list of model parameter estimates and will either print a plot to the screen or output the length-at-age estimates as part of the list.Use of this function should cite Smart et al. (2016).
#' @param data a data frame which includes 'Age' and 'Length - ideally with these names but the function will except some variation to these
#' @param models a vector of models to be fitted. These can include" VB", "Log" and "Gom". A subset can also be used
#' @param Birth.Len The length-at-birth to be used for two parameter models. If a value is provided, two parameter models are automatically run
#' @param correlation.matrix Should the correlation matrix of parameters be returned? This is the only object returned if TRUE.
#' @param n.bootstraps The number of bootstraps performed for model 95 confidence intervals
#' @param plots Should plots be printed to the screen. If FALSE then the model estimates and CI's are returned as an additional output
#' @param plot.legend Do you want a legend for the different models on the plot
#' @param Max.Age Specify the max age for bootstrapped confidence intervals to be produced over. Default is the max age in the data.
#' @return Returns a list of parameter estimates with errors and AIC results. If plots is TRUE then a plot is printed to the screen. If plots is FALSE then the length-at-age estimates are returned as a list element
#' @import broom MuMIn rlist minpack.lm dplyr tidyr ggplot2
#' @importFrom magrittr %>%
#' @importFrom stats glm lm nls nls.control predict quantile na.omit
#' @export
#' @references Smart et al. (2016) Multi-model approaches in shark and ray growth studies: strengths, weaknesses and the future. Fish and Fisheries. 17: 955-971\url{https://onlinelibrary.wiley.com/doi/abs/10.1111/faf.12154}

Estimate_Growth<-function(data, models = c("VB", "Log", "Gom"),  Birth.Len = NULL, correlation.matrix = FALSE, n.bootstraps = 1000, plots = T,
                          Max.Age = NULL,
                          plot.legend = T){

  if(!any(models %in% c("VB", "Log", "Gom"))) {stop("Models an only be 'VB', 'Log', 'Gom' or a combination of these")}

  age_col <- grep("age", substr(tolower(names(data)),1,30))
  len_col <- grep("len|tl|lt|siz|fl", substr(tolower(names(data)),1,30))

  if(length(age_col) <1) {stop("Age column heading could not be distinguished ")}

  if(length(age_col) >1) {stop("Multiple age columns detected. Please remove one")}


  if(length(len_col) <1) {stop("Length column heading could not be distinguished ")}


  if(length(len_col) >1) {stop("Multiple length columns detected. Please remove one")}


  Data <- data.frame(Age = data[,age_col],
                     Length = data[,len_col])

  Data <- stats::na.omit(Data)# remove NA's

  mean.age<-tapply(Data$Length, round(Data$Age), mean,na.rm = T)
  Lt1<-mean.age[2:length(mean.age)]
  Lt<-mean.age[1:length(mean.age)-1]
  model<-lm(Lt1 ~ Lt)
  k <- suppressWarnings(abs(-log(model$coef[2]))) #in case a nan occurs
  k <- ifelse(is.nan(k),0.1,k) # in case a nan occurs
  g<-k
  Linf<-abs(model$coef[1]/(1-model$coef[2]))
  if(is.null(Birth.Len)){
    L0<-lm(mean.age ~ poly(as.numeric(names(mean.age)), 2, raw = TRUE))$coef[1]
    vb3.start<-list(Linf = log(Linf) , k = log(k),L0 = log(L0))
    gom.start<-list(Linf = log(Linf),g = log(g), L0 = log(L0))
    log.start<-list(Linf = log(Linf),g = log(g), L0 = log(L0))
  } else{
    L0<-log(Birth.Len)
    vb3.start<-list(Linf = log(Linf) , k = log(k))
    gom.start<-list(Linf = log(Linf),g = log(g))
    log.start<-list(Linf = log(Linf),g = log(g))
  }

  vb3.model<-Length~exp(Linf)-(exp(Linf)-exp(L0))*(exp(-exp(k)*Age)) #von bertalanffy equation
  log.model<-Length~(exp(Linf)*exp(L0)*exp(exp(g)*Age))/(exp(Linf)+exp(L0)*(exp(exp(g)*Age)-1))#logistic equation
  gom.model<-Length~exp(L0)*exp(log(exp(Linf)/exp(L0))*(1-exp(-exp(g)*Age))) # Gompertz equation

  AIC.vals <- NULL
  Results <- list()
  if(is.null(Birth.Len)){
    if(any(models %in% "VB")){
      VB3<-minpack.lm::nlsLM(vb3.model, data = Data, start = vb3.start, algorithm = "port", control = nls.lm.control(maxiter = 1000, ftol = 1e-05))
      VonB<-rbind(as.vector(c(exp(summary(VB3)$coef[1,1]),exp(summary(VB3)$coef[1,1])*((summary(VB3)$coef[1,2])*sqrt(length(Data[,1]))/sqrt(length(Data[,1]))))),
                  as.vector(c(exp(summary(VB3)$coef[2,1]),exp(summary(VB3)$coef[2,1])*((summary(VB3)$coef[2,2])*sqrt(length(Data[,1]))/sqrt(length(Data[,1]))))),
                  as.vector(c(exp(summary(VB3)$coef[3,1]),exp(summary(VB3)$coef[3,1])*((summary(VB3)$coef[3,2])*sqrt(length(Data[,1]))/sqrt(length(Data[,1]))))),
                  cbind(summary(VB3)$sigma,NA))
      rownames(VonB)<-c("Linf","k","L0","RSE");colnames(VonB)<-c("Parameter","SE")
      AIC.vals <- rbind(AIC.vals, AICc(VB3))
      Results <- list.append(Results, VonB = VonB)

    }

    if(any(models %in% "Log")){
      Log<-minpack.lm::nlsLM(log.model, data = Data, start = log.start, algorithm = "port", control = nls.lm.control(maxiter = 1000, ftol = 1e-05))
      LOG<-rbind(as.vector(c(exp(summary(Log)$coef[1,1]),exp(summary(Log)$coef[1,1])*((summary(Log)$coef[1,2])*sqrt(length(Data[,1]))/sqrt(length(Data[,1]))))),
                 as.vector(c(exp(summary(Log)$coef[2,1]),exp(summary(Log)$coef[2,1])*((summary(Log)$coef[2,2])*sqrt(length(Data[,1]))/sqrt(length(Data[,1]))))),
                 as.vector(c(exp(summary(Log)$coef[3,1]),exp(summary(Log)$coef[3,1])*((summary(Log)$coef[3,2])*sqrt(length(Data[,1]))/sqrt(length(Data[,1]))))),
                 cbind(summary(Log)$sigma,NA))
      rownames(LOG)<-c("Linf","g","L0","RSE");colnames(LOG)<-c("Parameter","SE")
      AIC.vals <- rbind(AIC.vals, AICc(Log))
      Results <- list.append(Results, Logistic = LOG)
    }

    if(any(models %in% "Gom")){
      gom<-minpack.lm::nlsLM(gom.model, data = Data, start = gom.start, algorithm = "port", control = nls.lm.control(maxiter = 1000, ftol = 1e-05))
      GOM<-rbind(as.vector(c(exp(summary(gom)$coef[1,1]),exp(summary(gom)$coef[1,1])*((summary(gom)$coef[1,2])*sqrt(length(Data[,1]))/sqrt(length(Data[,1]))))),
                 as.vector(c(exp(summary(gom)$coef[2,1]),exp(summary(gom)$coef[2,1])*((summary(gom)$coef[2,2])*sqrt(length(Data[,1]))/sqrt(length(Data[,1]))))),
                 as.vector(c(exp(summary(gom)$coef[3,1]),exp(summary(gom)$coef[3,1])*((summary(gom)$coef[3,2])*sqrt(length(Data[,1]))/sqrt(length(Data[,1]))))),
                 cbind(summary(gom)$sigma,NA))
      rownames(GOM)<-c("Linf","g","L0","RSE");colnames(GOM)<-c("Parameter","SE")
      AIC.vals <- rbind(AIC.vals, AICc(gom))
      Results <- list.append(Results, Gompertz = GOM)
    }
  } else {
    if(any(models %in% "VB")){
      VB3<-minpack.lm::nlsLM(vb3.model, data = Data, start = vb3.start, algorithm = "port", control = nls.lm.control(maxiter = 1000, ftol = 1e-05))
      VonB<-rbind(as.vector(c(exp(summary(VB3)$coef[1,1]),exp(summary(VB3)$coef[1,1])*((summary(VB3)$coef[1,2])*sqrt(length(Data[,1]))/sqrt(length(Data[,1]))))),
                  as.vector(c(exp(summary(VB3)$coef[2,1]),exp(summary(VB3)$coef[2,1])*((summary(VB3)$coef[2,2])*sqrt(length(Data[,1]))/sqrt(length(Data[,1]))))),
                  cbind(summary(VB3)$sigma,NA))
      rownames(VonB)<-c("Linf","k","RSE");colnames(VonB)<-c("Parameter","SE")
      AIC.vals <- rbind(AIC.vals, AICc(VB3))
      Results <- list.append(Results, VonB = VonB)
    }
    if(any(models %in% "Log")){
      Log<-minpack.lm::nlsLM(log.model, data = Data, start = log.start, algorithm = "port", control = nls.lm.control(maxiter = 1000, ftol = 1e-05))
      LOG<-rbind(as.vector(c(exp(summary(Log)$coef[1,1]),exp(summary(Log)$coef[1,1])*((summary(Log)$coef[1,2])*sqrt(length(Data[,1]))/sqrt(length(Data[,1]))))),
                 as.vector(c(exp(summary(Log)$coef[2,1]),exp(summary(Log)$coef[2,1])*((summary(Log)$coef[2,2])*sqrt(length(Data[,1]))/sqrt(length(Data[,1]))))),
                 cbind(summary(Log)$sigma,NA))
      rownames(LOG)<-c("Linf","g","RSE");colnames(LOG)<-c("Parameter","SE")
      AIC.vals <- rbind(AIC.vals, AICc(Log))
      Results <- list.append(Results, Logistic = LOG)
    }
    if(any(models %in% "Gom")){
      gom<-minpack.lm::nlsLM(gom.model, data = Data, start = gom.start, algorithm = "port", control = nls.lm.control(maxiter = 1000, ftol = 1e-05))
      GOM<-rbind(as.vector(c(exp(summary(gom)$coef[1,1]),exp(summary(gom)$coef[1,1])*((summary(gom)$coef[1,2])*sqrt(length(Data[,1]))/sqrt(length(Data[,1]))))),
                 as.vector(c(exp(summary(gom)$coef[2,1]),exp(summary(gom)$coef[2,1])*((summary(gom)$coef[2,2])*sqrt(length(Data[,1]))/sqrt(length(Data[,1]))))),
                 cbind(summary(gom)$sigma,NA))
      rownames(GOM)<-c("Linf","g","RSE");colnames(GOM)<-c("Parameter","SE")
      AIC.vals <- rbind(AIC.vals, AICc(gom))
      Results <- list.append(Results, Gompertz = GOM)
    }
  }


  if(correlation.matrix == TRUE)
  {
    correlation.matrices <- list()
    if(any(models == "VB")){
      correlation.matrices[["VonB"]] <- summary(VB3, correlation = TRUE)$cor
      Vb_names <- c("Linf", "k", "L0")
      colnames(correlation.matrices[["VonB"]] ) <- Vb_names[c(1:length(colnames(correlation.matrices[["VonB"]] )))]
      rownames(correlation.matrices[["VonB"]] ) <-  Vb_names[c(1:length(rownames(correlation.matrices[["VonB"]] )))]
    }
    if(any(models == "Log")){
      Log_names <- c("Linf", "g", "L0")
      correlation.matrices[["Logistic"]] <- summary(Log, correlation = TRUE)$cor
      colnames(correlation.matrices[["Logistic"]] ) <- Log_names[c(1:length(colnames(correlation.matrices[["Logistic"]] )))]
      rownames(correlation.matrices[["Logistic"]] ) <- Log_names[c(1:length(rownames(correlation.matrices[["Logistic"]] )))]
    }
    if(any(models == "Gom")){
      Gom_names <- c("Linf", "g", "L0")
      correlation.matrices[["Gompertz"]] <- summary(gom, correlation = TRUE)$cor
      colnames(correlation.matrices[["Gompertz"]] ) <- Gom_names[c(1:length(colnames(correlation.matrices[["Logistic"]] )))]
      rownames(correlation.matrices[["Gompertz"]] ) <- Gom_names[c(1:length(rownames(correlation.matrices[["Gompertz"]] )))]
    }

    return(correlation.matrices)
  }

  # Calculate AIC differences and weights
  AIC.table<-cbind(round(AIC.vals,2),round(AIC.vals-min(AIC.vals),2),
                   round(exp(-0.5*as.numeric(AIC.vals-min(AIC.vals)))/sum(exp(-0.5*as.numeric(AIC.vals-min(AIC.vals)))),2))
  AIC.table<-cbind(models,as.data.frame(AIC.table))
  colnames(AIC.table)<-c("Model","AICc","AIC diff","Weight")
  Results <- list.append(Results,  AIC = AIC.table)

  if(n.bootstraps == 0){
    message("Only parameter estimates are returned. Set 'n.boostraps' to be larger than 0 for these results and associated plots \n")
    return(Results)
  }


  ####---------------------
  # Length-at-age estimates
  ####---------------------

  if(is.null(Max.Age)){
    Max.Age <- max(Data$Age)
  }

  Age<-seq(0,Max.Age,0.1)
  if(is.null(Birth.Len)){

    if(any(models %in% "VB")){ VBTL<-VonB[1]-(VonB[1]-VonB[3])*(exp(-VonB[2]*Age))}
    if(any(models %in% "Log")){ LogTL<-(LOG[1]*LOG[3]*exp(LOG[2]*Age))/(LOG[1]+LOG[3]*(exp(LOG[2]*Age)-1))}
    if(any(models %in% "Gom")){GomTL<-GOM[3]*exp(log(GOM[1]/GOM[3])*(1-exp(-GOM[2]*Age)))}
  } else {
    if(any(models %in% "VB")){VBTL<-VonB[1]-(VonB[1]-Birth.Len)*(exp(-VonB[2]*Age))}
    if(any(models %in% "Log")){LogTL<-(LOG[1]*Birth.Len*exp(LOG[2]*Age))/(LOG[1]+Birth.Len*(exp(LOG[2]*Age)-1))}
    if(any(models %in% "Gom")){ GomTL<-Birth.Len*exp(log(GOM[1]/Birth.Len)*(1-exp(-GOM[2]*Age)))}
  }
  if(any(models %in% "VB")){
    VB_Estimates <- data.frame(Model = "Von Bertalanffy",
                               Age = Age,
                               AVG = VBTL,
                               low = NA,
                               upp = NA)
  }
  if(any(models %in% "Log")){
    Log_Estimates <- data.frame(Model = "Logistic",
                                Age = Age,
                                AVG = LogTL,
                                low = NA,
                                upp = NA)
  }
  if(any(models %in% "Gom")){
    Gom_Estimates <- data.frame(Model = "Gompertz",
                                Age = Age,
                                AVG = GomTL,
                                low = NA,
                                upp = NA)
  }

  Estimates <- NULL

  if(any(models %in% "VB")){
    message("\nBootstrapping von Bertalanffy model \n")
    if(is.null(Birth.Len)){
      bootnls <- Data %>% boot_data(n.bootstraps) %>%
        do(tryCatch(suppressWarnings(tidy(minpack.lm::nlsLM(formula = vb3.model,
                                                            data = .,
                                                            start = vb3.start))),
                    error=function(e){
                      suppressWarnings(tryCatch(tidy(minpack.lm::nlsLM(formula = vb3.model,
                                                                       data = .,
                                                                       start = list(Linf = log(max(.$Length)),
                                                                                    k = vb3.start$k,
                                                                                    L0 = log(min(.$Length))))),
                                                error=function(e){
                                                  return(data.frame(term = as.character(c("Linf.(Intercept)","k.Lt","L0.(Intercept)")),
                                                                    estimate = NA,
                                                                    std.error= NA,
                                                                    statistic = NA,
                                                                    p.value=  NA,stringsAsFactors = FALSE)
                                                  )
                                                },
                                                silent=TRUE
                      )
                      )

                    },
                    silent=TRUE)) %>% as.data.frame()


      bootnls[which(bootnls$term == "Linf.(Intercept)"), "term"] <- "Linf"
      bootnls[which(bootnls$term == "k.Lt"), "term"] <- "k"
      bootnls[which(bootnls$term == "L0.(Intercept)"), "term"] <- "L0"
      bootnls[1, "estimate"] <- NA
      bootnls[2, "estimate"] <- NA
      bootnls[3, "estimate"] <- NA

      boot.Linf<- exp(as.numeric(as.matrix(subset(bootnls,term == "Linf")[,3])))
      boot.K<-exp(as.numeric(as.matrix(subset(bootnls,term == "k")[,3])))
      boot.L0<-exp(as.numeric(as.matrix(subset(bootnls,term == "L0")[,3])))
      for (i in 1:length(Age)) {
        pv <- boot.Linf-(boot.Linf-boot.L0)*(exp(-boot.K*Age[i]))
        VB_Estimates[i,"low"] <- quantile(pv,0.025,na.rm = TRUE)
        VB_Estimates[i,"upp"] <- quantile(pv,0.975,na.rm = TRUE)
      }
    } else {
      bootnls <- Data %>% boot_data(n.bootstraps) %>%
        do(tryCatch(suppressWarnings(tidy(minpack.lm::nlsLM(formula = vb3.model,
                                                            data = .,
                                                            start = vb3.start))),
                    error=function(e){
                      suppressWarnings(tryCatch(tidy(minpack.lm::nlsLM(formula = vb3.model,
                                                                       data = .,
                                                                       start = list(Linf = log(max(.$Length)),
                                                                                    k = vb3.start$k,
                                                                                    L0 = log(min(.$Length))))),
                                                error=function(e){
                                                  return(data.frame(term = as.character(c("Linf.(Intercept)","k.Lt")),
                                                                    estimate = NA,
                                                                    std.error= NA,
                                                                    statistic = NA,
                                                                    p.value=  NA,stringsAsFactors = FALSE)
                                                  )
                                                },
                                                silent=TRUE
                      )
                      )

                      },
                    silent=TRUE)) %>% as.data.frame()


      bootnls[which(bootnls$term == "Linf.(Intercept)"), "term"] <- "Linf"
      bootnls[which(bootnls$term == "k.Lt"), "term"] <- "k"

      boot.Linf<- exp(as.numeric(as.matrix(subset(bootnls,term == "Linf")[,3])))
      boot.K<-exp(as.numeric(as.matrix(subset(bootnls,term == "k")[,3])))
      for (i in 1:length(Age)) {
        pv <- boot.Linf-(boot.Linf-Birth.Len)*(exp(-boot.K*Age[i]))
        VB_Estimates[i,"low"] <- quantile(pv,0.025,na.rm = TRUE)
        VB_Estimates[i,"upp"] <- quantile(pv,0.975,na.rm = TRUE)
      }
    }
    Estimates <- suppressWarnings(bind_rows(Estimates, VB_Estimates))
  }

  if(any(models %in% "Log")){
    message("\nBootstrapping Logistic model \n")
    if(is.null(Birth.Len)){
      bootnls <- Data %>% boot_data(n.bootstraps) %>%
        do(tryCatch(suppressWarnings(tidy(minpack.lm::nlsLM(formula = log.model,
                                                            data = .,
                                                            start = log.start))),
                    error=function(e){
                      suppressWarnings(tryCatch(tidy(minpack.lm::nlsLM(formula = log.model,
                                                                       data = .,
                                                                       start = list(Linf = log(max(.$Length)),
                                                                                    g = log.start$g,
                                                                                    L0 = log(min(.$Length))
                                                                                    )
                                                                       )
                                                     ),
                                                error=function(e){
                                                  return(data.frame(term = as.character(c("Linf.(Intercept)","g.Lt","L0.(Intercept)")),
                                                                    estimate = NA,
                                                                    std.error= NA,
                                                                    statistic = NA,
                                                                    p.value=  NA,stringsAsFactors = FALSE)
                                                  )
                                                },
                                                silent=TRUE
                      )
                      )

                      },
                    silent=TRUE)) %>% as.data.frame()


      bootnls[which(bootnls$term == "Linf.(Intercept)"), "term"] <- "Linf"
      bootnls[which(bootnls$term == "g.Lt"), "term"] <- "g"
      bootnls[which(bootnls$term == "L0.(Intercept)"), "term"] <- "L0"

      boot.Linf<- exp(as.numeric(as.matrix(subset(bootnls,term == "Linf")[,3])))
      boot.g<-exp(as.numeric(as.matrix(subset(bootnls,term == "g")[,3])))
      boot.L0<-exp(as.numeric(as.matrix(subset(bootnls,term == "L0")[,3])))
      for (i in 1:length(Age)) {
        pv <- (boot.Linf*boot.L0*exp(boot.g*Age[i]))/(boot.Linf+boot.L0*(exp(boot.g*Age[i])-1))
        Log_Estimates[i,"low"] <- quantile(pv,0.025,na.rm = TRUE)
        Log_Estimates[i,"upp"] <- quantile(pv,0.975,na.rm = TRUE)
      }
    } else {
      bootnls <- Data %>% boot_data(n.bootstraps) %>%
        do(tryCatch(suppressWarnings(tidy(minpack.lm::nlsLM(formula = log.model,
                                                            data = .,
                                                            start = log.start))),
                    error=function(e){
                      suppressWarnings(tryCatch(tidy(minpack.lm::nlsLM(formula = log.model,
                                                                       data = .,
                                                                       start = list(Linf = log(max(.$Length)),
                                                                                    g = log.start$g
                                                                       )
                      )
                      ),
                      error=function(e){
                        return(data.frame(term = as.character(c("Linf.(Intercept)","g.Lt")),
                                          estimate = NA,
                                          std.error= NA,
                                          statistic = NA,
                                          p.value=  NA,stringsAsFactors = FALSE)
                        )
                      },
                      silent=TRUE
                      )
                      )
                      },
                    silent=TRUE)) %>% as.data.frame()


      bootnls[which(bootnls$term == "Linf.(Intercept)"), "term"] <- "Linf"
      bootnls[which(bootnls$term == "g.Lt"), "term"] <- "g"

      boot.Linf<- exp(as.numeric(as.matrix(subset(bootnls,term == "Linf")[,3])))
      boot.g<-exp(as.numeric(as.matrix(subset(bootnls,term == "g")[,3])))
      for (i in 1:length(Age)) {
        pv <- (boot.Linf*Birth.Len*exp(boot.g*Age[i]))/(boot.Linf+Birth.Len*(exp(boot.g*Age[i])-1))
        Log_Estimates[i,"low"] <- quantile(pv,0.025,na.rm = TRUE)
        Log_Estimates[i,"upp"] <- quantile(pv,0.975,na.rm = TRUE)
      }
    }
    Estimates <- suppressWarnings(bind_rows(Estimates, Log_Estimates))
  }

  if(any(models %in% "Gom")){
    message("\nBootstrapping Gompertz model")
    if(is.null(Birth.Len)){
      bootnls <- Data %>% boot_data(n.bootstraps) %>%
        do(tryCatch(suppressWarnings(tidy(minpack.lm::nlsLM(formula = gom.model,
                                                            data = .,
                                                            start = gom.start))),
                    error=function(e){
                      suppressWarnings(try(tidy(minpack.lm::nlsLM(formula = gom.model,
                                                                  data = .,
                                                                  start = list(Linf = log(max(.$Length)),
                                                                               g = gom.start$g,
                                                                               L0 = log(min(.$Length)))))))

                      suppressWarnings(tryCatch(tidy(minpack.lm::nlsLM(formula = gom.model,
                                                                       data = .,
                                                                       start = list(Linf = log(max(.$Length)),
                                                                                    g = gom.start$g,
                                                                                    L0 = log(min(.$Length))
                                                                       )
                      )
                      ),
                      error=function(e){
                        return(data.frame(term = as.character(c("Linf.(Intercept)","g.Lt","L0.(Intercept)")),
                                          estimate = NA,
                                          std.error= NA,
                                          statistic = NA,
                                          p.value=  NA,stringsAsFactors = FALSE)
                        )
                      },
                      silent=TRUE
                      )
                      )


                      },
                    silent=TRUE)) %>% as.data.frame()


      bootnls[which(bootnls$term == "Linf.(Intercept)"), "term"] <- "Linf"
      bootnls[which(bootnls$term == "g.Lt"), "term"] <- "g"
      bootnls[which(bootnls$term == "L0.(Intercept)"), "term"] <- "L0"

      boot.Linf<- exp(as.numeric(as.matrix(subset(bootnls,term == "Linf")[,3])))
      boot.g<-exp(as.numeric(as.matrix(subset(bootnls,term == "g")[,3])))
      boot.L0<-exp(as.numeric(as.matrix(subset(bootnls,term == "L0")[,3])))
      for (i in 1:length(Age)) {
        pv <- boot.L0*exp(log(boot.Linf/boot.L0)*(1-exp(-boot.g*Age[i])))
        Gom_Estimates[i,"low"] <- quantile(pv,0.025,na.rm = TRUE)
        Gom_Estimates[i,"upp"] <- quantile(pv,0.975,na.rm = TRUE)
      }
    } else {
      bootnls <- Data %>% boot_data(n.bootstraps) %>%
        do(tryCatch(suppressWarnings(tidy(minpack.lm::nlsLM(formula = gom.model,
                                                            data = .,
                                                            start = gom.start))),
                    error=function(e){

                      suppressWarnings(tryCatch(tidy(minpack.lm::nlsLM(formula = gom.model,
                                                                       data = .,
                                                                       start = list(Linf = log(max(.$Length)),
                                                                                    g = gom.start$g
                                                                       )
                      )
                      ),
                      error=function(e){
                        return(data.frame(term = as.character(c("Linf.(Intercept)","g.Lt")),
                                          estimate = NA,
                                          std.error= NA,
                                          statistic = NA,
                                          p.value=  NA,stringsAsFactors = FALSE)
                        )
                      },
                      silent=TRUE
                      )
                      )
                      },
                    silent=TRUE)) %>% as.data.frame()


      bootnls[which(bootnls$term == "Linf.(Intercept)"), "term"] <- "Linf"
      bootnls[which(bootnls$term == "g.Lt"), "term"] <- "g"

      boot.Linf<- exp(as.numeric(as.matrix(subset(bootnls,term == "Linf")[,3])))
      boot.g<-exp(as.numeric(as.matrix(subset(bootnls,term == "g")[,3])))
      for (i in 1:length(Age)) {
        pv <- Birth.Len*exp(log(boot.Linf/Birth.Len)*(1-exp(-boot.g*Age[i])))
        Gom_Estimates[i,"low"] <- quantile(pv,0.025,na.rm = TRUE)
        Gom_Estimates[i,"upp"] <- quantile(pv,0.975,na.rm = TRUE)
      }
    }
    Estimates <- suppressWarnings(bind_rows(Estimates, Gom_Estimates))
  }

  if(plots == T){

    max_len <- ifelse(max(Data$Length) > 500, "(mm)", "(cm)")
    p <- ggplot(Estimates, aes(Age, AVG, col = Model, fill = Model))+
      geom_point(data = Data, aes(Age, Length, col = NULL, fill = NULL), alpha = .2, col = "black") +
      geom_ribbon(aes(ymin = low, ymax = upp), alpha = .4)+
      geom_line(size = 1)+
      scale_y_continuous(name = paste("Length",max_len))+
      scale_x_continuous(name = "Age (years)", breaks = seq(0, Max.Age,1), expand = c(0,0), limits = c(0,Max.Age+0.5))+
      theme_bw()+
      theme(legend.position = c(0.8,0.2),
            legend.background = element_rect(colour = "black"),
            panel.grid.minor = element_blank())
    if(plot.legend == F){ p <- p + guides(col = "none", fill = "none")}

    print(p)




  } else {
    Results <- list.append(Results, Estimates = Estimates)
  }
  return(Results)
}

#' Calculate model averaged length-at-age estimates and parameters
#' @description `Calculate_MMI` takes the outputs from an `Estimate_Growth` function with plots = FALSE and returns the calculated model averaged parameters, SE and estimates based on AIC scores. It should be used if no candidate model has an AIC weight higher than 0.9. Use of this function should cite Smart et al (2016)
#' @param data An output from the Estimate_Growth function with plots = FALSE
#'
#' @return A list with model averaged parameters and a dataframe of model averaged length-at-age estimates
#' @export
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom plyr ldply
#' @importFrom stats glm lm nls nls.control predict quantile
#' @references Smart et al. (2016) Multi model approaches in shark and ray growth studies: strengths, weaknesses and the future. Fish and Fisheries. 17: 955-971\url{https://onlinelibrary.wiley.com/doi/abs/10.1111/faf.12154}


Calculate_MMI <- function(data){

  if(!any(names(data) == "Estimates")) stop("No length-at-age estimates provided")
  AICw<- data$AIC[,"Weight"]

  Linfs <- NULL
  L0s <- NULL
  for(i in 1:length(AICw)){
    Linfs[i] <- data[[i]][1] *AICw[i]
    L0s[i] <- data[[i]][3] *AICw[i]
  }
  AVG.Linf <- sum(Linfs)
  AVG.L0 <- sum(L0s)

  Linfs.SE <- NULL
  L0s.SE <- NULL
  for(i in 1:length(AICw)){
    Linfs.SE[i] <- AICw[i]*(data[[i]][1,2]^2+(Linfs[i]-AVG.Linf)^2)^0.5
    L0s.SE[i] <- AICw[i]*(data[[i]][3,2]^2+(L0s[i]-AVG.L0)^2)^0.5
  }
  AVG.Linf.SE <- sum(Linfs.SE)
  AVG.L0.SE <- sum(L0s.SE)

  AVG.pars <- data.frame(Parameter = c("Linf", "L0"),
                         AVG = c(AVG.Linf, AVG.L0),
                         SE = c(AVG.Linf.SE, AVG.L0.SE))

  Estimates <- data$Estimates %>% dplyr::filter(Age %in% seq(0,max(data$Estimates[,"Age"]),1)) %>% dplyr::select(Model, Age, AVG)

  tmp<- list()
  for(i in 1:length(AICw)){
    tmp[[i]] <- dplyr::filter(Estimates, Model == unique(Estimates$Model)[i]) %>% mutate(w.AVG = AVG *AICw[i])
  }
  Estimates <- plyr::ldply(tmp) %>% dplyr::select(-AVG) %>% spread(Model, w.AVG) %>% apply(MARGIN = 1,sum) %>%
    cbind(Age = 0:max(data$Estimates[,"Age"]), AVG = .) %>% as.data.frame()

  return(list("MMI parametrs" = AVG.pars, "MMI estimates" = Estimates))

}
