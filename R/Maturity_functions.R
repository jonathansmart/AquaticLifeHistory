
#' Estimate age-at-maturity
#' @description Age-at-maturity is estimated from binary maturity data using a logistic ogive.
#'Two options are availale depending on error structure. If binary data are used then a binomial
#'error stucture is required. If the user wishes to bin the data by age class then a quasibinomial error
#'structure is needed with the data weighted by the sample size of each bin. This is handled automatically by the function.
#' @param data A dataframe that includes age and a binary maturity status (immature = 0 and mature = 1).
#' Columns should be named "Age" and "Maturity" but the function is robust enough to accept some reasonable variations to these
#' @param error.structure The distribution for the glm used to produce the logistic ogive. Must be either "binomial"
#'  for binary data or "quasibinomial" for binned maturity at age. Proportion mature at each age is automatically calculated within the function
#' @param n.bootstraps Number of bootstrap iterations required to produce 95\% confidence intervals about the logistic ogive
#' @param display.points Should the raw data be plotted for the binomial model?
#' @param return Either: \describe{
#' \item{parameters}{The estimated logistic parameters and their standard error (A50 and A95)}
#' \item{estimates}{The logistic ogive predictions with 95 percent confidence intervals (useful for creating ones own plots)}
#' \item{plot}{a ggplot object of the logistic ogive.}
#' }
#'
#' @return Either: \describe{
#' \item{parameters}{a dataframe of the estimated logistic parameters and their standard error (A50 and A95)}
#' \item{estimates}{a dataframe of logistic ogive predictions with 95 percent confidence intervals}
#' \item{plot}{a ggplot object of the logistic ogive}
#' }
#' @import readr broom ggplot2 dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom MASS dose.p
#' @importFrom stats glm lm nls nls.control predict quantile
#' @export
#'
Estimate_Age_Maturity <- function(data, error.structure = "binomial", n.bootstraps = 1000, display.points = FALSE,  return = "parameters"){

  if(!error.structure %in% c("binomial", "quasibinomial")) {
    warning("'error.structure' has not been specified correctly. Defaulting to a logistic model with a binomial error structure")
    error.structure <- "binomial"
  }
  if(!return %in% c("parameters", "estimates", "plot")) stop("return must be either parameters, estimates or plot")
  if(return != "plot" & display.points == TRUE) warning("'display.points' argument ignored as no plot is being returned")

  if(error.structure == "quasibinomial" & display.points == TRUE) warning("'display.points' argument ignored for quasibinomial models")

  if(is.data.frame(data[,grep("age", substr(tolower(names(data)),1,3))])) stop("Age column heading could not be distinguished ")
  if(is.data.frame(data[,grep("mat", substr(tolower(names(data)),1,3))])) stop("Maturity column heading could not be distinguished ")

  age_col <- grep("age", substr(tolower(names(data)),1,3))
  if(length(age_col)>1){stop("Multiple columns determined for age variable")}
  mat_col <- grep("mat", substr(tolower(names(data)),1,3))
  if(length(mat_col)>1){stop("Multiple columns determined for maturity variable")}

  processed_data <- data.frame(Age = data[,age_col],
                               Maturity = data[,mat_col]) %>%
    dplyr::filter_if(is.numeric, all_vars(!is.na(.)))

  if(any(!unique(processed_data$Maturity) %in% c(0,1))) stop("Maturity data is not binary")

  if(error.structure == "binomial"){

    Maturity.Model<-glm(Maturity~Age,data = processed_data, family = "binomial")
    A50<- MASS::dose.p(Maturity.Model,p=c(0.5))
    A95<- MASS::dose.p(Maturity.Model,p=c(0.95))

    if(return == "parameters") {
      pars <- data.frame(rbind(A50,A95), rbind(attr(A50,"SE"), attr(A95,"SE")))
      colnames(pars) <- c("Estimate", "SE")
      return(pars)
    }

    new <- data.frame(Age = seq(0,max(processed_data$Age),0.01))

    preddat<-cbind( Age = new, predict(Maturity.Model, data.frame(Age=new$Age),se.fit=T,type = "response"))

    message("Bootstrapping logistic model with a binomial error structure")
    boot_maturity <-  processed_data %>% boot_data(n.bootstraps) %>%
      dplyr::do(glm(Maturity~Age,data = ., family = "binomial")%>%
           predict(data.frame(Age=new$Age),type="response") %>% cbind(Age = new$Age, Pred =.) %>% as.data.frame())

    boot_ests <- boot_maturity %>% dplyr::group_by(Age) %>% dplyr::summarize(high=quantile(Pred, 0.025),
                                                               low=quantile(Pred, 0.975))

    results <- cbind(boot_ests, Estimate = preddat$fit) %>%  dplyr::select(Age, Estimate, high,low)

    p <- ggplot(results, aes(x = Age, y = Estimate)) +
      geom_ribbon(aes(ymin = low, ymax = high), col = "darkgrey", fill = "grey", alpha =.3)+
      geom_line(size = 2, col = "royalblue") +
      geom_point(aes(A50[1], 0.5), col = 'black', size = 4)+
      scale_y_continuous(name = "Proportion mature",  limits = c(0,1), breaks = seq(0,1,.2), expand = c(0,0)) +
      scale_x_continuous(name = "Age (years)", expand = c(0,0),breaks = seq(0,ceiling(max(processed_data$Age)),1))+
      theme_bw()
    if(display.points == T){
      p <- suppressMessages(p + geom_point(data = processed_data, aes(Age, Maturity), alpha = .3, size = 2)+
                              scale_y_continuous(name = "Proportion mature",  limits = c(0,1), breaks = seq(0,1,.2)))

    }
  }

  if(error.structure == "quasibinomial"){
    weighted_data <- processed_data %>% dplyr::mutate(Age = round(Age)) %>%
      dplyr::group_by(Age) %>%
      dplyr::summarise(Maturity.prop = mean(Maturity), N = n())

    Maturity.Model<-glm(Maturity.prop~Age,data = weighted_data, family = "quasibinomial", weights = N)
    A50<- MASS::dose.p(Maturity.Model,p=c(0.5))
    A95<- MASS::dose.p(Maturity.Model,p=c(0.95))

    if(return == "parameters") {
      pars <- data.frame(rbind(A50,A95), rbind(attr(A50,"SE"), attr(A95,"SE")))
      colnames(pars) <- c("Estimate", "SE")
      return(pars)
    }

    new <- data.frame(Age = seq(0,ceiling(max(processed_data$Age))+1,0.01))
    preddat<-cbind(Age = new, predict(Maturity.Model, data.frame(Age=new$Age),se.fit=T,type = "response"))

    message("Bootstrapping logistic model with a quasibinomial error structure")

    boot_maturity <-  processed_data %>% boot_data(n.bootstraps) %>%
      dplyr::do(dplyr::mutate(.,Age = round(Age)) %>%
                  dplyr::group_by(Age) %>%
                  dplyr::summarise(Maturity.prop = mean(Maturity), N = n()) %>%
           glm(Maturity.prop~Age, data=.,family= "quasibinomial", weights = N) %>%
           predict(data.frame(Age=new$Age),type="response") %>% cbind(Age = new$Age, Pred =.) %>% as.data.frame())

    boot_ests <- boot_maturity %>% dplyr::group_by(Age) %>% dplyr::summarize(high=quantile(Pred, 0.025),
                                                               low=quantile(Pred, 0.975))

    results <- cbind(boot_ests, Estimate = preddat$fit) %>% dplyr::select(Age, Estimate, high,low)


    p <- ggplot(results, aes(x = Age, y = Estimate)) +
      geom_bar(data = weighted_data %>% mutate(Immaturity.prop = 1 - Maturity.prop) %>%
                 tidyr::gather(Maturity.group, prop, -Age, -N),
               aes(Age + 0.5, prop, fill = Maturity.group), col = "black", stat="identity", width = 1) +
      geom_ribbon(aes(ymin = low, ymax = high), col = "royalblue", fill = "lightblue", alpha = .6)+
      geom_line(size = 2, col = "royalblue") +
      geom_text(data = weighted_data , inherit.aes = F ,aes(Age + 0.5, y = 1.05, label = N))+
      geom_point(aes(A50[1], 0.5), col = 'black', size = 4)+
      scale_fill_manual(values=c("light grey", "grey"), guide = F)+

      scale_y_continuous(name = "Proportion mature",  limits = c(0,1.1), expand = c(0,0), breaks = seq(0,1,.2)) +
      scale_x_continuous(name = "Age (years)", expand = c(0,0), breaks = seq(0,ceiling(max(processed_data$Age)),1))+
      theme_bw()+
      theme(panel.grid = element_blank())

  }
   if (return == "estimates"){
    return(results)
  } else if(return == "plot"){
    return(p)
  }

}

#' Estimate length-at-maturity
#' @description Length-at-maturity is estimated from binary maturity data using a logistic ogive.
#'Two options are availale depending on error structure. If binary data are used then a binomial
#'error stucture is required. If the user wishes to bin the data by length class then a quasibinomial error
#'structure is needed with the data weighted by the sample size of each bin. This is handled automatically by the function.
#' @param data A dataframe that includes length and a binary maturity status (immature = 0 and mature = 1).
#' Columns should be named "Length" and "Maturity" but the function is robust enough to accept some reasonable variations to these
#' @param error.structure The distribution for the glm used to produce the logistic ogive. Must be either "binomial"
#'  for binary data or "quasibinomial" for binned maturity at length. Proportion mature at each length bin is automatically calculated within the function
#' @param n.bootstraps Number of bootstrap iterations required to produce 95\% confidence intervals about the logistic ogive
#' @param bin.width The width of the length-class bins used for a quasibinomial logistic model. These should on the same unit as the length data.
#' The y axis on any plots will automatically scale to the correct unit ("cm" or "mm")
#' @param display.points Should the raw data be plotted for the binomial model?
#' @param return Either: \describe{
#' \item{parameters}{The estimated logistic parameters and their standard error (L50 and L95)}
#' \item{estimates}{The logistic ogive predictions with 95 percent confidence intervals (useful for creating ones own plots)}
#' \item{plot}{a ggplot object of the logistic ogive.}
#' }
#'
#' @return Either: \describe{
#' \item{parameters}{a dataframe of the estimated logistic parameters and their standard error (L50 and L95)}
#' \item{estimates}{a dataframe of logistic ogive predictions with 95 percent confidence intervals}
#' \item{plot}{a ggplot object of the logistic ogive. If binned length classes are used, this includes a bar plot of proportional maturity }
#' }
#' @import readr broom ggplot2 dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom MASS dose.p
#' @importFrom stats glm lm nls nls.control predict quantile
#' @export


Estimate_Len_Maturity <- function(data, error.structure = "binomial", n.bootstraps = 1000, bin.width = NA, display.points = FALSE, return = "parameters"){

  if(!error.structure %in% c("binomial", "quasibinomial")) {
    warning("'error.structure' has not been specified correctly. Defaulting to a logistic model with a binomial error structure")
    error.structure <- "binomial"
  }
  if(!return %in% c("parameters", "estimates", "plot")) stop("return must be either parameters, estimates or plot")
  if(return != "plot" & display.points == TRUE) warning("'display.points' argument ignored as no plot is being returned")

  if(error.structure == "quasibinomial" & is.na(bin.width))stop("'bin.width' must be specified for a quasibinomial model")

  if(error.structure == "quasibinomial" & display.points == TRUE) warning("'display.points' argument ignored for quasibinomial models")

  if(is.data.frame(data[,grep("len|tl|lt|siz", substr(tolower(names(data)),1,3))])) stop("Length column heading could not be distinguished ")

  if(is.data.frame(data[,grep("mat", substr(tolower(names(data)),1,3))])) stop("Maturity column heading could not be distinguished ")

  len_col <- grep("len|tl|lt|siz", substr(tolower(names(data)),1,3))
  if(length(len_col)>1){stop("Multiple columns determined for length variable")}
  mat_col <- grep("mat", substr(tolower(names(data)),1,3))
  if(length(mat_col)>1){stop("Multiple columns determined for maturity variable")}

  processed_data <- data.frame(Length = data[,len_col],
                               Maturity = data[,mat_col]) %>%
    dplyr::filter_if(is.numeric, all_vars(!is.na(.)))

  if(any(!unique(processed_data$Maturity) %in% c(0,1))) stop("Maturity data is not binary")

  if(error.structure == "binomial"){

    Maturity.Model<-glm(Maturity~Length,data = processed_data, family = "binomial")
    L50<- MASS::dose.p(Maturity.Model,p=c(0.5))
    L95<- MASS::dose.p(Maturity.Model,p=c(0.95))

    if(return == "parameters") {
      pars <- data.frame(rbind(L50,L95), rbind(attr(L50,"SE"), attr(L95,"SE")))
      colnames(pars) <- c("Estimate", "SE")
      return(pars)
    }

    if(max(processed_data$Length) < 500){
      new <- data.frame(Length = seq(0,max(processed_data$Length),0.01))
    } else{
      new <- data.frame(Length = seq(0,max(processed_data$Length),0.1))
    }

    preddat<-cbind(Length = new, predict(Maturity.Model, data.frame(Length=new$Length),se.fit=T,type = "response"))

    message("Bootstrapping logistic model with a binomial error structure")
    boot_maturity <-  processed_data %>% boot_data(n.bootstraps) %>%
      dplyr::do(glm(Maturity~Length,data = ., family = "binomial")%>%
           predict(data.frame(Length=new$Length),type="response") %>%
           cbind(Length = new$Length, Pred =.) %>%
           as.data.frame())

    boot_ests <- boot_maturity %>% dplyr::group_by(Length) %>% dplyr::summarize(high=quantile(Pred, 0.025),
                                                                  low=quantile(Pred, 0.975))

    results <- cbind(boot_ests, Estimate = preddat$fit) %>% dplyr::select(Length, Estimate, high,low)

    p <- ggplot(results, aes(x = Length, y = Estimate)) +
      geom_ribbon(aes(ymin = low, ymax = high), col = "darkgrey", fill = "grey", alpha =.3)+
      geom_line(size = 2, col = "royalblue") +
      geom_point(aes(L50[1], 0.5), col = 'black', size = 4)+
      scale_y_continuous(name = "Proportion mature",  limits = c(0,1), breaks = seq(0,1,.2), expand = c(0,0)) +
      scale_x_continuous(name = "Length (cm)",limits =c(min(processed_data$Length)*.8,max(processed_data$Length)*1.1), expand = c(0,0))+
      theme_bw()
    if(display.points == T){
      p <- suppressMessages(p + geom_point(data = processed_data, aes(Length, Maturity), alpha = .3, size = 2)+
                              scale_y_continuous(name = "Proportion mature",  limits = c(0,1), breaks = seq(0,1,.2)))
    }
    if(max(processed_data$Length) > 500){
      p <- suppressMessages(p + scale_x_continuous(name = "Length (mm)",
                                                   limits =c(min(processed_data$Length)*.8,max(processed_data$Length)*1.1),
                                                   expand = c(0,0)))
    }
  }

  if(error.structure == "quasibinomial"){
    if(max(processed_data$Length) < 500){
    weighted_data <- processed_data %>% dplyr::mutate(Len.bin = cut(Length, breaks = seq(0, max(Length)+bin.width, bin.width))) %>%
      tidyr::separate(Len.bin, into = c("Len.start","Len.fin"), sep = ",", remove = F) %>%
      dplyr::mutate(Length = as.numeric(readr::parse_number(Len.start)),
             Len.bin = paste(readr::parse_number(Len.start), readr::parse_number(Len.fin), sep = "-")) %>%
      dplyr::group_by(Length, Len.bin) %>%
      dplyr::summarise(Maturity.prop = mean(Maturity), N = n())
    }else{
      weighted_data <- processed_data %>% dplyr::mutate( Length = Length/10,
                                                  Len.bin = cut(Length, breaks = seq(0, max(Length)+bin.width, bin.width/10))) %>%
        tidyr::separate(Len.bin, into = c("Len.start","Len.fin"), sep = ",", remove = F) %>%
        dplyr::mutate(Length = as.numeric(readr::parse_number(Len.start))*10,
               Len.bin = paste(readr::parse_number(Len.start)*10, readr::parse_number(Len.fin)*10, sep = "-")) %>%
        dplyr::group_by(Length, Len.bin) %>%
        dplyr::summarise(Maturity.prop = mean(Maturity), N = n())
    }

    Maturity.Model<-glm(Maturity.prop~Length,data = weighted_data, family = "quasibinomial", weights = N)
    L50<- MASS::dose.p(Maturity.Model,p=c(0.5))
    L95<- MASS::dose.p(Maturity.Model,p=c(0.95))

    if(return == "parameters") {
      pars <- data.frame(rbind(L50,L95), rbind(attr(L50,"SE"), attr(L95,"SE")))
      colnames(pars) <- c("Estimate", "SE")
      return(pars)
    }

    if(max(processed_data$Length) < 500){
      new <- data.frame(Length = seq(0,max(processed_data$Length),0.01))
    } else{
      new <- data.frame(Length = seq(0,max(processed_data$Length),0.1))
    }

    preddat<-cbind(Length = new, predict(Maturity.Model, data.frame(Length=new$Length),se.fit=T,type = "response"))

    message("Bootstrapping logistic model with a quasibinomial error structure")
    if(max(processed_data$Length) < 500){
    boot_maturity <-  processed_data  %>% boot_data(n.bootstraps) %>%
      dplyr::do(dplyr::mutate(.,Len.bin = cut(Length, breaks = seq(0, max(Length)+bin.width, bin.width))) %>%
           tidyr::separate(Len.bin, into = c("Len.start","Len.fin"), sep = ",", remove = F) %>%
             dplyr::mutate(Length = as.numeric(readr::parse_number(Len.start)),
                  Len.bin = paste(readr::parse_number(Len.start), readr::parse_number(Len.fin), sep = "-")) %>%
             dplyr::group_by(Length, Len.bin) %>%
           dplyr::summarise(Maturity.prop = mean(Maturity), N = n()) %>%
           glm(Maturity.prop~Length, data=.,family= "quasibinomial", weights = N) %>%
           predict(data.frame(Length=new$Length),type="response") %>%
           cbind(Length = new$Length, Pred =.) %>% as.data.frame())
    }else{
      boot_maturity <-  processed_data  %>% boot_data(n.bootstraps) %>%
        dplyr::do(dplyr::mutate(.,
                  Length = Length/10,
                  Len.bin = cut(Length, breaks = seq(0, max(Length)+bin.width, bin.width/10))) %>%
             tidyr::separate(Len.bin, into = c("Len.start","Len.fin"), sep = ",", remove = F) %>%
               dplyr::mutate(Length = as.numeric(readr::parse_number(Len.start))*10,
                    Len.bin = paste(readr::parse_number(Len.start)*10, readr::parse_number(Len.fin)*10, sep = "-")) %>%
               dplyr::group_by(Length, Len.bin) %>%
             dplyr::summarise(Maturity.prop = mean(Maturity), N = n()) %>%
             glm(Maturity.prop~Length, data=.,family= "quasibinomial", weights = N) %>%
             predict(data.frame(Length=new$Length),type="response") %>%
             cbind(Length = new$Length, Pred =.) %>% as.data.frame())
    }

    boot_ests <- boot_maturity %>% dplyr::group_by(Length) %>% dplyr::summarize(high=quantile(Pred, 0.025),
                                                                  low=quantile(Pred, 0.975))

    results <- cbind(boot_ests, Estimate = preddat$fit) %>% dplyr::select(Length, Estimate, high,low)

    p <- ggplot(results, aes(x = Length, y = Estimate)) +
      geom_bar(data = weighted_data %>% mutate(Immaturity.prop = 1 - Maturity.prop) %>%
                 tidyr::gather(Maturity.group, prop, -Length, -N, -Len.bin),
               aes(Length, prop, fill = Maturity.group), col = "black", stat="identity", width = bin.width) +
      geom_ribbon(aes(ymin = low, ymax = high), col = "royalblue", fill = "lightblue", alpha = .6)+
      geom_line(size = 2, col = "royalblue") +
      geom_text(data = weighted_data , inherit.aes = F ,aes(Length, y = 1.1, label = N))+
      geom_text(data = weighted_data , inherit.aes = F ,aes(Length, y = 1.05, label = Len.bin))+
      geom_point(aes(L50[1], 0.5), col = 'black', size = 4)+
      scale_fill_manual(values=c("light grey", "grey"), guide = F)+
      scale_y_continuous(name = "Proportion mature",  limits = c(0,1.15), expand = c(0,0), breaks = c(seq(0,1,.2),1.05,1.1),
                         labels = c(seq(0,1,.2), "Length bin", "n")) +
      scale_x_continuous(name = "Length (cm)",
                         limits = c(floor(min(weighted_data$Length))-bin.width/2,ceiling(max(weighted_data$Length))+bin.width/2),
                         expand = c(0,0),
                         breaks = weighted_data$Length)+
      theme_bw()+
      theme(panel.grid = element_blank())
    if(max(processed_data$Length) > 500){
      p <- suppressMessages(p + scale_x_continuous(name = "Length (mm)",
                                                   limits =c(min(processed_data$Length)*.8,max(processed_data$Length)*1.1),
                                                   expand = c(0,0),
                                                   breaks = weighted_data$Length))
    }

  }

  if (return == "estimates"){
    return(results)
  }
  if(return == "plot"){
    return(p)
  }
}

