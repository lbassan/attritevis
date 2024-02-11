
# ----------------------------------------------------------------------
# Function 1: skip_to_attrite() [not available/necessary to users]
# ----------------------------------------------------------------------

# This function takes a matrix of 0,1s in which 1 indicates missingness (NA) per respondent per question and removes `skippers`.
# Skippers are individuals who have a 0s in their row followed by 1s.
skip_to_attrite <- function(arg) {
  n_col = length(arg)
  for (j in 1:n_col) {
    if (prod(arg[j:n_col]) == 1) {
      arg[j] = 1
    } else {
      arg[j] = 0
    }
  }
  return(arg)
}

# ----------------------------------------------------------------------
# Function 2: attrition()
# ----------------------------------------------------------------------

#' Transform dataframe into an attrition dataframe.
#'
#' The attrition dataframe indicates, per variable, how many respondents attrited. Note that this dataframe does not include "skippers", i.e. respondents who skipped questions. The dataframe also includes a variable that is the proportion of total N attrited, calculated as number of attrited respondents / starting N, and a variable that is the proportion of attrited by n at Qi, calculated as attrited respondents /  number of respondents entering into the question.

#' @param data A data.frame where variables are ordered by survey questions, such that earlier survey questions appear in smaller valued columns.
#' @return An attrition dataframe that contains the following variables:
#'  \describe{
#'     \item{attrited}{How many respondents attrited (left the survey) at each question}
#'     \item{proportion}{Number of attrited respondents / number of respondents who entered survey.}
#'     \item{prop_q}{Number of attrited respondents / number of respondents entering into the question}
#'     \item{questions}{Question names}
#'     \item{responded}{How many respondents responded in each question}
#'     \item{prop_r}{Number of respondents who responded / number of respondents who entered survey}
#'  }
#' @examples
#' attrition(empathy)
#' @importFrom Hmisc Lag
#' @export

attrition <- function(data) {
  #make sure arguments are correct
  if(is.null(data))
    stop("Data is null, please supply value")

  if(!inherits(data, "data.frame"))
    stop("Data must be data.frame")

  #save original dataset
  data_original <- data

  #for each missing value in the dataframe `dataset` give value 1, otherwise give 0.
  data <- apply(data_original, 2, function(x) {
    ifelse(is.na(x), 1, 0)
  })
  #First, create `attrited` variable by removing skippers
  #change `skippers` into 0 (we are only interested in respondents that attrited).
  data <- t(apply(data, 1, skip_to_attrite))
  data <- data.frame(colSums(data)) #sum the number of missing (minus skippers)
  colnames(data) <- "missing"
  #create variable `attrited` (completely left survey), rather than missing minus skippers
  data$attrited <- c(data[1, ], data[-1, ] - data[-nrow(data), ])
  #create variable `prop_q` = attrited / n entering into the question
  data$n_prev <- Hmisc::Lag(nrow(data_original) - as.numeric(data$missing), +1)
  data$n_prev[1] <- nrow(data_original)
  data$prop_q <- round(data$attrited/data$n_prev, 2)
  #`proportion`= attrited / starting N
  data$proportion <- round(data$attrited/nrow(data_original), 2)
  #add variable for question name
  data$questions <- colnames(data_original)

  #Next, we also create a `responded` variable that takes the number of individuals who responded
  #for each missing value in the dataframe `dataset` give value 0, otherwise give 1.
  data2 <- apply(data_original, 2, function(x) {
    ifelse(is.na(x), 0, 1)
  })
  data2 <- data.frame(colSums(data2)) #sum the number of responded
  colnames(data2) <- "responded"

  #create variable `prop_r` = responded / starting N
  data2$prop_r <- round(data2$responded/nrow(data_original),2)

  #combine the vars
  data$responded <- data2$responded
  data$prop_r <- data2$prop_r

  data$missing <- NULL
  data$n_prev <- NULL

  rownames(data) <- c()

  #return dataframe
  return(data)
}


# ----------------------------------------------------------------------
# Function 3: plot_attrition()
# ----------------------------------------------------------------------

#' Plot attrition/response in survey data over time.
#'
#' The function allows us to plot attrition across survey questions, indicating where treatment and outcome questions were collected.
#' @param data A data.frame where variables are ordered by survey questions, such that earlier survey questions appear in smaller valued columns.
#' @param y A character that corresponds to the Y axis. When y = "attrited", attrition is plotted. When y= "responded" responses are plotted. Default is y = "attrited".
#' @param freq A logical argument that notes whether Y axis is a raw number or a proportion. Default is freq=TRUE, which is the frequency of attrited/responded respondents. When freq=FALSE Y axis is the proportion of total N (attrited/responded), calculated as number of attrited/responded divided by the number of respondents entering into the question.
#' @param treatment_q A character of name(s) of question(s) in which treatments were administered. Marked in the plot with a red vertical line.
#' @param outcome_q A character of name(s) of outcome question(s). Marked in the plot with a blue vertical line.
#' @param mycolors A character string of color names to be used as values in "scale_colour_manual" argument in ggplot. Default is mycolors=NULL, which defaults to grayscale. "mycolors" must be == length of the unique values of the "treatment_q" variable.
#' @param title A character string to be used for plot title.
#' @param total A logical argument that notes whether the total # of attrited/responded is plotted. Default is TRUE. Argument can be changed to FALSE only when "treatment_q" is full.
#' @param tline A logical argument that allows users to remove  treatment vline, default is tline=TRUE
#' @param outcomeline A logical argument that allows users to remove outcome vlines, default is outcomeline=TRUE
#' @return An attrition plot.
#' @examples
#' # Simply plugging in the dataset into the function yields a figure
#' # that plots the number of respondents that attrited (left the survey
#' # completely) over each question in the study.
#' plot_attrition(empathy)
#'
#' # When users specify freq=FALSE, the y axis plots the proportion of
#' # attrited.
#' plot_attrition(empathy,
#'      freq = FALSE)
#'
#' # Users can further specify y="responded" to account for response,
#' # rather than attrition. This argument can be used with either freq=TRUE
#' # (default), or freq=FALSE, plotting response or proportion of responded,
#' # accordingly.
#' plot_attrition(empathy,
#'      y = "responded")
#'
#' # Using the outcome_q argument, users can specify where outcome questions
#' # were measured. These are noted with gray vertical lines.
#' plot_attrition(empathy,
#'      outcome_q = c("cards1", "cards2", "cards3"))
#'
#' # When treatment_q, which corresponds to treatment variable, is not NULL,
#' # the plot both notes where treatment was collected with a vertical line,
#' # and breaks down attrition by treatment conditions.
#' plot_attrition(empathy,
#'      y = "responded",
#'      outcome_q = c("cards1", "cards2", "cards3"),
#'      treatment_q = "treat1")
#' @importFrom dplyr filter rename
#' @importFrom data.table rbindlist
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @importFrom Hmisc Lag
#' @export

plot_attrition <- function(data
                           ,y = "attrited"
                           ,freq = TRUE
                           ,treatment_q = NULL
                           ,outcome_q = NULL
                           ,mycolors= NULL
                           ,title = NULL
                           ,total = TRUE
                           ,tline = TRUE
                           ,outcomeline = TRUE) {

  #make sure arguments are correct
  if(is.null(data))
    stop("Data is null, please supply value")

  if(!inherits(data, "data.frame"))
    stop("Data must be data.frame")

  if((y != "attrited") & (y != "responded"))
    stop("`y` must either be `attrited` or `responded`." )

  if(!inherits(y, "character"))
    stop("`y` must be a character.")

  if(!is.null(treatment_q) & !inherits(treatment_q, "character"))
    stop("`treatment_q` must be a character.")

  if(!is.null(outcome_q) & !inherits(outcome_q, "character"))
    stop("`outcome_q` must be a character.")

  if(!is.null(mycolors) & !inherits(mycolors, "character"))
    stop("`mycolors` must be a character.")

  if(!is.null(title) & !inherits(title, "character"))
    stop("`title` must be a character.")

  if(!inherits(freq, "logical"))
    stop("freq must be logical. Default is freq=TRUE.")

  if(!inherits(tline, "logical"))
    stop("tline must be logical. Default is tline=TRUE.")

  if(!inherits(outcomeline, "logical"))
    stop("outcomeline must be logical. Default is outcomeline=TRUE.")


  if(!is.null(treatment_q)) {
    #measure length of the treatment variable
    data3 <- dplyr::rename(data, cond_new = treatment_q)
    new_treat <- na.omit(data3$cond_new)
    #if length of mycolors is not equal fo length of treatment STOP
    if(!is.null(mycolors) & (length(mycolors)) != length(unique(new_treat)))
      stop("mycolors must be length of unique values of the `treatment_q`.")

  }

  data_original <- data #save this original data for reference

  #create an attrition/response dataset for ALL observations (not by condition)
  all_data <- attrition(data)
  all_data$treatment <- "Total"
  #we only want to keep the following variables:
  myvars <- c("attrited", "prop_q", "questions", "treatment", "responded", "prop_r")
  all_data <- all_data[myvars]

  # if users specify treatment_q, we split data *by conditions*
  if(!is.null(treatment_q)) {

    data2 <- dplyr::rename(data, cond_new = treatment_q) #create `cond_new` var based on conditions
    data$cond_new <- data2$cond_new

    #split the dataset into a list by conditions
    data_split <- split(data, with(data, cond_new), drop = TRUE)

    #for loop to account for attrition by condition
    listofdfs1 <- list()
    listofdfs2 <- list()
    for (i in 1:length(data_split)) {
      #first remove the `cond_new` var we created before
      df <- as.data.frame(data_split[i])
      #save ref of dataframe by condition to create n_prev
      df_orig <- df

      df[ncol(df)] <- NULL
      #for each missing value assign value 1, for complete response assign 0.
      df <- apply(df, 2, function(x) {
        ifelse(is.na(x), 1, 0)
        })
      #apply "skip_to_attrite" to get rid of skippers
      df <- t(apply(df, 1, skip_to_attrite))
      #sum the number of missing (minus skippers) per q
      df <- data.frame(colSums(df))
      #rename this variable `missing`
      colnames(df) <- "missing"
      #create variable `attrited`, rather than missing minus skippers
      df$attrited <-c(df[1, ], df[-1, ] - df[-nrow(df),])
      #`prop_q` = attrited / n entering into the question
      df$n_prev <- Hmisc::Lag(nrow(df_orig) - as.numeric(df$missing), +1)
      df$n_prev[1] <- nrow(df_orig)
      df$prop_q <- round(df$attrited / df$n_prev, 2)
      #add variable for question name
      df$questions <-colnames(data_original)
      #based on rownames per dataset, create `treatment` var
      df$treatment <- rownames(df)
      df$treatment <- gsub("\\..*", "", df$treatment)

      df$missing <- NULL
      df$n_prev <- NULL

      #create `responded` variable.
      df1 <- as.data.frame(data_split[i])
      df1[ncol(df1)] <- NULL
      df1 <- apply(df1, 2, function(x) {
        ifelse(is.na(x), 0, 1)
        })
      df1 <- data.frame(colSums(df1)) #sum the number of responded
      colnames(df1) <- "responded"

      #create variable `prop_r` = responded / starting N
      df1$prop_r <- round(df1$responded / nrow(data_original), 2)

      #remove rownames
      rownames(df) <- c()
      #save as a list
      listofdfs1[[i]] <- df
      listofdfs2[[i]] <- df1
    }

    #merge all datasets in the list
    data_combined_a <- data.table::rbindlist(listofdfs1)
    data_combined_b <- data.table::rbindlist(listofdfs2)

    data_combined_a$responded <- data_combined_b$responded
    data_combined_a$prop_r <- data_combined_b$prop_r

    #merge the combined dataset with the `all` data
    data <- data.table::rbindlist(list(all_data,data_combined_a),
                                  fill  = T)

  } else {
    data <- all_data
    }

  #create a vector for the unique values of the question names
  question_names<-unique(data$questions)
  #change question var to factor and numeric for plotting
  data$questions <- factor(data$questions,
                           levels=question_names)
  data$questions2<-as.numeric(data$questions)

  if(!is.null(treatment_q)){
    #create indicators for Vlines
    #treatment Vline
    treatment_vars<-as.data.frame(match(treatment_q, question_names))
    colnames(treatment_vars) <- "treatment_q"
    treatment_vars$label<-"Treatment Given" #labels
    treatment_vars$color<- "black" #color of vline
    #where the label appears on yaxis
    if(freq==FALSE){treatment_vars$ynum<- 0.5}
    if(freq==TRUE){treatment_vars$ynum<- nrow(data_original)/2}
    treatment_vars$size<-1
  }

  if(!is.null(outcome_q)){
    #outcome Vline
    DV<-as.data.frame(match(outcome_q, question_names))
    colnames(DV) <- "outcome_q"
    DV$label<-"Outcome Question"
    DV$color<- "gray48"
    if(freq==FALSE){DV$ynum<- 0.5}
    if(freq==TRUE){DV$ynum<- nrow(data_original)/2}
    DV$size<-1
  }

  #if `freq=TRUE` and `y = "attrited"` we plot the frequency of attrited (y=attrited)
  if(freq==TRUE & y == "attrited"){data$y<-data$attrited}
  if(freq==TRUE & y == "attrited"){yname<-"Attrited"}
  #if `freq=TRUE` and `y = "responded"` we plot the frequency of responded (y=responded)
  if(freq==TRUE & y == "responded"){data$y<-data$responded}
  if(freq==TRUE & y == "responded"){yname<-"Responded"}
  #if `freq=FALSE` and `y = "attrited"` we plot the proportion of attrited (y=prop_q)
  if(freq==FALSE & y == "attrited"){data$y<-data$prop_q}
  if(freq==FALSE & y == "attrited"){yname<-"Proportion of Attrited"}
  #if `freq=FALSE` and `y = "responded"` we plot the proportion of responded (y=prop_r)
  if(freq==FALSE & y == "responded"){data$y<-data$prop_r}
  if(freq==FALSE & y == "responded"){yname<-"Proportion of Responded"}

  #if users don't want to use total remove it
  if(total == FALSE) {data <-  dplyr::filter(data, .data$treatment!="Total")}

  if(!is.null(treatment_q)){
    p <- data %>%
      ggplot(aes(x = .data$questions2,
                 y = .data$y, group = .data$treatment)) +

      #scale x axis from 1:10
      scale_x_continuous(breaks=unique(data$questions2),
                         labels=question_names) + #label questions with Q

      #create geomlines for `treatment` and `control`
      geom_line(data = data, aes(x = .data$questions2,
                                 y = .data$y,
                                 color = .data$treatment,
                                 linetype = .data$treatment),
                size = 1.1,
                show.legend = FALSE) +

      #label `treatment` and `control`

      ggrepel::geom_text_repel(data = dplyr::filter(data, .data$questions2 == length(question_names)),
                      aes(label = .data$treatment,
                          x = .data$questions2,
                          y = .data$y,
                          color = .data$treatment),
                      min.segment.length = 0,
                      show.legend = FALSE) +

      #add a geom_point
      geom_point(size=2, aes(colour = factor(.data$treatment),
                             fill = factor(.data$treatment)), show.legend = FALSE)

  }else{

    p <- data %>%
      ggplot(aes(x = .data$questions2,
                 y = y)) +
      #scale x axis from 1:10
      scale_x_continuous(breaks=unique(data$questions2),
                         labels=question_names) + #label questions with Q
      geom_line(size = 1.1) +
      #add a geom_point
      geom_point(size=2)

  }

#set colors of geom_lines
  if(!is.null(mycolors) & total == TRUE) {p <- p + scale_colour_manual(values=c(Total = "gray", mycolors))}
  if(!is.null(mycolors) & total == FALSE) {p <- p + scale_colour_manual(values=c(mycolors))}
  if(is.null(mycolors) & total == TRUE) {p <- p + scale_colour_grey()}
  if(is.null(mycolors) & total == FALSE) {p <- p + scale_colour_grey()}

  #make treatment red and control blue

  #remove gray background
  p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor =
                   element_blank(), panel.background = element_blank(),
                 axis.line = element_line(colour = "black"),
                 axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +

    labs(x = "Survey Questions", y = yname)


  if(!is.null(title)){p <- p + ggtitle(title)}


  if(freq==FALSE){p <- p + ylim (0, 1)}
  if(freq==TRUE){p <- p + ylim (0, nrow(data_original))}

  # add title and labels to axis

  if(!is.null(outcome_q) & outcomeline==TRUE){
    #add the vertical lines
    p<-p +
      #DV vertical lines
      #annotate(geom = "vline",
       #        x = c(DV$outcome_q),
        geom_vline(xintercept = c(DV$outcome_q),
               color = c(DV$color),
               size = c(DV$size)) +

      annotate(geom = "text",
               label = c(DV$label),
               x = c(DV$outcome_q),
               y = c(DV$ynum),
               color = c(DV$color),
               angle = 90,
               vjust = 1.5)
  }


  if(!is.null(treatment_q) & tline == TRUE){
    #treatments vertical lines
    p<-p + #annotate(geom = "vline",
           # x = c(treatment_vars$treatment_q),
            geom_vline(xintercept = c(treatment_vars$treatment_q),
                    color = c(treatment_vars$color),
                    size = c(treatment_vars$size)) +

      annotate(geom = "text",
               label = c(treatment_vars$label),
               x = c(treatment_vars$treatment_q),
               y = c(treatment_vars$ynum),
               color = c(treatment_vars$color),
               angle = 90,
               vjust = 1.5)
  }
  print(p)
}
# ----------------------------------------------------------------------
# Function 4: balance_cov()
# ----------------------------------------------------------------------

#' Test for balance of covariates across specified treatment and control groups.
#'
#' Tests whether specified covariates are balanced across specified treatment and control groups. Output is a t-test if covariate is a numeric or integer, and a 2-sample proportion test if covariate is a factor.
#' @param data A data.frame from which \code{treatment} and \code{question} are taken.
#' @param treatment A character string that corresponds to the name of the treatment variable. Note that values of said variable must be specified as "treatment" and "control".
#' @param question A character string that corresponds to the name of the point in survey (question), for which balance test is required.
#' @param factor Logical argument that specifies whether question is a factor. Default is \code{factor=  FALSE} (i.e. question is a numeric or integer).
#' @param factor_name Character that corresponds to specific factor (i.e. female), if question is a factor (i.e. sex).
#' @param p_adjust Vector of numbers that correspond to p-values obtained in all tests. Use this to adjust for p-values if running multiple tests.
#' @return A t-test if covariate is a numeric or integer, and a 2-sample proportion test if covariate is a factor.
#' @examples
#' balance_cov(data = empathy,
#'      treatment = "treat1",
#'      question = "sex",
#'      factor = TRUE,
#'      factor_name = "female")
#' @importFrom dplyr rename
#' @importFrom knitr kable
#' @importFrom stats binomial na.omit p.adjust prop.test t.test
#' @export

balance_cov <- function(data, treatment,
                        question,
                        factor = FALSE,
                        factor_name = NULL,
                        p_adjust = NULL)
{

  #make sure arguments are correctly specified
  if(is.null(data))
    stop("Data is null, please supply value")

  if(!inherits(data, "data.frame"))
    stop("Data must be data.frame")

  if(is.null(treatment))
    stop("Treatment is null, please supply value")

  if(!inherits(treatment, "character"))
    stop("Treatment must be a character")

  if(is.null(question))
    stop("Question is null, please supply value")

  if(!inherits(question, "character"))
    stop("Question must be a character")

  if(!inherits(factor, "logical"))
    stop("Factor must be logical. Default is factor=FALSE")

  if((factor=FALSE) & is.null(factor_name))
    stop("Factor must be TRUE if factor_name is defined")

  if(!is.null(factor_name) & !inherits(factor_name, "character"))
    stop("factor_name must be character")

  if(!is.null(p_adjust) & !inherits(p_adjust, "numeric"))
    stop("p_adjust must be numeric")


  #subset datasets based on treatment and control arms
  data <- dplyr::rename(data, question1 = question,
                 treatment1 = treatment)

  treat_data<-data[ which(data$treatment1=='treatment'), ]
  control_data<- data[ which(data$treatment1=='control'), ]

  if(is.null(factor_name)){
    test <- t.test(treat_data$question1, control_data$question1) #if question is not a factor, run t.test
  }else{

    #define factor treatment and control
    factor_treat<- treat_data[ which(treat_data$question1==factor_name), ]
    factor_control<- control_data[ which(control_data$question1==factor_name), ]

    #define not_factor treatment and control
    not_factor_treat<- treat_data[ which(treat_data$question1 != factor_name), ]
    not_factor_control<- control_data[ which(control_data$question1 != factor_name), ]

    x<-c(nrow(factor_treat), nrow(factor_control))
    n<-c(nrow(factor_treat)+nrow(not_factor_treat),
         nrow(factor_control)+nrow(not_factor_control))

    #run two sample proportion test
    test <- prop.test(x,n)
  }


  print(test)

  if(!is.null(p_adjust)){

    a<- p.adjust(p = p_adjust, method = "BH", n = length(p_adjust))
    a<- data.frame(Original_p = p_adjust,
                   Adjusted_p = a)
    knitr::kable(a)
  }

}

# ----------------------------------------------------------------------
# Function 5: balance_attrite()
# ----------------------------------------------------------------------

#' Test for balance of attrition rate across specified treatment and control groups.
#'
#' Tests whether specified treatment causes attrition in a specified question.
#'
#' @param data A data.frame from which \code{treatment} and \code{question} are taken.
#' @param treatment A character string that corresponds to the name of the treatment variable. Note that values of said variable must be specified as "treatment" and "control".
#' @param question A character string that corresponds to the name of the point in survey (question), for which balance test is required.

#' @return A logistic regression, regressing attrition (remain in survey=0, attrited=1) over specified treatment.
#' @examples
#' balance_attrite(data = empathy,
#'      treatment = "treat1",
#'      question = "Happy_3_1")
#' @importFrom dplyr rename
#' @importFrom stats binomial glm
#' @export
#'

balance_attrite <- function(data, treatment,
                            question)
{

  #make sure arguments are correctly specified
  if(is.null(data))
    stop("Data is null, please supply value")

  if(!inherits(data, "data.frame"))
    stop("Data must be data.frame")

  if(is.null(treatment))
    stop("Treatment is null, please supply value")

  if(!inherits(treatment, "character"))
    stop("Treatment must be a character")

  if(is.null(question))
    stop("Question is null, please supply value")

  if(!inherits(question, "character"))
    stop("Question must be a character")

  data <- dplyr::rename(data, question1 = question,
                 treatment1 = treatment)

  #for each missing value in the dataframe `data` give value 1, otherwise give 0.
  data1 <- apply(data,2, function(x) {
    ifelse(is.na(x), 1, 0)
    })

  #change `skippers` into 0 (we are only interested in respondents that attrited).

  data1 <- apply(data1, 1, skip_to_attrite)
  data1 <- t(data1) #transpose data

  data1 <- data.frame(data1)

  #subset datasets based on treatment and control arms

  treatment1 <- data$treatment1 #from original data
  question1 <- data1$question1 #from attrition data (0 remained in survey, 1 attrited)
  data2 <- data.frame(treatment1, question1)

  model <- glm(question1~treatment1,
               data = data2,
               family = binomial(link = "logit")) #run glm

  print(summary(model))

}


# ----------------------------------------------------------------------
# Function 6: bounds()
# ----------------------------------------------------------------------

#' Calculate treatment effect bounds
#'
#' Yields extreme (Manski) bounds or trimming (Lee) bounds, using the \code{attrition} package by Alex Coppock.
#'
#' @param data A data.frame from which \code{treatment} and \code{DV} are taken.
#' @param treatment A character string that corresponds to the name of the treatment variable. Note that values of said variable must be specified as "treatment" and "control".
#' @param DV A character string that corresponds to the name of the outcome variable
#' @param type A character string that corresponds to the type of bounds required ("Manski" or "Lee"). Default is \code{type = "Manski"}.

#' @return A logistic regression, regressing attrition (remain in survey=0, attrited=1) over specified treatment.
#' @examples
#' # Generate simulated data, where Q1 to Q4 are pre-treatment questions,
#' # Q5 is the treatment assignment,
#' # and subsequent questions are post-treatment questions
#' n <- 1000
#' test_sim <- data.frame (
#'   Q1 = "agree", # consent
#'   Q2 = sample(c(18:90), n, rep = TRUE), # age
#'   Q3 = sample(c("m", "f"), n, rep = TRUE, prob = c(0.55, 0.45)), # sex
#'   Q4 = sample(c(0, 1), n, rep = TRUE),
#'   Q5 = sample(c("treatment", "control"), n, rep = TRUE), # treatment
#'   Q6 = sample(1:7, n, rep = TRUE), # outcome variable
#'   Q7 = sample(1:7, n, rep = TRUE),
#'   Q8 = sample(1:7, n, rep = TRUE),
#'   Q9 = sample(1:7, n, rep = TRUE),
#'   Q10 = sample(1:7, n, rep = TRUE)) # other general pre-treatment questions
#'
#' # Generate general attrition throughput
#' invisible(sapply(sample(1:nrow(test_sim), 500, 0.8 * nrow(test_sim)), function(x) {
#'   a <- sample(1:10, 1)
#'   test_sim[x, a:ncol(test_sim)] <<- NA
#' }))
#'
#' # Generate attrition that's correlated with the treatment
#' # specifically, we want to demonstrate attrition that happens at a certain time
#' # to do so, we add a running var that will demonstrate time
#' test_sim$no <- rownames(test_sim)
#' test_sim$Q6 <- ifelse(test_sim$Q5 == "control" & (test_sim$no > 200 & test_sim$no <
#'                                                       300), NA, test_sim$Q6)
#' test_sim$Q7 <- ifelse(is.na(test_sim$Q6), NA, test_sim$Q7)
#' test_sim$Q8 <- ifelse(is.na(test_sim$Q6), NA, test_sim$Q8)
#' test_sim$Q9 <- ifelse(is.na(test_sim$Q6), NA, test_sim$Q9)
#' test_sim$Q10 <- ifelse(is.na(test_sim$Q6), NA, test_sim$Q10)
#' test_sim$no <- NULL
#'
#' # Manski bounds (default option)
#' bounds(data = test_sim,
#'        treatment = "Q5",
#'        DV = "Q8")
#'
#' # Lee sharp bounds
#' bounds(data = test_sim,
#'        treatment = "Q5",
#'        DV = "Q7",
#'        type = "Lee")
#'
#' @importFrom dplyr rename
#' @import attrition
#' @export

bounds <- function(data, treatment,
                   DV, type = "Manski")
{

  #make sure arguments are correctly specified
  if(is.null(data))
    stop("Data is null, please supply value")

  if(!inherits(data, "data.frame"))
    stop("Data must be data.frame")

  if(is.null(treatment))
    stop("Treatment is null, please supply value")

  if(!inherits(treatment, "character"))
    stop("Treatment must be a character")

  if(is.null(DV))
    stop("Question is null, please supply value")

  if(!inherits(DV, "character"))
    stop("Question must be a character")

  if(!inherits(type, "character"))
    stop("Question must be a character")

  data <- dplyr::rename(data, DV1 = DV,
                 treatment1 = treatment)

  data <- data[which(!is.na(data$treatment1)), ]
  data$Y <- data$DV1
  data$Z <- ifelse(data$treatment1 == "treatment", 1, 0)
  data$R <- ifelse(is.na(data$Y), 0, 1)

  minY <- min(data$Y, na.rm = TRUE)
  maxY <- max(data$Y, na.rm = TRUE)

  # Set Y Z R to NULL first to satisfy CRAN checks
  Y <- NULL
  Z <- NULL
  R <- NULL

  if(type == "Lee") { print(
    attrition::estimator_trim(Y = Y,
                              Z = Z,
                              R = R, R1 = NULL, Attempt = NULL,
                              R2 = NULL, strata = NULL,
                              alpha = 0.05,
                              data = data))
    } else {
      print(attrition::estimator_ev(Y = Y,
                                    Z = Z,
                                    R = R,
                                    minY = minY, maxY = maxY, strata = NULL,
                                    alpha = 0.05,
                                    data = data))}
}


# ----------------------------------------------------------------------
# Function 7: attrition_table()
# ----------------------------------------------------------------------

#' Transform dataframe into attrition table(s)
#'
#' Yields the same data.frame as function attrition, but converts it into a table. Allows to subset table by treatment and control groups, which yields several tables by condition.
#'
#' @param data A data.frame where variables are ordered by survey questions, such that earlier survey questions appear in smaller valued columns.
#' @param treatment_q A character string that corresponds to treatment variable. When specified, the function yields several tables by condition.
#' @return A table or list of tables, each of which describes attrition by a specific treatment condition.
#' @examples
#' attrition_table(empathy)
#' attrition_table(data = empathy,
#'                    treatment_q = "treat1")
#' @importFrom dplyr all_of rename
#' @importFrom knitr kable
#' @import kableExtra
#' @export
#'

attrition_table <- function(data, treatment_q = NULL)
{

  list <- attrition(data)
  list <- knitr::kable(list)

  if(!is.null(treatment_q)){

    data <- dplyr::rename(data, cond_new = all_of(treatment_q)) #create `cond_new` var based on conditions
    data_split <- split(data, with(data, cond_new), drop = TRUE)
    listofdfs <- list()
    for (i in 1:length(data_split)) {
      #first remove the `cond_new` var we created before
      df <- as.data.frame(data_split[i])
      df1 <- attrition(df)
      listofdfs[[i]] <- df1

      list <- lapply(X = listofdfs, FUN = function(i) {
        knitr::kable(x = i, caption = i[1, treatment_q])})

    }}

  list

}

# ----------------------------------------------------------------------
# Function 8: vis_miss_treat()
# ----------------------------------------------------------------------

#' Visualize attrition and missingness
#'
#' Calls the \code{vis_miss} function from \code{visdat} package. We allow users to facet missingness by conditions, creating several missingness maps per condition.
#'
#' @param data A data.frame where variables are ordered by survey questions, such that earlier survey questions appear in smaller valued columns.
#' @param treatment_q A character string that corresponds to treatment variable. If \code{treatment_q = NULL}, missingness map appears for all data. If \code{treatment_q != NULL}, missingness is faceted by condition.
#' @return A plot that shows pattern of missingness.
#' @examples
#' vis_miss_treat(empathy)
#'
#' # The function allows users to facet missingness by conditions, creating
#' # several missingness maps per condition, and marks treatment variable with
#' # a red vertical line.
#' vis_miss_treat(empathy,
#'      treatment_q = "treat1")
#' @importFrom dplyr rename
#' @importFrom visdat vis_miss
#' @importFrom grid gpar textGrob
#' @importFrom ggpubr rremove ggarrange annotate_figure
#' @export
#'

vis_miss_treat <- function(data ,treatment_q = NULL)

{

  figure <- visdat::vis_miss(data)

  #split datasets
  if(!is.null(treatment_q)){
    data2 <- dplyr::rename(data, cond_new = treatment_q) #create `cond_new` var based on conditions
    data$cond_new <- data2$cond_new
    data_split <- split(data, with(data, cond_new), drop = TRUE)

    listofdfs <- list()
    list <- list()

    for (i in 1:length(data_split)) {
      #first remove the `cond_new` var we created before
      df <- as.data.frame(data_split[i])
      colnames(df) <- colnames(data)
      df$cond_new <- NULL
      listofdfs[[i]] <- df

      list <- lapply(X = listofdfs, FUN = function(i) {
        visdat::vis_miss(x = i) + theme(legend.position = "right") +
          geom_vline(xintercept = treatment_q
                     ,colour = "red") +
          ggpubr::rremove("ylab") +
          ggtitle(i[1, treatment_q])})
    }

    figure <-  ggpubr::ggarrange(plotlist=list, ncol = 1)

    figure <- ggpubr::annotate_figure(figure, left = grid::textGrob("Observations",
                                                      rot = 90, vjust = 1, gp = grid::gpar(cex = 1.5)))

  }

  print(figure)

}

