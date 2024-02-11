#' Illustration data for the atritevis package
#'
#' This dataset contains a sample of data from Lo, Renshon, and Bassan-Nygate
#' (2021), which is an experimental survey study on whether peer-praise can
#' encourage respondents to choose an empathy task.
#'
#' The experiment manipulates peer-praise and measures empathy in a behavioral
#' task. There are two arms in the peer-praise randomization: peer-praise and
#' no praise (control). In the first arm, a word cloud of praise, drawn from
#' real praise collected in a pilot study, is given for people who behave
#' empathetically, with a line of text about peer group average thermometer
#' ratings towards people who are empathetic – “Peers of yours on this platform
#' have said they hold favorable feelings towards people who engage in empathetic
#' behavior, with an average of 7.9, on a scale of 0 (least favorable) to 10
#' (most favorable), That same peer group provided real feedback for empathetic
#' behavior which is pictured in the word cloud below”. Respondents in the
#' control condition do not receive any additional information.
#'
#' The outcome of interest is choosing to empathize with an image in a behavioral
#' task. In the task, subjects choose between two “cards” a FEEL and a DESCRIBE
#' task, that correspond to an empathy or
#' objective task, in which they empathize/describe an image of a man.
#'
#' @docType data
#'
#' @usage data(empathy)
#'
#' @keywords datasets
#'
#' @format A dataframe with 624 observations and 72 variables.
#'
#' @source Lo, Renshon & Bassan-Nygate (2023), "Can Praise from Peers Promote
#' Empathy and Inclusive Behavior towards Racial or Ethnic Outgroups?" Working paper.
#'
#' @examples
#' data(empathy)
#' table(empathy$sex)
#' attrition_table(data = empathy,
#'                    treatment_q = "treat1")
"empathy"
