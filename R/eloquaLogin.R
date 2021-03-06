#' Create a set of authentication credentials required for Eloqua
#'
#' added comment again and again
#'
#' @param pod The pod your Eloqua instance resides in (used to create the UIR string)
#' @param company The company/name of your Eloqua instance
#' @param username Your username
#' @param password Your password
#' @return An object of class eloquaLogin
eloquaLogin <- setClass(Class = "eloquaLogin",
                        slots = c(pod = "numeric", company = "character",
                                  username = "character", password = "character"))

