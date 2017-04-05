#' notify_cell
#'
#' Send a text notification to cell phone via text.
#' Only works with ATT carriers.
#' Uses MMS in order to appear as coming from email address to recipient, rather than a new number every time.
#'
#' @param cell.number 10-digit phone number to contact, without hyphens or periods.
#' @param text.content String that is body of message, i.e. contents of text.
#' @export
#' @examples notify_cell(8005555555, "The simulation is finished.")

notify_cell = function(cell.number, text.content) {
    mailR::send.mail(from = "sack2.notifier@gmail.com", to = paste0(cell.number, "@mms.att.net"),
                     subject = "", body = paste0(text.content, "\n --from R ", Sys.time()),
                     smtp = list(host.name = "smtp.gmail.com", port = 465,
                                 user.name = "sack2.notifier", passwd = "Sack2notifier!", ssl = TRUE),
                                 authenticate = TRUE, send = TRUE)
}
