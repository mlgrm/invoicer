#' Create an invoice object
#' 
#' @param month specify month for a one-month invoice
#' @param year invoice year
#' @param since if month is not specified, beginning of invoice period
#' @param until end of invoice period
#' @param date invoice date, defaults to the day after until
#' @param client client name
#' @param address client address
#' @param expenses amount of expenses for current month
#' @param template_key the google drive key for the template to use.
#'    must be in your google drive
#' @param inv_number invoice number/code
#' 
#' @return an invoice object, a tibble with the following attributes:
#' \itemize{
#'    \item {fields} {the various invoice-specific values, such as date
#'    and address}
#'    \item {tmpl} {google drive document key of the template}
#'    \item {url} {url of the google sheet generated}
#' }
#' 
#' @examples
#' \dontrun{
#' # create an invoice for the latest complete calendar month using the default
#' # client, address, and template
#' invoice()
#' 
#' # create an invoice for this month to date
#' invoice(
#'   since = lubridate::floor_date(today(),"month"),
#'   until = lubridate::today()
#' )
#' }
#' 
#' @export
#' @import googlesheets
#' @import googledrive
#' @import tibble
#' @importFrom togglr get_dashboard


invoice <- function(
  month = NULL,
  year = lubridate::year(lubridate::today()),
  since = if (is.null(month)) {
    d <- lubridate::today()-months(1)
    lubridate::day(d) <- 1
    d
  } else lubridate::ymd(sprintf("%s-%s-01", year, month)),
  until = since+months(1)-lubridate::days(1),
  client = getOption("invoicer_default_client","without client"),
  date = until + lubridate::days(1),
  address = getOption("invoicer_default_address",""),
  expenses = 0,
  template_key = getOption("invoicer_template_key", 
                           stop("template key must be specified")),
  inv_num = 0
){
  smry <- get_dashboard(since = since, until = until)$synthese
  smry <- smry[smry$client == client,]
  if(nrow(smry) == 0){
    message("no results for month ", month)
    return(NULL)
  }
  proj_code <- sub("\\s.+$", "", smry$project)
  proj_desc <- sub("^[A-Z0-9]+\\s+","", smry$project)
  recs <- tibble(
    inum = 1:nrow(smry), 
    proj_code = proj_code, 
    proj_desc = proj_desc,
    quant = smry$time/24/60/60/1000,
    rate = 50,
    total = 24 * rate * quant
  )
  fields <- list(
    period = sprintf(
      "%s - %s", format(since,"%d.%m.%Y"),
      format(until, "%d.%m.%Y")
    ),
    date = format(date,"%d.%m.%Y"),
    inv_num = inv_num,
    client = client,
    address = address,
    fees_total = sum(recs$total),
    expenses = expenses,
    inv_total = expenses + sum(recs$total)
  )
  sh <- gs_key(template_key)
  
  # replace keys with field values in tmpl
  tmpl <- gs_read(sh,"Fees",col_names = F)
  tmpl[is.na(tmpl)] <- ""
  for(fn in names(fields)){
    addr <- which(tmpl==sprintf("${%s}",fn),arr.ind = T)
    tmpl[addr[1],addr[2]] <- fields[[fn]]
  }
  
  # create a copy of template and open it into sh
  sh <- gs_key(
    drive_cp(
      as_id(template_key),
      sprintf("Invoice %s - %s", client, format(date,"%Y%m%d"))
    )$id
  )
  
  # overwrite the copy with tmpl
  sh <- gs_edit_cells(sh,"Fees",tmpl,col_names = F)
  
  # copy recs into the copy at position "recs_anchor"
  anchor <- which(tmpl == "${recs_anchor}", arr.ind = T)
  anchor <- sprintf("R%dC%d",anchor[1],anchor[2])
  sh <- gs_edit_cells(sh,"Fees", recs, anchor = anchor, col_names = F)
  sh
}

get_pdf <- function(key,path="."){
  drive_download(as_id(key),paste0(path,'/',as_dribble(as_id(key))$name,'.pdf'))
}

