#'
sql_read <- function(x) {
  if(file.exists(system.file("sql", x, package = "goa_pcod"))) {
    readLines(system.file("sql", x, package = "goa_pcod"))
  } else {
    stop("The sql file does not exist.")
  }
}

collapse_filters <- function(x) {
  sprintf("'%s'", paste(x, collapse = "','"))
}

sql_add <- function(x, sql_code, flag = "-- insert table") {
  
  i = suppressWarnings(grep(flag, sql_code))
  sql_code[i] <- x
  sql_code
}
sql_filter <- function(sql_precode = "=", x, sql_code, flag = "-- insert species") {
  
  i = suppressWarnings(grep(flag, sql_code))
  sql_code[i] <- paste0(
    sql_precode, " (",
    collapse_filters(x), ")"
  )
  sql_code
}

sql_run <- function(database, query) {
  query = paste(query, collapse = "\n")
  DBI::dbGetQuery(database, query, as.is=TRUE, believeNRows=FALSE)
}

get_bin <- function(data, bins){
  bins %>% 
    tidytable::bind_cols(less = c(bins[2:length(bins)], 1000),
                         bin = seq(1, length(bins))) %>% 
    tidytable::rename(more = '...1') -> bin_test
  bin = vector(length = length(data$length))
  
  for(i in 1:length(data$length)){
    bin[i] = which(data$length[i] > bin_test$more & data$length[i] < bin_test$less)
  }
  data %>% 
    tidytable::bind_cols(bin) %>% 
    tidytable::rename(bin = '...4')
}

