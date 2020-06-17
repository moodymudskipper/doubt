is_syntactic_symbol <- function(x){
  make.names(x) == x
}

regex.escape <- function(string) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", string)
}

# two question marks surrounding any legal function name
double_qm_pattern <- "^[^?]*?(\\?([a-zA-Z.][a-zA-Z0-9._]*)\\?).*$"

op_qm_pattern <- "^.*?(([<]*[-+:*\\/<>!&|~=%^][%&|=]?)([a-zA-Z.][a-zA-Z0-9._]*)\\?).*$"
# the following to get several matches
op_qm_pattern2 <- "(([<]*[-+:*\\/<>!&|~=%^][%&|=]?)([a-zA-Z.][a-zA-Z0-9._]*)\\?)"
