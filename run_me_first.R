#
# Packages for Timeseries analysis
#
# By Luis Paulo, 2017-03-01
#
# requires:
install.packages("data.tree", dependencies = TRUE)
install.packages("TTR", dependencies = TRUE)
# defines:
# Effort x Code Smells (Evolution) Correlation Breakdown Algorithm (ECCOBA)
#
# it creates a tree to analyse the minimal timespan of correlation 
# between effort and smells datasets
#
eccoba <- function(node,
                   effort_data, 
                   smells_churn_data, 
                   minimal_timeframe, 
                   minimal_correlation) {
  elen = length(effort_data)
  nelen <- as.integer(elen/2)
  # recursive calling until we reach minimal timeframe
  if (nelen > minimal_timeframe) {
    effort_data_left <- effort_data[1:nelen]
    smells_chrn_left <- smells_churn_data[1:nelen]
    nelen = nelen+1
    effort_data_rigt <- effort_data[nelen:elen]
    smells_chrn_rigt <- smells_churn_data[nelen:elen]
    
    corr_left <- cor(effort_data_left, smells_chrn_left)
    corr_rigt <- cor(effort_data_rigt, smells_chrn_rigt)
    # add node to the left
    str_node <- ""
    if (is.na(corr_left)) {
      str_node <- sprintf("NA[%d to %d]", 1, nelen)
    } else {
      if (corr_left < minimal_correlation) {
        str_node <- "low corr"
      } else {
        str_node <- "high corr"
      }
      str_node = sprintf("%s(%f)[%d to %d]", str_node, corr_left, 1, nelen)
    }
    tree_left <- node$AddChild(str_node)
    eccoba(tree_left, 
           effort_data_left, 
           smells_chrn_left, 
           minimal_timeframe = minimal_timeframe, 
           minimal_correlation = minimal_correlation)
    
    # add node to the right 
    str_node <- ""
    if (is.na(corr_rigt)) {
      str_node <- "NA"
      str_node <- sprintf("NA[%d to %d]", nelen, elen)
    } else {
      if (corr_rigt < minimal_correlation) {
        str_node <- "low corr"
      } else {
        str_node <- "high corr"
      }
      str_node = sprintf("%s(%f)[%d to %d]", str_node, corr_rigt, nelen + 1, elen)
    }
    tree_rigt <- node$AddChild(str_node)
    eccoba(tree_rigt, 
           effort_data_rigt, 
           smells_chrn_rigt, 
           minimal_timeframe = minimal_timeframe, 
           minimal_correlation = minimal_correlation)
  } else {
    return ()
  }
}
eccoba.caller <- function(effort_data, smells_churn_data, minimal_timeframe = 10, minimal_correlation = 0.75) {
  # we need library tree. If you dont have it, please, install it: 
  # install.packages("data.tree", dependencies = TRUE)
  library('data.tree')
  # we can only breakdown same-sized effort+churns timespans
  elen = length(effort_data)
  slen = length(smells_churn_data)
  if (elen != slen) {
    print('Effort data and Churn data must have the same size!')
  } else {
    tree <- Node$new("ECCOBA")
    eccoba(tree, 
           effort_data, 
           smells_churn_data, 
           minimal_timeframe = minimal_timeframe, 
           minimal_correlation = minimal_correlation)
    # print the whole tree
    print(tree)
  } 
}
# uncomment bellow if it is necessary to debug eccoba
#debug(eccoba.caller)
