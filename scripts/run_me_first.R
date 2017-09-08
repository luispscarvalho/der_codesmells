# By Luis Paulo 
#
# First version: 2017-03
# Revision: 2017-08
#

#requires
install.packages("data.tree", dependencies = TRUE)
install.packages("TTR", dependencies = TRUE)
#defines
#paths
workspace.path   <- '/misc/workspace/doutorado/workspaces/research/effortprocessing'
workspace.dspath <- paste(workspace.path, 'datasets', sep = '/')
resys.csvpath    <- '/misc/workspace/doutorado/workspaces/research/resys/web/csv/input'
#Effort x Code Smells Correlation Breakdown Algorithm (ECCOBA)
#
#it creates a tree to analyse the minimal timespan of correlation 
#between effort and smells datasets
eccoba <- function(project,
                   node,
                   data,
                   minimal_timeframe, 
                   minimal_correlation,
                   listener_function) {
  #preps effort and smells datasets
  effort_data <- data$enhancement + data$bug
  smells_data <- data$gc + data$bc + data$dc + data$bm + data$lm
  
  elen <- length(effort_data)
  nelen <- as.integer(elen/2)
  #recursive calling until we reach minimal timeframe
  if (nelen > minimal_timeframe) {
    effort_data_left <- effort_data[1:nelen]
    smells_data_left <- smells_data[1:nelen]
    nelen = nelen+1
    effort_data_rigt <- effort_data[nelen:elen]
    smells_data_rigt <- smells_data[nelen:elen]
    
    corr_left <- cor(effort_data_left, smells_data_left)
    corr_rigt <- cor(effort_data_rigt, smells_data_rigt)
    #add node to the left
    str_node <- ""
    if (is.na(corr_left)) {
      str_node <- sprintf("NA[%d to %d]", 1, nelen)
    } else {
      if (corr_left < minimal_correlation) {
        str_node <- "low cor."
      } else {
        #effort correlates with smells, call injected listener
        listener_function(project, data[1:nelen,], corr_left)
        str_node <- "high cor."
      }
      #formatting node's output
      str_node = sprintf("%s(%f)[%d to %d]", str_node, corr_left, 1, nelen)
    }
    tree_left <- node$AddChild(str_node)
    #recursive calling to process next left sub-level
    eccoba(project, 
           tree_left, 
           data[1:nelen,],
           minimal_timeframe = minimal_timeframe, 
           minimal_correlation = minimal_correlation,  
           listener_function = listener_function)
    
    #add node to the right 
    str_node <- ""
    if (is.na(corr_rigt)) {
      str_node <- "NA"
      str_node <- sprintf("NA[%d to %d]", nelen, elen)
    } else {
      if (corr_rigt < minimal_correlation) {
        str_node <- "low cor."
      } else {
        #effort correlates with smells, call injected listener
        listener_function(project, data[nelen + 1:elen,], corr_rigt)
        str_node <- "high cor."
      }
      #formatting node's output
      str_node = sprintf("%s(%f)[%d to %d]", str_node, corr_rigt, nelen + 1, elen)
    }
    tree_rigt <- node$AddChild(str_node)
    #recursive calling to process next right sub-level
    eccoba(project, 
           tree_rigt, 
           data[nelen:elen,],
           minimal_timeframe = minimal_timeframe, 
           minimal_correlation = minimal_correlation,
           listener_function = listener_function)
  } else {
    return ()
  }
}
#preferably, ECCOBA must be activated by the caller
eccoba.caller <- function(project, data, minimal_timeframe = 10, minimal_correlation = 0.75,
                          listener_function) {
  #we need library tree
  library('data.tree')
  dlen = length(data)
  if (dlen == 0) {
    print('Data is empty!')
  } else {
    tree <- Node$new("ECCOBA")
    #init recursive calling of ECCOBA
    eccoba(project, 
           tree, 
           data, 
           minimal_timeframe = minimal_timeframe, 
           minimal_correlation = minimal_correlation, 
           listener_function = listener_function)
    #print the tree
    print(tree)
  } 
}
#debug(eccoba.caller)
#COntextualized REfactorings REcommendation ALgorithm (CORAL)
EM    <<- 0
RTWQ  <<- 0
IPO   <<- 0
PWO   <<- 0
DC    <<- 0
RMWMO <<- 0
CCE   <<- 0
MM    <<- 0
EF    <<- 0
ECo   <<- 0
HM    <<- 0
EI    <<- 0
ESubC <<- 0
RDWO  <<- 0
EH    <<- 0
ESupC <<- 0
MF    <<- 0
PF    <<- 0
PM    <<- 0
RCWP  <<- 0
EC    <<- 0
#reset all recommendations
coral.reset <- function() {
  EM    <<- 0
  RTWQ  <<- 0
  IPO   <<- 0
  PWO   <<- 0
  DC    <<- 0
  RMWMO <<- 0
  CCE   <<- 0
  MM    <<- 0
  EF    <<- 0
  ECo   <<- 0
  HM    <<- 0
  EI    <<- 0
  ESubC <<- 0
  RDWO  <<- 0
  EH    <<- 0
  ESupC <<- 0
  MF    <<- 0
  PF    <<- 0
  PM    <<- 0
  RCWP  <<- 0
  EC    <<- 0
}
#recommend by each smell
coral.gc <- function(gc) {
  if ((!is.na(gc)) && (gc > 0)) {
    # recommend refactorings for GC
    EI    <<- EI + gc
    ESubC <<- ESubC + gc
    RDWO  <<- RDWO + gc
    EH    <<- EH + gc
    ESupC <<- ESupC + gc
    MF    <<- MF + gc
    MM    <<- MM + gc
    PF    <<- PF + gc
    PM    <<- PM + gc
    RCWP  <<- RCWP + gc
  }
}
coral.bc <- function(bc) {
  if ((!is.na(bc)) && (bc > 0)) {
    # recommend refactorings for BC
    EC <<- EC + bc
    EM <<- EM + bc
  }
}
coral.dc <- function(dc) {
  if ((!is.na(dc)) && (dc > 0)) {
    # recommend refactorings for DC
    MM  <<- MM + dc
    EF  <<- EF + dc
    ECo <<- ECo + dc
    HM  <<- HM + dc
    EM  <<- EM + dc
  }
}
coral.bm <- function(bm) {
  if ((!is.na(bm)) && (bm > 0)) {
    # recommend refactorings for BM
    EM    <<- EM + bm
    MM    <<- MM + bm
    RCWP  <<- RCWP + bm
    RMWMO <<- RMWMO + bm
  }
}
coral.lm <- function(lm) {
  if ((!is.na(lm)) && (lm > 0)) {
    # recommend refactorings for LM
    EM    <<- EM + lm
    RTWQ  <<- RTWQ + lm
    IPO   <<- IPO + lm
    PWO   <<- PWO + lm
    DC    <<- DC + lm
    RMWMO <<- RMWMO + lm
    CCE   <<- CCE + lm
  }
}
#configure a new recommendation line
coral.line <- function() {
  line <- paste(EM, RTWQ, IPO, PWO, DC, RMWMO, CCE, MM, EF, ECo, HM, EI, ESubC, RDWO, EH, 
                ESupC, MF, PF, PM, RCWP, EC, sep = ',')
  
  return(line)
}
#main caller function
coral.caller <- function(project, data, correlation) {
  csv = paste(project, 'refactorings.csv', sep = '_')
  csv = paste(workspace.dspath, csv, sep = '/')
  #create a new csv file if it does not exist
  if (!file.exists(csv)) {
    #add header to new file
    header <- 'date,corr,EM,RTWQ,IPO,PWO,DC,RMWMO,CCE,MM,EF,ECo,HM,EI,ESubC,RDWO,EH,ESupC,MF,PF,PM,RCWP,EC'
    write(header, file = csv)
  }
  #fill the recommended refactorings in the file
  #open file for appending
  data_conn <- file(csv, 'a+b')
  for(i in 1:nrow(data)) {
    #reset all recommendations
    coral.reset()
    #prepare for recommendations
    row <- data[i,]
    date <- row$date
    if (!is.na(date)) {
      gc <- row$gc
      bc <- row$bc
      dc <- row$dc
      bm <- row$bm
      lm <- row$lm
      #recommend by smell
      coral.gc(gc)
      coral.bc(bc)
      coral.dc(dc)
      coral.bm(bm)
      coral.lm(lm)
      #write recommendations to csv file    
      line <- paste(date, correlation, coral.line(), sep = ',')
      write(line, file = csv, append = TRUE)
    }
  }
}
# exporter funtion for resys recommendations processing
export.to.resys <- function(project, data, correlation) {
  csv = paste("eccoba", project, 'corrcommits.csv', sep = '_')
  csv = paste(resys.csvpath, csv, sep = '/')
  #create a new csv file if it does not exist
  if (!file.exists(csv)) {
    #add header to new file
    header <- 'date,corr,commits'
    write(header, file = csv)
  }
  #fill the recommended refactorings in the file
  #open file for appending
  data_conn <- file(csv, 'a+b')  
  for(i in 1:nrow(data)) {
    row <- data[i,]
    date <- row$date
    if (!is.na(date)) {
      commits <- row$commits
    
      line <- paste(date, correlation, commits, sep = ',')
      write(line, file = csv, append = TRUE)
    }
  }
}
