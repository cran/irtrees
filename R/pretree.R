pull = function(m, i) {
  stopifnot(length(i)==1)
  if (is.matrix(m)) m[,i] else m[[i]]
  } 

########################## recode wide-format datasets to wide-format IRTree datasets ##############################

################ when all items have one common tree structure ###################

#' Wide-to-wide, one tree
#' 
#' Recode a wide format data frame to a wide format IRTrees data frame
#' in the case when all items have the same tree structure
#' 
#' 
#' @param data a wide-format (person-by-item) data set
#' @param cmx a a category-by-node mapping matrix
#' @param id.col the ID column
#' @param resp.col the columns containing the item responses
#' @param covar.col columns containing covariates
#' @param time.col the time column when there are repeated (longitudinal) data
#' @details Many examples of mapping matrices are given in the vignette. Columns may be specified 
#' by numeric index or by name. Response categories must be coded with consecutive integers 
#' starting from 1, not 0. Missing data should be properly represented as NA.
WtoW_single.tree <- function(data, cmx, id.col=NULL, resp.col = NULL, covar.col = NULL, time.col=NULL){
  
  stopifnot("data must be a matrix or data frame" = (is.matrix(data) | is.data.frame(data)))
  stopifnot("cmx must be a matrix or data frame" = (is.matrix(cmx) | is.data.frame(cmx)))
  n.node <- ncol(cmx) # number of nodes
  n.cate <- nrow(cmx) # number of categories
  # if all columns are not specified, then treat all columns as resp.col
  # otherwise, resp.col must be specified
  if (is.null(id.col) & is.null(resp.col) & is.null(covar.col) & is.null(time.col)){
    resp.data <- as.matrix(data)
    resp.col <- c(1:ncol(data))
  } else {
    stopifnot("resp.col must be a vector including the columns of the categorical responses to be modeled.
              The columns can be specified by the column names or the column numbers" = (is.vector(resp.col)&(is.character(resp.col)|is.numeric(resp.col))))
    if (length(resp.col)==1){resp.data <- matrix(pull(data,resp.col),ncol = 1)} else {
      resp.data <- as.matrix(data[,resp.col])
    }
  }
  
  stopifnot("item categories should be integers from 1 to nrow(cmx). If the item categories are character or factor, please convert them to numeric." 
            = (length(setdiff(unique(na.omit(as.vector(resp.data))),c(1:nrow(cmx))))==0))
  
  n.var <- ncol(resp.data) # n.var is the number of items
  # recode the dataset to IRTree dataset based on the mapping matrix cmx
  IRTree.data <- data.frame(matrix(as.vector(cmx[as.vector(resp.data),]),nrow = nrow(resp.data)))
  
  # create column names for IRTree.data
  for (node.i in 1:n.node){
    # if resp.col already includes column names, IRTree.data will keep these names and add ":node#"
    if (is.character(resp.col)){
      colnames(IRTree.data)[((node.i-1)*n.var+1):(node.i*n.var)] <- paste(resp.col,
                                                                          sprintf(paste(":node%0", nchar(n.node), "d", sep=''),node.i),sep = "")
    }
    
    if (is.numeric(resp.col)){
      # if resp.col includes column numbers and the columns do not have column names, IRTree.data will call the variables as item1, item2, ...
      if (is.null(colnames(resp.data))){
        colnames(IRTree.data)[((node.i-1)*n.var+1):(node.i*n.var)] <- paste(sprintf(paste("item%0", nchar(n.var), "d", sep=''),seq_len(n.var)),
                                                                            sprintf(paste(":node%0", nchar(n.node), "d", sep=''),node.i),sep = "")
      } else {
        colnames(IRTree.data)[((node.i-1)*n.var+1):(node.i*n.var)] <- paste(colnames(resp.data),
                                                                            sprintf(paste(":node%0", nchar(n.node), "d", sep=''),node.i),sep = "")}
    }# if resp.col includes column numbers but these columns have column names in the data, IRTree.data will use these column names
  }
  
  # if covar.col is not null, we will add columns of the covaraites in IRTree.data
  if (!is.null(covar.col)) {
    stopifnot("covar.col must be a vector including the columns of the covariates.
              The columns can be specified by the column names or the column numbers" = (is.vector(covar.col)&(is.character(covar.col)|is.numeric(covar.col))))
    n.covar <- length(covar.col)
    covar.data <- data.frame(data[,covar.col])
    
    # if the covariates have column names, we will keep the names. Otherwise, we will call them covariate1, covariate2, ...
    if (is.character(covar.col)){colnames(covar.data) <- covar.col}
    if (is.numeric(covar.col)){
      if (is.null(colnames(data)[covar.col])){
        colnames(covar.data) <- (sprintf(paste("covariate%0", nchar(n.covar), "d", sep=''),seq_len(n.covar)))
      } else{colnames(covar.data) <- colnames(data)[covar.col]}
    }
    
    IRTree.data <- cbind(IRTree.data,covar.data)
  }
  # add columns of id and time if not null
  if (!is.null(id.col)){
    stopifnot("id.col must be the id column" = ((length(id.col)==1)&(is.character(id.col)|is.numeric(id.col))))
    IRTree.data <- data.frame(id=pull(data,id.col),IRTree.data)
  }
  if (!is.null(time.col)){
    stopifnot("time.col must be the column of time" = ((length(time.col)==1)&(is.character(time.col)|is.numeric(time.col))))
    IRTree.data <- data.frame(time=pull(data,time.col),IRTree.data)
  }
  
  return(IRTree.data)
}

################ items have different tree structures #######################

#' Wide-to-wide, multiple trees
#' 
#' Recode a wide format data frame to a wide format IRTRees data frame
#' in the case when items may have different tree structures
#' 
#' 
#' @param data a wide-format (person-by-item) data set
#' @param cmx_list a list including all tree structures
#' @param id.col the ID column
#' @param resp.col_list a list of vectors, with a length matching the length of \code{cmx_list}; 
#' each element of such a vector points to an item (response variable) using the corresponding
#' mapping matrix
#' @param covar.col columns containing covariates
#' @param time.col the time column when there are repeated (longitudinal) data
#' @details Many examples of mapping matrices are given in the vignette. Columns may be specified 
#' by numeric index or by name. Response categories must be coded with consecutive integers 
#' starting from 1, not 0. Missing data should be properly represented as NA.
WtoW_multi.tree <- function(data, cmx_list, id.col=NULL, resp.col_list, covar.col = NULL, time.col=NULL){
  
  stopifnot("data must be a matrix or data frame" = (is.matrix(data) | is.data.frame(data)))
  stopifnot("cmx_list must be a list and the lth element is a category*node mapping matrix corresponding to items specified by the lth vector in resp.col_list" = is.list(cmx_list))
  stopifnot("resp.col_list must be a list and the lth element is a vector containing columns of items corresponding to the lth mapping matrix in cmx_list" = is.list(resp.col_list))
  stopifnot("vectors in resp.col_list cannot have the same item column(s)" = (anyDuplicated(unlist(resp.col_list))==0))
  stopifnot("cmx_list and resp.col_list should have the same number of elements,
            the lth vector in resp.col_list contains columns of items corresponding to the lth mapping matrix in cmx_list" = (length(cmx_list)==length(resp.col_list)))
   

  # split the dataset into sub-dataset for each tree, and transform each with WtoW_single.tree
  n.tree <- length(cmx_list)
  for (tree.i in 1:n.tree) {
    temp.cmx <- cmx_list[[tree.i]]
    stopifnot("each element in cmx_list must be a matrix or data frame" = (is.matrix(temp.cmx) | is.data.frame(temp.cmx)))
    
    temp.resp.col <- resp.col_list[[tree.i]]
    stopifnot("each element in resp.col_list must be a vector including the columns of the categorical responses to be modeled.
              The columns can be specified by the column names or the column numbers" 
              = (is.vector(temp.resp.col)&(is.character(temp.resp.col)|is.numeric(temp.resp.col))))
    temp.IRTree.data <- WtoW_single.tree(data=data,cmx=temp.cmx,resp.col=temp.resp.col)
    colnames(temp.IRTree.data) <- paste(colnames(temp.IRTree.data),
                                        sprintf(paste(":tree%0", nchar(n.tree), "d", sep=''),tree.i),
                                        sep = "")
    if(tree.i==1){IRTree.data<-temp.IRTree.data} else{IRTree.data <- cbind(IRTree.data,temp.IRTree.data)}
  }
  
  # if covar.col is not null, we will add columns of the covaraites in IRTree.data
  if (!is.null(covar.col)) {
    stopifnot("covar.col must be a vector including the columns of the covariates.
              The columns can be specified by the column names or the column numbers" = (is.vector(covar.col)&(is.character(covar.col)|is.numeric(covar.col))))
    n.covar <- length(covar.col)
    covar.data <- data.frame(data[,covar.col])
    
    # if the covariates have column names, we will keep the names. Otherwise, we will call them covariate1, covariate2, ...
    if (is.character(covar.col)){colnames(covar.data) <- covar.col}
    if (is.numeric(covar.col)){
      if (is.null(colnames(data)[covar.col])){
        colnames(covar.data) <- (sprintf(paste("covariate%0", nchar(n.covar), "d", sep=''),seq_len(n.covar)))
      } else{colnames(covar.data) <- colnames(data)[covar.col]}
    }
    
    IRTree.data <- cbind(IRTree.data,covar.data)
  }
  
  # add columns of id and time if not null
  if (!is.null(id.col)){
    stopifnot("id.col must be the id column" = ((length(id.col)==1)&(is.character(id.col)|is.numeric(id.col))))
    IRTree.data <- data.frame(id=pull(data,id.col),IRTree.data)
  }
  if (!is.null(time.col)){
    stopifnot("time.col must be the column of time" = ((length(time.col)==1)&(is.character(time.col)|is.numeric(time.col))))
    IRTree.data <- data.frame(time=pull(data,time.col),IRTree.data)
  }
  
  return(IRTree.data)
}


########################## recode long-format datasets to long-format IRTree datasets ##############################

################ all items have one common tree structure ###################


#' Long-to-long, single tree
#' 
#' Recode a long format data frame to a long format IRTrees data frame
#' in the case when items have the same tree structure
#' 
#' 
#' @param data a long-format (person-item-response) data set
#' @param cmx a a category-by-node mapping matrix
#' @param id.col the person ID column
#' @param item.col the item ID column
#' @param resp.col the response column
#' @param covar.col columns containing covariates
#' @param time.col the time column. If not NULL, \code{time.col} 
#' should be nested in \code{id.col}, since it indicates repeated measures within persons
#' @details Many examples of mapping matrices are given in the vignette. Columns may be specified 
#' by numeric index or by name. Response categories must be coded with consecutive integers 
#' starting from 1, not 0. Missing data should be properly represented as NA.
LtoL_single.tree <- function(data, cmx, id.col, item.col, resp.col, covar.col = NULL, time.col = NULL){
  
  stopifnot("data must be a matrix or data frame" = (is.matrix(data) | is.data.frame(data)))
  stopifnot("cmx must be a matrix or data frame" = (is.matrix(cmx) | is.data.frame(cmx)))
  stopifnot("id.col must be the id column" = ((length(id.col)==1)&(is.character(id.col)|is.numeric(id.col))))
  stopifnot("item.col must be the item column" = ((length(item.col)==1)&(is.character(item.col)|is.numeric(item.col))))
  stopifnot("resp.col must be the column of the responses to be modeled" = ((length(resp.col)==1)&(is.character(resp.col)|is.numeric(resp.col))))
  stopifnot("item categories should be integers from 1 to nrow(cmx). If the item categories are character or factor, please convert them to numeric."
            = (length(setdiff(unique(na.omit(pull(data,resp.col))),c(1:nrow(cmx))))==0))
  
  n.node <- ncol(cmx)
  n.cate <- nrow(cmx)
  
  if (!is.null(time.col)){
    stopifnot("time.col must be the column of time" = ((length(time.col)==1)&(is.character(time.col)|is.numeric(time.col))))
    IRTree.data <- data.frame(time = rep(pull(data,time.col),each=n.node),
                              id = rep(pull(data,id.col),each=n.node),
                              item = rep(pull(data,item.col),each=n.node),
                              node = rep(sprintf(paste("node%0", nchar(n.node), "d", sep=''),seq_len(n.node)),
                                         times=nrow(data)),
                              sub = paste(rep(pull(data,item.col),each=n.node),":",
                                          rep(sprintf(paste("node%0", nchar(n.node), "d", sep=''),seq_len(n.node)),
                                              times=nrow(data)),
                                          sep = ""),
                              resp = as.vector(t(cmx[pull(data,resp.col),])))
  } else{
    IRTree.data <- data.frame(id = rep(pull(data,id.col),each=n.node),
                              item = rep(pull(data,item.col),each=n.node),
                              node = rep(sprintf(paste("node%0", nchar(n.node), "d", sep=''),seq_len(n.node)),
                                         times=nrow(data)),
                              sub = paste(rep(pull(data,item.col),each=n.node),":",
                                          rep(sprintf(paste("node%0", nchar(n.node), "d", sep=''),seq_len(n.node)),
                                              times=nrow(data)),
                                          sep = ""),
                              resp = as.vector(t(cmx[pull(data,resp.col),])))
  }
  
  # add columns of covariates if not null
  if (!is.null(covar.col)){
    stopifnot("covar.col must be a vector including the columns of the covariates.
              The columns can be specified by the column names or the column numbers" = (is.vector(covar.col)&(is.character(covar.col)|is.numeric(covar.col))))
    n.covar <- length(covar.col)
    covar.data_pre <- data.frame(data[,covar.col])
    covar.data <- data.frame(covar.data_pre[rep(seq_len(nrow(covar.data_pre)),each=n.node), ])
    if (is.character(covar.col)){colnames(covar.data) <- covar.col}
    if (is.numeric(covar.col)){
      if (is.null(colnames(data)[covar.col])){
        colnames(covar.data) <- (sprintf(paste("covariate%0", nchar(n.covar), "d", sep=''),seq_len(n.covar)))
      } else{colnames(covar.data) <- colnames(data)[covar.col]}
    }
    stopifnot("covariates cannot have names as time,id,item,node,sub,or resp since these names are used in the transformed data set" 
              = !(colnames(covar.data) %in% c("time","id","item","node","sub","resp")))
    IRTree.data <- cbind(IRTree.data,covar.data)
  }
  
  # order the dataset by node
  IRTree.data <- IRTree.data[order(IRTree.data$node),]
  return(IRTree.data)
}

################ items may have different tree structures ###################

#' Long-to-long, multiple trees
#' 
#' Recode a long format data frame to a long format IRTRees data frame
#' in the case when items may have different tree structures
#' 
#' 
#' @param data a long-format (person-item-response) data set
#' @param cmx_list a list including all tree structures
#' @param item_list a list of vectors, with a length matching the length of \code{cmx_list}; 
#' each element of such a vector points to an item ID in \code{item.col} using the corresponding
#' mapping matrix
#' @param id.col the person ID column
#' @param item.col the item ID column
#' @param resp.col the response column
#' @param covar.col columns containing covariates
#' @param time.col the time column. If not NULL, \code{time.col} 
#' should be nested in \code{id.col}, since it indicates repeated measures within persons
#' @details Many examples of mapping matrices are given in the vignette. Columns may be specified 
#' by numeric index or by name. Response categories must be coded with consecutive integers 
#' starting from 1, not 0. Missing data should be properly represented as NA.
LtoL_multi.tree <- function(data, cmx_list, item_list, id.col, item.col, resp.col, covar.col = NULL, time.col=NULL){
  
  stopifnot("data must be a matrix or data frame" = (is.matrix(data) | is.data.frame(data)))
  stopifnot("cmx_list must be a list and the lth element is a category*node mapping matrix corresponding to items specified by the lth vector in resp.col_list" = is.list(cmx_list))
  stopifnot("item_list must be a list and the lth element is a vector containing items corresponding to the lth mapping matrix in cmx_list" = is.list(item_list))
  stopifnot("vectors in item_list cannot have the same item(s)" = (anyDuplicated(unlist(item_list))==0))
  stopifnot("cmx_list and item_list should have the same number of elements,
            the lth vector in item_list contains items corresponding to the lth mapping matrix in cmx_list" = (length(cmx_list)==length(item_list)))
  stopifnot("id.col must be the id column" = ((length(id.col)==1)&(is.character(id.col)|is.numeric(id.col))))
  stopifnot("item.col must be the item column" = ((length(item.col)==1)&(is.character(item.col)|is.numeric(item.col))))
  stopifnot("resp.col must be the column of the responses to be modeled" = ((length(resp.col)==1)&(is.character(resp.col)|is.numeric(resp.col))))
   
  # for each tree structure, repeat the data transformation as in LtoL_single.tree
  n.tree <- length(cmx_list)
  for (tree.i in 1:n.tree){ #very long loop over the distinct trees
    temp.cmx <- cmx_list[[tree.i]]
    stopifnot("all elements in cmx_list must be a matrix or data frame" = (is.matrix(temp.cmx) | is.data.frame(temp.cmx)))
    temp.n.node <- ncol(temp.cmx)
    temp.n.cate <- nrow(temp.cmx)
    stopifnot("the items specified in item_list cannot be found"=(length(which(pull(data,item.col) %in% item_list[[tree.i]]))!=0))
    temp.data <- data[which(pull(data,item.col) %in% item_list[[tree.i]]),]
    stopifnot("item categories should be integers from 1 to nrow(cmx). If the item categories are character or factor, please convert them to numeric."
              = (length(setdiff(unique(na.omit(pull(temp.data,resp.col))),c(1:nrow(temp.cmx))))==0))
    
    if (!is.null(time.col)){
      stopifnot("time.col must be the column of time" = ((length(time.col)==1)&(is.character(time.col)|is.numeric(time.col))))
      temp.IRTree.data <- data.frame(time = rep(pull(temp.data,time.col),each=temp.n.node),
                                     id = rep(pull(temp.data,id.col),each=temp.n.node),
                                     item = rep(pull(temp.data,item.col),each=temp.n.node),
                                     node = rep(sprintf(paste("node%0", nchar(temp.n.node), "d", sep=''), 
                                                        seq_len(temp.n.node)),times=nrow(temp.data)),
                                     tree = tree.i,
                                     sub = paste(rep(pull(temp.data,item.col),each=temp.n.node),":",
                                                 rep(sprintf(paste("node%0", nchar(temp.n.node), "d", sep=''),seq_len(temp.n.node)),times=nrow(temp.data)),
                                                 ":tree",as.character(tree.i),
                                                 sep = ""),
                                     resp = as.vector(t(temp.cmx[pull(temp.data,resp.col),])))
    } else{
      temp.IRTree.data <- data.frame(id = rep(pull(temp.data,id.col),each=temp.n.node),
                                     item = rep(pull(temp.data,item.col),each=temp.n.node),
                                     node = rep(sprintf(paste("node%0", nchar(temp.n.node), "d", sep=''), 
                                                        seq_len(temp.n.node)),times=nrow(temp.data)),
                                     tree = tree.i,
                                     sub = paste(rep(pull(temp.data,item.col),each=temp.n.node),":",
                                                 rep(sprintf(paste("node%0", nchar(temp.n.node), "d", sep=''),seq_len(temp.n.node)),times=nrow(temp.data)),
                                                 ":tree",as.character(tree.i),
                                                 sep = ""),
                                     resp = as.vector(t(temp.cmx[pull(temp.data,resp.col),])))
    }
    
    # add columns of covariates in not null
    if (!is.null(covar.col)){
      stopifnot("covar.col must be a vector including the columns of the covariates.
                The columns can be specified by the column names or the column numbers" = (is.vector(covar.col)&(is.character(covar.col)|is.numeric(covar.col))))
      n.covar <- length(covar.col)
      temp.covar.data_pre <- data.frame(temp.data[,covar.col])
      temp.covar.data <- data.frame(temp.covar.data_pre[rep(seq_len(nrow(temp.covar.data_pre)),each=temp.n.node), ])
      if (is.character(covar.col)){colnames(temp.covar.data) <- covar.col}
      if (is.numeric(covar.col)){
        if (is.null(colnames(temp.data)[covar.col])){
          colnames(temp.covar.data) <- (sprintf(paste("covariate%0", nchar(n.covar), "d", sep=''),seq_len(n.covar)))
        } else{colnames(temp.covar.data) <- colnames(temp.data)[covar.col]}
      }
      stopifnot("covariates cannot have names as time,id,item,node,sub,or resp since these names are used in the transformed data set" 
                = !(colnames(temp.covar.data) %in% c("time","id","item","node","sub","resp")))
      temp.IRTree.data <- cbind(temp.IRTree.data,temp.covar.data)
      temp.IRTree.data <- temp.IRTree.data[order(temp.IRTree.data$node),]
    }
    
    if (tree.i==1){
      IRTree.data <- temp.IRTree.data
    } else {IRTree.data <- rbind(IRTree.data,temp.IRTree.data)} 
  } #end of for loop
  
  return(IRTree.data)
  
}


########################## recode long-format datasets to wide-format IRTree datasets ##############################

################ all items have one common tree structure ###################


#' Long-to-wide, single tree
#' 
#' Recode a long format data frame to a wide format IRTrees data frame
#' in the case when items have the same tree structure
#' 
#' 
#' @param data a long-format (person-item-response) data set
#' @param cmx a a category-by-node mapping matrix
#' @param id.col the person ID column
#' @param item.col the item ID column
#' @param resp.col the response column
#' @param covar.col columns containing covariates
#' @param time.col the time column. If not NULL, \code{time.col} 
#' should be nested in \code{id.col}, since it indicates repeated measures within persons
#' @details Many examples of mapping matrices are given in the vignette. Columns may be specified 
#' by numeric index or by name. Response categories must be coded with consecutive integers 
#' starting from 1, not 0. Missing data should be properly represented as NA.
LtoW_single.tree <- function(data, cmx, id.col, item.col, resp.col, covar.col = NULL, time.col=NULL){

  # the idea is to first generate the long-format IRTree dataset and then use tidyr::pivot_wider to transform it to wide-format
  long_IRTree.data <- LtoL_single.tree(data,cmx,id.col,item.col,resp.col,covar.col,time.col)
  
  # if (!is.null(time.col)){lhs = 'time + id'} else {lhs = 'id'}
  if (!is.null(time.col)){ids = c('time', 'id')} else {ids = c('id')}
  
  # get covariate columns if existing

  if (!is.null(covar.col)){
   
    if (is.character(covar.col)){covar.names <- covar.col}
    if (is.numeric(covar.col)){covar.names <- colnames(data)[covar.col]}
    
    # won't work with item covariates, so remove if any
    old_cov_names = covar.names
    good = sapply(old_cov_names, function(x){
      var(tapply(as.numeric(pull(data,x)),list(pull(data,id.col)),mean))>0
    })
    covar.names = old_cov_names[good]
    if (length(covar.names) != length(old_cov_names)) warning('Removing some covariates')
    
    if(sum(good>0)) {
#      lhs = paste (lhs, '+', paste(covar.names, collapse='+'))  
      ids = c(ids, covar.names)  
    }  
  }
#    fla = as.formula(paste(lhs, '~', 'sub'))
#    reshape2::dcast(long_IRTree.data, fla, value.var = "resp")
  pivot_wider(data=long_IRTree.data, id_cols=ids, names_from='sub', values_from='resp')
}

################ items may have different tree structures ###################

#' Long-to-wide, multiple trees
#' 
#' Recode a long format data frame to a wide format IRTRees data frame
#' in the case when items may have different tree structures
#' 
#' 
#' @param data a long-format (person-item-response) data set
#' @param cmx_list a list including all tree structures
#' @param item_list a list of vectors, with a length matching the length of \code{cmx_list}; 
#' each element of such a vector points to an item ID in \code{item.col} using the corresponding
#' mapping matrix
#' @param id.col the person ID column
#' @param item.col the item ID column
#' @param resp.col the response column
#' @param covar.col columns containing covariates
#' @param time.col the time column. If not NULL, \code{time.col} 
#' should be nested in \code{id.col}, since it indicates repeated measures within persons
#' @details Many examples of mapping matrices are given in the vignette. Columns may be specified 
#' by numeric index or by name. Response categories must be coded with consecutive integers 
#' starting from 1, not 0. Missing data should be properly represented as NA.
LtoW_multi.tree <- function(data, cmx_list, item_list, id.col, item.col, resp.col, covar.col = NULL, time.col=NULL){

  # first generate the long-format IRTree dataset and transform it to wide-format
  long_IRTree.data <- LtoL_multi.tree(data,cmx_list,item_list,id.col,item.col,resp.col,covar.col,time.col)
  
  # if (!is.null(time.col)){lhs = 'time + id'} else {lhs = 'id'}
  if (!is.null(time.col)){ids = c('time', 'id')} else {ids = c('id')}
  
  # get covariate columns if existing
  
  if (!is.null(covar.col)){
    
    if (is.character(covar.col)){covar.names <- covar.col}
    if (is.numeric(covar.col)){covar.names <- colnames(data)[covar.col]}
    
    # won't work with item covariates, so remove if any
    old_cov_names = covar.names
    good = sapply(old_cov_names, function(x){
      var(tapply(as.numeric(pull(data,x)),list(pull(data,id.col)),mean))>0
    })
    covar.names = old_cov_names[good]
    if (length(covar.names) != length(old_cov_names)) warning('Removing some covariates')
    
    if(sum(good>0)) {
      #      lhs = paste (lhs, '+', paste(covar.names, collapse='+'))  
      ids = c(ids, covar.names)  
    }  
  }
  #    fla = as.formula(paste(lhs, '~', 'sub'))
  #    reshape2::dcast(long_IRTree.data, fla, value.var = "resp")
  pivot_wider(data=long_IRTree.data, id_cols=ids, names_from='sub', values_from='resp')
}


########################## recode wide-format datasets to long-format IRTree datasets ##############################

################ all items have one common tree structure ###################

#' Wide-to-long, one tree
#' 
#' Recode a wide format data frame to a long format IRTrees data frame
#' in the case when all items have the same tree structure
#' 
#' 
#' @param data a wide-format (person-by-item) data set
#' @param cmx a a category-by-node mapping matrix
#' @param id.col the ID column
#' @param resp.col the columns containing the item responses
#' @param covar.col columns containing covariates
#' @param time.col the time column when there are repeated (longitudinal) data
#' @details Many examples of mapping matrices are given in the vignette. Columns may be specified 
#' by numeric index or by name. Response categories must be coded with consecutive integers 
#' starting from 1, not 0. Missing data should be properly represented as NA.
WtoL_single.tree <- function(data, cmx, id.col, resp.col, covar.col = NULL, time.col=NULL){
  
  # first transform the wide-format data set to long-format data set, then use LtoL_single.tree
  
  stopifnot("data must be a matrix or data frame" = (is.matrix(data) | is.data.frame(data)))
  stopifnot("cmx must be a matrix or data frame" = (is.matrix(cmx) | is.data.frame(cmx)))
  n.node <- ncol(cmx) # number of nodes
  n.cate <- nrow(cmx) # number of categories
  
  # if all columns are not specified, then treat all columns as resp.col
  # otherwise, resp.col must be specified
  if (is.null(id.col) & is.null(resp.col) & is.null(covar.col) & is.null(time.col)){
    resp.data <- as.matrix(data)
    resp.col <- c(1:ncol(data))
  } else {
    stopifnot("resp.col must be a vector including the columns of the categorical responses to be modeled.
              The columns can be specified by the column names or the column numbers" = (is.vector(resp.col)&(is.character(resp.col)|is.numeric(resp.col))))
    if (length(resp.col)==1){resp.data <- matrix(pull(data,resp.col),ncol = 1)} else {
      resp.data <- as.matrix(data[,resp.col])
    }
  }
  
  stopifnot("item categories should be integer from 1 to nrow(cmx). If the item categories are character or factor, please convert them to numeric." 
            = (length(setdiff(unique(na.omit(as.vector(resp.data))),c(1:nrow(cmx))))==0))
  n.var <- ncol(resp.data)
  if (is.null(colnames(resp.data))){
    colnames(resp.data) <- (sprintf(paste("item%0", nchar(n.var), "d", sep=''),seq_len(n.var)))
  }
  
  stopifnot("it's not allowed to have only time.col but no id.col, because time.col should be nested in id.col"
            =!(is.null(id.col) & !is.null(time.col)))
  
  if (!is.null(id.col)){
    if (!is.null(time.col)){
      stopifnot("id.col must be the id column" = ((length(id.col)==1)&(is.character(id.col)|is.numeric(id.col))))
      stopifnot("time.col must be the column of time" = ((length(time.col)==1)&(is.character(time.col)|is.numeric(time.col))))
      long_data <- data.frame(time = rep(pull(data,time.col),each = n.var),
                              id = rep(pull(data,id.col),each = n.var),
                              item = rep(colnames(resp.data),times = nrow(data)),
                              resp = as.vector(t(resp.data)))
    } else {
      stopifnot("id.col must be the id column" = ((length(id.col)==1)&(is.character(id.col)|is.numeric(id.col))))
      long_data <- data.frame(id = rep(pull(data,id.col),each = n.var),
                              item = rep(colnames(resp.data),times = nrow(data)),
                              resp = as.vector(t(resp.data)))
    }
    
  } else {
    long_data <- data.frame(id = rep(c(1:nrow(data)),each = n.var),
                            item = rep(colnames(resp.data),times = nrow(data)),
                            resp = as.vector(t(resp.data)))
  }
  
  
  # add possible covariates columns to the long-format data set
  if(!is.null(covar.col)){
    stopifnot("covar.col must be a vector including the columns of the covariates.
              The columns can be specified by the column names or the column numbers" = (is.vector(covar.col)&(is.character(covar.col)|is.numeric(covar.col))))
    n.covar <- length(covar.col)
    covar.data_pre <- data.frame(data[,covar.col])
    covar.data <- data.frame(covar.data_pre[rep(seq_len(nrow(covar.data_pre)),each=n.var), ])
    if (is.character(covar.col)){colnames(covar.data) <- covar.col}
    if (is.numeric(covar.col)){
      if (is.null(colnames(data)[covar.col])){
        colnames(covar.data) <- (sprintf(paste("covariate%0", nchar(n.covar), "d", sep=''),seq_len(n.covar)))
      } else{colnames(covar.data) <- colnames(data)[covar.col]}
    } 
    long_data <- cbind(long_data,covar.data)
    
    if (!is.null(time.col)){
      long_IRTree.data <- LtoL_single.tree(long_data,cmx,id.col="id",
                                                    item.col="item",resp.col="resp",
                                                    covar.col=colnames(covar.data),
                                                    time.col = "time")
    } else {
      long_IRTree.data <- LtoL_single.tree(long_data,cmx,id.col="id",
                                                    item.col="item",resp.col="resp",
                                                    covar.col=colnames(covar.data))
    }
    
  } else{
    if (!is.null(time.col)){
      long_IRTree.data <- LtoL_single.tree(long_data,cmx,id.col="id",
                                                    item.col="item",resp.col="resp",
                                                    time.col = "time")
      } else {long_IRTree.data <- LtoL_single.tree(long_data,cmx,id.col="id",item.col="item",resp.col="resp")}
  }
  return(long_IRTree.data)
}

################ items may have different tree structures ###################

#' Wide-to-long, multiple trees
#' 
#' Recode a wide format data frame to a long format IRTRees data frame
#' in the case when items may have different tree structures
#' 
#' 
#' @param data a wide-format (person-by-item) data set
#' @param cmx_list a list including all tree structures
#' @param id.col the ID column
#' @param resp.col_list a list of vectors, with a length matching the length of \code{cmx_list}; 
#' each element of such a vector points to an item (response variable) using the corresponding
#' mapping matrix
#' @param covar.col columns containing covariates
#' @param time.col the time column when there are repeated (longitudinal) data
#' @details Many examples of mapping matrices are given in the vignette. Columns may be specified 
#' by numeric index or by name. Response categories must be coded with consecutive integers 
#' starting from 1, not 0. Missing data should be properly represented as NA.
WtoL_multi.tree <- function(data, cmx_list, id.col, resp.col_list, covar.col = NULL, time.col=NULL){
  
  stopifnot("data must be a matrix or data frame" = (is.matrix(data) | is.data.frame(data)))
  stopifnot("cmx_list must be a list and the lth element is a category*node mapping matrix corresponding to items specified by the lth vector in resp.col_list" = is.list(cmx_list))
  stopifnot("resp.col_list must be a list and the lth element is a vector containing columns of items corresponding to the lth mapping matrix in cmx_list." = is.list(resp.col_list))
  stopifnot("The vectors in resp.col_list should be either vectors of character(s) or vectors of number(s)"=(is.character(unlist(resp.col_list))|is.numeric(unlist(resp.col_list))))
  stopifnot("vectors in resp.col_list cannot have the same item column(s)" = (anyDuplicated(unlist(resp.col_list))==0))
  stopifnot("cmx_list and resp.col_list should have the same number of elements,
            the lth vector in resp.col_list contains columns of items corresponding to the lth mapping matrix in cmx_list" = (length(cmx_list)==length(resp.col_list)))
  
  # first transform the data set to long-format, then use LtoL_multi.tree
  
  resp.col <- unlist(resp.col_list)
  n.var <- length(resp.col)
  resp.data <- as.matrix(data[,resp.col])
  
  stopifnot("colnames(data) cannot be null when transforming wide-format data to long-format data"
            = !is.null(colnames(data)))
  stopifnot("it's not allowed to have only time.col but no id.col, because time.col should be nested in id.col"
            =!(is.null(id.col) & !is.null(time.col)))
  
  if (!is.null(id.col)){
    if (!is.null(time.col)){
      stopifnot("id.col must be the id column" = ((length(id.col)==1)&(is.character(id.col)|is.numeric(id.col))))
      stopifnot("time.col must be the column of time" = ((length(time.col)==1)&(is.character(time.col)|is.numeric(time.col))))
      long_data <- data.frame(time = rep(pull(data,time.col),each = n.var),
                              id = rep(pull(data,id.col),each = n.var),
                              item = rep(colnames(resp.data),times = nrow(data)),
                              resp = as.vector(t(resp.data)))
    } else {
      stopifnot("id.col must be the id column" = ((length(id.col)==1)&(is.character(id.col)|is.numeric(id.col))))
      long_data <- data.frame(id = rep(pull(data,id.col),each = n.var),
                              item = rep(colnames(resp.data),times = nrow(data)),
                              resp = as.vector(t(resp.data)))
    }
    
  } else {
    long_data <- data.frame(id = rep(c(1:nrow(data)),each = n.var),
                            item = rep(colnames(resp.data),times = nrow(data)),
                            resp = as.vector(t(resp.data)))
  }
  
  
  if(!is.null(covar.col)){
    n.covar <- length(covar.col)
    covar.data_pre <- data.frame(data[,covar.col])
    covar.data <- data.frame(covar.data_pre[rep(seq_len(nrow(covar.data_pre)),each=n.var), ])
    if (is.character(covar.col)){colnames(covar.data) <- covar.col}
    if (is.numeric(covar.col)){
      if (is.null(colnames(data)[covar.col])){
        colnames(covar.data) <- (sprintf(paste("covariate%0", nchar(n.covar), "d", sep=''),seq_len(n.covar)))
      } else{colnames(covar.data) <- colnames(data)[covar.col]}
    } 
    long_data <- cbind(long_data,covar.data)
  }
  
  
  if (is.character(unlist(resp.col_list))){item_list <- resp.col_list} 
  if (is.numeric(unlist(resp.col_list))){
    item_list <- lapply(resp.col_list,function(resp.col,data){(colnames(data)[resp.col])},data=data)}
  
  
  ## after obtaining the long-format dataset, transform it to long-format IRTree dataset
  if (!is.null(covar.col) & !is.null(time.col)){
    long_IRTree.data <- LtoL_multi.tree(data=long_data,cmx_list=cmx_list,item_list=item_list,
                                                 id.col="id",item.col="item",resp.col="resp",
                                                 covar.col=colnames(covar.data),
                                                 time.col="time")
  } else if (!is.null(covar.col) & is.null(time.col)){
    long_IRTree.data <- LtoL_multi.tree(data=long_data,cmx_list=cmx_list,item_list=item_list,
                                                 id.col="id",item.col="item",resp.col="resp",
                                                 covar.col=colnames(covar.data))
  } else if (is.null(covar.col) & !is.null(time.col)){
    long_IRTree.data <- LtoL_multi.tree(data=long_data,cmx_list=cmx_list,item_list=item_list,
                                                 id.col="id",item.col="item",resp.col="resp",
                                                 time.col = "time")
  } else if (is.null(covar.col) & is.null(time.col)){
    long_IRTree.data <- LtoL_multi.tree(data=long_data,cmx_list=cmx_list,item_list=item_list,
                                                 id.col="id",item.col="item",resp.col="resp")
  }
  
  return(long_IRTree.data)
}


