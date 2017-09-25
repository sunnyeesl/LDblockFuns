#
sourceCpp("D:/Dropbox/0share/result/LDblock/R-code/test/newCLQD.cpp")

mergehighLD = function(nowgeno, highLDval){
  i = 2
  TotalmergeList = NULL   #start index, end index
  while(i <= dim(nowgeno)[2]){
    StrongLD = TRUE
    subregion = c(i-1)
    is = 0
    while(StrongLD ==TRUE){
      nowr2 = (cor(nowgeno[,(i-1):(i-1+is)], nowgeno[,(i+is)], use = "pairwise.complete.obs"))^2
      if(all(nowr2>=highLDval)){
        subregion = c(subregion, i+is)
        is = is+1
      }else{
        TotalmergeList = rbind(TotalmergeList, c(min(subregion), max(subregion)))
        StrongLD = FALSE
        i = i+is+1
      }
    }
  }
  return(TotalmergeList)
  mergeR2Mat = matrix(0,dim(TotalmergeList)[1], dim(TotalmergeList)[1])
  OR2Mat = cor(nowgeno, use = "pairwise.complete.obs")^2
  system.time({mergeR2Mat = MergeR2mat(OR2Mat, TotalmergeList)}) 
  return(list(mergeR2Mat, TotalmergeList))
}

# mergeCorMat = function(nowgeno, highLDval)