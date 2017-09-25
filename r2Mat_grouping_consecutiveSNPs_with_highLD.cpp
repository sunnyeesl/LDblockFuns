#include <Rcpp.h>
using namespace Rcpp;
#include <algorithm> 
// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericMatrix MergeR2mat(NumericMatrix Or2Mat, IntegerMatrix mergeList) {
  int mergeNum;
  mergeNum = mergeList.nrow();
  NumericMatrix Merger2Mat(mergeNum, mergeNum);
  
  for (int i=0;i<(mergeNum-1);i++){
    int nowst1 = mergeList(i,0)-1;
    int nowed1 = mergeList(i,1)-1;
    for(int j=(i+1);j<mergeNum;j++){
      int nowst2 = mergeList(j,0)-1;
      int nowed2 = mergeList(j,1)-1;
      double sum=0;
      double num = 0;
      // printf("%f\n", Or2Mat(i,j));
      // printf("%d %d \n", nowst1, nowed1);
      // printf("%d %d \n", nowst2, nowed2);
      for(int r=nowst1;r<=nowed1;r++){
        for(int c=nowst2;c<=nowed2;c++){
          // printf("%d %d \n", nowst1, nowed1);
          // printf("%d %d \n", nowst2, nowed2);
          // printf("%d %d\n", r, c);
          sum += Or2Mat(r,c);
          num += 1;
        }
      }
      // printf("%f %f %d\n", sum, sum/num, num);
      Merger2Mat(i,j) = sum/num;
      Merger2Mat(j,i) = sum/num;
      // printf("%f\n", sum/num);
    }
    // if(i>0){
    //   break;
    // } 
  }
  return Merger2Mat;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
# timesTwo(42)
*/
