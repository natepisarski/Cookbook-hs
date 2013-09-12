#include <stdlib.h>
#include <stdbool.h>
#include "../../Common.c"

//Count defined in Common.c

bool contains(int* arr, int arln, int *inArr, int inArrLn){
  for(int i = 0; i < arln; i++){
    if(take(sub(arr,arln,i),(arln - i),inArrLn) == inArr) return true;
  }
  return false;
}

int* mqsort (int* lst,int ll){
  int *rL = (int *) malloc(sizeof(int) * ll);

  for(int i = 0; i < ll; i++)
    rL[i] = lst[i];
  

  for(int i = 0; i < ll-1; i++){
    while(rL[i]>rL[i+i]){
      int temp = rL[i];
      rL[i] = rL[i + 1];
      rL[i + 1] = temp;
    }
  }
}
