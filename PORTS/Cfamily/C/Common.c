/*Common.cpp*/
/*Root-level library, undependant on the rest of cookbook.*/

//#include "Ingredients/Functional/Break.c"
#include <stdio.h>
#include <stdlib.h>

int* sub(int *arrBnds, int arlen, int position){
  int * diff = (int*) malloc(sizeof(int) * (arlen - position));
  for(int i = position-1; i < arlen; i ++) diff[i] = arrBnds [i];
  return diff;
}

int* take(int *arrBnds, int arln, int takes){
  int *diff = (int*) (malloc (sizeof(int)*(takes)));
  for(int i = 0; i < takes && i < arln; i++) diff[i] = arrBnds[i];
  return diff;
}

int count(int* arr, int arln, int* x){
  int y = 0;
  for(int i = 0; i < arln; i ++)
    if(*x == arr[i]) y++;
  return y;
}

int* positions(int* arr, int arln, int* x){
  int *rArr = (int*) malloc(sizeof(int)*count(arr,arln,x));
  int cHold = 0;
  for(int i=0; i<arln; i++){
    if(*x == arr[i]){
      rArr[cHold] = i;
      cHold++;
    }
  }
  return rArr;
}

int pos(int * arr, int arln, int * x){
  return positions(arr,arln,x)[0];
}

void main(void){;}
