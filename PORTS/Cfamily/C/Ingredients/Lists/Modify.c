#include "../../Common.c"

int *after(int *arr, int arln, int * c){
  return sub(arr,arln,pos(arr,arln,c));
}

int *before(int *arr, int arln, int *c){
  return take(arr,arln,pos(arr,arln,c));
}

int *rev(int *arr, int arln){
  int *rL = (int*) malloc(sizeof(int)*arln);

  for(int i=0; i < arln; i++)
    rL[i] = arr[i];
  
  int temp = 0;

  for(int i = 0; i < arln; i++){
    temp = rL[i];
    rL[i] = rL[arln-1];
    rL[arln-1] = temp;
  }
  return rL;
}

int *rm(int * arr, int arln, int* x){
  int *rL = (int*) malloc(sizeof(int)*count(arr,arln,x));
  int ctr = 0;

  for(int i=0; i < arln; i++)
    rL[i] = arr[i];
  
  for(int i = 0; i < arln; i++){
    if(!arr[i]==*x){ rL[ctr] = arr[i]; ctr++;}
  }
}

//splitOn, snipe, insert omitted. Will be implemented in a later version.
//Not implemented because of the limitations of C arrays.
