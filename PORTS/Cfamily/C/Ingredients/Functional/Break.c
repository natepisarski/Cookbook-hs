#include <stdlib.h>
#include <stdbool.h>
#include "../../Common.c"

typedef  bool (*pred)(int*);
typedef  int* (*genf)(int*);
/*Functions helping determine array lengths for filters, maps, and breaks.*/
int truths(int * arr, int ll, pred x){
  int y = 0;
  for(int i = 0; i < ll; i++) if(x(&arr[i])) y++;
  return y;
}

//Length to FirstNotTrue.
int toFNT(int *arr, int ll, pred x){
  int ctr = 0;
  for(int i = 0; i < ll; i ++){
    if(!x(&arr[i])) return ctr; else ctr++;
  }
}

/*Common functional interfaces*/
int* map(int *lst, int ll, genf x){
  int * ref = (int*) malloc(sizeof(int)*ll);
  for(int i = 0; i < ll; i++) ref[i] = *x(&lst[i]);
  return ref;
}

int *filter(int *lst, int ll, pred x){
  int * retl = (int*) malloc(sizeof(int) * truths(lst,ll,x));
  int ctr = 0;
  for(int i = 0; i < ll; i++){
    if(x(&lst[i])){ retl[ctr] = lst[i]; ctr ++;}
  }
  return retl;
}


/*Breaks*/
int *filterBreak(int *lst, int ll, pred x){
  int * rL = (int*) malloc(sizeof(int) * toFNT(lst,ll,x));
  for(int i = 0; i < ll; i++){
    if(!x(&lst[i])) return rL; else rL[i] = lst[i];
  }
  return rL;
}

int *removeBreak(int *lst, int ll, pred x){
  for(int i = 0; i < ll; i++)
    if(!x(&lst[i])) return sub(lst,ll,i);
  return lst;
}
