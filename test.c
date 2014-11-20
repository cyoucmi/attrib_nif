#include <stdio.h>
#include <stdlib.h>
#include "attrib.h"


static void *alloc(void *ud, void *ptr, size_t size){
    if(!ptr){
        return malloc(size);
    }
    free(ptr);
    return 0;
    
}

int main(){
    Expressions *exps = exps_new(alloc, 0);
    char *err;
    exps_epush(exps, "a = b+c", &err);
    if(!exps_epush(exps, "fight_power = a*a + attack*attack + defence*3 + hp/5 + (hit + dodge + critvalue + tough)/4", &err)){
        printf("%s:%d", err, __LINE__);
        exit(0);
    };

    Attrib *attrib = attrib_new(exps);
    if(!attrib_set(attrib, "attack", 4, &err)){
        printf("%s:%d", err, __LINE__);
        exit(0);
    }; 

    attrib_set(attrib, "hp", 4, &err);

    attrib_set(attrib, "b", 4, &err);

    attrib_set(attrib, "c", 1, &err);

    float fight_power;

    if(!attrib_get(attrib, "fight_power", &fight_power, &err)){
        printf("%s:%d", err, __LINE__);
        exit(0);
    }else{
        printf("fight_power = %f\n", fight_power);

    };

    attrib_delete(attrib);

    exps_delete(exps);
    return 1;
}
