#ifndef __ATTRIB_H__
#define __ATTRIB_H__
#include <stdlib.h>

typedef struct Attrib Attrib;
typedef struct Expressions Expressions;
typedef void* (*AttribAlloc)(void *ud, void *ptr, size_t size);

Expressions *exps_new(AttribAlloc, void *ud);
int exps_epush(Expressions *exps, const char *formula, char **err);
void exps_delete(Expressions * exps);

Attrib *attrib_new(Expressions * exps);
int attrib_set(Attrib *attrib, const char *name, float value, char **err);
int attrib_get(Attrib *attrib, const char *name, float *value, char **err);
void attrib_delete(Attrib *attrib);


// only for debug
void attrib_dump(Attrib*);

#endif // _ATTRIB_H__
