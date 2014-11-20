#ifndef __ATTRIB_H__
#define __ATTRIB_H__

typedef struct Attrib Attrib;
typedef struct Expressions Expressions;

Expressions *exps_new();
int exps_epush(Expressions *exps, const char *formula, char **err);
void exps_delete(Expressions * exps);

Attrib *attrib_new(Expressions * exps);
int attrib_set(Attrib *attrib, const char *name, float value, char **err);
int attrib_get(Attrib *attrib, const char *name, float *value, char **err);
void attrib_delete(Attrib *attrib);

#endif // _ATTRIB_H__
