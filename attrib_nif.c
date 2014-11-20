#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "attrib.h"
#include "erl_nif.h"

typedef struct ExpressionsNif{
    struct Expressions *exps;
} ExpressionsNif;

typedef struct AttribNif{
    struct Attrib *attrib;
}AttribNif;


static ErlNifResourceType *exps_resource_type; 
static ErlNifResourceType *attrib_resource_type;

static void
exps_resource_destory(ErlNifEnv *env, void *obj){
   struct ExpressionsNif *exps_nif = obj;
   exps_delete(exps_nif->exps);
}

static void
attrib_resource_destory(ErlNifEnv *env, void *obj){
    struct AttribNif *attrib_nif = obj;
    attrib_delete(attrib_nif->attrib);
}

static void *
attrib_alloc(void *ud, void *ptr, size_t sz){
	if (ptr == NULL) {
		void *p = enif_alloc(sz);
		return p;
	}
	enif_free(ptr);
	return NULL;
}

static int
attrib_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info){
    ErlNifResourceFlags tried;
    exps_resource_type = enif_open_resource_type(env, NULL, "exps_resource_type", exps_resource_destory, ERL_NIF_RT_CREATE,&tried);
    attrib_resource_type = enif_open_resource_type(env, NULL, "attrib_resource_type", attrib_resource_destory, ERL_NIF_RT_CREATE,&tried);
	return 0;
};


static void 
attrib_unload(ErlNifEnv *env, void *priv_data){
}

static ERL_NIF_TERM 
exps_create_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    ExpressionsNif *exps_nif = enif_alloc_resource(exps_resource_type,sizeof(ExpressionsNif));
    exps_nif->exps = exps_new(attrib_alloc, NULL);

    ERL_NIF_TERM result = enif_make_resource(env, exps_nif);
    enif_release_resource(exps_nif);
    return result;
}

static ERL_NIF_TERM 
exps_epush_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    ExpressionsNif *exps_nif;
    if(!enif_get_resource(env, argv[0], exps_resource_type, (void**)&exps_nif)){
         return enif_make_atom(env, "in exps_epush_nif, arg[0] is not a handle to a resource object");
    }

    size_t formula_len = 2048;
    char formula[formula_len];
    if(enif_get_string(env, argv[1], formula, formula_len, ERL_NIF_LATIN1) <= 0){
        return enif_make_atom(env, "in attrib_update_nif, arg[1] something wrong");
    }

    char *err;
    if(exps_epush(exps_nif->exps, formula, &err)){
        return enif_make_atom(env, "ok");
    }else{
        return enif_make_atom(env, err);
    }
}


static ERL_NIF_TERM 
attrib_create_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    ExpressionsNif *exps_nif;
    if(!enif_get_resource(env, argv[0], exps_resource_type, (void**)&exps_nif)){
         return enif_make_atom(env, "in exps_epush_nif, arg[0] is not a handle to a resource object");
    }

    AttribNif *attrib_nif = enif_alloc_resource(attrib_resource_type,sizeof(AttribNif));
    attrib_nif->attrib = attrib_new(exps_nif->exps);

    ERL_NIF_TERM result = enif_make_resource(env, attrib_nif);
    enif_release_resource(attrib_nif);

    return result;
}


static ERL_NIF_TERM 
attrib_set_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    AttribNif *attrib_nif;
    if(!enif_get_resource(env, argv[0], attrib_resource_type, (void**)&attrib_nif)){
         return enif_make_atom(env, "in attrib_set_nif, arg[0] is not a handle to a resource object");
    }

    size_t token_len = 32+1;
    char token[token_len];
    if(enif_get_atom(env, argv[1], token, token_len, ERL_NIF_LATIN1) <= 0){
        return enif_make_atom(env, "in attrib_set_nif, arg[1] something wrong");
    }
    
    double value;
    enif_get_double(env, argv[2], &value);

    char *err;
    if(attrib_set(attrib_nif->attrib, token, (float)value, &err)){
        return enif_make_atom(env, "ok");
    }else{
        return enif_make_atom(env, err);
    }
}

static ERL_NIF_TERM 
attrib_get_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    AttribNif *attrib_nif;
    if(!enif_get_resource(env, argv[0], attrib_resource_type, (void**)&attrib_nif)){
         return enif_make_atom(env, "in attrib_get_nif, arg[0] is not a handle to a resource object");
    }

    size_t token_len = 32+1;
    char token[token_len];
    if(enif_get_atom(env, argv[1], token, token_len, ERL_NIF_LATIN1) <= 0){
        return enif_make_atom(env, "in attrib_get_nif, arg[1] something wrong");
    }
    
    float value;
    char *err;
    if(attrib_get(attrib_nif->attrib, token, &value, &err)){
        return enif_make_double(env, value);
    }else{
        return enif_make_atom(env, err);
    }
}

static ERL_NIF_TERM 
attrib_dump_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    AttribNif *attrib_nif;
    if(!enif_get_resource(env, argv[0], attrib_resource_type, (void**)&attrib_nif)){
         return enif_make_atom(env, "in attrib_dump_nif, arg[0] is not a handle to a resource object");
    }
    attrib_dump(attrib_nif->attrib);
    return enif_make_atom(env, "ok");
}



static ErlNifFunc nif_funcs[] = {
	{"exps_create", 0, exps_create_nif},
	{"exps_epush", 2, exps_epush_nif},
    {"attrib_create", 1, attrib_create_nif},
    {"attrib_set", 3, attrib_set_nif},
    {"attrib_get", 2, attrib_get_nif},
    {"attrib_dump", 1, attrib_dump_nif},
};

ERL_NIF_INIT(attrib_nif, nif_funcs, attrib_load, NULL, NULL, attrib_unload);

