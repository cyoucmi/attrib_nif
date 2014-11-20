-module(attrib).
-export([exps_create/0,exps_epush/2,attrib_create/1,attrib_set/3,attrib_get/2,attrib_dump/1]).

exps_create()->
    attrib_nif:exps_create().

exps_epush(Exps,Formula) when is_list(Formula)->
    attrib_nif:exps_epush(Exps,Formula).

attrib_create(Exps)->
    attrib_nif:attrib_create(Exps).

attrib_set(Attrib,Key,Value)when is_atom(Key), is_number(Value)->
    attrib_nif:attrib_set(Attrib, Key, float(Value)).

attrib_get(Attrib,Key)when is_atom(Key)->
    attrib_nif:attrib_get(Attrib, Key).

attrib_dump(Attrib)->
	attrib_nif:attrib_dump(Attrib).





