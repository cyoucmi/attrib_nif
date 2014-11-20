-module(attrib_nif).
-export([exps_create/0,exps_epush/2,attrib_create/1,attrib_set/3,attrib_get/2,attrib_dump/1]).

-on_load(load/0).

load()->
	erlang:load_nif("./attrib_nif", 0).

exps_create()->
	"attrib_nif library not loaded".

exps_epush(_,_)->
	"attrib_nif library not loaded".

attrib_create(_)->
	"attrib_nif library not loaded".

attrib_set(_,_,_)->
	"attrib_nif library not loaded".

attrib_get(_,_)->
	"attrib_nif library not loaded".

attrib_dump(_)->
	"attrib_nif library not loaded".





