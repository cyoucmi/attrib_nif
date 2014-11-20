-module(test).
-compile([export_all]).

test()->
    Exps = attrib:exps_create(),
    ok = attrib:exps_epush(Exps, "fight_power = attack*attack + defence*3 + hp/5 + (hit + dodge + critvalue + tough)/4"),
    Attrib = attrib:attrib_create(Exps),
    ok = attrib:attrib_set(Attrib, attack, 4),
    attrib:attrib_dump(Attrib),
    io:format("fight_power=~w~n",[attrib:attrib_get(Attrib, fight_power)]).
