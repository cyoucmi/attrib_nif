FLAGS = -g -std=c99

## TODO:
ERL_ROOT = /usr/local/lib/erlang/erts-5.10.4

ECC = erlc

OUTDIR = ./
RCS = $(wildcard *.erl)
OBJS = $(patsubst %.erl,$(OUTDIR)/%.beam,$(RCS))

all:attrib_nif.so test $(OBJS)

attrib_nif.so:attrib.c attrib.h attrib_nif.c
	gcc -o $@ $^ --shared -fpic -std=c99  $(FLAGS) -Wall -I $(ERL_ROOT)/emulator/beam -I $(ERL_ROOT)/include
	
test:attrib.c attrib.h test.c
	gcc -std=c99 -Wall -g -lm -o $@ $^  
	
%.beam:%.erl
	$(ECC) $^

clean: 
	rm  attrib_nif.so test *.beam
