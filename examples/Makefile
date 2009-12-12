#
# Makefile
#

EBIN=../ebin/
#ERLC=erlc
BEAM=erlc -b beam -pa $(EBIN) -I ../include \
	-I /usr/local/lib/erlang/lib/esdl-0.96.0626/include/
YRL=erlc -W
FIND=find

.SUFFIXES: .erl .beam

OBJS=\
all_balls \
balls \
bounce \
eurobot \
obstacle_avoid \
mill \
test

ERL_OBJS=$(foreach file, $(OBJS), $(file).beam)

OTHER_OBJS=
FIND=find

all: $(ERL_OBJS) $(OTHER_OBJS)

%.beam: %.erl
	$(BEAM) $<

clean:
	@$(RM) -f *.beam

clearbak:
	@$(FIND) . -type f -name \*~ -exec rm {} \;

