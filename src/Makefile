#
# Makefile
#
# $Id: Makefile,v 1.16 2008/02/27 15:29:13 corrado_santoro Exp $
#

include ./esdl.incl

EBIN=../ebin/
#ERLC=erlc
BEAM=erlc -b beam -pa $(EBIN) -I ../include \
	-I $(EDSLDIR)/include/ -o $(EBIN)
YRL=erlc -W
FIND=find
DOCDIR=../doc

.SUFFIXES: .erl .beam

OBJS=\
gen_activity \
box \
cone \
cylinder \
diff_drive \
geometry \
sphere \
compound \
cube \
line \
robot_path \
rosen \
gp2d12 \
kinematics \
object3d \
pipe \
simple_path \
surface \
trajectory_control \
robot \
world \
contact_point \
two_wheels_driving \
s_curve_profile \
z_system

ERL_OBJS=$(foreach file, $(OBJS), $(EBIN)/$(file).beam)
DOC_OBJS=$(foreach file, $(OBJS), $(DOCDIR)/$(file).html)

OTHER_OBJS=
FIND=find

all: $(ERL_OBJS) $(OTHER_OBJS)

# docs: #$(DOC_OBJS)
# 	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
#                '".."' '[{def,{vsn,"$(VSN)"}}]'

$(EBIN)/%.beam: %.erl
	$(BEAM) $<

# $(DOCDIR)/%.html:%.erl
# 	erl -noshell -run edoc_run file '"$<"' '[{dir,"$(DOCDIR)"}]' \
# 	-s init stop



clean:
	@(cd $(EBIN); $(RM) -f *.beam)

clearbak:
	@$(FIND) . -type f -name \*~ -exec rm {} \;

