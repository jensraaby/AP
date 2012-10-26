# leave these lines alone
.SUFFIXES: .erl .beam .yrl
.erl.beam:
	erlc -W $<
.yrl.erl:
	erlc -W $<
ERL = erl -boot start_clean

# Here's a list of the erlang modules you want compiling
# If the modules don't fit onto one line add a \ character
# to the end of the line and continue on the next line
# Edit the lines below
#MODS = mr test_mr read_mxm
MODS = mr test_mr read_mxm music_analysis
# The first target in any makefile is the default target.
# If you just type "make" then "make all" is assumed (because
#   "all" is the first target in this makefile)
all: compile
compile: ${MODS:%=%.beam} #subdirs

## special compilation requirements are added here
#special1.beam: special1.erl
#        ${ERL} -Dflag1 -W0 special1.erl

## run an application from the makefile
test_mr: compile
	${ERL} -pa Dir1  -s test_mr test_sum

read_mxm: compile
	${ERL} -pa Dir1  -s read_mxm from_file mxm_dataset_test.txt
	
	
# the subdirs target compiles any code in
# sub-directories
# subdirs:
#         cd dir1; make
#         cd dir2; make
#         ...

# remove all the code
clean:
	rm -rf *.beam erl_crash.dump
        # cd dir1; make clean
        # cd dir2; make clean
