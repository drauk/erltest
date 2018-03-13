# src/erlang/makefile.pub   2018-3-13   Alan U. Kennington.
# Some Erlang/OTP testing.

ERL_FILES = test1.erl proc1.erl wx1.erl \
	mobsim1.erl mobsim2.erl mobsim3.erl \
	gs1a.erl gs1b.erl gs1c.erl \
	gsup1a.erl gsup1b.erl gsup1c.erl \
	mobsim4a.erl mobsim4b.erl mobsim4c.erl mobsim4d.erl

BEAM_FILES = $(patsubst %.erl,%.beam,$(ERL_FILES))

# Targets for things other than files.
.PHONY: all clean erlfiles wc

all: erlfiles
clean:
	rm -f $(BEAM_FILES) .erlfiles

# Compile all of the erlang files.
erlfiles: .erlfiles
.erlfiles: $(ERL_FILES)
	erlc $?
	@touch $@

# Show how many lines of code are in the erlang source files.
wc:
	@echo "-- All lines/words/chars of Erlang code."
	@wc $(ERL_FILES)
	@echo "-- Comment lines in Erlang code."
	@egrep '^ *$$|^ *%' $(ERL_FILES) | wc -l
	@echo "-- Non-comment lines in Erlang code."
	@egrep -v '^ *$$|^ *%' $(ERL_FILES) | wc -l
