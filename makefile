# src/erlang/makefile.pub   2018-3-5   Alan U. Kennington.
# Some Erlang/OTP testing.

ERL_FILES = test1.erl proc1.erl wx1.erl \
	mobsim1.erl mobsim2.erl mobsim3.erl \
	gs1a.erl gs1b.erl gs1c.erl
BEAM_FILES = $(patsubst %.erl,%.beam,$(ERL_FILES))

# Targets for things other than files.
.PHONY: all clean erlfiles

all: erlfiles
clean:
	rm -f $(BEAM_FILES) .erlfiles

erlfiles: .erlfiles
.erlfiles: $(ERL_FILES)
	erlc $?
	@touch $@
