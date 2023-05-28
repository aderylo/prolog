PL = sicstus
SPLD = spld
SPLDFLAGS = --static --exechome=/opt/sicstus/bin/

ALL = wyprawy

all: $(ALL)

%: %.sav
	$(SPLD) $(SPLDFLAGS) $< -o $@

%.sav: %.pl
	echo "compile('$<'). save_program('$@')." | $(PL)

clean:
	rm -f $(ALL) *.sav
