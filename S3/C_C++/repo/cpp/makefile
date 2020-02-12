CXXFLAGS= -std=c++2a -Wall
CC=$(CXX)

PROGRAMS=programm
OBJECTS=programm.o person.o familytree.o distances.o

ALLOBJECTS=$(OBJECTS) $(patsubst %,%.o,$(PROGRAMS))

default: $(PROGRAMS)

%.d: %.cc
			$(SHELL) -ec '$(CXX) -MM $(CXXFLAGS) $< | \
			perl -pe '"'"'s|($*\.o)[ :]*|\1 $@: |g'"'"' > $@'

programm: $(OBJECTS)

clean:
		-rm -f $(ALLOBJECTS) $(ALLOBJECTS:%.o=%.d) $(PROGRAMS)

include $(ALLOBJECTS:%.o=%.d) # Warum war das auskommentiert?  Das ist notwendig.  Rade
