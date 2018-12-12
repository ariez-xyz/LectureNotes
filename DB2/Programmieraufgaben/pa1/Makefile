JAVAC=javac -Xlint:unchecked
SHELL:=/bin/bash
SOURCES=$(shell find ./src -type f -name '*.java')
CLASSES=$(SOURCES:.java=.class)

APILINK=https://docs.oracle.com/javase/8/docs/api/
DOCDIR=./doc

default: $(SOURCES)
	$(JAVAC) $(SOURCES)

all: default doc

doc:
	javadoc -private -noqualifier all -link $(APILINK) -d $(DOCDIR) $(SOURCES)

test: default
	@printf "Script for running all available test inputs.\\n"
	@printf "Run in pa1/ folder.\\n"
	@printf "Empty lines between \"Testing\" lines mean success.\\n"
	@printf "Running tests...\\n"
	@for testinstance in 0 1 2 3 4 5 6 7 8 9; do \
		testinstancedir=$$(printf "test/%04d" $$testinstance); \
		printf "testing testinstance: test/%04d " $$testinstance; \
		testinstancefilein=$$(printf "%s/input_%04d" $$testinstancedir $$testinstance); \
		testinstancefileout=$$(printf "%s/output_%04d" $$testinstancedir $$testinstance); \
		output=$$(java -cp src/ Main "$$testinstancefilein"); \
		diffoutput=$$(diff "$$testinstancefileout" <(echo "$$output")); \
		diffexitcode=$$?; \
		if [ "$$diffexitcode" -ne "0" ]; then \
			printf "==> FAIL (on \'java -cp src/ Main %s')\\n" $$testinstancedir; \
			printf "%s\\n" $$diffoutput; \
			exit 1; \
		fi; \
		printf "==> OK\\n"; \
	done

clean:
	rm -f $(CLASSES)
	rm -rf $(DOCDIR)
