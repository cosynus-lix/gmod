OPLATE = ../../src/oplate

all: test

preprocess:
	mkdir -p pp
	for i in *.c; do cpp -E $$i > pp/$$i; done

test: preprocess
	$(OPLATE) -d trace pp/philo3.c
