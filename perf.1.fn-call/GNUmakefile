dirs=$(wildcard src/*)

build:

	printf "%s\n" $(dirs) | xargs -i% $(MAKE) -C % $@

run:

	printf "%s\n" $(dirs) | xargs -i@ sh -c 'printf "@: %s\n" "$$(@/main)"'
