# makefile

test: build
	silver_test=all ./silver.native

run: build
	./silver.native

build:
	rebuild silver.native -pkg str
