# makefile

test: build
	pear_test=all ./pear.native

run: build
	./pear.native

build:
	rebuild pear.native -pkg str
