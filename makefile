# makefile

test: build
	pear_test=all ./pear.native

build:
	rebuild pear.native -pkg str
