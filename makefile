# makefile

test: build
	apricot_test=all ./apricot.native

build:
	rebuild apricot.native -pkg str
