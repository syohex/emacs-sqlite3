UNAME_S=$(shell uname -s)
EMACS_ROOT ?= ../emacs/trunk
ifeq ($(UNAME_S),Darwin)
	EMACS ?= /Applications/Emacs.app/Contents/MacOS/Emacs
else
	EMACS ?= emacs
endif

CC      = gcc
LD      = gcc
CPPFLAGS = -I$(EMACS_ROOT)/src
CFLAGS = -std=gnu99 -ggdb3 -Wall -fPIC $(CPPFLAGS)

.PHONY : test

all: sqlite3-core.so

sqlite3-core.so: sqlite3-core.o
	$(LD) -shared $(LDFLAGS) -o $@ $^ -lsqlite3

sqlite3-core.o: sqlite3-core.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	-rm -f sqlite3-core.so sqlite3-core.o

test:
	$(EMACS) -Q -batch -L . $(LOADPATH) \
		-l test/test.el \
		-f ert-run-tests-batch-and-exit
