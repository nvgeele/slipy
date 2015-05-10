#/usr/bin/make -f

#
# Path of pypy checkout
PYPYPATH ?= /Users/nvgeele/Dev/pypy

# Invocation of pytest, defaults to pypy's stuff
# but may also be `py.test`
PYTEST ?= $(PYPYPATH)/pytest.py
RPYTHON ?= $(PYPYPATH)/rpython/bin/rpython --batch


TRANSLATE_TARGETS := translate-jit

PYFILES := $(shell find . -name '*.py' -type f)

translate-jit-all: $(TRANSLATE_TARGETS)
all: translate-jit-all translate-no-jit


translate-jit: slipy-c
translate-no-jit: slipy-c-nojit

slipy-c: $(PYFILES)
	$(RPYTHON) -Ojit targetslipy.py

slipy-c-nojit: $(PYFILES)
	$(RPYTHON) targetslipy.py