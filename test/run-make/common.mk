RUSTC   ?= rustc
TMPDIR  ?= $(shell mktemp -d)
BINDGEN ?= cargo run --bin=peasy --

LD_LIB_PATH_ENVVAR ?= LD_LIBRARY_PATH
include ../../../third_party/rust/run-make-fulldeps/tools.mk

CXX := $(CXX) -I.

BARE_BINDGEN := $(BINDGEN)
BINDGEN      := $(BARE_BINDGEN) --out-dir $(TMPDIR)

$(TMPDIR)/lib%.o: %.cc
	$(call COMPILE_OBJ_CXX,$@,$<)
$(TMPDIR)/lib%.o: $(TMPDIR)/%.cc
	$(call COMPILE_OBJ_CXX,$@,$<)

$(TMPDIR)/%_bind.rs $(TMPDIR)/%_bind.cc: %.h
	$(BINDGEN) $<
$(TMPDIR)/%_bind.rlib: $(TMPDIR)/%_bind.rs $(call STATICLIB,%_bind)
	$(RUSTC) --crate-type=lib -lstatic=$(notdir $*)_bind $(EXTRARSCXXFLAGS) $<
