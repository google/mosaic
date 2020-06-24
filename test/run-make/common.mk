RUSTC   ?= rustc
CC      ?= cc
AR      ?= ar
TMPDIR  ?= /youneedtosupply/tmpdir
BINDGEN ?= cargo run --bin=peasy --
EXTRACXXRSFLAGS := -lstdc++

BARE_RUSTC := $(RUSTC)
RUSTC      := $(BARE_RUSTC) --out-dir $(TMPDIR) -L $(TMPDIR)

BARE_BINDGEN := $(BINDGEN)
BINDGEN      := $(BARE_BINDGEN) --out-dir $(TMPDIR)

$(TMPDIR)/lib%.o: %.cc
	$(CC) -c -o $@ -I. $<
$(TMPDIR)/lib%.o: $(TMPDIR)/%.cc
	$(CC) -c -o $@ -I. $<
%.a: %.o
	$(AR) crs $@ $<

$(TMPDIR)/%_bind.rs $(TMPDIR)/%_bind.cc: %.h
	$(BINDGEN) $<
%_bind.rlib: %_bind.rs lib%_bind.a
	$(RUSTC) --crate-type=lib -lstatic=$(notdir $*)_bind $(EXTRACXXRSFLAGS) $<
