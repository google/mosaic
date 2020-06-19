RUSTC   ?= rustc
CC      ?= cc
AR      ?= ar
TMPDIR  := $(shell mktemp -d)
BINDGEN ?= cargo run --bin=peasy --
EXTRACXXRSFLAGS := -lstdc++

BARE_RUSTC := $(RUSTC)
RUSTC      := $(BARE_RUSTC) --out-dir $(TMPDIR) -L $(TMPDIR)

$(TMPDIR)/lib%.o: %.cc
	$(CC) -c -o $@ -I. $<
$(TMPDIR)/lib%.o: $(TMPDIR)/%.cc
	$(CC) -c -o $@ -I. $<
%.a: %.o
	$(AR) crs $@ $<

$(TMPDIR)/%_bind.rs $(TMPDIR)/%_bind.cc: %.h
	$(BINDGEN) --out-dir $(TMPDIR) $<
%_bind.rlib: %_bind.rs lib%_bind.a
	$(RUSTC) --crate-type=lib -lstatic=$(notdir $*)_bind $(EXTRACXXRSFLAGS) $<

all: $(TMPDIR)/pod_bind.rlib
	$(RUSTC) pod_use.rs
	$(TMPDIR)/pod_use > $(TMPDIR)/actual.stdout
	diff -u pod_use.stdout $(TMPDIR)/actual.stdout || (echo "ERROR: The actual and expected output differ"; exit 1)
	rm -rf $(TMPDIR)
