RUSTC   ?= rustc
CC      ?= cc
AR      ?= ar
TMPDIR  := $(shell mktemp -d)
BINDGEN ?= cargo run --
LDFLAGS := -lstdc++

all:
	$(BINDGEN) --out-dir $(TMPDIR) pod.h
	$(RUSTC) --out-dir $(TMPDIR) --crate-type=lib $(TMPDIR)/pod_bind.rs
	$(CC) -c -o $(TMPDIR)/pod_bind.o -I. $(TMPDIR)/pod_bind.cc
	$(AR) crs $(TMPDIR)/libpod_bind.a $(TMPDIR)/pod_bind.o
	$(RUSTC) --out-dir $(TMPDIR) -L$(TMPDIR) -lstatic=pod_bind $(LDFLAGS) pod_use.rs
	$(TMPDIR)/pod_use > $(TMPDIR)/actual.stdout
	diff -u pod_use.stdout $(TMPDIR)/actual.stdout || (echo "ERROR: The actual and expected output differ"; exit 1)
	rm -rf $(TMPDIR)
