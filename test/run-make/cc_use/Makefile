-include ../common.mk

all: $(TMPDIR)/cc_use_bind.rlib
	PKG_NAME=cc_use $(RUSTC) cc_use.rs
	$(TMPDIR)/cc_use > $(TMPDIR)/actual.stdout
	diff -u cc_use.stdout $(TMPDIR)/actual.stdout || (echo "ERROR: The actual and expected output differ"; exit 1)
