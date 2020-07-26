-include ../common.mk

all: $(TMPDIR)/pod_bind.rlib
	$(RUSTC) pod_use.rs
	$(TMPDIR)/pod_use > $(TMPDIR)/actual.stdout
	diff -u pod_use.stdout $(TMPDIR)/actual.stdout || (echo "ERROR: The actual and expected output differ"; exit 1)
