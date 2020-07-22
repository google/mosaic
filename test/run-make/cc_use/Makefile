-include ../common.mk

all: $(TMPDIR)/pod_bind.rlib
	$(RUSTC) pod.rs
	$(TMPDIR)/pod > $(TMPDIR)/actual.stdout
	diff -u pod.stdout $(TMPDIR)/actual.stdout || (echo "ERROR: The actual and expected output differ"; exit 1)
	rm -rf $(TMPDIR)
