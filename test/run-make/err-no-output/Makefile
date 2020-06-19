-include ../common.mk

all:
	$(BINDGEN) input.h && (echo "Error: Expected to fail"; exit 1) || true
	[ -z "$$(ls -A '$(TMPDIR)')" ] || (echo "Error: Expected no output files but found:"; ls -A $(TMPDIR); exit 1)
