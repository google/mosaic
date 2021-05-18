# Copyright (c) 2021 Google LLC
#
# Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
# https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
# <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
# option. This file may not be copied, modified, or distributed
# except according to those terms.

-include ../common.mk

all: $(TMPDIR)/cc_use_bind.rlib
	PKG_NAME=cc_use $(RUSTC) cc_use.rs
	$(TMPDIR)/cc_use > $(TMPDIR)/actual.stdout
	diff -u cc_use.stdout $(TMPDIR)/actual.stdout || (echo "ERROR: The actual and expected output differ"; exit 1)
