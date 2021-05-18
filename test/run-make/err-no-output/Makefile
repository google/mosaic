# Copyright (c) 2021 Google LLC
#
# Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
# https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
# <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
# option. This file may not be copied, modified, or distributed
# except according to those terms.

-include ../common.mk

all:
	$(BINDGEN) input.h && (echo "Error: Expected to fail"; exit 1) || true
	[ -z "$$(ls -A '$(TMPDIR)')" ] || (echo "Error: Expected no output files but found:"; ls -A $(TMPDIR); exit 1)
