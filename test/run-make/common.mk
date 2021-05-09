# Copyright (c) 2021 Google LLC
#
# Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
# https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
# <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
# option. This file may not be copied, modified, or distributed
# except according to those terms.

RUSTC   ?= rustc
TMPDIR  ?= $(shell mktemp -d)
BINDGEN ?= cargo run --bin=mosaic --
PROC_MACRO_DIR ?= foo

LD_LIB_PATH_ENVVAR ?= LD_LIBRARY_PATH
include ../../../third_party/rust/run-make-fulldeps/tools.mk

ifeq ($(UNAME),Darwin)
DYLIB_FILE = lib$(1).dylib
else
ifdef IS_WINDOWS
DYLIB_FILE = $(1).dll
else
DYLIB_FILE = lib$(1).so
endif
endif

CXX := $(CXX) -I.

BARE_BINDGEN := $(BINDGEN)
BINDGEN      := $(BARE_BINDGEN) --out-dir $(TMPDIR)

RUSTC := $(RUSTC) --edition 2018 -D warnings --extern cc_use=$(PROC_MACRO_DIR)/$(call DYLIB_FILE,cc_use)

$(TMPDIR)/lib%.o: %.cc
	$(call COMPILE_OBJ_CXX,$@,$<)
$(TMPDIR)/lib%.o: $(TMPDIR)/%.cc
	$(call COMPILE_OBJ_CXX,$@,$<)

$(TMPDIR)/%_bind.rs $(TMPDIR)/%_bind.cc: %.mod.h
	$(BINDGEN) --crate-name=$*_bind $<
$(TMPDIR)/%_bind.rs $(TMPDIR)/%_bind.cc: %.rs
	$(BINDGEN) --crate-name=$*_bind $<
$(TMPDIR)/%_bind.rlib: $(TMPDIR)/%_bind.rs $(call STATICLIB,%_bind)
	$(RUSTC) --crate-type=lib -lstatic=$(notdir $*)_bind $(EXTRARSCXXFLAGS) $<
