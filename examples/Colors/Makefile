##
## Copyright (c) 2018-2024 Stéphane Micheloud
##
## Licensed under the MIT License.
##
##############################################################################

include ../Makefile.inc

APP_NAME   ?= $(shell basename $(CURDIR))

SOURCE_DIR  = app
TARGET_DIR  = target
TARGET_GEN_DIR = $(TARGET_DIR)/gen

##############################################################################
## main rules

SOURCES   = $(shell $(FIND) "$(SOURCE_DIR)" -type f -name "*.hs")
TARGET    = $(TARGET_DIR)/$(APP_NAME)$(TARGET_EXT)

GHC_OPTS  = -Wall -Wmissing-import-lists -Wincomplete-uni-patterns\
            -hidir "$(TARGET_GEN_DIR)" -odir "$(TARGET_GEN_DIR)" -o "$(TARGET)"

all: build

build: $(TARGET)

$(TARGET): $(SOURCES)
	[ -d "$(TARGET_DIR)" ] || "$(MKDIR)" -p "$(TARGET_DIR)"
	"$(GHC)" $(GHC_OPTS) $(SOURCES)

clean:
	"$(RM)" -rf "$(TARGET_DIR)"

run: build
	$(TARGET)

help:
	@$(ECHO) "Usage: make all|build|clean|doc|help|lint|run"
	@$(ECHO) ""
	@$(ECHO) "  Subcommands:"
	@$(ECHO) "    all    alias for build"
	@$(ECHO) "    build  compile Haskell source files"
	@$(ECHO) "    clean  delete generated files"
	@$(ECHO) "    doc    generate HTML documentation"
	@$(ECHO) "    help   print this help message"
	@$(ECHO) "    lint   analyze Haskell source files with HLint"
	@$(ECHO) "    run    execute main program \"$(APP_NAME)\""

##############################################################################
## lint rules

HLINT_OPTS = --color=auto --report="$(TARGET_DIR)/report.html"

lint:
	"$(HLINT)" $(HLINT_OPTS) "$(SOURCE_DIR)"

##############################################################################
## doc rules

TARGET_DOCS_DIR = $(TARGET_DIR)/docs

PACKAGE_SYNOPSIS = Haskell Example
PACKAGE_NAME = $(APP_NAME)
PACKAGE_VERSION = 1.0.0

LIB_DIR = $(shell dirname $(shell $(FIND) "$(GHC_HTML_DIR)" -type f -name "base.haddock"))

HADDOCK_OPTS = --html --odir="$(TARGET_DOCS_DIR)" --title="$(PACKAGE_SYNOPSIS)" --package-name="$(PACKAGE_NAME)" --package-version="$(PACKAGE_VERSION)" --read-interface="$(LIB_DIR),$(LIB_DIR)/base.haddock"
##

doc: $(SOURCES)
	[ -d "$(TARGET_DOCS_DIR)" ] || "$(MKDIR)" -p "$(TARGET_DOCS_DIR)"
	"$(HADDOCK)" $(HADDOCK_OPTS) $(SOURCES)

##############################################################################
## phony

.PHONY: all build clean doc help lint run

.SUFFIXES:
.SUFFIXES: .exe .hs .html
