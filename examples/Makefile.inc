ifndef GIT_HOME
$(error Variable GIT_HOME is not set)
endif
ifndef CABAL_DIR
$(error Variable CABAL_DIR is not set)
endif
ifndef GHC_HOME
$(error Variable GHC_HOME is not set)
endif
ifndef MAVEN_HOME
$(error Variable MAVEN_HOME is not set)
endif

ifeq ($(OS),Windows_NT)
GHC        = $(subst \,/,$(GHC_HOME))/bin/ghc.exe
HADDOCK    = $(subst \,/,$(GHC_HOME))/bin/haddock.exe
HLINT      = $(subst \,/,$(CABAL_DIR))/bin/hlint.exe
MVN        = $(subst \,/,$(MAVEN_HOME))/bin/mvn.cmd
TARGET_EXT = .exe
PSEP       = ;
## $(MSYS_HOME)\usr\bin\{cp.exe, make.exe, wget.exe}
CP         = /usr/bin/cp.exe
ECHO       = /usr/bin/echo.exe
FIND       = /usr/bin/find.exe
MKDIR      = /usr/bin/mkdir.exe
RM         = /usr/bin/rm.exe
TOUCH      = /usr/bin/touch.exe
WGET       = /usr/bin/wget.exe
else
GHC        = $(GHC_HOME)/bin/ghc
HADDOCK    = $(GHC_HOME)/bin/haddock
HLINT      = $(CABAL_DIR))/bin/hlint
MVN        = $(MAVEN_HOME)/bin/mvn
TARGET_EXT =
PSEP       = :
CP         = cp
ECHO       = echo
FIND       = find
MKDIR      = mkdir
RM         = rm
TOUCH      = touch
WGET       = wget
endif

GHC_HTML_DIR = $(subst \,/,$(GHC_HOME))/doc/html/libraries

ifeq (,$(wildcard $(GHC_HTML_DIR)))
$(error Variable GHC_HTML_DIR is not set)
endif