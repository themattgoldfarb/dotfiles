#
# Global Settings
#

INSTALL = install
DESTDIR ?= /
PREFIX  ?= $(DESTDIR)/usr

PATH_XMONAD_XSESSION = $(PREFIX)/share/xsessions/xmonad.desktop
PATH_XMONAD_APPLICATION = $(PREFIX)/share/applications/xmonad.desktop

#
# Targets
# 

all:
	@echo "Nothing to do"

install:
	$(INSTALL) -m0644 -D xmonad.xsession.desktop $(PATH_XMONAD_XSESSION)
	$(INSTALL) -m0644 -D xmonad.application.desktop $(PATH_XMONAD_APPLICATION)

uninstall:
	rm -f $(PATH_XMONAD_XSESSION)
	rm -f $(PATH_XMONAD_APPLICATION)

.PHONY: all install uninstall
