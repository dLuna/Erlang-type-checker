OUR_APPS = contracts

IMPORTED_APPS =

APPS = $(IMPORTED_APPS) $(OUR_APPS)

.PHONY: all
all:    conf libs

.PHONY: conf
conf:
	cd config && $(MAKE)

.PHONY: libs
libs:	$(APPS)

.PHONY: $(APPS)
$(APPS):
	cd lib/$@ && $(MAKE) # && $(MAKE) dialyzer

.PHONY: clean
clean: $(APPS:%=clean-%)

.PHONY: real_clean
real_clean: clean
	cd config && $(MAKE) conf_clean

.PHONY: conf_clean
conf_clean:
	cd config && $(MAKE) conf_clean

.PHONY: $(APPS:%=clean-%)
$(APPS:%=clean-%): clean-%:
	cd lib/$(patsubst clean-%,%,$@) && $(MAKE) clean
