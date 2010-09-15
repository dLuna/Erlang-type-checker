#
# The _gen_files file has the list of generated files, it is created by
# extra commands to AC_OUTPUT in configure.in
#
CONFIG_GEN_FILES = $(shell cat _gen_files 2>/dev/null)
CONFIG_IN_FILES  = $(CONFIG_GEN_FILES:%=%.in)

.PHONY: conf
conf: $(CONFIG_GEN_FILES) config.status _tstamp_h

$(CONFIG_GEN_FILES): _tstamp_h

_tstamp_h: $(CONFIG_IN_FILES) _gen_files
	./config.status

_gen_files: configure.in

config.status: configure
	./configure

configure: configure.in
	autoconf

.PHONY: conf_clean
conf_clean: clean
	rm -f config.cache config.log config.status configure config.h.in

.PHONY: clean
clean:
	rm -f $(CONFIG_GEN_FILES) ../include/config.h _tstamp_h _gen_files
