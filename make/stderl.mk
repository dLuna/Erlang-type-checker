include ../../../make/include.mk

all: $(ERL_OBJECTS) $(ERL_DOCUMENTS) $(APP_OBJECTS)

debug:
	$(MAKE) TYPE=debug

$(ERL_OBJECTS): $(ERL_HEADERS)

clean: $(EXTRA_CLEAN)
	rm -f ../ebin/*.beam $(ERL_DOCUMENTS)
