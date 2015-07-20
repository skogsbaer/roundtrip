.PHONY: all clean

HTML_FILES = Spec.html Impl.html

all: $(HTML_FILES)

clean:
	rm -f $(HTML_FILES)

%.html: %.lhs
	HsColour -html $< > $@
