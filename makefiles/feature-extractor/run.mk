TARGET = feature-extractor
BUILDDIR = build/debug

run: $(BUILDDIR)/Makefile
	@cd $(BUILDDIR) && ./$(TARGET)
