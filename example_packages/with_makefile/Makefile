INCLUDE_FLAGS = $(addprefix -I,$(INCLUDE_DIRS))

$(BUILD_DIR)/libwith_makefile.a: $(BUILD_DIR)/hello_makefile.o
	ar rs $(@) $(^)

$(BUILD_DIR)/hello_makefile.mod: src/hello_makefile.f90

$(BUILD_DIR)/hello_makefile.o: src/hello_makefile.f90
	$(FC) -c -J$(BUILD_DIR) $(INCLUDE_FLAGS) $(FFLAGS) -o $(@) $(<)
