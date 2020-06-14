INCLUDE_FLAGS = $(addprefix -I,$(INCLUDE_DIRS))

$(BUILD_DIR)/libmakefile_complex.a: $(BUILD_DIR)/wrapper_mod.o
	ar rs $(@) $(^)

$(BUILD_DIR)/wrapper_mod.mod: src/wrapper_mod.f90

$(BUILD_DIR)/wrapper_mod.o: src/wrapper_mod.f90
	$(FC) -c -J$(BUILD_DIR) $(INCLUDE_FLAGS) $(FFLAGS) -o $(@) $(<)
