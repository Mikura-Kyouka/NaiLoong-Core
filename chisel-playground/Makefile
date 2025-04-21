BUILD_DIR = ./build

PRJ = playground

test:
	mill -i $(PRJ).test

verilog:
	mkdir -p $(BUILD_DIR)
	mill -i $(PRJ).runMain Elaborate --target-dir $(BUILD_DIR)
	find $(CHIPLAB_HOME)/IP/myCPU/ -type f ! -name 'core_top.v' -delete
	cp ./playground/src/utils/DiffTest/DiffBridge.v $(CHIPLAB_HOME)/IP/myCPU/
	find $(BUILD_DIR) -name "*.sv" -exec sh -c 'cp "$$1" "$(CHIPLAB_HOME)/IP/myCPU/$$(basename $$1 .sv).v"' sh {} \;

help:
	mill -i $(PRJ).runMain Elaborate --help

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.idea.GenIdea/idea

clean:
	-rm -rf $(BUILD_DIR)

.PHONY: test verilog help reformat checkformat clean

sim:
	$(call git_commit, "sim RTL") # DO NOT REMOVE THIS LINE!!!
	@echo "Write this Makefile by yourself."

-include ../Makefile
