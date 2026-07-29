BUILD_DIR = ./build
SINGLE_DIR = /home/mikura/la32r/la32r_soc_student333/la32r_soc_student3/rtl/mycpu
PRJ = playground
SOLO_DIR = /home/mikura/nscscc-solo-la-soc/rtl

test:
	mill -i $(PRJ).test

verilog:
	mkdir -p $(BUILD_DIR)
	mill -i $(PRJ).runMain Elaborate --target-dir $(BUILD_DIR)

chiplab:
	rm -rf $(CHIPLAB_HOME)/IP/myCPU
	rm -rf $(BUILD_DIR)
	mkdir -p $(CHIPLAB_HOME)/IP/myCPU
	mkdir -p $(BUILD_DIR)
	mill -i $(PRJ).runMain Elaborate --target-dir $(BUILD_DIR) --use-diff=true --use-simu=true --use-count=false
	find $(CHIPLAB_HOME)/IP/myCPU/ -type f ! -name 'core_top.v' -delete
	cp ./playground/src/utils/DiffTest/DiffBridge.v $(CHIPLAB_HOME)/IP/myCPU/
	cp ./playground/src/core/backend/issue_stage/FileLoggerBlackBox.v $(CHIPLAB_HOME)/IP/myCPU/
	cp ./playground/src/core/frontend/temp_if/core_top.sv $(CHIPLAB_HOME)/IP/myCPU/
# 	find $(BUILD_DIR) -name "*.sv" -exec sh -c 'cp "$$1" "$(CHIPLAB_HOME)/IP/myCPU/"' sh {} \;
	find $(BUILD_DIR) -name "*.sv" -exec cp {} $(CHIPLAB_HOME)/IP/myCPU/ \;

vivado:
	rm -rf $(CHIPLAB_HOME)/IP/myCPU
	rm -rf $(BUILD_DIR)
	mkdir -p $(CHIPLAB_HOME)/IP/myCPU
	mkdir -p $(BUILD_DIR)
	mill -i $(PRJ).runMain Elaborate --target-dir $(BUILD_DIR) --use-diff=false --use-simu=false --use-count=false
	find $(CHIPLAB_HOME)/IP/myCPU/ -type f ! -name 'core_top.v' -delete
# 	find $(BUILD_DIR) -name "*.sv" -exec cp {} $(VIVADO_FUNC_SRC)/ \;
# 	find $(BUILD_DIR) -name "*.sv" -exec cp {} $(VIVADO_PERF_SRC)/ \;
	find $(BUILD_DIR) -name "*.sv" -exec cp {} $(CHIPLAB_HOME)/IP/myCPU/ \;
	cp ./playground/src/core/frontend/temp_if/core_top.sv $(CHIPLAB_HOME)/IP/myCPU/

solo:
	rm -rf $(SOLO_DIR)/myCPU
	rm -rf $(BUILD_DIR)
	mkdir -p $(SOLO_DIR)/myCPU
	mkdir -p $(BUILD_DIR)
	mill -i $(PRJ).runMain Elaborate --target-dir $(BUILD_DIR) --use-diff=false --use-simu=true --use-count=false
	find $(SOLO_DIR)/myCPU/ -type f ! -name 'core_top.sv' -delete
	find $(BUILD_DIR) -name "*.sv" -exec cp {} $(SOLO_DIR)/myCPU \;
	cp $(SOLO_DIR)/../core_top.sv $(SOLO_DIR)/myCPU

single:
	rm -rf $(SOLO_DIR)/myCPU
	rm -rf $(BUILD_DIR)
	mkdir -p $(SOLO_DIR)/myCPU
	mkdir -p $(BUILD_DIR)
	mill -i $(PRJ).runMain Elaborate --target-dir $(BUILD_DIR) --use-diff=false --use-simu=false --use-count=false
	find $(SOLO_DIR)/myCPU/ -type f ! -name 'core_top.sv' -delete
	find $(BUILD_DIR) -name "*.sv" -exec cp {} $(SOLO_DIR)/myCPU \;
	cp $(SOLO_DIR)/../core_top.sv $(SOLO_DIR)/myCPU

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

.PHONY: test verilog chiplab vivado help reformat checkformat clean

sim:
	$(call git_commit, "sim RTL") # DO NOT REMOVE THIS LINE!!!
	@echo "Write this Makefile by yourself."

-include ../Makefile
