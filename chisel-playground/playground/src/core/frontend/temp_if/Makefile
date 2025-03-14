chiplab:
	rm -r $(CHIPLAB_HOME)/IP/myCPU
	mkdir -p $(CHIPLAB_HOME)/IP/myCPU
	cp ./core_top.sv $(CHIPLAB_HOME)/IP/myCPU/
	cp /home/cao/JLU-NSCSCC-25/chisel-playground/TempIf.sv $(CHIPLAB_HOME)/IP/myCPU/
	mv $(CHIPLAB_HOME)/IP/myCPU/core_top.sv $(CHIPLAB_HOME)/IP/myCPU/core_top.v
	mv $(CHIPLAB_HOME)/IP/myCPU/TempIf.sv $(CHIPLAB_HOME)/IP/myCPU/TempIf.v