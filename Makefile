install:
	@echo "Installing pulcli in $(HOME)/.local/bin/"
	@cp src/pulcli.py $(HOME)/.local/bin/pulcli
	
	@echo "Installing pulse in $(HOME)/.local/bin/"
	@cp src/pulse.py $(HOME)/.local/bin/pulse

unsintall:
	@echo "Uninstalling pulse (and pulcli) from $(HOME)/.local/bin/"
	$(RM) -f $(HOME)/.local/bin/pulcli
	$(RM) -f $(HOME)/.local/bin/pulse
