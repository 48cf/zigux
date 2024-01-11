QEMUFLAGS ?= -M q35,smm=off -m 2G -debugcon stdio -smp 1

.PHONY: all
all:
	rm -f zigux.iso
	$(MAKE) zigux.iso

.PHONY: distro-full
distro-full: jinx
	./jinx build-all

.PHONY: distro-base
distro-base: jinx
	./jinx build base-files kernel init bash coreutils

.PHONY: run
run: zigux.iso
	qemu-system-x86_64 -cdrom $< -cpu qemu64,+fsgsbase $(QEMUFLAGS)

.PHONY: run-kvm
run-kvm: zigux.iso
	qemu-system-x86_64 -cdrom $< -enable-kvm -cpu host $(QEMUFLAGS)

jinx:
	curl -o $@ https://raw.githubusercontent.com/mintsuki/jinx/trunk/jinx
	chmod +x $@

zigux.iso: jinx
	rm -f builds/kernel.packaged
	$(MAKE) distro-base
	./misc/create-image.sh
