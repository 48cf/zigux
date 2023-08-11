QEMUFLAGS ?= -M q35,smm=off -m 24G -debugcon stdio -smp 4 -cpu qemu64,+fsgsbase -d int -no-reboot

.PHONY: all
all:
	rm -f zigux.iso
	$(MAKE) zigux.iso

.PHONY: clean
clean:
	rm -rf initramfs.tar iso-root zigux.iso

.PHONY: distro-clean
distro-clean: clean
	./jinx clean

.PHONY: distro-full
distro-full: jinx
	./jinx build-all

.PHONY: distro-base
distro-base: jinx
	./jinx build kernel

.PHONY: run
run: zigux.iso
	qemu-system-x86_64 -cdrom $< $(QEMUFLAGS)

.PHONY: run-kvm
run-kvm: zigux.iso
	qemu-system-x86_64 -cdrom $< -enable-kvm -cpu host $(QEMUFLAGS)

jinx:
	curl -o $@ https://raw.githubusercontent.com/mintsuki/jinx/trunk/jinx
	chmod +x $@

zigux.iso: jinx
	rm -f builds/kernel.installed
	$(MAKE) distro-base
	./tools/create-iso.sh
