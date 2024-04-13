override QEMUFLAGS += -M q35,smm=off -m 2G -debugcon stdio -smp 1

.PHONY: all
all:
	rm -f zigux.iso
	$(MAKE) zigux.iso

.PHONY: kernel
kernel: jinx
	rm -f builds/kernel.built builds/kernel.packaged
	./jinx build kernel

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
	curl -o $@ https://raw.githubusercontent.com/mintsuki/jinx/ce37c30acbf59aa79d9b26e05f93ced3d5920b00/jinx
	chmod +x $@

zigux.iso: jinx
	$(MAKE) distro-base kernel
	./misc/create-image.sh
