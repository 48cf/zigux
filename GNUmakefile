override QEMUFLAGS += -M q35 -m 2G -debugcon stdio -smp 1 \
	-device qemu-xhci,id=xhci \
	-device usb-kbd,bus=xhci.0,port=1,pcap=keyboard.pcap \
	-device usb-mouse,bus=xhci.0,port=2,pcap=mouse.pcap \
	-device usb-net,bus=xhci.0,port=3,pcap=net.pcap \
	# -trace usb_xhci*

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
	qemu-system-x86_64 -cdrom $< $(QEMUFLAGS)

.PHONY: run-kvm
run-kvm: zigux.iso
	qemu-system-x86_64 -cdrom $< -enable-kvm $(QEMUFLAGS)

jinx:
	curl -o $@ https://raw.githubusercontent.com/mintsuki/jinx/802082d0389d0b73afed5f52875a204e9134a3fe/jinx
	chmod +x $@

zigux.iso: jinx
	$(MAKE) distro-base kernel
	./misc/create-image.sh
