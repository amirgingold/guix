#!/bin/sh

if dmesg | grep 'Hypervisor detected'; then # It's a vm
  device=/dev/sda
  boot_partition="$device"1
  swap_partition="$device"2
  root_partition="$device"3
else
  device=/dev/nvme0n1
  boot_partition="$device"p1
  swap_partition="$device"p2
  root_partition="$device"p3
fi

# Wiping
sgdisk -Z "$device"

# Partitioning
sgdisk "$device" --new=1:0:+1G  --typecode=1:ef00 --change-name=1:boot
sgdisk "$device" --new=2:0:+10G --typecode=2:8200 --change-name=2:swap
sgdisk "$device" --new=3:0:0    --typecode=3:8304 --change-name=3:root

# Formatting
mkfs.fat -n my-boot -F32 "$boot_partition"
mkswap "$swap_partition"; sudo swapon "$swap_partition"
mkfs.ext4 -L my-root -F -F "$root_partition"

# Mounting
mount "$root_partition" /mnt
mkdir /mnt/boot
mount "$boot_partition" /mnt/boot

# Installing
herd start cow-store /mnt
mkdir -p ~/.config/guix
cp channels.scm ~/.config/guix
guix pull
hash guix
mkdir /mnt/etc
cp config.scm /mnt/etc/
guix system init /mnt/etc/config.scm /mnt
