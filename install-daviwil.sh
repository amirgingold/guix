#is_vm () { dmesg | grep -q 'Hypervisor detected'; }

config='config-daviwil.scm'

device=/dev/nvme0n1

# Wiping
sgdisk -z "$device"

# Partitioning
boot_partition="$device"p1
swap_partition="$device"p2
root_partition="$device"p3

sgdisk "$device" --new=1:0:+1G  --typecode=1:ef00
sgdisk "$device" --new=2:0:+10G --typecode=2:8200
sgdisk "$device" --new=3:0:0    --typecode=3:8304

# Formatting
mkfs.fat -F32 "$boot_partition"
mkswap "$swap_partition"; swapon "$swap_partition"
mkfs.ext4 -F -F "$root_partition"

# Labeling
fatlabel "$boot_partition" BOOT
e2label "$root_partition" ROOT

# Mounting
mount "$root_partition" /mnt
mkdir /mnt/boot
mount "$boot_partition" /mnt/boot

# Making /gnu/store copy-on-write
herd start cow-store /mnt

# Channels pulling
mkdir -p /root/.config/guix
wget https://github.com/amirgingold/guix/raw/main/channels.scm --directory-prefix=/root/.config/guix
guix pull
hash guix

# Installing
wget https://github.com/amirgingold/guix/raw/main/$config
mkdir /mnt/etc
cp $config /mnt/etc/config.scm
guix system init /mnt/etc/config.scm /mnt
