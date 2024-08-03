#is_vm () { dmesg | grep -q 'Hypervisor detected'; }

device=/dev/sda

# Wiping
sgdisk -z "$device"

# Partitioning
boot_partition="$device"1
swap_partition="$device"2
root_partition="$device"3

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

# Installing
herd start cow-store /mnt
mkdir -p /root/.config/guix
wget https://github.com/amirgingold/guix/raw/main/channels.scm --directory-prefix=/root/.config/guix
guix pull
hash guix

wget https://github.com/amirgingold/guix/raw/main/config.scm
guix system init config.scm /mnt
