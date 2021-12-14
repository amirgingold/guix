is_not_vm=$(dmesg | grep 'Hypervisor detected' | wc -l)

if test $is_not_vm; then
  device=/dev/nvme0n1
    
  boot_partition="$device"p1
  swap_partition="$device"p2
  root_partition="$device"p3

  # Wiping
  sgdisk -Z "$device"
  
  # Partitioning
  sgdisk "$device" --new=1:0:+1G  --typecode=1:ef00
  sgdisk "$device" --new=2:0:+10G --typecode=2:8200
  sgdisk "$device" --new=3:0:0    --typecode=3:8304
else
  device=/dev/sda

  boot_partition="$device"1
  swap_partition="$device"2
  root_partition="$device"3

  # Partitioning
  sfdisk /dev/sda << EOF
    ,1G,83
    ,10G,82
    ,,
  EOF
fi

# Formatting
mkfs.fat -F32 "$boot_partition" 
mkswap "$swap_partition"; swapon "$swap_partition"
mkfs.ext4 -F -F "$root_partition"

# Labeling
fatlabel /dev/sda1 BOOT
e2label /dev/sda3 ROOT

# Mounting
mount "$root_partition" /mnt
mkdir /mnt/boot
mount "$boot_partition" /mnt/boot

# Installing
herd start cow-store /mnt
mkdir -p /root/.config/guix
wget https://github.com/amirgingold/guix/raw/main/channels.scm --directory-prefix=/root/.config/guix
guix pull
xfhash guix
mkdir /mnt/etc
if test $is_not_vm; then
  wget https://github.com/amirgingold/guix/raw/main/config.scm --directory-prefix=/mnt/etc/
else
  wget https://github.com/amirgingold/guix/raw/main/config-vm.scm --directory-prefix=/mnt/etc/
fi
guix system init /mnt/etc/config.scm /mnt
