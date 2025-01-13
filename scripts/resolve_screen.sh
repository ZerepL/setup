
echo "Removing old drivers"
sudo apt-get purge 'nvidia*' -y

echo "Add graphics drivers repo"
sudo add-apt-repository ppa:graphics-drivers -y

echo "Update packages"
sudo apt-get update

echo "Installing drivers"
sudo ubuntu-drivers autoinstall

echo "Comment nvidia kms file"
sudo sed -i "s/options nvidia-drm modeset=1/# options nvidia-drm modeset=1/" /lib/modprobe.d/nvidia-kms.conf

echo "Sleeping"
sleep 5

echo "Done, restarting"
sleep 2
sudo shutdown -r now
