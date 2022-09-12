export UTILS="$HOME/.utils"

echo "Gathering info about system"
if grep -i microsoft /proc/version > /dev/null 
then
	echo "Using WSL Linux"
    wsl=true
else
    echo "Using normal Linux"
    wsl=false
fi

echo "Installing packages"
sudo apt-get update
sudo apt-get install -y \
    curl \
    git \
    zsh \
    ca-certificates \
    gnupg \
    lsb-release \
    powerline \
    apt-transport-https \
    vim

echo "Creating and config utils"
mkdir -p $UTILS/scripts $UTILS/bin
cp files/aliases files/paths $UTILS/
cp scripts/* $UTILS/scripts
sudo chmod +x $UTILS/scripts/* 

echo "Installing oh my zsh"
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

echo "Config zsh"
if $wsl = true 
then
	echo "Using WSL, converting .zshrc file"
    sudo apt install dos2unix -y
    cp files/zshrc $HOME/.zshrc
    cat files/zshrc_docker_wsl >> $HOME/.zshrc
    dos2unix -f $HOME/.zshrc
else
    cp files/zshrc $HOME/.zshrc
fi

echo "Installing specific package" 
if $wsl = false; then
    packages/regular_linux.sh
else
    wsl_linux.sh
fi

echo "Config docker"
if grep -i docker /etc/group > /dev/null 
then
    echo "Docker group already exist... adding user"
    sudo usermod -aG docker $USER
else
    echo "Creating docker group and adding user"
    sudo groupadd docker
    sudo usermod -aG docker $USER
fi

echo "Done!"
read -p "Want to reboot the system? (y/n)" yn
    case $yn in
        [Yy]* ) sudo shutdown -r now;;
        [Nn]* ) exit;;
        * ) echo "Please answer yes or no.";;
    esac
