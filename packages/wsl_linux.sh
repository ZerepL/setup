#!/bin/bash

echo "Installing Docker Desktop for WSL2"
# Docker Desktop handles Docker in WSL2, so we don't install docker-ce
echo "Please install Docker Desktop on Windows and enable WSL2 integration"
echo "Visit: https://docs.docker.com/desktop/wsl/"

echo "Installing kubectl"
# Use the official Kubernetes method
curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"
sudo install -o root -g root -m 0755 kubectl /usr/local/bin/kubectl
rm kubectl

echo "Installing kubectx and kubens"
curl -L https://raw.githubusercontent.com/ahmetb/kubectx/master/kubens -o $UTILS/bin/kubens
curl -L https://raw.githubusercontent.com/ahmetb/kubectx/master/kubectx -o $UTILS/bin/kubectx
chmod +x $UTILS/bin/*

echo "Installing zsh-autosuggestions plugin"
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

echo "Installing WSL-specific utilities"
# Install wslu for WSL utilities
sudo apt update
sudo apt install -y wslu

echo "WSL setup complete!"
echo "Note: Remember to configure Docker Desktop WSL2 integration"
