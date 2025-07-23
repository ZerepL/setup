#!/bin/bash

# Exit on any error
set -e

export UTILS="$HOME/.utils"

echo "=== Personal Workspace Setup Script ==="
echo "This script will help you set up your development environment."
echo ""

# Check if whiptail is available, install if not
if ! command -v whiptail &> /dev/null; then
    echo "Installing whiptail for interactive menus..."
    sudo apt-get update
    sudo apt-get install -y whiptail
fi

# Function to create component selection menu
select_components() {
    local options=(
        "ZSH" "Zsh shell and Oh My Zsh" ON
        "DOCKER" "Docker and Docker Compose" ON
        "K8S" "Kubernetes tools (kubectl, minikube)" ON
        "HELM" "Helm package manager" ON
        "AWS" "AWS CLI v2" ON
        "DOOM" "Doom Emacs configuration" ON
    )
    
    local selected
    selected=$(whiptail --title "Component Selection" \
        --checklist "Choose components to install (use SPACE to select/deselect, ENTER to confirm):" \
        20 78 6 \
        "${options[@]}" \
        3>&1 1>&2 2>&3)
    
    if [ $? -ne 0 ]; then
        echo "Setup cancelled by user."
        exit 1
    fi
    
    # Parse selected components
    INSTALL_ZSH=false
    INSTALL_DOCKER=false
    INSTALL_K8S=false
    INSTALL_HELM=false
    INSTALL_AWS=false
    INSTALL_DOOM=false
    
    for component in $selected; do
        case $component in
            '"ZSH"') INSTALL_ZSH=true ;;
            '"DOCKER"') INSTALL_DOCKER=true ;;
            '"K8S"') INSTALL_K8S=true ;;
            '"HELM"') INSTALL_HELM=true ;;
            '"AWS"') INSTALL_AWS=true ;;
            '"DOOM"') INSTALL_DOOM=true ;;
        esac
    done
}

# Show component selection menu
select_components

# Show confirmation
echo ""
echo "=== Selected Components ==="
[ "$INSTALL_ZSH" = true ] && echo "✓ Zsh and Oh My Zsh"
[ "$INSTALL_DOCKER" = true ] && echo "✓ Docker"
[ "$INSTALL_K8S" = true ] && echo "✓ Kubernetes tools"
[ "$INSTALL_HELM" = true ] && echo "✓ Helm"
[ "$INSTALL_AWS" = true ] && echo "✓ AWS CLI"
[ "$INSTALL_DOOM" = true ] && echo "✓ Doom Emacs"
echo ""

if ! whiptail --title "Confirm Installation" --yesno "Proceed with installation of selected components?" 10 60; then
    echo "Installation cancelled."
    exit 0
fi

echo ""
echo "=== System Detection ==="

# Detect system type
if grep -i microsoft /proc/version > /dev/null 2>&1; then
    echo "✓ Detected WSL Linux environment"
    wsl=true
else
    echo "✓ Detected regular Linux environment"
    wsl=false
fi

echo ""
echo "=== Installing base packages ==="
sudo apt-get update
sudo apt-get install -y \
    curl \
    wget \
    git \
    ca-certificates \
    gnupg \
    lsb-release \
    apt-transport-https \
    vim \
    neovim \
    htop \
    tree \
    unzip \
    software-properties-common \
    build-essential

# Install dos2unix only if we're installing zsh or on WSL
if [ "$INSTALL_ZSH" = true ] || [ "$wsl" = true ]; then
    sudo apt-get install -y dos2unix
fi

# Install powerline only if installing zsh
if [ "$INSTALL_ZSH" = true ]; then
    sudo apt-get install -y powerline zsh
fi

echo ""
echo "=== Setting up utilities directory ==="
mkdir -p $UTILS/scripts $UTILS/bin
cp files/aliases files/paths $UTILS/
cp files/vimrc $HOME/.vimrc
cp scripts/* $UTILS/scripts/
sudo chmod +x $UTILS/scripts/* 

if [ "$INSTALL_ZSH" = true ]; then
    echo ""
    echo "=== Installing Oh My Zsh ==="
    if [ ! -d "$HOME/.oh-my-zsh" ]; then
        RUNZSH=no sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
    else
        echo "Oh My Zsh already installed, skipping..."
    fi

    echo ""
    echo "=== Configuring Zsh ==="
    if [ "$wsl" = true ]; then
        echo "Using WSL, converting .zshrc file..."
        cp files/zshrc $HOME/.zshrc
        dos2unix -f $HOME/.zshrc
    else
        cp files/zshrc $HOME/.zshrc
    fi

    # Install zsh-autosuggestions plugin
    if [ ! -d "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions" ]; then
        git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
    fi
fi

if [ "$INSTALL_DOCKER" = true ]; then
    echo ""
    echo "=== Installing Docker ==="
    if [ "$wsl" = false ]; then
        # Remove old Docker packages if they exist
        sudo apt-get remove -y docker docker-engine docker.io containerd runc 2>/dev/null || true

        # Add Docker's official GPG key
        sudo mkdir -p /etc/apt/keyrings
        curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
        sudo chmod a+r /etc/apt/keyrings/docker.gpg

        # Add Docker repository
        echo \
          "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
          $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
          sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

        # Install Docker
        sudo apt-get update
        sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
    else
        echo "WSL detected - Please install Docker Desktop on Windows and enable WSL2 integration"
        echo "Visit: https://docs.docker.com/desktop/wsl/"
    fi

    echo ""
    echo "=== Configuring Docker group ==="
    if getent group docker > /dev/null 2>&1; then
        echo "Docker group already exists, adding user..."
        sudo usermod -aG docker $USER
    else
        echo "Creating docker group and adding user..."
        sudo groupadd docker
        sudo usermod -aG docker $USER
    fi
fi

if [ "$INSTALL_K8S" = true ]; then
    echo ""
    echo "=== Installing Kubernetes tools ==="
    
    # Install kubectl
    if ! command -v kubectl &> /dev/null; then
        echo "Installing kubectl..."
        curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"
        sudo install -o root -g root -m 0755 kubectl /usr/local/bin/kubectl
        rm kubectl
    else
        echo "kubectl already installed"
    fi

    # Install minikube (only on regular Linux)
    if [ "$wsl" = false ]; then
        if ! command -v minikube &> /dev/null; then
            echo "Installing minikube..."
            curl -LO https://storage.googleapis.com/minikube/releases/latest/minikube_latest_amd64.deb
            sudo dpkg -i minikube_latest_amd64.deb
            rm minikube_latest_amd64.deb
        else
            echo "Minikube already installed"
        fi
    else
        echo "WSL detected - Skipping minikube installation (use Docker Desktop's Kubernetes instead)"
    fi

    # Install kubectx and kubens
    echo "Installing kubectx and kubens..."
    curl -L https://raw.githubusercontent.com/ahmetb/kubectx/master/kubens -o $UTILS/bin/kubens
    curl -L https://raw.githubusercontent.com/ahmetb/kubectx/master/kubectx -o $UTILS/bin/kubectx
    chmod +x $UTILS/bin/*
fi

if [ "$INSTALL_HELM" = true ]; then
    echo ""
    echo "=== Installing Helm ==="
    if ! command -v helm &> /dev/null; then
        echo "Installing Helm..."
        curl https://baltocdn.com/helm/signing.asc | gpg --dearmor | sudo tee /usr/share/keyrings/helm.gpg > /dev/null
        echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/helm.gpg] https://baltocdn.com/helm/stable/debian/ all main" | sudo tee /etc/apt/sources.list.d/helm-stable-debian.list
        sudo apt-get update
        sudo apt-get install -y helm
    else
        echo "Helm already installed"
    fi
fi

if [ "$INSTALL_AWS" = true ]; then
    echo ""
    echo "=== Installing AWS CLI v2 ==="
    if ! command -v aws &> /dev/null; then
        echo "Installing AWS CLI v2..."
        curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
        unzip awscliv2.zip
        sudo ./aws/install
        rm -rf aws awscliv2.zip
    else
        echo "AWS CLI already installed"
    fi
fi

if [ "$INSTALL_DOOM" = true ]; then
    echo ""
    echo "=== Installing Doom Emacs ==="
    if [ ! -d "$HOME/.config/emacs" ]; then
        echo "Installing Emacs..."
        sudo apt-get install -y emacs
        
        echo "Cloning Doom Emacs..."
        git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
        
        echo "Installing Doom Emacs..."
        ~/.config/emacs/bin/doom install
        
        echo "Copying Doom configuration files..."
        mkdir -p ~/.config/doom
        if [ -f "files/emacs/init.el" ]; then
            cp files/emacs/init.el ~/.config/doom/
        fi
        if [ -f "files/emacs/config.el" ]; then
            cp files/emacs/config.el ~/.config/doom/
        fi
        if [ -f "files/emacs/packages.el" ]; then
            cp files/emacs/packages.el ~/.config/doom/
        fi
        
        echo "Syncing Doom configuration..."
        ~/.config/emacs/bin/doom sync
    else
        echo "Doom Emacs already installed"
    fi
fi

echo ""
echo "=== Setup Complete! ==="
echo ""
echo "Installed components:"
[ "$INSTALL_ZSH" = true ] && echo "✓ Zsh and Oh My Zsh"
[ "$INSTALL_DOCKER" = true ] && echo "✓ Docker"
[ "$INSTALL_K8S" = true ] && echo "✓ Kubernetes tools"
[ "$INSTALL_HELM" = true ] && echo "✓ Helm"
[ "$INSTALL_AWS" = true ] && echo "✓ AWS CLI"
[ "$INSTALL_DOOM" = true ] && echo "✓ Doom Emacs"
echo ""

echo "Next steps:"
echo "1. Log out and log back in (or restart) for group changes to take effect"
if [ "$INSTALL_ZSH" = true ]; then
    echo "2. Run 'source ~/.zshrc' or restart your terminal"
    echo "3. Change your default shell: chsh -s \$(which zsh)"
fi
echo "4. Configure your Git credentials: git config --global user.name/user.email"
if [ "$wsl" = true ] && [ "$INSTALL_DOCKER" = true ]; then
    echo "5. Install Docker Desktop on Windows and enable WSL2 integration"
fi
echo ""

if whiptail --title "Reboot System" --yesno "Would you like to reboot the system now?" 10 60; then
    echo "Rebooting in 5 seconds..."
    sleep 5
    sudo shutdown -r now
else
    echo "Setup complete! Please log out and log back in for all changes to take effect."
    if [ "$INSTALL_ZSH" = true ]; then
        echo "Don't forget to change your default shell with: chsh -s \$(which zsh)"
    fi
fi
