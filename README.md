# Personal Workspace Setup

An interactive bash script to configure and install your personal development workspace with customizable component selection.

## Features

- **Interactive Terminal Menu** - Choose exactly what you want to install using a checkbox-style interface
- **Cross-Platform Support** - Works on both regular Linux and WSL environments
- **Modern Tool Versions** - Uses latest installation methods and official repositories
- **Flexible Configuration** - All components are optional with sensible defaults
- **Terminal-Only Compatible** - Perfect for servers without GUI using `whiptail`

## Available Components

| Component | Description | Default |
|-----------|-------------|---------|
| **Zsh** | Zsh shell with Oh My Zsh and autosuggestions plugin | ✅ Enabled |
| **Docker** | Docker CE with Docker Compose plugin | ✅ Enabled |
| **Kubernetes** | kubectl, minikube (Linux only), kubectx, kubens | ✅ Enabled |
| **Helm** | Kubernetes package manager | ✅ Enabled |
| **AWS CLI** | AWS CLI v2 with auto-completion | ✅ Enabled |
| **Doom Emacs** | Doom Emacs with custom configuration | ✅ Enabled |

## Quick Start

1. Clone this repository:
   ```bash
   git clone https://github.com/ZerepL/setup.git
   cd setup
   ```

2. Run the setup script:
   ```bash
   ./setup.sh
   ```

3. Use the interactive menu to select components:
   - Use **SPACE** to select/deselect items
   - Use **arrow keys** to navigate
   - Press **ENTER** to confirm

4. Follow the post-installation instructions

## What Gets Installed

### Base Packages (Always Installed)
- Essential development tools: `curl`, `wget`, `git`, `vim`, `neovim`
- System utilities: `htop`, `tree`, `unzip`, `build-essential`
- Security tools: `ca-certificates`, `gnupg`

### Optional Components

#### Zsh Setup
- Oh My Zsh framework with agnoster theme
- zsh-autosuggestions plugin
- Custom aliases and utilities
- Enhanced history and auto-completion

#### Docker
- **Linux**: Docker CE, Docker Compose plugin, buildx plugin
- **WSL**: Instructions for Docker Desktop integration
- User added to docker group

#### Kubernetes Tools
- kubectl (latest stable version)
- minikube (Linux only, WSL uses Docker Desktop's K8s)
- kubectx and kubens utilities
- Helm integration

#### AWS CLI
- AWS CLI v2 with auto-completion
- Ready for profile configuration

#### Doom Emacs
- Full Doom Emacs installation
- Custom configuration files from `files/emacs/`
- Automatic sync and setup

## File Structure

```
setup/
├── setup.sh              # Main interactive setup script
├── packages/
│   ├── regular_linux.sh   # Linux-specific installations (deprecated)
│   └── wsl_linux.sh      # WSL-specific installations (deprecated)
├── files/
│   ├── aliases           # Custom command aliases
│   ├── paths            # PATH configurations
│   ├── zshrc            # Zsh configuration
│   ├── vimrc            # Vim configuration
│   └── emacs/           # Doom Emacs configurations
├── scripts/
│   ├── push.sh          # Git push utility
│   └── resolve_screen.sh # Graphics driver fix
└── README.md
```

## Custom Utilities

The script creates `~/.utils/` directory with:

### Aliases Categories
- **Docker**: Container management shortcuts
- **Kubernetes**: kubectl and cluster management
- **Java/Maven**: Development and testing utilities
- **Git**: Version control shortcuts
- **System**: Monitoring and maintenance
- **AWS**: Profile and CLI shortcuts

### Example Aliases
```bash
# Docker
dps                    # Pretty docker ps output
dexec <container>      # Execute into container

# Kubernetes  
k                      # kubectl shortcut
kgp                    # Get pods
klogs <pod>           # Follow logs

# Git
gs                     # Git status
ga                     # Git add
gc                     # Git commit

# System
upgrade               # Update system packages
refresh               # Reload shell config
```

## Platform-Specific Behavior

### Regular Linux
- Full Docker installation with all plugins
- Minikube for local Kubernetes development
- All components work natively

### WSL (Windows Subsystem for Linux)
- Skips Docker installation (uses Docker Desktop)
- Skips minikube (uses Docker Desktop's Kubernetes)
- Includes WSL-specific utilities (`wslu`)
- Handles Windows/Linux file format conversion

## Post-Installation

After running the setup:

1. **Restart or log out/in** for group changes to take effect
2. **Change default shell** (if Zsh was installed):
   ```bash
   chsh -s $(which zsh)
   ```
3. **Configure Git**:
   ```bash
   git config --global user.name "Your Name"
   git config --global user.email "your.email@example.com"
   ```
4. **Configure AWS** (if installed):
   ```bash
   aws configure
   ```

## Requirements

- Ubuntu/Debian-based Linux distribution
- Internet connection for downloads
- sudo privileges
- Terminal access

## Troubleshooting

### Common Issues

**Permission denied errors**: Ensure you have sudo privileges

**Network timeouts**: Check internet connection and try again

**Zsh not default**: Run `chsh -s $(which zsh)` and restart terminal

**Docker permission denied**: Log out and back in, or restart system

### Getting Help

Check the installed aliases:
```bash
show-aliases          # Display all custom aliases
guts                  # Navigate to utilities directory
edit-alias           # Edit aliases file
```

**Note**: This setup script is designed for personal development environments. 
