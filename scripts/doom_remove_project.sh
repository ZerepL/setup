#!/bin/zsh

if [ -z "$1" ]; then
    echo "Usage: doom_remove_project.sh <project-name>"
    exit 1
fi

PROJECT_NAME="$1"
TREEMACS_FILE="$HOME/.config/emacs/.local/cache/treemacs-persist"

if [ ! -f "$TREEMACS_FILE" ]; then
    echo "Error: Treemacs persist file not found at $TREEMACS_FILE"
    exit 1
fi

# Check if project exists in the file
if ! grep -q "$PROJECT_NAME" "$TREEMACS_FILE"; then
    echo "Project '$PROJECT_NAME' not found in treemacs cache"
    exit 0
fi

# Create backup
cp "$TREEMACS_FILE" "$TREEMACS_FILE.bak"

# Remove the project entry (the ** line and the - path :: line that follows)
sed -i "/^\*\* $PROJECT_NAME$/,/^ - path ::.*$PROJECT_NAME/d" "$TREEMACS_FILE"

echo "Removed '$PROJECT_NAME' from Doom Emacs treemacs cache"
echo "Backup saved to $TREEMACS_FILE.bak"
echo "Restart Doom Emacs for changes to take effect"
