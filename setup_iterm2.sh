#!/bin/bash
# Setup script for iTerm2 preferences in dotfiles

set -e

DOTFILES_DIR="$HOME/dotfiles"
ITERM_PREFS_DIR="$DOTFILES_DIR/iterm2"

echo "Setting up iTerm2 to use dotfiles preferences..."

# Create directory if it doesn't exist
mkdir -p "$ITERM_PREFS_DIR"

# Configure iTerm2 to use custom folder
echo "✓ Configuring iTerm2 preferences location..."
defaults write com.googlecode.iterm2 PrefsCustomFolder -string "$ITERM_PREFS_DIR"
defaults write com.googlecode.iterm2 LoadPrefsFromCustomFolder -bool true

# Set up Semantic History
echo "✓ Configuring Semantic History for Neovim..."
python3 "$DOTFILES_DIR/scripts/setup_iterm_semantic_history.py"

echo ""
echo "=========================================="
echo "✓ iTerm2 setup complete!"
echo "=========================================="
echo ""
echo "Next steps:"
echo "1. Restart iTerm2 for changes to take effect"
echo "2. iTerm2 will now save preferences to: $ITERM_PREFS_DIR"
echo "3. Open nvim in one pane, then Cmd+Click file paths in other panes!"
echo ""
echo "Run tests with: ~/dotfiles/tests/test_nvim_server.sh"
