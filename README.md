My dotfiles
===========

My configuration files and development environment setup.

## Features

### iTerm2 Configuration
- Preferences stored in `iterm2/` directory (version controlled)
- **Cmd+Click to open files in Neovim**: Click on file paths (e.g., `file.py:42`) to open them in the Neovim instance running in the same tab
- Tab-specific Neovim servers for isolated editing sessions

Setup: `./setup_iterm2.sh`

### Neovim
- Configuration in `.config/nvim/`
- Server mode with `nvr` (Neovim Remote) integration
- Each iTerm tab gets its own Neovim server socket

### Shell (zsh)
- Custom functions and aliases in `.zshrc`
- Python, Node.js, and other development tools configured

## Quick Setup

1. Clone this repo
2. Run setup scripts:
   ```bash
   ./setup_iterm2.sh
   source ~/.zshrc
   ```
3. Restart iTerm2

## Testing

Test Neovim server functionality:
```bash
./tests/test_nvim_server.sh
```

## Documentation

- [iTerm2 Configuration](iterm2/README.md)
- [Neovim Configuration](.config/nvim/CLAUDE.md)

