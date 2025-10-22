# Neovim configuration

## Structure

- ./init.lua main entry point for the configuration
- ./lua/
- ./lua/plugins/ contains configuration for plugins (configuration for new plugins should go into a separate file here)
- ./lua/config/lazy.lua contains config for Lazy plugin manager, and may have configs for plugins
- ./lua/snippets/ contains snippets for the snippet plugin
- ./lua/config/keymaps.lua contains keymap configuration
- ./lua/config/options.lua basic options

## Plugins
- ALE for linters & fixers
- nvim-cmp for completions
- basedpyright for python LSP
