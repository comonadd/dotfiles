# Neovim Configuration Tests

This directory contains automated tests for the Neovim configuration.

## Test Files

- **test_config.lua** - Tests fnm Node.js configuration and environment setup
- **test_plugins.lua** - Tests plugin loading and initialization
- **test_typescript_lsp.lua** - Tests TypeScript LSP functionality
- **test_python_lsp.lua** - Tests Python LSP functionality
- **test_completion.lua** - Tests code completion for TypeScript and Python
- **test_lsp_features.lua** - Tests LSP features (hover, goto definition, rename, etc.)

## Running Tests

### Run All Tests

```bash
./tests/run_tests.sh
```

Or from the nvim config directory:

```bash
cd ~/.config/nvim
./tests/run_tests.sh
```

### Run Individual Tests

You can run individual tests using:

```bash
nvim --headless -c "luafile tests/test_config.lua" -c "qa!"
nvim --headless -c "luafile tests/test_typescript_lsp.lua" -c "qa!"
```

## What Gets Tested

### Configuration Tests
- ✓ fnm Node.js path is configured
- ✓ node_host_prog is set correctly
- ✓ PATH contains fnm
- ✓ node and tsserver are accessible

### Plugin Tests
- ✓ lazy.nvim plugin manager loads
- ✓ All critical plugins load (LSP, treesitter, cmp, etc.)
- ✓ Plugin counts and configuration

### LSP Tests
- ✓ TypeScript LSP attaches and provides diagnostics
- ✓ Python LSP attaches and provides diagnostics
- ✓ Code completion works for both languages
- ✓ All LSP features available (hover, goto definition, rename, references, code actions)

## Requirements

- Neovim 0.9+
- fnm with Node.js installed
- All language servers installed (tsserver, basedpyright)

## CI/CD Integration

These tests can be integrated into CI/CD pipelines to ensure configuration changes don't break functionality.

## Adding New Tests

To add a new test:

1. Create a new `test_*.lua` file in this directory
2. Add the test to `run_tests.sh`
3. Follow the existing test patterns for consistency
