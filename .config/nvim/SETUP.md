# Neovim Setup Dependencies

This document lists all external dependencies required for the neovim configuration.

## NPM Packages (Language Servers & Tools)

Install globally with:
```bash
npm install -g yaml-language-server vscode-langservers-extracted typescript typescript-language-server prettier eslint js-beautify
```

Or from the nvim directory:
```bash
npm run install-global
```

Individual packages:
- `yaml-language-server` - YAML language server
- `vscode-langservers-extracted` - Includes jsonls, cssls, htmlls, eslint-lsp
- `typescript` - TypeScript compiler (required for typescript-tools.nvim)
- `typescript-language-server` - TypeScript language server (backup)
- `prettier` - Code formatter for JS/TS/JSON/YAML/HTML
- `eslint` - JavaScript/TypeScript linter
- `js-beautify` - HTML beautifier

## Python Packages

Install with pip:
```bash
pip install -r requirements.txt
```

Or install individually:
```bash
pip install basedpyright ruff yamllint bashate pynvim
```

Individual packages:
- `basedpyright` - Python language server (LSP)
- `ruff` - Fast Python linter and formatter
- `yamllint` - YAML linter
- `bashate` - Bash script linter
- `pynvim` - Python client for neovim (recommended)

## Other Tools (Optional)

These tools are referenced in the config but need separate installation:

### Via Homebrew (macOS)
```bash
brew install stylua shfmt actionlint
```

### Via Cargo (Rust)
```bash
cargo install stylua
```

Individual tools:
- `stylua` - Lua formatter
- `shfmt` - Shell script formatter
- `actionlint` - GitHub Actions workflow linter

## Verification

After installation, verify the tools are available:

```bash
# LSP servers
which yaml-language-server
which vscode-json-language-server
which typescript-language-server

# Python tools
which basedpyright
which ruff

# Formatters
which prettier
which eslint
```

## Troubleshooting

If neovim complains about missing language servers:
1. Check that the tools are in your PATH: `echo $PATH`
2. Restart your terminal/shell after installation
3. Run `:checkhealth` in neovim to diagnose issues
4. Run `:CheckLSPHealth` (custom command) for LSP-specific diagnostics
