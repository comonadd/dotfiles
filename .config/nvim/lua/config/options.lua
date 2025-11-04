-- Disable compatibility with vi
vim.opt.compatible = false

-- Configure Node.js from fnm for all language servers and plugins
-- This ensures we always use the global node from fnm, not local node_modules
local function setup_fnm_node()
  -- Get the fnm root directory
  local fnm_dir = vim.fn.expand('~/.local/share/fnm')

  -- Get the global node path from fnm
  local handle = io.popen('fnm exec --using default which node 2>/dev/null')
  if handle then
    local node_path = handle:read("*a"):gsub("%s+$", "")
    handle:close()

    if node_path and node_path ~= "" and vim.fn.executable(node_path) == 1 then
      -- Set the node path for neovim-node-host
      vim.g.node_host_prog = node_path

      -- Get the bin directory from the node path
      local node_bin_dir = vim.fn.fnamemodify(node_path, ':h')

      -- Prepend the fnm node bin directory to PATH
      -- This ensures all language servers and plugins use the global node
      local current_path = vim.env.PATH or ""
      vim.env.PATH = node_bin_dir .. ":" .. current_path

      return true
    end
  end

  return false
end

-- Setup fnm node
if not setup_fnm_node() then
  -- Fallback: try to find node in PATH
  vim.notify("Warning: Could not find fnm node installation, using system node", vim.log.levels.WARN)
end

-- Basic settings
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.wrap = false
vim.opt.backspace = {'indent', 'eol', 'start'}
vim.opt.incsearch = true
vim.opt.showmode = true
vim.opt.wildmenu = true
vim.opt.lazyredraw = true
vim.opt.hidden = true
vim.opt.softtabstop = 4
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.cindent = true
vim.opt.mouse = 'a'
vim.opt.cursorline = true
vim.opt.numberwidth = 6
vim.opt.encoding = 'utf-8'
vim.opt.modeline = false
vim.opt.ignorecase = true
vim.opt.clipboard = 'unnamedplus'

-- Swap and backup
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.writebackup = false

-- Undo
vim.opt.undofile = true
vim.opt.undolevels = 1000
vim.opt.undoreload = 10000

-- Display
vim.opt.list = true
vim.opt.listchars = {tab = '  ', trail = '·', extends = '>'}
vim.opt.cmdheight = 2
vim.opt.updatetime = 300
vim.opt.shortmess:append('c')
vim.opt.signcolumn = 'yes'
vim.opt.laststatus = 2

-- Wildignore patterns
vim.opt.wildignore:append({
  '*/node_modules/*', '*/dist/*', '*/__pycache__/*', '*/venv/*',
  '*.o', '*.obj', '*.ilk', '*/build/*', '*/build_native/*',
  '*/build-*/*', '*/vendor/*', '*.patch', 'moc_*.cpp', 'moc_*.h',
  '*/target/debug/*', '*/target/release/*', '*/Assets/*.meta'
})

vim.cmd([[
let g:fzf_colors = {
\ 'fg':      ['fg', 'Normal'],
\ 'bg':      ['bg', 'Normal'],
\ 'hl':      ['fg', 'Comment'],
\ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
\ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
\ 'hl+':     ['fg', 'Statement'],
\ 'info':    ['fg', 'PreProc'],
\ 'border':  ['fg', 'Ignore'],
\ 'prompt':  ['fg', 'Conditional'],
\ 'pointer': ['fg', 'Exception'],
\ 'marker':  ['fg', 'Keyword'],
\ 'spinner': ['fg', 'Label'],
\ 'header':  ['fg', 'Comment']
\ }
]])

-- Lightline configuration
vim.g.lightline = {
  active = {
    left = {{'mode', 'paste'}, {'relativepath', 'modified'}}
  },
  colorscheme = 'spaceduck'
}

-- Colorscheme

vim.cmd('hi clear')
if vim.fn.exists('syntax_on') then
  vim.cmd('syntax reset')
end

vim.cmd('colorscheme PaperColor')

vim.api.nvim_create_autocmd('VimEnter', {
  callback = function()
    vim.cmd('colorscheme PaperColor')
    -- Also reset any highlight overrides
    vim.cmd('hi clear')
    vim.cmd('colorscheme PaperColor')
  end,
})

-- Leader key
vim.g.mapleader = ' '

-- Plugin-specific settings
vim.g.vim_markdown_folding_disabled = 1
vim.g.svelte_indent_script = 0
vim.g.svelte_indent_style = 0
vim.g.svelte_preprocessors = {'typescript'}
vim.g.go_fmt_command = 'gofmt'
vim.g.go_fmt_options = {gofmt = '-s'}
vim.g.rustfmt_autosave = 1
vim.g.vim_isort_python_version = 'python3'
vim.g.clang_format_enable_fallback_style = 1
vim.g.graphviz_output_format = 'png'
vim.g.graphviz_viewer = 'sxiv'
vim.g.lsp_preview_keep_focus = 0
vim.g.VM_mouse_mappings = 1
vim.g.VM_theme = 'iceblue'

-- Environment variable
if vim.env.LEVELS_OF_VIM then
  vim.env.LEVELS_OF_VIM = tostring(tonumber(vim.env.LEVELS_OF_VIM) + 1)
else
  vim.env.LEVELS_OF_VIM = '1'
end
