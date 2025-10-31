-- Key mappings
local keymap = vim.keymap.set

-- Basic mappings
keymap('n', ';', ':', {desc = 'Command mode'})
keymap('n', 'Q', '<nop>', {desc = 'Disable Ex mode'})
keymap('n', '<F1>', '<nop>', {desc = 'Disable help'})
keymap('n', '<S-k>', '<nop>', {desc = 'Disable man pages'})

-- Function keys
keymap('n', '<F5>', '<cmd>source ~/.config/nvim/init.lua<CR>', {desc = 'Reload config'})
keymap('n', '<silent><F5>', '<cmd>lnext<CR>', {desc = 'Next location'})
keymap('n', '<silent><F6>', '<cmd>lprev<CR>', {desc = 'Previous location'})
keymap('n', '<silent><C-Space>', '<cmd>ll<CR>', {desc = 'Current location'})

-- Leader mappings
keymap('n', '<leader>cr', '<cmd>source ~/.config/nvim/init.lua<CR>', {desc = 'Reload config'})
keymap('n', '<leader>ce', '<cmd>e ~/.config/nvim/init.lua<CR>', {desc = 'Edit config'})
keymap('n', '<leader>rr', '<cmd>MRU<CR>', {desc = 'Recent files'})

-- Clipboard
keymap('v', '<leader>y', '"+y', {desc = 'Yank to clipboard'})
keymap({'v', 'n'}, '<leader>p', '"+p', {desc = 'Paste from clipboard'})

-- Save and quit
keymap('n', '<C-s>', '<cmd>w<CR>', {desc = 'Save'})
keymap('n', '<D-s>', '<cmd>w<CR>', {desc = 'Save (Mac)'})
keymap('n', '<leader>ww', '<cmd>w<CR>', {desc = 'Save'})
keymap('n', '<C-w>', '<cmd>q!<CR>', {desc = 'Quit without saving'})
keymap({'n', 'v'}, '<C-S>', '<cmd>update<CR>', {desc = 'Update file'})
keymap('i', '<C-S>', '<C-O><cmd>update<CR>', {desc = 'Update file'})

-- Window navigation
keymap('n', '<C-k>', '<cmd>wincmd k<CR>', {silent = true, desc = 'Move up'})
keymap('n', '<C-j>', '<cmd>wincmd j<CR>', {silent = true, desc = 'Move down'})
keymap('n', '<C-h>', '<cmd>wincmd h<CR>', {silent = true, desc = 'Move left'})
keymap('n', '<C-l>', '<cmd>wincmd l<CR>', {silent = true, desc = 'Move right'})

-- Buffer navigation
keymap('n', '<tab>', '<C-^>', {silent = true, desc = 'Switch buffer'})

-- FZF mappings
keymap('n', '<leader>h', '<cmd>History<CR>', {desc = 'File history'})
keymap('n', '<leader>t', '<cmd>Tags<CR>', {desc = 'Tags'})
keymap('n', '<leader>:', '<cmd>History:<CR>', {desc = 'Command history'})
keymap('n', '<leader>/', '<cmd>History/<CR>', {desc = 'Search history'})
keymap('n', '<leader>b', '<cmd>Buffers<CR>', {desc = 'List buffers'})
keymap('n', '<leader>l', '<cmd>Lines<CR>', {desc = 'Find lines'})
keymap('n', '<leader>fi', '<cmd>RgExact<CR>', {desc = 'Exact search'})

-- Telescope
keymap('n', '<leader>fp', '<cmd>Telescope find_files<CR>', {desc = 'Find files with preview'})
keymap('n', '<leader>g', '<cmd>Telescope live_grep<CR>', {desc = 'Find text in project'})
keymap('n', '<leader>s', '<cmd>Telescope lsp_document_symbols<CR>', {desc = 'Symbols in current file'})
keymap('n', '<leader>S', '<cmd>Telescope lsp_dynamic_workspace_symbols<CR>', {desc = 'Symbols in workspace'})
keymap('n', 'gr', '<cmd>Telescope lsp_references<CR>', {desc = 'References to this symbol'})

-- Oil file explorer
keymap('n', '<F4>', function() require("oil").open(vim.fn.expand("%:p:h")) end, {desc = 'Open oil in current directory'})
keymap('n', '<C-o>', function() require("oil").open(vim.fn.expand("%:p:h")) end, {desc = 'Open oil in current directory'})
keymap('n', '<C-n>', function() require("oil").open() end, {desc = 'Open oil'})

-- LSP diagnostic mappings
keymap('n', '[g', vim.diagnostic.goto_prev, {desc = 'Previous diagnostic'})
keymap('n', ']g', vim.diagnostic.goto_next, {desc = 'Next diagnostic'})

-- Clear search
keymap('n', '<Leader><Space>', '<cmd>noh<CR>', {desc = 'Clear search highlight'})

-- Alignment
keymap('v', '<Leader>a', '<cmd>Align<CR>', {desc = 'Align selection'})

-- Other mappings
keymap('n', '<leader>p/', '<cmd>Ack ', {desc = 'Ack search'})
keymap('n', '<leader>e', '<cmd>RemoveUnusedImports<CR>', {desc = 'Remove unused imports'})
keymap('n', '<Leader>uu', '<cmd>RemoveUnusedImports<CR>', {desc = 'Remove unused imports'})
-- Note: Tab/Shift-Tab/CR completion mappings are handled by nvim-cmp in lazy.lua
-- Note: gd, gr, K, <leader>rn are handled by LSP on_attach in lsp.lua

local functions = require('config.functions')
vim.keymap.set('n', '<leader>ai', functions.add_missing_imports, { noremap = true, silent = true })
vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, { noremap=true, silent=true })
vim.keymap.set('n', '<F12>', '<cmd>Telescope colorscheme<cr>', {
  noremap = true,
  silent = true,
  desc = "Select colorscheme"
})

vim.keymap.set('n', '<leader>lsp', function()
  local clients = vim.lsp.get_active_clients({ bufnr = 0 })
  for _, client in ipairs(clients) do
    print("Client: " .. client.name)
    print("Server capabilities:")
    print(vim.inspect(client.server_capabilities))
  end
end, { desc = "Show LSP capabilities" })

vim.keymap.set('n', '<leader>ii', function()
  vim.lsp.buf.code_action({
    context = {
      only = { "source.addMissingImports.ts" },
      diagnostics = {}
    },
    apply = true,
  })
end, { noremap = true, silent = true, desc = "Add all missing imports" })

-- ALE error navigation keybindings
vim.keymap.set('n', ']q', ':ALENext<CR>', { desc = 'Go to next ALE error' })
vim.keymap.set('n', '[q', ':ALEPrevious<CR>', { desc = 'Go to previous ALE error' })

-- Luasnip

vim.keymap.set({"i", "s"}, "<C-k>", function()
  if require("luasnip").expand_or_jumpable() then
    require("luasnip").expand_or_jump()
  end
end, {silent = true})

vim.keymap.set({"i", "s"}, "<C-j>", function()
  if require("luasnip").jumpable(-1) then
    require("luasnip").jump(-1)
  end
end, {silent = true})
