-- Autocommands
local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

local keymap = vim.keymap.set

-- File type specific settings
autocmd('FileType', {
  pattern = {'javascript', 'html'},
  callback = function()
    vim.opt_local.tabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.softtabstop = 2
  end,
})

-- Go specific mappings
autocmd('FileType', {
  pattern = 'go',
  callback = function()
    keymap('n', '<leader>r', '<Plug>(go-run)', {buffer = true, desc = 'Go run'})
    keymap('n', '<leader>b', '<Plug>(go-build)', {buffer = true, desc = 'Go build'})
    keymap('n', '<leader>t', '<Plug>(go-test)', {buffer = true, desc = 'Go test'})
    keymap('n', '<leader>c', '<Plug>(go-coverage)', {buffer = true, desc = 'Go coverage'})
    keymap('n', '<leader>d', '<Plug>(go-doc)', {buffer = true, desc = 'Go doc'})
    keymap('n', '<leader>e', '<cmd>GoIfErr<CR>', {buffer = true, desc = 'Go if err'})
  end,
})

-- C/C++ specific mappings
autocmd('FileType', {
  pattern = {'cpp', 'c'},
  callback = function()
    keymap('n', '<leader>f', '<Plug>(operator-clang-format)', {buffer = true, desc = 'Format'})
    keymap('n', '<leader>h', '<cmd>call CurtineIncSw()<CR>', {buffer = true, desc = 'Switch header/source'})
  end,
})

-- Python specific mappings
autocmd('FileType', {
  pattern = 'python',
  callback = function()
    keymap('n', '<leader>f', '<Plug>(ale_fix)', {buffer = true, desc = 'Fix with ALE'})
  end,
})

-- TypeScript/JavaScript/JSON specific mappings
autocmd('FileType', {
  pattern = {'typescript', 'javascript', 'typescriptreact', 'javascriptreact', 'json'},
  callback = function()
    keymap('n', '<leader>f', '<Plug>(ale_fix)', {buffer = true, desc = 'Format with Prettier'})
  end,
})

-- Lua specific mappings
autocmd('FileType', {
  pattern = 'lua',
  callback = function()
    keymap('n', '<leader>f', '<Plug>(ale_fix)', {buffer = true, desc = 'Format with stylua'})
  end,
})

-- Auto-remove trailing whitespace
autocmd('BufWritePre', {
  pattern = {'*.c', '*.cpp', '*.java', '*.php', '*.javascript', '*.typescript', '*.javascriptreact', '*.python'},
  command = '%s/\\s\\+$//e'
})

-- Auto-create directories
autocmd('BufWritePre', {
  callback = function()
    local dir = vim.fn.expand('<afile>:p:h')
    if vim.fn.isdirectory(dir) == 0 then
      vim.fn.mkdir(dir, 'p')
    end
  end,
})

-- GLSL file types
autocmd({'BufNewFile', 'BufRead'}, {
  pattern = {'*.vs', '*.fs', '*.glsl'},
  command = 'set ft=glsl'
})

-- React file types
autocmd({'BufRead', 'BufNewFile'}, {
  pattern = '*.jsx',
  command = 'set filetype=javascriptreact'
})

autocmd({'BufRead', 'BufNewFile'}, {
  pattern = '*.tsx',
  command = 'set filetype=typescriptreact'
})


-- Luasnip

vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    local luasnip = require("luasnip")
    local s = luasnip.snippet
    local t = luasnip.text_node
    local i = luasnip.insert_node

    -- TypeScript snippets
    luasnip.add_snippets("typescript", {
      s("esld", {
        t("// eslint-disable-next-line "),
        i(1, "no-unused-vars")
      }),
      s("tsd", {
        t("// @ts-ignore")
      }),
      s("tse", {
        t("// @ts-expect-error")
      })
    })

    -- Also for TSX
    luasnip.add_snippets("typescriptreact", {
      s("esld", {
        t("// eslint-disable-next-line "),
        i(1, "no-unused-vars")
      }),
      s("tsd", {
        t("// @ts-ignore")
      })
    })

    print("TypeScript snippets loaded!")
  end
})
