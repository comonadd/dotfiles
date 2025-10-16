local lspconfig = require('lspconfig')

vim.api.nvim_create_user_command('CheckLSPHealth', function()
  vim.cmd('checkhealth vim.lsp')
end, {})

-- Debug function to show all available code actions
vim.api.nvim_create_user_command('ShowCodeActions', function()
  local params = vim.lsp.util.make_range_params()
  params.context = {
    diagnostics = vim.lsp.diagnostic.get_line_diagnostics(),
    only = nil -- Get all actions
  }

  vim.lsp.buf_request(0, 'textDocument/codeAction', params, function(err, result)
    if err then
      print("Error:", err)
      return
    end

    if not result or #result == 0 then
      print("No code actions available at cursor position")
      return
    end

    print("Available code actions:")
    for i, action in ipairs(result) do
      local title = action.title or "No title"
      local kind = action.kind or "no kind"
      print(string.format("%d: %s [%s]", i, title, kind))
    end
  end)
end, {})

-- Debug LSP capabilities
vim.api.nvim_create_user_command('DebugLSP', function()
  local clients = vim.lsp.get_active_clients({ bufnr = 0 })
  if #clients == 0 then
    print("No LSP clients attached to current buffer")
    return
  end

  for _, client in ipairs(clients) do
    print("Client:", client.name)
    print("Code action support:", client.server_capabilities.codeActionProvider ~= nil)
    if client.server_capabilities.codeActionProvider then
      print("Code action details:", vim.inspect(client.server_capabilities.codeActionProvider))
    end
    print("Diagnostics in buffer:", #vim.diagnostic.get(0))
  end
end, {})

lspconfig.ts_ls.setup({
  on_attach = function(client, bufnr)
    local opts = { noremap=true, silent=true, buffer=bufnr }

    -- Basic LSP mappings
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)

    -- Code actions (includes add missing imports)
    vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, opts)

    -- Specific add missing imports
    vim.keymap.set('n', '<leader>ai', function()
      vim.lsp.buf.code_action({
        filter = function(action)
          return action.kind and string.match(action.kind, "source.addMissingImports")
        end,
        apply = true,
      })
    end, opts)
  end,
})

vim.lsp.enable('basedpyright')
