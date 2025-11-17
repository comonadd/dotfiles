local lspconfig = require('lspconfig')

-- Setup nvim-cmp capabilities
local capabilities = require('cmp_nvim_lsp').default_capabilities()

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

-- Common on_attach function for all LSP servers
local on_attach = function(client, bufnr)
  local opts = { noremap=true, silent=true, buffer=bufnr }

  -- Basic LSP mappings
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
  vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)

  -- Signature help (show function parameters)
  vim.keymap.set('i', '<C-h>', vim.lsp.buf.signature_help, opts)

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

  -- Auto-organize imports on save for TypeScript/JavaScript files
  if client.name == "typescript-tools" then
    vim.api.nvim_create_autocmd("BufWritePre", {
      buffer = bufnr,
      callback = function()
        -- Organize imports (removes unused imports)
        vim.lsp.buf.code_action({
          context = {
            only = { "source.organizeImports" },
            diagnostics = {},
          },
          apply = true,
        })
      end,
    })
  end
end

-- TypeScript/JavaScript LSP (using typescript-tools instead of ts_ls)
-- Get the global node path from vim.g.node_host_prog (set in options.lua)
local tsserver_path = nil
if vim.g.node_host_prog then
  local node_bin_dir = vim.fn.fnamemodify(vim.g.node_host_prog, ':h')
  tsserver_path = node_bin_dir .. '/tsserver'
  -- Check if tsserver exists at this location
  if vim.fn.executable(tsserver_path) == 0 then
    tsserver_path = nil
  end
end

require("typescript-tools").setup({
  capabilities = capabilities,
  on_attach = on_attach,
  -- Use the global node from fnm if available
  tsserver_path = tsserver_path,
  -- Performance: Separate diagnostic server for better responsiveness
  separate_diagnostic_server = true,
  settings = {
    -- Performance: Increase memory limit for large projects
    tsserver_max_memory = "8192",
    tsserver_file_preferences = {
      includeInlayParameterNameHints = "all",
      includeInlayParameterNameHintsWhenArgumentMatchesName = false,
      includeInlayFunctionParameterTypeHints = true,
      includeInlayVariableTypeHints = true,
      includeInlayPropertyDeclarationTypeHints = true,
      includeInlayFunctionLikeReturnTypeHints = true,
      includeInlayEnumMemberValueHints = true,
    },
    tsserver_format_options = {
      allowIncompleteCompletions = false,
      allowRenameOfImportPath = false,
    },
    complete_function_calls = true,
  },
})

-- Python LSP
-- NOTE: This uses project Python (.venv/bin/python) for LSP analysis
-- Neovim itself uses a different Python (vim.g.python3_host_prog) for pynvim
-- This separation allows using latest Python for neovim while using project Python for LSP
lspconfig.basedpyright.setup({
  capabilities = capabilities,
  on_attach = on_attach,
  settings = {
    basedpyright = {
      analysis = {
        autoSearchPaths = true,
        useLibraryCodeForTypes = true,
        -- Performance: Only analyze open files for faster symbol search
        diagnosticMode = "openFilesOnly",
        typeCheckingMode = "basic",
        -- Performance: Disable pre-emptive indexing
        indexing = false,
      },
    },
    python = {
      pythonPath = vim.fn.getcwd() .. "/.venv/bin/python",
      venvPath = vim.fn.getcwd(),
      venv = ".venv",
    },
  },
})

-- YAML LSP with SchemaStore support
lspconfig.yamlls.setup({
  capabilities = capabilities,
  on_attach = on_attach,
  settings = {
    yaml = {
      schemaStore = {
        enable = true,
        url = "https://www.schemastore.org/api/json/catalog.json",
      },
      schemas = require("schemastore").yaml.schemas(),
      format = {
        enable = true,
      },
      validate = true,
      completion = true,
    },
  },
})

-- JSON LSP with SchemaStore support
lspconfig.jsonls.setup({
  capabilities = capabilities,
  on_attach = on_attach,
  settings = {
    json = {
      schemas = require("schemastore").json.schemas(),
      validate = { enable = true },
    },
  },
})
