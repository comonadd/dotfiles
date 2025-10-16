local F = {}

function F.show_documentation()
  local filetype = vim.bo.filetype
  if vim.tbl_contains({'vim', 'help'}, filetype) then
    vim.cmd('h ' .. vim.fn.expand('<cword>'))
  else
    vim.lsp.buf.hover()
  end
end

function F.check_back_space()
  local col = vim.fn.col('.') - 1
  return col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') ~= nil
end

-- Generic LSP approach that works with any TypeScript LSP
function F.add_missing_imports()
  local params = vim.lsp.util.make_range_params()
  params.context = {
    only = { 'source.addMissingImports' },
    diagnostics = vim.lsp.diagnostic.get_line_diagnostics()
  }

  local result = vim.lsp.buf_request_sync(0, 'textDocument/codeAction', params, 1000)
  if result then
    for _, res in pairs(result) do
      if res.result then
        for _, action in pairs(res.result) do
          if action.edit then
            vim.lsp.util.apply_workspace_edit(action.edit, 'utf-8')
          end
        end
      end
    end
  end
end

function F.add_missing_imports_v2()
  -- Get current buffer
  local bufnr = vim.api.nvim_get_current_buf()

  -- Get all diagnostics for the current buffer
  local diagnostics = vim.diagnostic.get(bufnr)

  -- Filter for import-related diagnostics
  local import_diagnostics = {}
  for _, diagnostic in ipairs(diagnostics) do
    if diagnostic.message and (
      string.match(diagnostic.message, "is not defined") or
      string.match(diagnostic.message, "undefined name") or
      string.match(diagnostic.message, "imported but unused") == nil -- avoid unused imports
    ) then
      table.insert(import_diagnostics, diagnostic)
    end
  end

  -- Request code actions for each diagnostic
  for _, diagnostic in ipairs(import_diagnostics) do
    vim.lsp.buf.code_action({
      range = {
        start = { line = diagnostic.lnum, character = diagnostic.col },
        ['end'] = { line = diagnostic.end_lnum or diagnostic.lnum, character = diagnostic.end_col or diagnostic.col }
      },
      filter = function(action)
        return action.kind and (
          string.match(action.kind, "quickfix") or
          string.match(action.kind, "source.organizeImports") or
          action.title:match("Import") or
          action.title:match("Add import")
        )
      end,
      apply = true,
    })
  end
end

return F
