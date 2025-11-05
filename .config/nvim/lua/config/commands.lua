-- Commands
vim.api.nvim_create_user_command('Prettier', 'CocCommand prettier.formatFile', {})
vim.api.nvim_create_user_command('FilesHere', 'Files %:h', {})
vim.api.nvim_create_user_command('RemoveUnusedImports', '%!autoflake --remove-all-unused-imports --stdin-display-name %', {})
vim.api.nvim_create_user_command('RgExact', function(opts)
  vim.fn['fzf#vim#grep']('rg --column --line-number --no-heading --color=always --smart-case --fixed-strings -- ' .. vim.fn.shellescape(opts.args), 1, vim.fn['fzf#vim#with_preview'](), opts.bang)
end, {bang = true, nargs = '*'})

vim.api.nvim_create_user_command('CreateHere', function(opts)
  vim.cmd('e %:h/' .. opts.args)
end, {nargs = 1})

vim.api.nvim_create_user_command('Align', function(opts)
  local line1 = opts.line1
  local line2 = opts.line2
  local regex = opts.args ~= '' and opts.args or '='

  local function align_line(line, sep, maxpos, extra)
    local pattern = '(.-) %-(' .. sep .. '.*)'
    local m1, m2 = line:match(pattern)
    if not m1 then return line end
    local spaces = string.rep(' ', maxpos - #m1 + extra)
    return m1 .. spaces .. m2
  end

  local function align_section()
    local extra = 1
    local sep = regex
    local maxpos = 0
    local lines = vim.api.nvim_buf_get_lines(0, line1 - 1, line2, false)

    for _, line in ipairs(lines) do
      local pos = line:find(' *' .. sep)
      if pos and maxpos < pos then
        maxpos = pos
      end
    end

    for i, line in ipairs(lines) do
      lines[i] = align_line(line, sep, maxpos, extra)
    end

    vim.api.nvim_buf_set_lines(0, line1 - 1, line2, false, lines)
  end

  align_section()
end, {range = true, nargs = '?'})

-- Create command abbreviation
vim.cmd('cnoreabbrev Ack Ack!')

local functions = require('config.functions')

vim.api.nvim_create_user_command('AddMissingImports', functions.add_missing_imports, {})
vim.api.nvim_create_user_command('AddMissingImportsV2', functions.add_missing_imports_v2, {})

-- Check Python and Node.js versions used by neovim
vim.api.nvim_create_user_command('CheckVersions', function()
  print("=== Neovim Runtime Versions ===\n")

  -- Neovim Python (for pynvim)
  if vim.g.python3_host_prog then
    local python_version = vim.fn.system(vim.g.python3_host_prog .. ' --version'):gsub("\n", "")
    print("Neovim Python (pynvim): " .. vim.g.python3_host_prog)
    print("  Version: " .. python_version)
  else
    print("Neovim Python: Not configured")
  end

  print("")

  -- Project Python (for LSP)
  local project_python = vim.fn.getcwd() .. "/.venv/bin/python"
  if vim.fn.executable(project_python) == 1 then
    local project_version = vim.fn.system(project_python .. ' --version'):gsub("\n", "")
    print("Project Python (LSP/linters): " .. project_python)
    print("  Version: " .. project_version)
  else
    print("Project Python: .venv/bin/python not found")
  end

  print("")

  -- Node.js
  if vim.g.node_host_prog then
    local node_version = vim.fn.system(vim.g.node_host_prog .. ' --version'):gsub("\n", "")
    print("Node.js: " .. vim.g.node_host_prog)
    print("  Version: " .. node_version)
  else
    print("Node.js: Not configured")
  end

  print("\n=== LSP Servers ===")
  local clients = vim.lsp.get_active_clients({ bufnr = 0 })
  if #clients == 0 then
    print("No LSP clients active in current buffer")
  else
    for _, client in ipairs(clients) do
      print("  - " .. client.name)
    end
  end
end, {})
