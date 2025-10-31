-- Bootstrap lazy.nvim plugin manager
require("config.lazy")

require("config.functions")

-- Load core configuration
require("config.options")

require("plugins.fzf")
require("plugins.ack")
require("plugins.lsp")
require("plugins.luasnip")

require("config.autocmds")
require("config.commands")
require("config.keymaps")

vim.api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
  pattern = ".env.*",
  command = "set filetype=sh",
})
