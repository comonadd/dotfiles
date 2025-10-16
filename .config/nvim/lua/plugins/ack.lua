-- Ack configuration
if vim.fn.executable('ag') == 1 then
  vim.g.ackprg = 'ag --vimgrep'
end
