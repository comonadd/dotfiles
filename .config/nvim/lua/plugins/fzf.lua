-- FZF settings
vim.g.fzf_tags_command = 'ctags -R --exclude=.git --exclude=node_modules --exclude="*static/dist*" --exclude="*static/vendor*" --exclude="*.css" --exclude="*cassettes*" --exclude="*package-lock.json*"'
vim.env.FZF_DEFAULT_COMMAND = 'ag -g ""'
