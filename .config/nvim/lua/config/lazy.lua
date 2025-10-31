-- Plugin setup with lazy.nvim (modern plugin manager)
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable",
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

-- Plugin specifications
local plugins = {
	-- Visual
	"RRethy/vim-illuminate",

	-- Linting and formatting
	{
		"dense-analysis/ale",
		config = function()
			vim.g.ale_fixers = {
				python = { "ruff", "ruff_format" },
				javascript = { "prettier", "eslint" },
				typescript = { "prettier", "eslint" },
				typescriptreact = { "prettier", "eslint" },
				javascriptreact = { "prettier", "eslint" },
				json = { "prettier", "trim_whitespace", "remove_trailing_lines" },
				lua = { "stylua", "trim_whitespace", "remove_trailing_lines" },
				make = { "trim_whitespace", "remove_trailing_lines" },
				html = { "remove_trailing_lines", "html-beautify", "trim_whitespace" },
				yaml = { "remove_trailing_lines", "trim_whitespace", "prettier" },
				sh = { "remove_trailing_lines", "trim_whitespace", "shfmt" },
				dosini = { "trim_whitespace", "remove_trailing_lines" },
			}

			vim.g.ale_linters = {
				python = { "ruff" },
				yaml = { "yamllint", "actionlint" },
				sh = { "bashate" },
			}

			vim.g.ale_python_ruff_options = "--config=/Users/guzeev/Work/visible-web/backend/ruff.toml"
			vim.g.ale_python_autoflake_options = "--remove-all-unused-imports --remove-unused-variables --in-place"
			vim.g.ale_python_mypy_options = "--show-error-codes --no-error-summary"
			vim.g.ale_fix_on_save = 1
			vim.g.ale_lint_on_text_changed = 'never'
			vim.g.ale_lint_on_insert_leave = 0
		end,
	},

	-- Search
	"mileszs/ack.vim",

	-- FZF
	{
		"junegunn/fzf",
		dir = "~/.fzf",
		build = "./install --all",
	},
	"junegunn/fzf.vim",
	"eugen0329/vim-esearch",

	-- Text manipulation
	"tpope/vim-abolish",
	"tpope/vim-eunuch",
	"tpope/vim-surround",

	-- Syntax highlighting
	"vim-python/python-syntax",
	"justinmk/vim-syntax-extra",
	"elzr/vim-json",
	"plasticboy/vim-markdown",
	"pboettch/vim-cmake-syntax",
	"evanleck/vim-svelte",
	"sheerun/vim-polyglot",
	"pineapplegiant/spaceduck",

	-- Recent files
	"yegappan/mru",

	-- Telescope
	{
		"nvim-telescope/telescope-fzf-native.nvim",
		build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release",
		config = function()
			require("telescope").setup({
				defaults = {
					file_ignore_patterns = {
						"%.lock",
						"node_modules/.*",
						"%.git/.*",
						"__pycache__/.*",
						"%.venv/.*",
						"venv/.*",
						"%.tox/.*",
						"%.pytest_cache/.*",
						"%.mypy_cache/.*",
						"htmlcov/.*",
						"%.egg%-info/.*",
						"dist/.*",
						"build/.*",
						"%.so$",
						"%.pyc$",
						"migrations/.*",
					},
					vimgrep_arguments = {
						"rg",
						"--color=never",
						"--no-heading",
						"--with-filename",
						"--line-number",
						"--column",
						"--smart-case",
						"--hidden",
						"--glob=!.git/",
						"--glob=!.venv/",
						"--glob=!node_modules/",
						"--glob=!__pycache__/",
					},
					initial_mode = "insert",
					sorting_strategy = "ascending",
					layout_config = {
						horizontal = {
							prompt_position = "top",
							preview_width = 0.55,
						},
					},
					cache_picker = {
						num_pickers = 10,
					},
				},
				pickers = {
					lsp_dynamic_workspace_symbols = {
						sorter = require("telescope").extensions.fzf.native_fzf_sorter(),
						show_line = false,
						entry_maker = function(entry)
							-- Get the default entry maker
							local make_entry = require("telescope.make_entry")
							local default_entry = make_entry.gen_from_lsp_symbols({})(entry)

							-- Filter out admin.py files
							if default_entry and default_entry.filename then
								if string.match(default_entry.filename, "admin%.py$") then
									return nil
								end
							end

							return default_entry
						end,
					},
					lsp_document_symbols = {
						sorter = require("telescope").extensions.fzf.native_fzf_sorter(),
						show_line = false,
					},
				},
			})
			require("telescope").load_extension("fzf")
		end,
	},

	"nvim-lua/plenary.nvim",

	{
		"nvim-telescope/telescope.nvim",
	},

	-- TypeScript
	"jose-elias-alvarez/typescript.nvim",

	-- LSP
	"neovim/nvim-lspconfig",

	-- Git
	"airblade/vim-gitgutter",
	"tpope/vim-fugitive",

	-- Snippets
	"honza/vim-snippets",

	-- Python
	"brentyi/isort.vim",

	-- Go
	"fatih/vim-go",

	-- Editor config
	"editorconfig/editorconfig-vim",

	-- Operators
	"kana/vim-operator-user",

	-- C/C++
	"rhysd/vim-clang-format",
	"ericcurtin/CurtineIncSw.vim",

	-- Comments
	"tomtom/tcomment_vim",

	-- Languages
	"tikhomirov/vim-glsl",
	"leafgarland/typescript-vim",
	"liuchengxu/graphviz.vim",
	"prabirshrestha/async.vim",
	"martinda/Jenkinsfile-vim-syntax",
	"modille/groovy.vim",
	"rust-analyzer/rust-analyzer",

	-- Utilities
	"sk1418/HowMuch",
	"vim-scripts/loremipsum",

	{
		"nvim-pack/nvim-spectre",
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
	},

	-- Multiple cursors
	"mg979/vim-visual-multi",

	-- completion
	{
		"hrsh7th/nvim-cmp",
		event = "InsertEnter",
		config = function()
			local cmp = require("cmp")
			local luasnip = require("luasnip")

			cmp.setup({
				snippet = {
					expand = function(args)
						luasnip.lsp_expand(args.body)
					end,
				},
				window = {
					completion = cmp.config.window.bordered(),
					documentation = cmp.config.window.bordered(),
				},
				completion = {
					autocomplete = {
						cmp.TriggerEvent.TextChanged,
						cmp.TriggerEvent.InsertEnter,
					},
					completeopt = "menu,menuone,noinsert",
				},
				mapping = cmp.mapping.preset.insert({
					["<C-b>"] = cmp.mapping.scroll_docs(-4),
					["<C-f>"] = cmp.mapping.scroll_docs(4),
					["<C-Space>"] = cmp.mapping.complete(),
					["<C-e>"] = cmp.mapping.abort(),
					["<CR>"] = cmp.mapping.confirm({ select = true }),
					["<Tab>"] = cmp.mapping(function(fallback)
						if cmp.visible() then
							cmp.select_next_item()
						elseif luasnip.expand_or_jumpable() then
							luasnip.expand_or_jump()
						else
							fallback()
						end
					end, { "i", "s" }),
					["<S-Tab>"] = cmp.mapping(function(fallback)
						if cmp.visible() then
							cmp.select_prev_item()
						elseif luasnip.jumpable(-1) then
							luasnip.jump(-1)
						else
							fallback()
						end
					end, { "i", "s" }),
				}),
				sources = cmp.config.sources({
					{ name = "nvim_lsp", priority = 1000 },
					{ name = "luasnip", priority = 750 },
					{ name = "path", priority = 500 },
					{ name = "buffer", priority = 250, keyword_length = 3 },
				}),
			})

			-- Filetype-specific settings for Python
			cmp.setup.filetype("python", {
				sources = cmp.config.sources({
					{ name = "nvim_lsp", priority = 1000 },
					{ name = "luasnip", priority = 750 },
					{ name = "path", priority = 500 },
					{ name = "buffer", priority = 250, keyword_length = 2 },
				}),
			})

			-- Filetype-specific settings for TypeScript/JavaScript/React
			cmp.setup.filetype({ "typescript", "typescriptreact", "javascript", "javascriptreact" }, {
				sources = cmp.config.sources({
					{ name = "nvim_lsp", priority = 1000 },
					{ name = "luasnip", priority = 750 },
					{ name = "path", priority = 500 },
					{ name = "buffer", priority = 250, keyword_length = 2 },
				}),
			})
		end,
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-path",
			"L3MON4D3/LuaSnip",
			"saadparwaiz1/cmp_luasnip",
		},
	},

	-- Status line
	"itchyny/lightline.vim",

	-- Color schemes
	-- 'xolox/vim-misc',
	-- 'xolox/vim-colorscheme-switcher',
	"fenetikm/falcon",
	"nanotech/jellybeans.vim",
	"tomasr/molokai",
	"ayu-theme/ayu-vim",
	"therubymug/vim-pyte",
	"vim-scripts/eclipse.vim",
	"vim-scripts/summerfruit256.vim",
	"vim-scripts/AutumnLeaf",
	"vim-scripts/ironman.vim",
	"NLKNguyen/papercolor-theme",

	-- Case conversion
	"chiedo/vim-case-convert",

	{
		"L3MON4D3/LuaSnip",
		version = "v2.*",
		build = "make install_jsregexp",
	},

	-- File explorer
	{
		"preservim/nerdtree",
		config = function()
			vim.g.NERDTreeIgnore = { "\\.pyc$" }
			vim.g.NERDTreeShowHidden = 1
			vim.g.NERDTreeShowLineNumbers = 1
			vim.g.NERDTreeWinSize = 35
			vim.g.NERDTreeQuitOnOpen = 0
			vim.g.NERDTreeAutoDeleteBuffer = 1
			vim.g.NERDTreeAutoRefreshOnWrite = 1
		end,
	},

	-- COC
	-- {
	-- 	"neoclide/coc.nvim",
	-- 	branch = "release",
	-- 	config = function()
	-- 		vim.g.coc_enable_locationlist = 0
	-- 		vim.g.coc_global_extensions = {
	-- 			"coc-css",
	-- 			"coc-json",
	-- 			"coc-pyright",
	-- 			"coc-tsserver",
	-- 			"coc-eslint",
	-- 			"coc-prettier",
	-- 		}
	-- 	end,
	-- },
}

require("lazy").setup(plugins)
