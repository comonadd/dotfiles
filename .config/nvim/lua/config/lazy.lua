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
			}

			vim.g.ale_linters = {
				python = { "ruff", "mypy" },
				yaml = { "yamllint", "actionlint" },
			}

			vim.g.ale_python_ruff_options = "--config=/Users/guzeev/Work/visible-web/backend/ruff.toml"
			vim.g.ale_python_autoflake_options = "--remove-all-unused-imports --remove-unused-variables --in-place"
			vim.g.ale_python_mypy_options = "--show-error-codes --no-error-summary --verbose"
			vim.g.ale_fix_on_save = 1
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
		setup = function()
			cmp = require("cmp")

			cmp.setup({
				snippet = {
					expand = function(args)
						vim.fn["vsnip#anonymous"](args.body)
					end,
				},
				window = {
					-- completion = cmp.config.window.bordered(),
					-- documentation = cmp.config.window.bordered(),
				},
				mapping = cmp.mapping.preset.insert({
					["<C-b>"] = cmp.mapping.scroll_docs(-4),
					["<C-f>"] = cmp.mapping.scroll_docs(4),
					["<C-Space>"] = cmp.mapping.complete(),
					["<C-e>"] = cmp.mapping.abort(),
					["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
				}),
				sources = cmp.config.sources({
					{ name = "nvim_lsp" },
					{ name = "vsnip" }, -- For vsnip users.
					-- { name = 'luasnip' }, -- For luasnip users.
					-- { name = 'ultisnips' }, -- For ultisnips users.
					-- { name = 'snippy' }, -- For snippy users.
				}, {
					{ name = "buffer" },
				}),
			})
		end,
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-path",
			-- Add a snippet engine like:
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
	{
		"neoclide/coc.nvim",
		branch = "release",
		config = function()
			vim.g.coc_enable_locationlist = 0
			vim.g.coc_global_extensions = {
				"coc-css",
				"coc-json",
				"coc-pyright",
				"coc-tsserver",
				"coc-eslint",
				"coc-prettier",
			}
		end,
	},
}

require("lazy").setup(plugins)
