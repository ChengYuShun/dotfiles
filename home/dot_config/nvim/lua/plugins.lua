-- Download if needed
local github_url = 'https://github.com'

local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system{'git', 'clone', '--depth', '1', github_url..'/wbthomason/packer.nvim', install_path}
end

local packer = require('packer')

local use = packer.use

-- Config
packer.init{
  git = {
    clone_timeout = 180,
    default_url_format = github_url..'/%s',
  },
}
packer.reset()

use{'wbthomason/packer.nvim', as = 'packer.nvim'}

-- Themes
use{'eFyhDv6dLgi9TWN/vim-tomorrow-theme', as = 'tomorrow-theme.vim'}

-- Language
use{'neovim/nvim-lspconfig', as = 'lspconfig.nvim'}
use{'hrsh7th/cmp-nvim-lsp', as = 'cmp-lsp.nvim'}
use{'hrsh7th/cmp-buffer', as = 'cmp-buffer.nvim'}
use{"hrsh7th/nvim-cmp", as = 'cmp.nvim'}
use{'b3nj5m1n/kommentary', as = 'kommentary.nvim'}
-- python
use{'Vimjas/vim-python-pep8-indent', as = 'python-pep8-indent.vim'}
-- fish
use{'dag/vim-fish', as = 'fish.vim'}

-- Automatic setup
if packer_bootstrap then
  packer.sync()
end

-- vim: syntax=lua
