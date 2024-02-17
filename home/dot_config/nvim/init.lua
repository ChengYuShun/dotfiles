-- Basic Setup
require('plugins')
-- require('lang_conf')

-- IM
-- im = 'xkb:us::eng'

-- function enter_insert()
--   local handle = io.popen('ibus engine '..im)
--   handle:read('*a')
--   handle:close()
-- end

-- function exit_insert()
--   local handle = io.popen('ibus engine')
--   im = handle:read('*a')
--   handle:close()

--   handle = io.popen('ibus engine xkb:us::eng')
--   handle:read('*a')
--   handle:close()
-- end

-- vim.cmd('nnoremap i :lua enter_insert()<CR>i')
-- vim.cmd('nnoremap a :lua enter_insert()<CR>a')
-- vim.cmd('nnoremap o :lua enter_insert()<CR>o')
-- vim.cmd('nnoremap I :lua enter_insert()<CR>I')
-- vim.cmd('nnoremap A :lua enter_insert()<CR>A')
-- vim.cmd('nnoremap O :lua enter_insert()<CR>O')
-- vim.cmd('inoremap <ESC> <ESC>:lua exit_insert()<CR>')

-- Control
vim.opt.backupcopy = 'yes'
vim.cmd('cnoreabbrev E e')
vim.cmd('cnoreabbrev W w')
vim.cmd('cnoreabbrev vsp vsp <bar> wincmd l')

-- Appearance
-- vim.cmd('colorscheme Tomorrow-Night-Bright')
vim.opt.guifont = 'monospace:h19'

vim.opt.relativenumber = true
vim.opt.number = true
vim.opt.colorcolumn = '80'

vim.api.nvim_set_keymap('n', '<ESC>', ':nohlsearch<CR>:echo \'\'<CR>', { silent = true })
