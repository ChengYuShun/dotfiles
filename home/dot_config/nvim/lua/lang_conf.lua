local cmp = require('cmp')
local kommentary_config = require('kommentary.config')
local nvim_lsp = require('lspconfig')

-- cmp Settings
cmp.setup {
  mapping = {
    ['<Tab>'] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
    ['<S-Tab>'] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
    ['<Down>'] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
    ['<Up>'] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<Right>'] = cmp.mapping.close(),
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'buffer' }
  }
}

-- kommentary Settings
kommentary_config.configure_language('c', {
  single_line_comment_string = false,
})

-- LSP Settings
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap('n', '<space>f', '<cmd>lua vim.lsp.buf.format { async = true }<CR>', opts)
end

nvim_lsp['bashls'].setup {
  on_attach = on_attach,
}

nvim_lsp['ccls'].setup {
  on_attach = on_attach,
  root_dir = nvim_lsp.util.root_pattern('Makefile', '*.c', '*.h', 'include'),
}

nvim_lsp['gopls'].setup {
  on_attach = on_attach,
  root_dir = nvim_lsp.util.root_pattern('go.mod', '.git')
}

--[[ nvim_lsp['jedi_language_server'].setup {
  on_attach = on_attach,
  root_dir = nvim_lsp.util.root_pattern('setup.py', 'setup.cfg', '__init__.py'),
} ]]

nvim_lsp['pylsp'].setup {
  on_attach = on_attach,
  root_dir = nvim_lsp.util.root_pattern('setup.py', 'setup.cfg', '__init__.py'),
}

--[[ nvim_lsp['pyright'].setup {
  on_attach = on_attach,
  root_dir = nvim_lsp.util.root_pattern('setup.py', 'setup.cfg', '__init__.py'),
} ]]

nvim_lsp['rust_analyzer'].setup {
  on_attach = on_attach,
}

-- Language Specified
vim.cmd('filetype on')
-- default
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
-- linux kernal
vim.cmd('autocmd FileType c,mix set tabstop=8 shiftwidth=8')
vim.cmd('autocmd BufNewFile,BufRead *.h set filetype=c')
-- shell
vim.cmd('autocmd FileType sh,lua set expandtab shiftwidth=2')
vim.cmd('autocmd BufNewFile,BufRead PKGBUILD set filetype=sh syntax=PKGBUILD')
-- lisp
vim.cmd('autocmd FileType lisp set expandtab')
