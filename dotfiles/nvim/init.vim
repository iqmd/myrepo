set nocompatible " turn off vi-compatible features/bindings
set tabstop=4 " number of visual spaces per tab
set softtabstop=4 " number of spaces in tab when editing
set shiftwidth=4 " number of spaces to use for each step of (auto)indent
set autoindent " copy indent from current line when starting a new line
set expandtab " transform tabs to spaces
set number " show line numbers
set relativenumber " show line numbers relative to current position
set showcmd " show command in bottom bar
filetype indent on " load filetype-specific indent files
set wildmenu " visual autocomplete for command menu
set showmatch " highlight matching/closing brackets [{()}]
set incsearch " search as characters are entered
set hlsearch " highlight matches, turn off with nohlsearch
set formatoptions=tq " default is tcq, remove c to stop auto-insert of hashes when inserting comments
set listchars=tab:>>,trail:. " show tabs and spaces
set termguicolors
syntax enable
colorscheme gruvbox

highlight Normal ctermbg=none " turn off background colours from theme
highlight NonText ctermbg=none " turn off background colours from theme

call plug#begin('~/.config/nvim/plugged')
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
let g:deoplete#enable_at_startup = 1
Plug 'deoplete-plugins/deoplete-jedi'
Plug 'vim-airline/vim-airline'
Plug 'jiangmiao/auto-pairs'
Plug 'neomake/neomake'
let g:neomake_python_enabled_makers = ['flake8']
call plug#end()
call neomake#configure#automake('nrwi')
