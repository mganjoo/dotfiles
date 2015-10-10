" Vim configuration.

" == Prologue == {{{1
scriptencoding utf-8      " Enable UTF-8 chars in listchars
set nocompatible          " Disable compatibility mode

if filereadable(expand("~/.vimrc.before"))
  source ~/.vimrc.before
endif

let mapleader = ' '       " Change leader key
let maplocalleader = ','  " Change local leader key

" == Pathogen == {{{1
runtime bundle/vim-pathogen/autoload/pathogen.vim
execute pathogen#infect()

" == Syntax & Indentation == {{{1
syntax on                  " Syntax highlighting
filetype plugin indent on  " Enable file-specific indentation and other settings
set sw=2 sts=2 ts=2        " Tab and shift sizes
set expandtab              " Expand tab to spaces
set shiftround             " Round indent to multiple of shiftwidth
set smarttab               " Expand tabs to spaces in front of line
set copyindent             " Copy indent structure from previous line
set autoindent             " Copy actual indent from previous line

" == General Settings == {{{1
set autoread             " Reload files changed outside vim
set autowrite            " Save before :next, :make etc
set bs=indent,eol,start  " Allow backspace in insert mode
set cursorline           " Show line cursor is on
set encoding=utf-8       " Ensure correct encoding
set hidden               " For correct behavior of buffers
set history=5000         " Store lots of :cmdline history
set number               " Always show line numbers
set relativenumber       " Use relative numbers by default
set showcmd              " Show incomplete cmds down the bottom
set showmode             " Show current mode down the bottom
set visualbell           " No sounds
set shortmess+=I         " Disable startup message
set mouse=a              " Enable mouse mode
set timeoutlen=600       " Shorten the timeout for multi-key commands
set clipboard=unnamedplus " Sync clipboard

" == Search Settings {{{1
set incsearch   " Find the next match as we type the search
set hlsearch    " Highlight searches by default
set ignorecase  " Case insensitive searches
set smartcase   " Ignore sensitivity setting with uppercase patterns

" == Backup == {{{1
set backup                     " Create backups
set writebackup                " Create backup before writing
set backupdir=~/.vim-backup//  " Change backup dir
set directory=~/.vim-swap//    " Change swap directory

" == Folds == {{{1
set foldmethod=indent  " Fold based on indent
set foldnestmax=4      " Set conservative fold limit
set nofoldenable       " Don't fold by default
set foldcolumn=1       " Show a single fold column

" == Completion == {{{1
set omnifunc=syntaxcomplete#Complete " Function for omni-completion
set wildmode=list:longest            " List all matches; complete till longest string
set wildmenu                         " Show a menu of completion options

" == Appearance == {{{1
set nowrap      " Don't wrap lines
set linebreak   " Break lines at useful points
set list        " Show auxiliary characters
" Note: 'scriptencoding utf-8' must be called for this to work (see top)
set listchars=tab:▸\ ,trail:·
set colorcolumn=80 " Show column at 80 characters
set laststatus=2   " Always show the status line
set background=dark
set t_Co=256
let base16colorspace=256 " Access colors present in 256 colorspace
silent! colorscheme base16-tomorrow

" == Enhancements == {{{1
" Use ag instead of grep, if available
if executable("ag")
  set grepprg=ag\ --nogroup\ --nocolor
endif

" == Additional settings == {{{1
for fpath in split(globpath('~/.vim/settings', '*.vim'), '\n')
  exe 'source' fpath
endfor

" == Modeline == {{{1
" vim: foldmethod=marker:fen
