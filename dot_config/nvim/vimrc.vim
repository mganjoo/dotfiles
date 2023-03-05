" Vim configuration.

" == Prologue == {{{1
scriptencoding utf-8      " Enable UTF-8 chars in listchars
set nocompatible          " Disable compatibility mode

let mapleader = ' '       " Change leader key
let maplocalleader = ','  " Change local leader key

" == Plugins == {{{1

let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()

" Motion enhancements
Plug 'Raimondi/delimitMate'
Plug 'qpkorr/vim-bufkill'
Plug 'tommcdo/vim-exchange'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'justinmk/vim-sneak'
Plug 'regedarek/ZoomWin'
Plug 'tpope/vim-repeat'

" UI enhancements
Plug 'projekt0n/github-nvim-theme', { 'tag': 'v0.0.7' }
Plug 'vim-airline/vim-airline'
Plug 'f-person/auto-dark-mode.nvim'
Plug 'airblade/vim-gitgutter'
Plug 'milkypostman/vim-togglelist'
Plug 'edkolev/tmuxline.vim'

" External program interaction
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'

" Search and replace
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'benjifisher/matchit.zip'
Plug 'nelstrom/vim-visual-star-search'

call plug#end()

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
set clipboard=unnamed    " Sync clipboard
set modeline=5           " Check for modelines in files

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
set nowrap         " Don't wrap lines
set linebreak      " Break lines at useful points
set list           " Show auxiliary characters
set colorcolumn=80 " Show column at 80 characters
set laststatus=2   " Always show the status line
" Note: 'scriptencoding utf-8' must be called for this to work (see top)
set listchars=tab:▸\ ,trail:·

" == Enhancements == {{{1
" Use rg instead of grep, if available
if executable("rg")
  set grepprg=rg\ --vimgrep
endif

" == Plugin Settings == {{{1

" == delimitMate == {{{2
let g:delimitMate_expand_cr = 1               " Create line break on enter
let g:delimitMate_expand_space = 1            " Expand spaces inside delimiters
let g:delimitMate_nesting_quotes = ['"', '`'] " Allows for triple quotes

" == python (neovim) == {{{2
let b:brew_prefix = substitute(system("brew --prefix"), '\n\+$', '', '')
let g:python3_host_prog = b:brew_prefix . "/bin/python3"

" == tmuxline.vim == {{{2
let g:tmuxline_powerline_separators = 0
let g:tmuxline_preset = {
  \'a': '#S',
  \'win': ['#I', '#W'],
  \'cwin': ['#I', '#W'],
  \'x': '#I.#P',
  \'y': ['%a', '%Y-%m-%d', '%R']}

" == vim-sneak == {{{2
let g:sneak#label = 1

" }}}1
" == Keymaps == {{{1

" == Tab manipulation == {{{2
nnoremap <silent> [W :tabfirst<CR>
nnoremap <silent> ]W :tabnext<CR>
nnoremap <silent> [w :tabprevious<CR>
nnoremap <silent> ]w :tabnext<CR>
nnoremap <silent> <Leader>wx :tabclose<CR>

" == Search shortcuts == {{{2
" Mute highlighting temporarily
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>
" Show number of matches made by a recent search
nnoremap <silent> <Leader>s :%s///gn<CR>

" == Paste toggle == {{{2
set pastetoggle=<F6>

" == Bindings to source vim files == {{{2
nnoremap <silent> <Leader>vv :source ~/.vimrc<CR>

" == Bindings to enable/disable plugins == {{{2
nnoremap co. :DelimitMateSwitch<CR>

" == fugitive == {{{2
nnoremap <leader>gw :Gwrite<CR>
nnoremap <leader>gr :Gread<CR>
nnoremap <leader>gm :Gmove<Space>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>gb :Gblame<CR>

" == FZF == {{{2
nnoremap <silent> <leader><Space> :Files<CR>

" }}}1
" == Modeline == {{{1
" vim: foldmethod=marker:fen
