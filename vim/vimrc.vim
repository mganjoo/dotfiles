" Vim configuration.

" == Prologue == {{{1
scriptencoding utf-8      " Enable UTF-8 chars in listchars
set nocompatible          " Disable compatibility mode

let mapleader = ' '       " Change leader key
let maplocalleader = ','  " Change local leader key

" == Plugins == {{{1
call plug#begin("~/.vim/external")

" Motion enhancements
Plug 'Raimondi/delimitMate'
Plug 'bkad/CamelCaseMotion'
Plug 'junegunn/vim-easy-align'
Plug 'qpkorr/vim-bufkill'
Plug 'tommcdo/vim-exchange'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'justinmk/vim-sneak'
Plug 'regedarek/ZoomWin'
Plug 'tpope/vim-repeat'

" UI enhancements
Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'f-person/auto-dark-mode.nvim'
Plug 'airblade/vim-gitgutter'
Plug 'milkypostman/vim-togglelist'
Plug 'edkolev/tmuxline.vim'

" External program interaction
Plug 'tpope/vim-eunuch'
Plug 'benekastah/neomake'
Plug 'tpope/vim-fugitive'

" Search and replace
Plug '~/.external/fzf'
Plug 'junegunn/fzf.vim'
Plug 'benjifisher/matchit.zip'
Plug 'nelstrom/vim-visual-star-search'
Plug 'tpope/vim-abolish'

" Languages
Plug 'derekwyatt/vim-scala', { 'for': 'scala' }
Plug 'hynek/vim-python-pep8-indent', { 'for': 'python' }
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
Plug 'solarnz/thrift.vim', { 'for': 'thrift' }
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
Plug 'mustache/vim-mustache-handlebars'
Plug 'wellle/tmux-complete.vim'
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

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

" == Theme == {{{1
autocmd vimenter * ++nested colorscheme gruvbox

" == Enhancements == {{{1
" Use ag instead of grep, if available
if executable("ag")
  set grepprg=ag\ --nogroup\ --nocolor
endif

" == Filetype-specific settings == {{{1
au BufRead,BufNewFile *.mesos set filetype=python
au BufRead,BufNewFile *.aurora set filetype=python
au BufRead,BufNewFile BUILD setlocal filetype=pants

" == Plugin Settings == {{{1

" Use ag for searching, if available; otherwise fall back to find
if executable("ag")
  let s:search_command = 'ag %s -i --nocolor --nogroup --hidden
      \ --ignore .git
      \ --ignore .svn
      \ --ignore .hg
      \ --ignore .pants.d
      \ -g ""'
else
  let s:search_command = 'find %s -type f'
endif

" == delimitMate == {{{2
let g:delimitMate_expand_cr = 1               " Create line break on enter
let g:delimitMate_expand_space = 1            " Expand spaces inside delimiters
let g:delimitMate_nesting_quotes = ['"', '`'] " Allows for triple quotes

" == vim-signify == {{{2
let g:signify_vcs_list = [ 'git', 'hg' ]
let g:signify_sign_overwrite = 1

" == UltiSnips == {{{2
let g:UltiSnipsSnippetDirectories=["UltiSnips", "MySnips"]

" == neomake == {{{2
let g:neomake_python_enabled_makers = ['flake8']
au BufWritePost *.py Neomake

" == markdown == {{{2
let g:vim_markdown_folding_disabled = 1

" == python (neovim) == {{{2
let b:brew_prefix = substitute(system("brew --prefix"), '\n\+$', '', '')
let g:python3_host_prog = b:brew_prefix . "/bin/python3"

" == tmux-complete.vim == {{{2
let g:tmuxcomplete#trigger = ''

" == vimtex == {{{2
let g:vimtex_view_general_viewer = 'displayline'
let g:vimtex_view_general_options = '@line @pdf @tex'


" == tmuxline.vim == {{{2
let g:tmuxline_powerline_separators = 0
let g:tmuxline_preset = {
  \'a': '#S',
  \'win': ['#I', '#W'],
  \'cwin': ['#I', '#W'],
  \'x': '#I.#P',
  \'y': ['%a', '%Y-%m-%d', '%R']}

" == localvimrc == {{{2
let g:localvimrc_persistent=2

" == vim-sneak == {{{2
let g:sneak#label = 1

" == vimwiki == {{{2
let b:personal_wiki = { 'path': '~/Dropbox/wiki/personal' }
let g:vimwiki_list = [b:personal_wiki]
au BufRead,BufNewFile *.wiki setlocal textwidth=80

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

" == Bindings to show panels == {{{2
nnoremap <silent> <Leader>t :TagbarToggle<CR>

" == Bindings to enable/disable plugins == {{{2
nnoremap co. :DelimitMateSwitch<CR>

" == vim-signify bindings == {{{2
let g:signify_mapping_next_hunk = '<leader>cj'
let g:signify_mapping_prev_hunk = '<leader>ck'
let g:signify_mapping_toggle = '<leader>ct'
let g:signify_mapping_toggle_highlight = '<leader>ch'

" == fugitive == {{{2
nnoremap <leader>gw :Gwrite<CR>
nnoremap <leader>gr :Gread<CR>
nnoremap <leader>gm :Gmove<Space>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>gb :Gblame<CR>

" == UltiSnips == {{{2
let g:UltiSnipsExpandTrigger="<c-e>"
let g:UltiSnipsJumpForwardTrigger="<c-e>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

" == Dash == {{{2
nmap <silent> gK <Plug>DashSearch


" == vim-easy-align == {{{2
vmap <Enter> <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" == CamelCaseMotion == {{{2
call camelcasemotion#CreateMotionMappings('<leader>')

" == FZF == {{{2
call camelcasemotion#CreateMotionMappings('<leader>')
nnoremap <silent> <leader><Space> :Files<CR>

" }}}1
" == Modeline == {{{1
" vim: foldmethod=marker:fen
