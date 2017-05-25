" For GUI, use solarized-light
set background=light
silent! colorscheme solarized
let g:lightline = { 'colorscheme': 'solarized' }

set guioptions-=rL         " Remove left and right scrollbars
set guioptions-=T          " Remove toolbar
set clipboard=unnamed      " Clipboard sync
set guifont=Fira\ Mono:h15 " Font
set shell=/usr/bin/env\ bash
