" Side Windows
" ,aa " outline
" ,tr " Tree Current File
" ,tt " Tree Toggle
" ,ee buffer explorer

" Commenting
" ,cc " Comment
" ,cu " Uncomment

" Ctrl-P Fuzzy Finder
" <C-p> " open fuzzy file finder
" ,p paste from register

" EasyMotion
" ,,<motion> to use easy motion.
"
" Snippets
" <C-j> " expand snippet
" <C-j> " jump to next location
" <C-k> " jump to previos location

" Coercion
" cr<case> change case
" * 's' snake_case
" * 'm' MixedCase
" * 'u' UPPER_CASE
" * 'c' camelCase
" * '-' kebab-case
" * '.' dot.case
" * ' ' space case
" * 't' Title Case


let g:OmniSharp_server_use_net6 = 1

let g:ale_linters = {
\  'cs': ['OmniSharp']
\}

set nocompatible              " be iMproved, required
let mapleader = ','
filetype plugin indent on

func! VimrcCallback()
endfunc

func! AddPlugValues()
endfunction



if filereadable(expand('~/.vimrc_work'))
  source ~/.vimrc_work
endif

if filereadable(expand('~/.vimrc_home'))
  source ~/.vimrc_home
endif

set nu  " show line numbers
set so=5  " scroll offset
set ignorecase  " make search case insensitive
set tabstop=2
filetype off                  " required

au FocusGained,BufEnter,CursorHold * checktime

call plug#begin('~/vim.plugged')

Plug 'nvim-tree/nvim-web-devicons' " optional
Plug 'nvim-tree/nvim-tree.lua'

Plug 'scrooloose/nerdcommenter'

Plug 'ctrlpvim/ctrlp.vim'

Plug 'bling/vim-airline'

Plug 'gcmt/taboo.vim'

Plug 'SirVer/ultisnips'

Plug 'honza/vim-snippets'

Plug 'tpope/vim-surround'

Plug 'easymotion/vim-easymotion'

Plug 'tpope/vim-abolish'

Plug 'christoomey/vim-tmux-navigator'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'skywind3000/asyncrun.vim'

Plug 'jlanzarotta/bufexplorer'

Plug 'tmux-plugins/vim-tmux'

Plug 'dense-analysis/ale'

Plug 'OmniSharp/omnisharp-vim'

Plug 'github/copilot.vim'

Plug 'neovim/nvim-lspconfig'

Plug 'stevearc/aerial.nvim'

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

Plug 'sainnhe/gruvbox-material'


" Do I need this?
Plug 'tpope/vim-obsession'


call AddPlugValues()

"if has('nvim')
  "Plug 'raghur/vim-ghost', {'do': ':GhostInstall'}
"endif

call plug#end()            " required
filetype plugin indent on    " required

set tabstop=2
set shiftwidth=2
set expandtab

au BufRead,BufNewFile \.bash_*  set filetype=sh

syntax on
set noruler
set laststatus=2

luafile ~/.config/nvim/luascripts/lsp.lua
luafile ~/.config/nvim/luascripts/omnisharplsp.lua
luafile ~/.config/nvim/luascripts/treesitter.lua

nnoremap <leader>ee :BufExplorer<CR>

" NvimTree settings
"
luafile ~/.config/nvim/luascripts/nvimtree.lua

" NERDTree remappings
nnoremap <leader>tr :NvimTreeFindFile<CR>
nnoremap <leader>tt :NvimTreeToggle<CR>

" paste remapping
nnoremap <leader>p "*p

" Taboo commands
nnoremap <leader>rr :TabooRename
nnoremap <leader>rt :TabooOpen

noremap <leader>so :so $MYVIMRC <CR>

" filetype plugin indent o
hi StatusLine ctermbg=3 ctermfg=4
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

nnoremap <leader>en :lnext<CR>
nnoremap <leader>ep :lprevious<CR>

" gutter shortcuts
nnoremap <leader>gut :GitGutterLineHighlightsToggle<cr>

" CtrlP remaps
"nnoremap <c-l> :CtrlPBuffer<cr>
let g:ctrlp_max_files = 50000
let g:ctrlp_max_depth = 10
"let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --hidden
      "\ --ignore .git
      "\ --ignore .svn
      "\ --ignore .hg
      "\ --ignore .DS_Store
      "\ --ignore "**/*.pyc"
      "\ --ignore .git5_specs
      "\ --ignore review
      "\ -g ""'
"let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }


" remap ultisnips
let g:UltiSnipsExpandTrigger = "<c-j>"
let g:UltiSnipsJumpForwardTrigger = "<c-j>"
let g:UltiSnipsJumpBackwardTrigger = "<c-k>"

imap jk <Esc>

nnoremap <leader><leader>o :Files<cr>
nnoremap <leader><leader>t :BTags<cr>
nnoremap <leader><leader>l :Lines<cr>

syntax enable
if !has('nvim')
  set term=screen-256color
  set t_Co=256
endif
set background=dark

nnoremap <leader>gf :call fzf#run({
      \ 'source': 'source ~/scripts/editedfiles.sh && __editedFiles',
      \ 'sink': 'e',
      \ 'down' : '30%'}) <cr>


nnoremap <leader>rb :call fzf#run({
      \ 'source': 'source ~/scripts/targets_fast.sh && _get_targets_fast',
      \ 'sink' : 'AsyncRun blaze build',
      \ 'down' : '30%' }) <cr>

call VimrcCallback()

  nnoremap <leader>cs :CSearch<Space>

function! Wipeout()
    "From tabpagebuflist() help, get a list of all buffers in all tabs
    let tablist = []
    for i in range(tabpagenr('$'))
        call extend(tablist, tabpagebuflist(i + 1))
    endfor

    "Below originally inspired by Hara Krishna Dara and Keith Roberts
    "http://tech.groups.yahoo.com/group/vim/message/56425
    let nWipeouts = 0
    for i in range(1, bufnr('$'))
        if bufexists(i) && !getbufvar(i,"&mod") && index(tablist, i) == -1
        "bufno exists AND isn't modified AND isn't in the list of buffers open in windows and abs
            silent exec 'bwipeout' i
            let nWipeouts = nWipeouts + 1
        endif
    endfor
    echomsg nWipeouts . ' buffer(s) wiped out'
endfunction

nnoremap <leader>wo :call Wipeout()<cr>

nnoremap <leader>qa :qa<CR>



set diffopt=filler,vertical,context:10000000
set cursorline

if &diff
  set cursorline
  nnoremap <leader>dn ]c
  nnoremap <leader>dp [c
endif

" Configure colorscheme
if has('termguicolors')
  set termguicolors
endif

" can be light or dark
set background=dark

" Set contrast.
" Available values: 'hard', 'medium'(default), 'soft'
let g:gruvbox_material_background = 'hard'
let g:gruvbox_material_foreground = 'original'
" For better performance
let g:gruvbox_material_better_performance = 1
let g:gruvbox_material_enable_italic = 1
let g:gruvbox_material_enable_bold = 1
colorscheme gruvbox-material

