if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

syntax on
filetype plugin indent on
set mouse=
set expandtab
set makeprg=scan-build-3.8\ make
set backupcopy=yes

set backupdir=~/.vim/backup//
set directory=~/.vim/swap//
set undodir=~/.vim/undo//
set nohlsearch

autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = -25
let g:netrw_wiw = -25
let g:netrw_list_hide = '.*\.s\?o\*\?,\.git.*$,.depend$'
if isdirectory(argv(0))
  bd
  autocmd vimenter * exe "cd" argv(0)
  augroup ProjectDrawer
    autocmd!
    autocmd VimEnter * :Lexplore | wincmd p
    autocmd FileType netrw nnoremap <buffer> q :bd<CR>
    "autocmd VimEnter * :NERDTree
  augroup END
endif

set tags=./.git/tags
function! DelTagOfFile(file)
  let fullpath = a:file
  let cwd = getcwd()
  let tagfilename = cwd . "/.git/tags"
  let f = substitute(fullpath, cwd . "/", "", "")
  let f = escape(f, './')
  let cmd = 'sed -i "/' . f . '/d" "' . tagfilename . '"'
  let resp = system(cmd)
endfunction

function! UpdateTags()
  let f = expand("%:p")
  let cwd = getcwd()
  let tagfilename = cwd . "/.git/tags"
  let cmd = 'ctags -a -f ' . tagfilename . ' --c++-kinds=+p --fields=+iaS --extra=+fq ' . '"' . f . '"'
  call DelTagOfFile(f)
  let resp = system(cmd)
endfunction
autocmd BufWritePost *.cpp,*.h,*.c call UpdateTags()

if has("cscope")
    set csprg=/usr/bin/cscope
    set csto=0
    set cst
    set csverb

    " Automatically make cscope connections
    function! LoadCscope()
        let db = findfile("cscope.out", ".;")
        if (!empty(db))
            let path = strpart(db, 0, match(db, "/cscope.out$"))
            set nocscopeverbose " suppress 'duplicate connection' error
            exe "cs add " . db . " " . path
            set cscopeverbose
        endif
    endfunction
    au BufEnter /* call LoadCscope()
endif

autocmd FileType rust set makeprg=cargo\ build

call plug#begin()
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif
let g:deoplete#enable_at_startup = 0
if 0
    Plug 'Rip-Rip/clang_complete'
    let g:clang_library_path = "/usr/lib/llvm-4.0/lib/libclang-4.0.so.1"
    Plug 'tweekmonster/deoplete-clang2'
    let g:deoplete#sources#clang#libclang_path = "/usr/lib/llvm-4.0/lib/libclang.so.1"
    let g:deoplete#sources#clang#clang_header = "/usr/lib/llvm-4.0/lib/clang/"
    let g:deoplete#sources#clang#flags = ["-I", "libwebsockets/include", "-I", "json-c"]
endif
Plug 'Shougo/neoinclude.vim'
Plug 'Shougo/deoplete-clangx'
"Plug 'Shougo/echodoc.vim'
let g:echodoc#enable_at_startup = 1
set cmdheight=2
Plug 'rust-lang/rust.vim'
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
"Plug 'w0rp/ale'
let g:ale_cpp_cppcheck_options = "--inline-suppr --enable=style"
"Plug 'tpope/vim-fugitive'
"Plug 'scrooloose/nerdtree'
"Plug 'xuyuanp/nerdtree-git-plugin'
Plug 'purescript-contrib/purescript-vim'
Plug 'tpope/vim-abolish'
call plug#end()

call deoplete#custom#var('clangx', 'clang_binary', '/usr/bin/clang')
call deoplete#custom#var('clangx', 'default_c_options', '')
call deoplete#custom#var('clangx', 'default_cpp_options', '')

if executable('rls')
    au User lsp_setup call lsp#register_server({
            \ 'name': 'rls',
            \ 'cmd': {server_info->['rustup', 'run', 'nightly', 'rls']},
            \ 'whitelist': ['rust'],
            \ })
endif
