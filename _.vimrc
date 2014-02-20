
set runtimepath=~/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,~/.vim/after

" REQUIRED. This makes vim invoke Latex-Suite when you open a tex file.
filetype plugin on

" IMPORTANT: win32 users will need to have 'shellslash' set so that latex
" can be called correctly.
set shellslash

set nobackup

" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse Latex-Suite. Set your grep
" program to always generate a file-name.
set grepprg=grep\ -nH\ $*

" OPTIONAL: This enables automatic indentation as you type.
filetype indent on

" OPTIONAL: Starting with Vim 7, the filetype of empty .tex files defaults to
" 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" The following changes the default filetype back to 'tex':
let g:tex_flavor='latex'

let g:Tex_ViewRule_dvi = 'run evince'
"let g:Tex_ViewRule_pdf = 'run evince'

let Tex_DefaultTargetFormat = 'pdf'

set background=dark
"hi Normal guifg=white guibg=black

syntax enable

set modeline
set foldmethod=syntax
set nofen
set laststatus=2

set mouse=i
set mousemodel=popup

set nowrap

set spl=en_gb

set ai
set ts=4
set sw=4
set expandtab

au VimEnter * au FileType tex silent! unmap! <buffer> é
au VimEnter * au FileType tex silent! unmap! <buffer> ì
au VimEnter * au FileType tex silent! unmap! <buffer> â
" save file whenever cursor moves
function! Update_if_possible()
	if filewritable(bufname("%"))
		update
	endif
endfunction
"au FileType tex au CursorMoved * call Update_if_possible()
"au FileType tex au CursorMovedI * call Update_if_possible()

" Custom Mappings
nmap ZzZ :w !sudo tee % > /dev/null<CR><CR><CR>:q!<CR>
nmap wc :w !sed '/end{document}/,/$p/d; s/{.\+}/{}/' % \| detex \| wc -w<CR>

if has("autocmd")
	au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endif

" -- Word Count --

"returns the count of how many words are in the entire file excluding the current line
"updates the buffer variable Global_Word_Count to reflect this
fu! OtherLineWordCount()
    let data = []
    "get lines above and below current line unless current line is first or last
    if line(".") > 1
        let data = getline(1, line(".")-1)
    endif
    if line(".") < line("$")
        let data = data + getline(line(".")+1, "$")
    endif
    let count_words = 0
    let pattern = "\\<\\(\\w\\|-\\|'\\)\\+\\>"
    for str in data
        let count_words = count_words + NumPatternsInString(str, pattern)
    endfor
    let b:Global_Word_Count = count_words
    return count_words
endf    

"returns the word count for the current line
"updates the buffer variable Current_Line_Number
"updates the buffer variable Current_Line_Word_Count
fu! CurrentLineWordCount()
    if b:Current_Line_Number != line(".") "if the line number has changed then add old count
        let b:Global_Word_Count = b:Global_Word_Count + b:Current_Line_Word_Count
    endif
    "calculate number of words on current line
    let line = getline(".")
    let pattern = "\\<\\(\\w\\|-\\|'\\)\\+\\>"
    let count_words = NumPatternsInString(line, pattern)
    let b:Current_Line_Word_Count = count_words "update buffer variable with current line count
    if b:Current_Line_Number != line(".") "if the line number has changed then subtract current line count
        let b:Global_Word_Count = b:Global_Word_Count - b:Current_Line_Word_Count
    endif
    let b:Current_Line_Number = line(".") "update buffer variable with current line number
    return count_words
endf    

"returns the word count for the entire file using variables defined in other procedures
"this is the function that is called repeatedly and controls the other word
"count functions.
fu! WordCount()
    if exists("b:Global_Word_Count") == 0
        let b:Global_Word_Count = 0
        let b:Current_Line_Word_Count = 0
        let b:Current_Line_Number = line(".")
        call OtherLineWordCount()
    endif
    call CurrentLineWordCount()
    return printf("%d/%d", b:Current_Line_Word_Count, b:Global_Word_Count + b:Current_Line_Word_Count)
endf

"returns the number of patterns found in a string
fu! NumPatternsInString(str, pat)
    let i = 0
    let num = -1
    while i != -1
        let num = num + 1
        let i = matchend(a:str, a:pat, i)
    endwhile
    return num
endf

let g:tagbar_usearrows = 1
nnoremap <leader>l :TagbarToggle<CR>

nnoremap <silent> <LocalLeader><cr> :Validate<cr>
nnoremap <silent> <LocalLeader>c :JavaCorrect<cr>
" map <buffer> <LocalLeader>bc ^y$:r!echo 'scale=6; "'\|bc  "
map <buffer> <LocalLeader>bc ^y$:r!echo 'scale=6; <C-R>0'\|bc -l<cr>

set switchbuf=useopen,usetab,split

set tags=~/.tags
set complete=.,w,b,u,t,i

"noremap <M-W> <C-w><Up>
"noremap <M-S> <C-w><Down>
"noremap <M-A> <C-w><Left>
"noremap <M-D> <C-w><Right>

noremap <leader>o <Esc>:CommandT<CR>
noremap <leader>O <Esc>:CommandTFlush<CR>
noremap <leader>m <Esc>:CommandTBuffer<CR>

set statusline=%<\%f\ %y%m%r\ %{fugitive#statusline()}\ %=\ [%{WordCount()}]\ \ %l,%c%V\ \ \ \ \ \ \ \ \ \ %P
