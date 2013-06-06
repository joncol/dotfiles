if exists("did_load_filetypes")
    finish
else
    augroup filetypedetect
    au! BufRead,BufNewFile *.m setfiletype objc
    augroup END
endif
      
