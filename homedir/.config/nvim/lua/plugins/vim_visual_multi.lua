return {
  "mg979/vim-visual-multi",

  init = function()
    vim.cmd([[
      let g:VM_leader = "\\"
      let g:VM_show_warnings = 0
      let g:VM_theme = "iceblue"

      let g:VM_maps = {}
      let g:VM_maps["Find Under"] = "<A-n>"
      let g:VM_maps["Find Subword Under"] = "<A-n>"
      let g:VM_maps["Undo"] = "u"
      let g:VM_maps["Redo"] = "<C-r>"
    ]])
  end,
}
