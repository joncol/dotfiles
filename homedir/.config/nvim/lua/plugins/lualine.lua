return {
  "nvim-lualine/lualine.nvim",
  dependencies = { "kyazdani42/nvim-web-devicons" },
  event = "VeryLazy",
  opts = {
    options = {
      theme = "nightfly",
      section_separators = { left = "", right = "" },
      component_separators = { left = "", right = "" }
    }
  }
}
