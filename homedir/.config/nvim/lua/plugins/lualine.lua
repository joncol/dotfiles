return {
  "nvim-lualine/lualine.nvim",
  dependencies = { "kyazdani42/nvim-web-devicons" },
  event = "VeryLazy",
  opts = {
    options = {
      theme = "nightfly",
      section_separators = { left = "", right = "" },
      component_separators = "|",
    },
    sections = {
      lualine_a = { "mode" },
      lualine_b = {
        "branch",
        require("github-notifications").statusline_notification_count,
      },
      lualine_c = { "filename" },
      lualine_x = { "encoding", "fileformat", "filetype" },
      lualine_y = { "progress" },
      lualine_z = { "location" },
    },
  },
}
