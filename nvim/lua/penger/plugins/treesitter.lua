return {
  "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
  event = "BufReadPost",
  config = function()
    require("nvim-treesitter.config").setup({
      highlight = { enable = true },
      indent = { enable = false },
    })
  end
}
