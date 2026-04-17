return {
  "stevearc/oil.nvim",
  dependencies = {},
  lazy = false,
  config = function()
    require("oil").setup({
      default_file_explorer = true,
      skip_confirm_for_simple_edits = true,
      view_options = {
        show_hidden = true,
      },
    })
    vim.keymap.set("n", "<leader>o", "<CMD>Oil<CR>", { desc = "Open Oil file explorer" })
  end
}
