return {
    "vague-theme/vague.nvim",
    config = function()
        vim.cmd.colorscheme('vague')
        vim.api.nvim_set_hl(0, "Normal", { bg = "NONE", ctermbg = "NONE" })
        vim.api.nvim_set_hl(0, "NormalNC", { bg = "NONE", ctermbg = "NONE" })
    end
}
