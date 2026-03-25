return {
    {
        "folke/tokyonight.nvim",
        lazy = false,
        priority = 1000,
    },
    {
        "blazkowolf/gruber-darker.nvim",
        lazy = false,
        priority = 1000,
        config = function()
            if vim.g.neovide then
                vim.cmd("colorscheme tokyonight-night")
            else
                vim.cmd("colorscheme gruber-darker")
            end
            local function set_highlights()
                if vim.g.colors_name == "gruber-darker" then
                    vim.api.nvim_set_hl(0, "CursorLine", { bg = "#444444" })
                    vim.api.nvim_set_hl(0, "CursorLineNr", {
                        fg = "#ffdd33",
                        bold = true,
                    })
                end
            end
            set_highlights()
            vim.api.nvim_create_autocmd("ColorScheme", {
                callback = set_highlights,
            })
        end
    }
}
