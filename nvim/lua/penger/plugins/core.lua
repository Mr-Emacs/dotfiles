return {
    { "nvim-lua/plenary.nvim",    lazy = true },

    {
        "windwp/nvim-autopairs",
        event = "InsertEnter",
        config = true,
    },

    { "mbbill/undotree",          cmd = "UndotreeToggle" },
    { "axieax/urlview.nvim",      cmd = "UrlView" },
    { "esmuellert/codediff.nvim", cmd = "CodeDiff" },

    {
        "NeogitOrg/neogit",
        cmd = "Neogit",
        dependencies = { "nvim-lua/plenary.nvim" },
        config = true,
    },

    {
        "williamboman/mason.nvim",
        config = true,
    },

    {
        "williamboman/mason-lspconfig.nvim",
        dependencies = {
            "neovim/nvim-lspconfig",
        },

        opts = {
            automatic_enable = true,
        },
    },
    {
        'stevearc/oil.nvim',
        opts = {
            view_options = {
                show_hidden = true,
            },
        },
        dependencies = { { "nvim-mini/mini.icons", opts = {} } },
        lazy = false,
    }
}
