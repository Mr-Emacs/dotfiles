require('telescope').setup {
    defaults = {
        prompt_prefix = "λ ",
        selection_caret = "> ",
        layout_strategy = "horizontal",
        layout_config = {
            horizontal = {
                preview_width = 0.3,
            },
            vertical = {
                width = 0.9,
                height = 0.9,
            },
        },
        file_sorter = require('telescope.sorters').get_fuzzy_file,
        color_devicons = true,
    },
    pickers = {
        oldfiles = {
            theme = "ivy",
            previewer = false,
            layout_config = {
                height = 0.4,
            },
        },
        find_files = {
            layout_strategy = "vertical",
            previewer = false,
        },
        live_grep = {
            theme = "ivy",
            previewer = true,
            layout_config = {
                height = 0.5,
            },
        },
        buffers = {
            theme = "ivy",
            previewer = true,
            layout_config = {
                height = 0.4,
            },
        },
    },
    extensions = {
    }
}
