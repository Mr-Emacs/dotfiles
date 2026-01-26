#!/usr/bin/env bash
INSTALL_DIR="$HOME/.local/share/nvim/site/pack/packer/start"

git_wrap() {
    local plug_name=$1
    local git_url='https://github.com/'
    local git_ext='.git'
    echo "${git_url}${plug_name}${git_ext}"
}

install_plugin() {
    local plugin_name=$1
    local url
    url=$(git_wrap "$plugin_name")

    local dirname
    dirname=$(basename "$plugin_name")

    mkdir -p "$INSTALL_DIR"

    if [[ -d "$INSTALL_DIR/$dirname" ]]; then
        return
    fi

    git clone -q "$url" "$INSTALL_DIR/$dirname"
    echo "Installed plugin: $plugin_name"
}

delete_plugin() {
    echo "Available Plugins: "
    select plugin in $(ls -d "$INSTALL_DIR"/*/ 2>/dev/null); do
        if [[ -n "$plugin" ]]; then
            plugin_name=$(basename "$plugin")
            read -p "Delete plugin '$plugin_name'? (y/n): " confirm
            if [[ "$confirm" =~ ^[Yy]$ ]]; then
                rm -rf "$plugin"
                echo "Deleted: $plugin_name"
            else
                echo "Canceled"
            fi
        fi
    done
}

PLUGINS=(
    "nvim-telescope/telescope.nvim"
    "nvim-lua/plenary.nvim"
    "windwp/nvim-autopairs"
    "goolord/alpha-nvim"
    "kdheepak/lazygit.nvim"
    "axieax/urlview.nvim"
    "mbbill/undotree"
    "hrsh7th/nvim-cmp"
    "hrsh7th/cmp-buffer"
    "hrsh7th/cmp-path"
    "L3MON4D3/LuaSnip"
    "saadparwaiz1/cmp_luasnip"
    "voldikss/vim-floaterm"
    "mason-org/mason.nvim"
    "jdhao/whitespace.nvim"
    "ej-shafran/compile-mode.nvim"
    "blazkowolf/gruber-darker.nvim"
    "NeogitOrg/neogit"
    "sindrets/diffview.nvim"
    "mikavilpas/yazi.nvim"
    "3rd/image.nvim"
)

if [[ $# -eq 0 ]]; then
    echo "Installing plugins..."
    for plugin in "${PLUGINS[@]}"; do
        install_plugin "$plugin"
    done
elif [[ "$1" == "delete" ]]; then
    delete_plugin
else
    echo "Usage: $0 [delete]"
    echo "  No argument: Installs the default plugins."
    echo "  'delete': Deletes a plugin after confirmation."
fi
