#!/usr/bin/env bash

set -euo pipefail

INPUT_DIR="$1"
OUTPUT_DIR="$2"

# Convert input and output paths to absolute
INPUT_DIR="$(realpath "$INPUT_DIR")"
OUTPUT_DIR="$(realpath "$OUTPUT_DIR")"

# Create the output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

export INPUT_DIR OUTPUT_DIR

# Function to process each file
process_file() {
    local src="$1"
    local rel_path="${src#$INPUT_DIR/}"
    local dest="$OUTPUT_DIR/$rel_path"

    # Skip .git and .obsidian directories
    if [[ "$rel_path" == .git/* || "$rel_path" == .obsidian/* ]]; then
        return
    fi

    # Skip .org files in the input
    if [[ "$src" == *.org ]]; then
        return
    fi

    # If it's a Markdown file, convert it
    if [[ "$src" == *.md ]]; then
        dest="${dest%.md}.org"
        mkdir -p "$(dirname "$dest")"
        pandoc --wrap=none --from=markdown --to=org "$src" -o "$dest"
    else
        # Copy all other files as-is
        mkdir -p "$(dirname "$dest")"
        cp -a "$src" "$dest"
    fi
}

export -f process_file

# Use find to recurse through all files
find "$INPUT_DIR" -type f | while read -r file; do
    process_file "$file"
done
