#!/usr/bin/env bash

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_TYPE="Debug"
CXX_FLAGS="-std=c23 -Wall -Wextra -g"

OUTPUT_FILE="$PROJECT_ROOT/compile_commands.json"

SRC_FILES=()
while IFS= read -r -d $'\0' file; do
    SRC_FILES+=("$file")
done < <(find "$PROJECT_ROOT" -type f \( -name "*.cpp" -o -name "*.c" \) -print0)

rm -f "$OUTPUT_FILE"

echo "[" > "$OUTPUT_FILE"
for i in "${!SRC_FILES[@]}"; do
    src="${SRC_FILES[$i]}"
    dir="$PROJECT_ROOT"
    command="clang++ $CXX_FLAGS -I$PROJECT_ROOT -c $src"
    echo "  {" >> "$OUTPUT_FILE"
    echo "    \"directory\": \"$dir\"," >> "$OUTPUT_FILE"
    echo "    \"command\": \"$command\"," >> "$OUTPUT_FILE"
    echo "    \"file\": \"$src\"" >> "$OUTPUT_FILE"
    if [[ $i -lt $((${#SRC_FILES[@]} - 1)) ]]; then
        echo "  }," >> "$OUTPUT_FILE"
    else
        echo "  }" >> "$OUTPUT_FILE"
    fi
done
echo "]" >> "$OUTPUT_FILE"

echo "Generated $OUTPUT_FILE with ${#SRC_FILES[@]} source files."
