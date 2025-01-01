#!/bin/bash

# Check if zenity is installed
if ! command -v zenity &> /dev/null
then
    echo "Zenity is required but not installed. Please install it using your package manager."
    exit 1
fi

# Check if xclip is installed
if ! command -v xclip &> /dev/null
then
    echo "Xclip is required but not installed. Please install it using your package manager."
    exit 1
fi

# Get the current directory of the script
script_dir="$(dirname "$(realpath "$0")")"

# Ask the user for a filename using a dialog box
filename=$(zenity --entry --title="Save Clipboard Image" --text="Enter the filename for the PNG file:")

# Check if the user cancelled or entered no filename
if [ -z "$filename" ]; then
    echo "No filename provided or action cancelled. Exiting."
    exit 1
fi

# Ensure the filename ends with .png
if [[ "$filename" != *.png ]]; then
    filename="$filename.png"
fi

# Prepend the script directory to the filename to save in the script's directory
full_path="$script_dir/$(basename "$filename")"

# Check if clipboard contains an image
if xclip -selection clipboard -t image/png -o > /dev/null 2>&1; then
    # Save the clipboard image to the specified file
    xclip -selection clipboard -t image/png -o > "$full_path"
    if [ $? -eq 0 ]; then
        # zenity --info --title="Success" --text="Image saved to $full_path"
        true
    else
        zenity --error --title="Error" --text="Failed to save the image."
    fi
else
    zenity --error --title="Error" --text="Clipboard does not contain an image."
fi
