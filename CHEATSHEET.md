# CLI Tools Cheatsheet

## Oil.nvim (File Browser in Neovim)
Edit your filesystem like a buffer. Changes are applied when you save with `:w`.

### Opening Oil
- `<F4>` - Open Oil in current file's directory
- `<C-o>` - Open Oil in current file's directory
- `<C-n>` - Open Oil in current working directory

### Navigation
- `<CR>` (Enter) - Open file or enter directory
- `-` - Go to parent directory
- `_` - Open current working directory
- `/` - Search within the directory listing

### File Operations (Edit like a buffer!)
- `dd` - Delete file/directory (like deleting a line)
- `yy` - Yank (copy) file path
- `p` - Paste (copy file to current directory)
- `d{motion}` - Delete multiple files (e.g., `d3j` deletes 3 files)
- `i` or `a` - Insert mode to rename file (edit the filename directly)
- `cc` - Change entire line to rename file
- `:w` or `<C-s>` - **Save changes (performs actual file operations!)**
- `u` - Undo changes before saving

### Creating New Files/Directories
- `i` or `o` - Enter insert mode and type new filename
- Add `/` at end of name to create a directory (e.g., `newfolder/`)
- `:w` - Save to create the file/directory

### View Options
- `g?` - Show help menu with all keybindings
- `g.` - Toggle hidden files (dotfiles)
- `g\\` - Toggle trash (show deleted items)
- `gs` - Change sort order
- `gd` - Show detail view
- `g<C-h>` - Toggle hidden files

### Other Actions
- `gx` - Open file with system default application
- `<C-p>` - Open file preview
- `<C-c>` - Close Oil buffer
- `q` - Close Oil buffer

### Tips
- Oil treats directories as editable buffers - use normal Vim motions!
- You can use visual mode to select multiple files: `V` then `j/k` to select, then `d` to delete
- Changes are **not applied** until you `:w` (write/save)
- Press `u` to undo changes before saving

## lazygit
Terminal GUI for git workflow management.

### Keybindings
- `w` - Save work in progress (WIP)
- `c` - Commit changes
- `P` - Push changes to origin
- `p` - Pull changes from remote
- `space` - Stage/unstage file or hunk
- `d` - View diff options
- `a` - Stage all changes
- `A` - Amend last commit
- `shift+s` - Stash changes
- `shift+p` - Pop stash
- `q` - Quit lazygit
- `?` - Show help menu

## fd
Fast and user-friendly alternative to `find` for searching file names.

### Usage
```bash
fd <pattern>                    # Search for files matching pattern in current directory
fd <pattern> <path>             # Search in specific directory
fd -e <ext> <pattern>           # Search for files with specific extension
fd -t f <pattern>               # Search for files only
fd -t d <pattern>               # Search for directories only
fd -H <pattern>                 # Include hidden files
fd -I <pattern>                 # Include ignored files (.gitignore)
fd -E <pattern> <search>        # Exclude pattern from search
```

## rg (ripgrep)
Fast text search tool for searching content recursively.

### Usage
```bash
rg <pattern>                    # Search for pattern in current directory
rg <pattern> <path>             # Search in specific directory
rg -i <pattern>                 # Case-insensitive search
rg -w <pattern>                 # Match whole words only
rg -t <type> <pattern>          # Search in specific file types (e.g., -t py)
rg -T <type> <pattern>          # Exclude specific file types
rg -l <pattern>                 # List only filenames containing matches
rg -c <pattern>                 # Count matches per file
rg -A <n> <pattern>             # Show n lines after match
rg -B <n> <pattern>             # Show n lines before match
rg -C <n> <pattern>             # Show n lines before and after match
rg --hidden <pattern>           # Search hidden files
rg --no-ignore <pattern>        # Don't respect .gitignore
```
