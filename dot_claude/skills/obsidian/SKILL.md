---
name: obsidian
description: Use the Obsidian CLI to create, read, edit, search, and manage notes. Covers daily notes, templates, properties, tags, tasks, and vault navigation.
---

# Obsidian CLI

Use the `obsidian` CLI to interact with the vault. Requires the Obsidian app to be running. All commands go through Obsidian's API so changes are indexed instantly (links, properties, etc.).

**Note**: CLI output includes a "Loading updated app package" log line on stderr — ignore it.

## File Targeting

Most commands accept `file=` or `path=` to target a note. If neither is given, the active file is used.

- `file=<name>` — wikilink-style resolution (just the name, no path or `.md` needed)
- `path=<path>` — exact path from vault root (e.g. `00-Inbox/My Note.md`)

Wrap values with spaces in quotes: `file="My Note"`

## Vault Targeting

If the terminal's cwd is a vault folder, that vault is used. Otherwise use `vault=<name>` as the **first** parameter:

```
obsidian vault=Notes daily
```

## Creating Notes

```bash
obsidian create name="Note Title" template=Zettel silent
obsidian create name="Note Title" path="00-Inbox/Note Title.md" content="# Hello" silent
obsidian create name="Note Title" template=Zettel content="Extra content" silent
```

- `name=<name>` — file name
- `path=<path>` — exact vault-relative path
- `content=<text>` — initial content (use `\n` for newlines, `\t` for tabs)
- `template=<name>` — apply a template (e.g. `Zettel`, `Daily`)
- `overwrite` — overwrite if file exists
- `silent` — create without opening in Obsidian
- `newtab` — open in new tab

## Reading Notes

```bash
obsidian read file="Note Title"
obsidian read path="01-Projects/My Project.md"
```

## Editing Notes

### Append (to end of file)

```bash
obsidian append file="Note Title" content="New content at the end"
obsidian append file="Note Title" content="inline addition" inline
```

`inline` — append without a preceding newline.

### Prepend (after frontmatter)

```bash
obsidian prepend file="Note Title" content="Content after frontmatter"
```

### Full File Replacement

Read, modify, then overwrite:

```bash
obsidian read file="Note Title"
# modify content
obsidian create path="exact/path.md" content="<full new content>" overwrite silent
```

For very large notes, direct file editing with the Edit tool may be more practical.

## Daily Notes

```bash
obsidian daily silent          # Open/create today's daily note (silent returns path)
obsidian daily:read            # Read today's daily note
obsidian daily:append content="- [ ] Buy groceries" silent
obsidian daily:prepend content="## Morning" silent
```

- `silent` — don't open the file in Obsidian
- `inline` — no newline before content

## Templates

```bash
obsidian templates             # List available templates
obsidian template:read name=Zettel              # Raw template
obsidian template:read name=Zettel resolve      # With variables resolved
obsidian template:read name=Zettel resolve title="My Note"  # With title
obsidian template:insert name=Zettel            # Insert into active file
```

Template variables resolved by `resolve`: `{{date}}`, `{{time}}`, `{{title}}`

**Preferred way to create notes from templates:**
```bash
obsidian create name="My Note" template=Zettel silent
```

## Properties (Frontmatter)

```bash
obsidian properties file="My Note"                    # List properties for a file
obsidian properties all                                # All properties vault-wide
obsidian properties all counts sort=count              # With occurrence counts

obsidian property:read name=id file="My Note"          # Read a property
obsidian property:set name=id value=202602151200 file="My Note"
obsidian property:set name=tags value="idea" type=list file="My Note"
obsidian property:remove name=draft file="My Note"
```

Types: `text`, `list`, `number`, `checkbox`, `date`, `datetime`

## Tags

```bash
obsidian tags all counts           # All tags with counts
obsidian tags file="My Note"       # Tags in a specific file
obsidian tag name=idea verbose     # Files tagged #idea
```

## Search

```bash
obsidian search query="meeting notes"
obsidian search query="TODO" path="01-Projects" matches
obsidian search query="meeting" format=json limit=10
```

- `matches` — show match context
- `case` — case-sensitive
- `format=text|json`

## Tasks

```bash
obsidian tasks all todo            # All incomplete tasks in vault
obsidian tasks daily               # Tasks from today's daily note
obsidian tasks daily todo          # Incomplete daily tasks
obsidian tasks file="My Note" done # Completed tasks in a file
obsidian tasks all verbose         # Grouped by file with line numbers

obsidian task daily line=3 toggle  # Toggle a daily note task
obsidian task file="My Note" line=8 done    # Mark done
obsidian task ref="My Note.md:8" todo       # Mark incomplete
obsidian task file="My Note" line=5 status=- # Custom status [-]
```

## File Management

```bash
obsidian move file="Old Name" to="01-Projects/New Name.md"
obsidian delete file="Old Note"              # Move to trash
obsidian delete file="Old Note" permanent    # Delete permanently
obsidian open file="My Note"                 # Open in Obsidian
obsidian open file="My Note" newtab
```

## Vault Navigation

```bash
obsidian files                         # List all files
obsidian files folder="01-Projects"    # Files in a folder
obsidian files ext=md                  # Only markdown files
obsidian folders                       # List all folders
obsidian folder path="01-Projects" info=files
```

## Links & Graph

```bash
obsidian backlinks file="My Note"      # What links to this note
obsidian links file="My Note"          # What this note links to
obsidian orphans                       # Files with no incoming links
obsidian deadends                      # Files with no outgoing links
obsidian unresolved                    # Broken/unresolved links
```

## Outline

```bash
obsidian outline file="My Note"               # Heading tree
obsidian outline file="My Note" format=md      # As markdown
```

## Clipboard

Append `--copy` to any command to copy output to clipboard:

```bash
obsidian read file="My Note" --copy
obsidian search query="TODO" --copy
```
