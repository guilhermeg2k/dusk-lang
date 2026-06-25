#!/usr/bin/env python3
"""Migrate test files from indent-based block syntax to keyword-based block syntax."""

import os


def transform_line(content):
    """Add do/then keywords to for/if lines."""
    if content.startswith('for ') and not content.endswith(' do'):
        return content + ' do'
    if content.startswith('if ') and not content.endswith(' then'):
        return content + ' then'
    if content.startswith('else if ') and not content.endswith(' then'):
        return content + ' then'
    return content


def detect_block_opener(content, indent, next_indent):
    """Detect if this line opens a block.
    
    Returns the block type or None.
    """
    # Struct definition: line ends with 'struct'
    if content.rstrip().endswith('struct'):
        return 'struct'

    # For loop
    if content.startswith('for '):
        return 'for'

    # If statement (not else if)
    if content.startswith('if '):
        return 'if'

    # Function definition (multi-line): contains -> but NOT -> return,
    # and the next effective line has greater indentation
    if '->' in content and '-> return' not in content and '(' in content:
        if next_indent is not None and next_indent > indent:
            return 'fn'

    return None


def transform_file(filepath):
    with open(filepath, 'r') as f:
        raw = f.read()

    # Split into lines, preserving the trailing newline behavior
    lines = raw.split('\n')
    # Handle trailing newline
    had_trailing_newline = raw.endswith('\n')
    if lines and lines[-1] == '':
        lines = lines[:-1]

    # Parse lines: (indent, content)
    parsed = []
    for line in lines:
        indent = len(line) - len(line.lstrip(' '))
        content = line[indent:]
        parsed.append((indent, content))

    # Find effective line indices (non-blank, non-comment)
    effective = []
    for i, (_, content) in enumerate(parsed):
        stripped = content.strip()
        if stripped and not stripped.startswith('#'):
            effective.append(i)

    # Map each effective index to the next effective index
    next_eff = {}
    for j in range(len(effective) - 1):
        next_eff[effective[j]] = effective[j + 1]
    if effective:
        next_eff[effective[-1]] = None

    output = []
    stack = []  # (indent, block_type)

    for i, (indent, content) in enumerate(parsed):
        stripped = content.strip()

        # Blank lines and comments pass through unchanged
        if not stripped or stripped.startswith('#'):
            output.append(' ' * indent + stripped)
            continue

        # Existing 'end' keyword: pop the matching block (idempotent)
        if stripped == 'end':
            # Pop the matching block if it exists at this indent
            if stack and stack[-1][0] == indent:
                stack.pop()
            output.append(' ' * indent + stripped)
            continue

        # Close blocks whose indent is >= current indent
        while stack and indent <= stack[-1][0]:
            b_indent, b_type = stack[-1]
            # if-block continuation: else / else if (only when indent matches exactly)
            if indent == b_indent and b_type == 'if' and (stripped == 'else' or stripped.startswith('else if')):
                break
            output.append(' ' * b_indent + 'end')
            stack.pop()

        # Transform the line (add do/then)
        transformed = transform_line(stripped)
        output.append(' ' * indent + transformed)

        # Check if this line opens a block
        next_idx = next_eff.get(i)
        next_indent = parsed[next_idx][0] if next_idx is not None else None
        btype = detect_block_opener(stripped, indent, next_indent)
        if btype:
            stack.append((indent, btype))

    # Close remaining blocks at EOF
    while stack:
        b_indent, b_type = stack.pop()
        output.append(' ' * b_indent + 'end')

    # Post-process: remove blank lines that appear directly before 'end' lines
    cleaned = []
    for line in reversed(output):
        if line.strip() == '' and cleaned and cleaned[-1].strip() == 'end':
            continue
        cleaned.append(line)
    output = list(reversed(cleaned))

    # Write back
    result = '\n'.join(output)
    if had_trailing_newline:
        result += '\n'

    with open(filepath, 'w') as f:
        f.write(result)


def main():
    base = '/home/cyan-dev/workspace/dusk-lang'
    dirs = ['test/success', 'test/error']
    count = 0
    for d in dirs:
        dirpath = os.path.join(base, d)
        for fname in sorted(os.listdir(dirpath)):
            if fname.endswith('.dsk'):
                filepath = os.path.join(dirpath, fname)
                transform_file(filepath)
                count += 1
    print(f"Transformed {count} files.")


if __name__ == '__main__':
    main()
