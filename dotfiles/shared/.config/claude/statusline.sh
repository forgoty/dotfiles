#!/usr/bin/env bash

input=$(cat)

# Parse fields from JSON
model=$(echo "$input" | jq -r '.model.display_name // "unknown"')
cwd=$(echo "$input" | jq -r '.cwd // .workspace.current_dir // ""')
folder=$(basename "$cwd")
transcript=$(echo "$input" | jq -r '.transcript_path // ""' | sed "s|^$HOME|~|")
used_pct=$(echo "$input" | jq -r '.context_window.used_percentage // empty')

# ANSI color codes
reset='\033[0m'
bold='\033[1m'
dim='\033[2m'
green='\033[32m'
yellow='\033[33m'
red='\033[31m'
cyan='\033[36m'
white='\033[37m'

# Build context progress bar
BAR_WIDTH=10
if [ -n "$used_pct" ]; then
    used_int=$(printf "%.0f" "$used_pct")
    filled=$(( used_int * BAR_WIDTH / 100 ))
    empty=$(( BAR_WIDTH - filled ))

    if [ "$used_int" -ge 90 ]; then
        bar_color="$red"
    elif [ "$used_int" -ge 70 ]; then
        bar_color="$yellow"
    else
        bar_color="$green"
    fi

    bar=""
    for i in $(seq 1 $filled); do bar="${bar}#"; done
    for i in $(seq 1 $empty);  do bar="${bar}-"; done

    ctx_part="${bar_color}[${bar}]${reset} ${bar_color}${used_int}%${reset}"
else
    ctx_part="${dim}[ctx: n/a]${reset}"
fi

# Transcript path (shortened)
transcript_part=""
if [ -n "$transcript" ]; then
    transcript_part=" ${dim}${transcript}${reset}"
fi

# Compose status line
printf '%b\n' "${dim}${folder}${reset} | ${cyan}${model}${reset} | ${ctx_part}${transcript_part}"
