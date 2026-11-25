#!/bin/sh
# A release of mk52 for the system it is made on: a folder that is the whole tool, to be
# used where no OCaml is installed.
#
#   bin/mk52              the command
#   bin/mk52-build        the builder: it carries the compiler, and reads a design's code
#   interfaces/           what the builder checks that code against: the kernel's
#                         compiled interfaces, and the standard library's
#   web/view.html         the page
#   web/board.html        the page a circuit board is looked at in
#   lib/, models/         the kernel's code and a worked example, to read: a design
#                         folder reaches them through its link mk52
#
#   tools/release.sh [folder]     the folder must not be there yet; unsaid, it is
#                                 _release/mk52-<system>-<machine>
set -e
home=$(cd "$(dirname "$0")/.." && pwd -P)
cd "$home"
out=${1:-_release/mk52-$(uname -s | tr '[:upper:]' '[:lower:]')-$(uname -m)}
if [ -e "$out" ]; then echo "$out is there already: remove it, or name another folder" >&2; exit 1; fi
dune build cli/main.exe embed/embed.bc.exe
mkdir -p "$out/bin" "$out/interfaces/kernel" "$out/interfaces/ocaml" "$out/web" "$out/lib" "$out/models"
cp _build/default/cli/main.exe "$out/bin/mk52"
cp _build/default/embed/embed.bc.exe "$out/bin/mk52-build"
chmod 755 "$out/bin/mk52" "$out/bin/mk52-build"
cp _build/default/lib/.mk52.objs/byte/*.cmi "$out/interfaces/kernel/"
cp "$(ocamlc -where)"/*.cmi "$out/interfaces/ocaml/"
cp web/view.html web/board.html "$out/web/"
cp lib/*.ml "$out/lib/"
cp models/bracket.ml "$out/models/"
mkdir -p "$out/docs"
cp README.md NOTES.md LICENSE "$out/"
cp docs/*.png "$out/docs/"
# The builder is bytecode, and OCaml's bytecode runtime is linked to the zstd library
# where OCaml was built with it. On macOS that library is carried along, and the builder
# told to look for it beside itself; the change is then signed, as the system asks.
if [ "$(uname -s)" = Darwin ]; then
  zstd=$(otool -L "$out/bin/mk52-build" | awk '/libzstd/ { print $1 }')
  if [ -n "$zstd" ]; then
    cp "$zstd" "$out/bin/"
    install_name_tool -change "$zstd" "@executable_path/$(basename "$zstd")" "$out/bin/mk52-build"
    codesign --force -s - "$out/bin/mk52-build" 2>/dev/null
  fi
fi
echo "$out"
