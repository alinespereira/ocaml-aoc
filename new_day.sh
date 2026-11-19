#!/usr/bin/env bash
set -euo pipefail

if [[ ! -f dune-project ]]; then
  echo "Erro: execute este script na raiz do projeto, onde está dune-project." >&2
  exit 1
fi

if [[ $# -ne 2 || ! $1 =~ ^[0-9]{4}$ || ! $2 =~ ^[0-9]{1,2}$ ]]; then
  echo "Uso: ./new_day.sh ANO DIA"
  echo "Exemplo: ./new_day.sh 2023 1"
  exit 1
fi

year=$1
day=$((10#$2))

if (( day < 1 || day > 25 )); then
  echo "Erro: o dia precisa estar entre 1 e 25." >&2
  exit 1
fi

printf -v day_dir 'day_%02d' "$day"
target="$year/$day_dir"

if [[ -e $target ]]; then
  echo "Erro: '$target' já existe; nenhum arquivo foi alterado." >&2
  exit 1
fi

mkdir -p "$target"

cat > "$target/dune" <<'EOF'
(executable
 (name main)
 (libraries aoc))
EOF

cat > "$target/main.ml" <<EOF
let () =
  let lines =
    Aoc.Common.Input.read_lines "$target/input.txt"
  in
  Printf.printf "Read %d lines\\n" (List.length lines)
EOF

touch "$target/input.txt" "$target/test.txt"

echo "Criado: $target"