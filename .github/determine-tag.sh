#! /usr/bin/sh

$base_image_changed && {
  printf "tag=%s\n" "$sha"
  printf "changed=true\n"
  exit 0
}

${push_changed} && {
  printf "tag=%s\n" "$sha"
  printf "changed=true\n"
  exit 0
}

${branch_changed} && {
  printf "tag=%s\n" "$branch_tag"
  printf "changed=false\n"
  exit 0
}

printf "tag=%s\n" "$default"
printf "changed=false"
