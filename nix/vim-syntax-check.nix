{ runCommand
, vim
, neovim
, src
}:
# Assert that the syntax file highlights every element of the corpus the way
# the golden dump says it does.
#
# Both editors are checked because the plugin is used from both, and because
# the syntax engine is where they are most likely to drift apart.
runCommand "centjes-vim-syntax-check"
{
  nativeBuildInputs = [ vim neovim ];
} ''
  export HOME="$TMPDIR"

  for editor in vim nvim; do
    CENTJES_VIM_DIR=${src} \
      CENTJES_SYNTAX_DUMP="$PWD/$editor.syntax" \
      "$editor" --clean -n -es -S ${src}/test/dump-syntax.vim \
        ${src}/test_resources/every-syntax-element.cent

    if ! diff -u \
      ${src}/test_resources/every-syntax-element.syntax \
      "$PWD/$editor.syntax"
    then
      echo "In $editor, the syntax file no longer highlights the corpus the way"
      echo "every-syntax-element.syntax says it does."
      echo "If the change is intended, regenerate the golden dump:"
      echo "  CENTJES_VIM_DIR=centjes-vim \\"
      echo "    CENTJES_SYNTAX_DUMP=centjes-vim/test_resources/every-syntax-element.syntax \\"
      echo "    vim --clean -n -es -S centjes-vim/test/dump-syntax.vim \\"
      echo "      centjes-vim/test_resources/every-syntax-element.cent"
      exit 1
    fi
  done

  touch $out
''
