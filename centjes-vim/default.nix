{ stdenv
}:
stdenv.mkDerivation {
  name = "centjes-vim";
  src = ./.;
  buildCommand = ''
    mkdir $out

    mkdir $out/ftdetect
    cp $src/ftdetect/centjes.vim $out/ftdetect/centjes.vim

    mkdir $out/syntax
    cp $src/syntax/centjes.vim $out/syntax/centjes.vim
  '';
}
