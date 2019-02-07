set flag ""
for pdb in $GHC_PACKAGE_PATH
    set flag "--package-db $pdb $flag"
end

set _gpp_back $GHC_PACKAGE_PATH
set -e GHC_PACKAGE_PATH

set confcmd runhaskell Setup.hs configure $flag --user
eval $confcmd

set GHC_PACKAGE_PATH _gpp_back
set -e _gpp_back
