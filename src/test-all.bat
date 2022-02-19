pushd %~dp0
for /R %%f in (*.rkt) do raco make %%f
raco test -s test -s slow-test .
popd