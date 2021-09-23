# Windows
Download fission-flare-windows.zip from [the Releases page](https://github.com/default-kramer/fission-flare/releases).
Extract it.
Run fission-flare.exe

# Mac
Download fission-flare-mac.tgz from [the Releases page](https://github.com/default-kramer/fission-flare/releases).
Extract it.
Run fission-flare-mac.app (in the `bin` directory).

# Linux
You'll have to build from source for now.

# Building from Source
Install [Racket](https://racket-lang.org/), preferably 8.2 or higher.
I'm not sure whether an older version will work.
Then
```
git clone https://github.com/default-kramer/fission-flare.git
cd ./fission-flare/src/ui
racket main-frame.rkt
```
You can also do `raco exe main-frame.rkt`; see [the docs for raco exe](https://docs.racket-lang.org/raco/exe.html)