# <span id="top">Cabal on Microsoft Windows</span> <span style="size:25%;"><a href="README.md">↩</a></span>

<table style="font-family:Helvetica,Arial;font-size:14px;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:100px;"><a href="https://www.haskell.org/" rel="external"><img style="border:0;" src="https://wiki.haskell.org/wikiupload/6/62/Double_lambda.png" width="100" alt="Haskell logo"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">This document gathers usage information on running <a href="https://www.haskell.org/cabal/" rel="external">Cabal</a>, a system for packaging and installing <a href="https://www.haskell.org/" rel="external">Haskell</a> libraries and programs, on the Windows platform.
  </td>
  </tr>
</table>

Useful Haskell packages are for instance [**`hlint`**](#hlint), [**`hpack`**](#hpack), [**`hspec`**](#hspec), [**`HTF`**](#htf), [**`HUnit`**](#hunit) and [**`ormolu`**](#ormolu).

## <span id="cabal-install"><code>cabal-install</code> installation</span>

We install [**`cabal-install`**](https://hackage.haskell.org/package/cabal-install) as follows <sup id="anchor_01">[1](#footnote_01)</sup> :

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> install cabal-install</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - Cabal-syntax-3.6.0.0 (lib) (requires download & build)
[...]
Starting     cabal-install-3.6.2.0 (exe:cabal)
Building     cabal-install-3.6.2.0 (exe:cabal)
Installing   cabal-install-3.6.2.0 (exe:cabal)
Completed    cabal-install-3.6.2.0 (exe:cabal)
Copying 'cabal.exe' to '%APPDATA%\cabal\bin\cabal.exe'
&nbsp;
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> --version</b>
cabal-install version 3.6.2.0
compiled using version 3.6.2.0 of the Cabal library
</pre>

After running the above command we have two `cabal.exe` installed :
- `%GHC_HOME%\bin\cabal.exe` which we initially extracted from [cabal-install-XXXX-x86_64-unknown-mingw32.zip][cabal_downloads]. 
- `%CABAL_DIR%\bin\cabal.exe` which comes with package `cabal-install`.

That means we have to carefully set up our **`PATH`** variable so that `%CABAL_DIR%\bin` <sup id="anchor_02">[2](#footnote_02)</sup> appears before `%GHC_HOME%\bin` (another option would be to remove `%GHC_HOME%\bin\cabal.exe`).  

## <span id="hlint"><code>hlint</code> installation</span>

[HLint][hlint_readme] is a tool for suggesting possible improvements to [Haskell] source code. We install [**`hlint`**][hlint_downloads] as follows.

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> install hlint</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
[...]
 - hlint-3.3.4 (lib) (requires download & build)
 - hlint-3.3.4  (exe:hlint) (requires download & build)
Downloading  hlint-3.3.4 
Downloaded   hlint-3.3.4 
Starting     hlint-3.3.4  (lib)
Building     hlint-3.3.4  (lib)
Installing   hlint-3.3.4  (lib)
Completed    hlint-3.3.4  (lib)
Starting     hlint-3.3.4  (exe:hlint)
Building     hlint-3.3.4  (exe:hlint)
Installing   hlint-3.3.4  (exe:hlint)
Completed    hlint-3.3.4  (exe:hlint)
Warning: installdir is not defined. Set it in your cabal config file or use
--installdir=&lt;path&gt;. Using default installdir:
"<a href="https://docs.microsoft.com/en-us/windows/deployment/usmt/usmt-recognized-environment-variables#variables-that-are-recognized-only-in-the-user-context">%APPDATA%</a>\\cabal\\bin"
Copying 'hlint.exe' to '%APPDATA%\cabal\bin\hlint.exe'
</pre>
<!--
Since the last installation step fails on MS Windows, we search for the path to the `hlint` installation directory inside the [Cabal][cabal_downloads] local store and copies its contents to a new directory (e.g. `c:\opt\ghc-9.0.1\hlint-3.2.7\`):

<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> /r  %APPDATA%\cabal hlint.exe</b>
%APPDATA%\cabal\store\ghc-9.0.1\hlint-3.2.7-5aa469904bdabf015c2e105bbe2c9a63561b1a33\\bin\hlint.exe
&nbsp;
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/xcopy">xcopy</a> /e /i /q %APPDATA%\cabal\store\ghc-9.0.1\hlint-3.2.7-5aa469904bdabf015c2e105bbe2c9a63561b1a33\ c:\opt\ghc-9.0.1\hlint-3.2.7</b>
12 file(s) copied
&nbsp;
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> /r %HLINT_HOME% hlint</b>
C:\opt\ghc-9.0.1\hlint-3.2.7\bin\hlint.exe
&nbsp;
<b>&gt; C:\opt\ghc-9.0.1\hlint-3.2.7\bin\<a href="https://hackage.haskell.org/package/hlint">hlint.exe</a> --version</b>
HLint v3.2.7, (C) Neil Mitchell 2006-2021
</pre>
-->
> **:mag_right:** Command [`cabal`][cabal_man] `list hlint` shows the latest available version of package **`hlint`** (*do not* forget `cabal update`) :
> <pre style="font-size:80%;">
> <b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> update</b>
> Downloading the latest package list from hackage.haskell.org
> To revert to previous state run:
>    cabal v2-update 'hackage.haskell.org,2021-08-21T05:55:43Z'
> &nbsp;
> <b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> list hlint | <a href="https://man7.org/linux/man-pages/man1/head.1.html">head</a> -6</b>
> * hlint
>     Synopsis: Source code suggestions
>     Default available version: 3.3.4
>     Installed versions: [ Not installed ]
>     Homepage: https://github.com/ndmitchell/hlint#readme
>     License:  BSD3
> </pre>

## <span id="hpack"><code>hpack</code> installation</span> <sup style="font-size:60%;">[**&#9650;**](#top)</sup>

[Hpack][hpack_readme] is a format for Haskell packages. Similarly to [**`hlint`**][hlint_downloads] we install [**`hpack`**][hpack_downloads] as follows.

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> install hpack</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
[...]
Starting     hpack-0.34.4 (lib)
Building     hpack-0.34.4 (lib)
Installing   hpack-0.34.4 (lib)
Completed    hpack-0.34.4 (lib)
Starting     hpack-0.34.4 (exe:hpack)
Building     hpack-0.34.4 (exe:hpack)
Installing   hpack-0.34.4 (exe:hpack)
Completed    hpack-0.34.4 (exe:hpack)
Warning: installdir is not defined. Set it in your cabal config file or use
--installdir=&lt;path&gt;. Using default installdir:
"<a href="https://docs.microsoft.com/en-us/windows/deployment/usmt/usmt-recognized-environment-variables#variables-that-are-recognized-only-in-the-user-context">%APPDATA%</a>\\cabal\\bin"
Copying 'hpack.exe' to '%APPDATA%\cabal\bin\hpack.exe'
</pre>

> **:mag_right:** Command [`cabal`][cabal_man] `list hpack` shows the latest available version of package [**`hpack`**][hpack_downloads] (*do not* forget `cabal update`) :
> <pre style="font-size:80%;">
> <b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> update</b>
> Downloading the latest package list from hackage.haskell.org
> To revert to previous state run:
>    cabal v2-update 'hackage.haskell.org,2021-08-21T05:55:43Z'
> &nbsp;
> <b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> list hpack | <a href="https://man7.org/linux/man-pages/man1/head.1.html">head</a> -6</b>
> * hpack
>     Synopsis: A modern format for Haskell packages
>     Default available version: 0.34.4
>     Installed versions: [ Not installed ]
>     Homepage: https://github.com/sol/hpack#readme
>     License:  MIT
> </pre>

## <span id="hspec"><code>hspec</code> installation</span> <sup style="font-size:60%;">[**&#9650;**](#top)</sup>

[Hspec] is a testing framework for Haskell (note the mandatory option `--lib`).

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> install --lib hspec</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
[...]
 - hspec-core-2.8.3 (lib) (requires download & build)
 - hspec-2.8.3 (lib) (requires download & build)
[...]
Starting     hspec-2.8.3 (lib)
Building     hspec-2.8.3 (lib)
Installing   hspec-2.8.3 (lib)
Completed    hspec-2.8.3 (lib)
</pre>

## <span id="htf"><code>HTF</code> installation</span> <sup style="font-size:60%;">[**&#9650;**](#top)</sup>

[HTF](https://hackage.haskell.org/package/HTF) (*Haskell Test Framework*) lets you define [unit tests](http://hunit.sourceforge.net), [QuickCheck properties](http://www.cs.chalmers.se/~rjmh/QuickCheck/), and black box tests in an easy and convenient way.

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> install HTF</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - HUnit-1.6.2.0 (lib) (requires download & build)
 - QuickCheck-2.14.2 (lib) (requires download & build)
[...]
Starting     HTF-0.14.0.6 (all, legacy fallback)
Building     HTF-0.14.0.6 (all, legacy fallback)
Installing   HTF-0.14.0.6 (all, legacy fallback)
Completed    HTF-0.14.0.6 (all, legacy fallback)
Warning: installdir is not defined. Set it in your cabal config file or use
--installdir=&lt;path&gt;. Using default installdir:
<a href="https://en.wikipedia.org/wiki/Environment_variable#Default_values">%APPDATA%</a>\\cabal\\bin"
Copying 'htfpp.exe' to '%APPDATA%\cabal\bin\htfpp.exe'
</pre>

## <span id="hunit"><code>HUnit</code> installation</span> <sup style="font-size:60%;">[**&#9650;**](#top)</sup>

[HUnit](https://hackage.haskell.org/package/HUnit) is a unit testing framework for Haskell, inspired by the [JUnit](http://www.junit.org/) tool for Java (note the mandatory option `--lib`).

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> install --lib HUnit</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - call-stack-0.3.0 (lib) (requires download & build)
 - HUnit-1.6.2.0 (lib) (requires download & build)
Downloading  call-stack-0.3.0
Downloaded   call-stack-0.3.0
Downloading  HUnit-1.6.2.0
Starting     call-stack-0.3.0 (lib)
Downloaded   HUnit-1.6.2.0
Building     call-stack-0.3.0 (lib)
Installing   call-stack-0.3.0 (lib)
Completed    call-stack-0.3.0 (lib)
Starting     HUnit-1.6.2.0 (lib)
Building     HUnit-1.6.2.0 (lib)
Installing   HUnit-1.6.2.0 (lib)
Completed    HUnit-1.6.2.0 (lib)
</pre>

> **:mag_right:** Command [`cabal`][cabal_man] `list hunit` shows the latest available version of package [`hunit`](https://hackage.haskell.org/package/HUnit) (*do not* forget `cabal update`) :
> <pre style="font-size:80%;">
> <b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> update</b>
> Downloading the latest package list from hackage.haskell.org
> To revert to previous state run:
>    cabal v2-update 'hackage.haskell.org,2021-03-05T07:35:37Z'
> &nbsp;
> <b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> list hunit | <a href="https://man7.org/linux/man-pages/man1/head.1.html">head</a> -6</b>
> * HUnit
>     Synopsis: A unit testing framework for Haskell
>     Default available version: 1.6.2.0
>     Installed versions: [ Not installed ]
>     Homepage: https://github.com/hspec/HUnit#readme
>     License:  BSD3
> </pre>

## <span id="ormolu"><code>ormolu</code> installation</span> <sup style="font-size:60%;">[**&#9650;**](#top)</sup>

[**`ormolu`**](https://hackage.haskell.org/package/ormolu) is a formatter for Haskell source code.

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> list ormolu</b>
* ormolu
    Synopsis: A formatter for Haskell source code
    Default available version: 0.4.0.0
    Installed versions: [ Not installed ]
    Homepage: https://github.com/tweag/ormolu
    License:  BSD-3-Clause
&nbsp;
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> install --overwrite-policy=always ormolu</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - Cabal-3.6.2.0 (lib) (requires download & build)
 - base-compat-0.12.1 (lib) (requires download & build)
 - ghc-lib-parser-9.2.1.20211101 (lib) (requires download & build)
[...]
 - ormolu-0.4.0.0 (exe:ormolu) (requires download & build)
Downloading  ormolu-0.4.0.0
[...]
Starting     ormolu-0.4.0.0 (lib)
Building     ormolu-0.4.0.0 (lib)
Installing   ormolu-0.4.0.0 (lib)
Completed    ormolu-0.4.0.0 (lib)
Starting     ormolu-0.4.0.0 (exe:ormolu)
Building     ormolu-0.4.0.0 (exe:ormolu)
Installing   ormolu-0.4.0.0 (exe:ormolu)
Completed    ormolu-0.4.0.0 (exe:ormolu)
Copying 'ormolu.exe' to
Copying 'ormolu.exe' to '%APPDATA%\cabal\bin\ormolu.exe
&nbsp;
<b>&gt; <a href="https://hackage.haskell.org/package/ormolu-0.4.0.0#usage">ormolu</a> --version</b>
ormolu 0.4.0.0 UNKNOWN UNKNOWN
using ghc-lib-parser 9.2.1.20211101
</pre>

## <span id="footnotes">Footnotes</span> <sup style="font-size:60%;">[**&#9650;**](#top)</sup>

<span id="footnote_01">[1]</span> **1<sup>st</sup> installation of `cabal.exe`** [↩](#anchor_01)

<dl><dd>
Windows users can download the Zip archive <a href="https://downloads.haskell.org/~cabal/cabal-install-latest/"><code>cabal-install-XXXX-x86_64-unknown-mingw32.zip</code></a> and put <a href="https://man.archlinux.org/man/cabal.1"><code>cabal.exe</code></a> somewhere on <code>%PATH%</code>
(in our case <code>C:\opt\ghc-8.10.7\bin\</code>).
</dd>
<dd>
<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> cabal</b>
C:\opt\ghc-8.10.7\bin\cabal.exe
&nbsp;
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> update</b>
Config file path source is default config file.
Config file %APPDATA%\cabal\config not found.
Writing default configuration to %APPDATA%\cabal\config
Downloading the latest package list from hackage.haskell.org
&nbsp;
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/echo">echo</a> %CABAL_DIR%</b>
<a href="https://docs.microsoft.com/en-us/windows/deployment/usmt/usmt-recognized-environment-variables#variables-that-are-recognized-only-in-the-user-context">%APPDATA%</a>\cabal
</pre>
</dd></dl>

<span id="footnote_02">[2]</span> **Variable  `CABAL_DIR`** [↩](#anchor_02)

<dl><dd>
The <a href="https://cabal.readthedocs.io/en/latest/installing-packages.html#environment-variables"><b><code>CABAL_DIR</code></b></a> variable defines where the <a href="https://man.archlinux.org/man/cabal.1"><code>cabal</code></a> command will install the software packages.
</dd>
<dd>
<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/dir">dir</a> /b <a href="https://cabal.readthedocs.io/en/latest/installing-packages.html#environment-variables">%CABAL_DIR%</a>\store\</b>
ghc-8.10.4
ghc-8.10.5
ghc-8.10.6
ghc-8.10.7
ghc-9.0.1
</pre>
</dd>
<dd>
For version GHC 8.10.7, installed packages whose name starts with letter <code>h</code> are:
</dd>
<dd>
<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/dir">dir</a> /b %CABAL_DIR%\store\ghc-8.10.7\package.db\h*</b>
hashable-1.3.3.0-9aa5b1c17f7be6618063ae943312ca890700d98f.conf
hlint-3.3.4-a3f166f4044b65bd5cb7051c1794269a28c4ebf6.conf
hourglass-0.2.12-0adfffb133b4ecb1429c8ab7a3a63e1f715772fc.conf
hpack-0.34.4-e960b875b1cbf4df20cc14c3d94710b436385e86.conf
hscolour-1.24.4-0372f886d39fa7f543b5ca4f492dac187f227b70.conf
hspec-2.8.3-c1637174f023f78391253f7cc431cb06249a2794.conf
hspec-core-2.8.3-a0c773b4747f383a6460d9b380f2a09f715ba4c1.conf
hspec-discover-2.8.3-31f13ffd41ab63d77a11628e8772aa520eada03b.conf
hspec-expecta_-0.8.2-b33ff16d3fc2949d866d11f9f449bfd6edadbb1c.conf
http-client-0.7.8-6232138c7b5fb2b04a9cb69f45f3e51acdcd1faa.conf
http-client-t_-0.3.5.3-8357a5c906524b43b069b4a786985f5fda5b6a48.conf
http-types-0.12.3-428886f417f521d2610e887cc591784748da284e.conf
HUnit-1.6.2.0-d7c91dc1d186c000539b73616c390e93daea894b.conf
</pre>
</dd></dl>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/February 2022* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[article_abela]: http://www.cse.chalmers.se/~abela/master/layout-parsing.html
[cabal_changelog]: https://hackage.haskell.org/package/Cabal/changelog
[cabal_man]: https://man.archlinux.org/man/cabal.1
[cabal_downloads]: https://downloads.haskell.org/~cabal/cabal-install-latest/
[dotty_examples]: https://github.com/michelou/dotty-examples
[ghc_parser]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/parser
[github_markdown]: https://github.github.com/gfm/
[graalvm_examples]: https://github.com/michelou/graalvm-examples
[haskell]: https://www.haskell.org
[haskell_downloads]: https://downloads.haskell.org/ghc/latest/
[haskell_relnotes]: https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/9.0.1-notes.html
[hlint_changelog]: https://hackage.haskell.org/package/hlint-3.2.3/changelog
[hlint_downloads]: https://hackage.haskell.org/package/hlint
[hlint_readme]: https://hackage.haskell.org/package/hlint-3.2#readme
[hpack_changelog]: https://hackage.haskell.org/package/hpack-0.34.3/changelog
[hpack_downloads]: https://hackage.haskell.org/package/hpack
[hpack_readme]: https://github.com/sol/hpack#readme
[hspec]: https://hackage.haskell.org/package/hspec
[kotlin_examples]: https://github.com/michelou/kotlin-examples
[llvm_examples]: https://github.com/michelou/llvm-examples
[nodejs_examples]: https://github.com/michelou/nodejs-examples
[stack_changelog]: https://docs.haskellstack.org/en/stable/ChangeLog/
[stack_downloads]: https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows
[trufflesqueak_examples]: https://github.com/michelou/trufflesqueak-examples
[unix_opt]: http://tldp.org/LDP/Linux-Filesystem-Hierarchy/html/opt.html
[windows_batch_file]: https://en.wikibooks.org/wiki/Windows_Batch_Scripting
[windows_limitation]: https://support.microsoft.com/en-gb/help/830473/command-prompt-cmd-exe-command-line-string-limitation
[windows_subst]: https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/subst
[zip_archive]: https://www.howtogeek.com/178146/htg-explains-everything-you-need-to-know-about-zipped-files/
