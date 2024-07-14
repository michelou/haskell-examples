# <span id="top">Running Cabal on Windows</span> <span style="font-size:90%;">[↩](README.md#top)</span>

<table style="font-family:Helvetica,Arial;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:100px;"><a href="https://github.com/haskell/cabal/#cabal" rel="external"><img style="border:0;" src="./docs/images/cabal.png" width="100" alt="Haskell project"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">This document gathers usage information on running <a href="https://www.haskell.org/cabal/" rel="external">Cabal</a>, a system for packaging and installing <a href="https://www.haskell.org/" rel="external">Haskell</a> libraries and programs, on a Windows machine.
  </td>
  </tr>
</table>

Useful Haskell packages are for instance :
- [haskell-language-server 2.1][haskell_lsp_downloads] ([*changelog*][haskell_lsp_changelog])
- [hlint 3.8][hlint_downloads] ([*changelog*][hlint_changelog])
- [hpack 0.35][hpack_downloads] ([*changelog*][hpack_changelog])
- [hspec 2.11][hspec_downloads] ([*changelog*][hspec_changelog])
- [HTF 0.15][htf_downloads] ([*changelog*][htf_changelog])
- [HUnit 1.6][hunit_downloads] ([*changelog*][hunit_changelog])
- [ormolu 0.7][ormolu_downloads] ([*changelog*][ormolu_changelog])

> **:mag_right:** Do not forget to execute command [`cabal`][cabal_man] `update` before running `list` and `install` :
> <pre style="font-size:80%;">
> <b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> update</b>
> Downloading the latest package list from hackage.haskell.org
> To revert to previous state run:
>    cabal v2-update 'hackage.haskell.org,2021-08-21T05:55:43Z'

## <span id="cabal-install"><code>cabal-install</code> installation</span>

We install [**`cabal-install`**](https://hackage.haskell.org/package/cabal-install) as follows <sup id="anchor_01">[1](#footnote_01)</sup> :

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> install cabal-install</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - Cabal-syntax-3.10.1.0 (lib) (requires download & build)
[...]
Starting     cabal-install-3.10.1.0 (exe:cabal)
Building     cabal-install-3.10.1.0 (exe:cabal)
Installing   cabal-install-3.10.1.0 (exe:cabal)
Completed    cabal-install-3.10.1.0 (exe:cabal)
Copying 'cabal.exe' to '%APPDATA%\cabal\bin\cabal.exe'
&nbsp;
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> --version</b>
cabal-install version 3.10.1.0
compiled using version 3.10.1.0 of the Cabal library
&nbsp;
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/echo">echo</a> %CABAL_DIR%</b>
<a href="https://docs.microsoft.com/en-us/windows/deployment/usmt/usmt-recognized-environment-variables#variables-that-are-recognized-only-in-the-user-context">%APPDATA%</a>\cabal
</pre>

We now have two `cabal.exe` installed after running the above command :
- `%GHC_HOME%\bin\cabal.exe` which we initially extracted from [cabal-install-XXXX-x86_64-unknown-mingw32.zip][cabal_downloads]. 
- `%CABAL_DIR%\bin\cabal.exe` which comes with package `cabal-install`.

That means we have to carefully set up our **`PATH`** variable so that `%CABAL_DIR%\bin` <sup id="anchor_02">[2](#footnote_02)</sup> appears before `%GHC_HOME%\bin` (another option would be to remove `%GHC_HOME%\bin\cabal.exe`).  

## <span id="haskell_lsp">`haskell-language-server` installation</span> [**&#x25B4;**](#top)

[`haskell-language-server`][haskell_lsp] is a LSP server for GHC ([changelog](https://hackage.haskell.org/package/haskell-language-server-1.10.0.0/changelog)).

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages" rel="external">cabal</a> list haskell-language-server</b>
* haskell-language-server
    Synopsis: LSP server for GHC
    Default available version: 1.10.0.0
    Installed versions: [ Not installed ]
    Homepage: https://github.com/haskell/haskell-language-server#readme
    License:  Apache-2.0
&nbsp;
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages" rel="external">cabal</a> install haskell-language-server</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - Diff-0.4.1 (lib) (requires download & build)
[...]

</pre>

## <span id="hlint"><code>hlint</code> installation</span> [**&#x25B4;**](#top)

[HLint][hlint_readme] is a tool for suggesting possible improvements to [Haskell] source code ([changelog](https://hackage.haskell.org/package/hlint-3.5/changelog)).

We install [**`hlint`**][hlint_downloads] as follows.

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> list hlint | <a href="https://man7.org/linux/man-pages/man1/head.1.html">head</a> -6</b>
* hlint
    Synopsis: Source code suggestions
    Default available version: 3.5
    Installed versions: [ Not installed ]
    Homepage: https://github.com/ndmitchell/hlint#readme
    License:  BSD3
&nbsp;
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> install --overwrite-policy=always hlint</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
[...]
 - hlint-3.8 (lib) (requires download & build)
 - hlint-3.8  (exe:hlint) (requires download & build)
Downloading  hlint-3.8 
Downloaded   hlint-3.8 
Starting     hlint-3.8  (lib)
Building     hlint-3.8  (lib)
Installing   hlint-3.8  (lib)
Completed    hlint-3.8  (lib)
Starting     hlint-3.8  (exe:hlint)
Building     hlint-3.8  (exe:hlint)
Installing   hlint-3.8  (exe:hlint)
Completed    hlint-3.8  (exe:hlint)
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

## <span id="hpack">`hpack` installation</span> [**&#x25B4;**](#top)

[Hpack][hpack_readme] is a format for Haskell packages ([changelog](https://hackage.haskell.org/package/hpack-0.35.2/changelog)).

Similarly to [**`hlint`**][hlint_downloads] we install [**`hpack`**][hpack_downloads] as follows.

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> list hpack | <a href="https://man7.org/linux/man-pages/man1/head.1.html">head</a> -6</b>
* hpack
    Synopsis: A modern format for Haskell packages
    Default available version: 0.35.2
    Installed versions: [ Not installed ]
    Homepage: https://github.com/sol/hpack#readme
    License:  MIT
&nbsp;
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> install --overwrite-policy=always hpack</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
[...]
Starting     hpack-0.35.2 (lib)
Building     hpack-0.35.2 (lib)
Installing   hpack-0.35.2 (lib)
Completed    hpack-0.35.2 (lib)
Starting     hpack-0.35.2 (exe:hpack)
Building     hpack-0.35.2 (exe:hpack)
Installing   hpack-0.35.2 (exe:hpack)
Completed    hpack-0.35.2 (exe:hpack)
Warning: installdir is not defined. Set it in your cabal config file or use
--installdir=&lt;path&gt;. Using default installdir:
"<a href="https://docs.microsoft.com/en-us/windows/deployment/usmt/usmt-recognized-environment-variables#variables-that-are-recognized-only-in-the-user-context">%APPDATA%</a>\\cabal\\bin"
Copying 'hpack.exe' to '%APPDATA%\cabal\bin\hpack.exe'
</pre>

## <span id="hspec">`hspec` installation</span> [**&#x25B4;**](#top)

[Hspec] is a testing framework for Haskell ([changelog](https://hackage.haskell.org/package/hspec-2.11.0.1/changelog)). Note the mandatory `--lib` option.

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages" rel="external">cabal</a> list hspec-core | <a href="https://man7.org/linux/man-pages/man1/head.1.html" rel="external">head</a> -6</b>
* hspec-core
    Synopsis: A Testing Framework for Haskell
    Default available version: 2.11.0.1
    Installed versions: [ Not installed ]
    Homepage: http://hspec.github.io/
    License:  MIT
&nbsp;
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> install --overwrite-policy=always --lib hspec</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
[...]
 - hspec-core-2.11.0.1 (lib) (requires download & build)
 - hspec-2.11.0.1 (lib) (requires download & build)
[...]
Starting     hspec-2.11.0.1 (lib)
Building     hspec-2.11.0.1 (lib)
Installing   hspec-2.11.0.1 (lib)
Completed    hspec-2.11.0.1 (lib)
</pre>

## <span id="htf">`HTF` installation</span> [**&#x25B4;**](#top)

[HTF](https://hackage.haskell.org/package/HTF) (*Haskell Test Framework*) lets you define [unit tests](http://hunit.sourceforge.net), [QuickCheck properties](http://www.cs.chalmers.se/~rjmh/QuickCheck/), and black box tests in an easy and convenient way.

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> install HTF</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - cpphs-1.20.9.1 (exe:cpphs) (requires build)
[...]
 - HTF-0.15.0.0 (lib:HTF, exe:htfpp) (requires download & build)
Starting     HTF-0.15.0.0 (all, legacy fallback)
Building     HTF-0.15.0.0 (all, legacy fallback)
Installing   HTF-0.15.0.0 (all, legacy fallback)
Completed    HTF-0.15.0.0 (all, legacy fallback)
Warning: installdir is not defined. Set it in your cabal config file or use
--installdir=&lt;path&gt;. Using default installdir:
<a href="https://en.wikipedia.org/wiki/Environment_variable#Default_values">%APPDATA%</a>\\cabal\\bin"
Copying 'htfpp.exe' to '%APPDATA%\cabal\bin\htfpp.exe'
</pre>

## <span id="hunit">`HUnit` installation</span> [**&#x25B4;**](#top)

[HUnit](https://hackage.haskell.org/package/HUnit) is a unit testing framework for Haskell, inspired by the [JUnit](http://www.junit.org/) tool for Java ([changelog](https://hackage.haskell.org/package/HUnit-1.6.2.0/changelog)). Note the mandatory  `--lib` option.

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages" rel="external">cabal</a> list hunit | <a href="https://man7.org/linux/man-pages/man1/head.1.html" rel="external">head</a> -6</b>
* HUnit
    Synopsis: A unit testing framework for Haskell
    Default available version: 1.6.2.0
    Installed versions: [ Not installed ]
    Homepage: https://github.com/hspec/HUnit#readme
    License:  BSD3
&nbsp;
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

## <span id="ormolu">`ormolu` installation</span> [**&#x25B4;**](#top)

[**`ormolu`**](https://hackage.haskell.org/package/ormolu) is a formatter for Haskell source code ([changelog](https://hackage.haskell.org/package/ormolu-0.6.0.1/changelog)).

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> list ormolu</b>
* ormolu
    Synopsis: A formatter for Haskell source code
    Default available version: 0.6.0.1
    Installed versions: [ Not installed ]
    Homepage: https://github.com/tweag/ormolu
    License:  BSD-3-Clause
&nbsp;
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.4/intro.html#a-tool-for-working-with-packages">cabal</a> install --overwrite-policy=always ormolu</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - Cabal-3.6.3.0 (lib) (requires download & build)
 - base-compat-0.12.1 (lib) (requires download & build)
 - ghc-lib-parser-9.2.1.20211101 (lib) (requires download & build)
[...]
 - ormolu-0.5.2.0 (exe:ormolu) (requires download & build)
Downloading  ormolu-0.6.0.1
[...]
Starting     ormolu-0.6.0.1 (lib)
Building     ormolu-0.6.0.1 (lib)
Installing   ormolu-0.6.0.1 (lib)
Completed    ormolu-0.6.0.1 (lib)
Starting     ormolu-0.6.0.1 (exe:ormolu)
Building     ormolu-0.6.0.1 (exe:ormolu)
Installing   ormolu-0.6.0.1 (exe:ormolu)
Completed    ormolu-0.6.0.1 (exe:ormolu)
Warning: installdir is not defined. Set it in your cabal config file or use
--installdir=&lt;path>. Using default installdir:
"<a href="https://docs.microsoft.com/en-us/windows/deployment/usmt/usmt-recognized-environment-variables#variables-that-are-recognized-only-in-the-user-context">%APPDATA%</a>\cabal\\bin"
Copying 'ormolu.exe' to
'<a href="https://docs.microsoft.com/en-us/windows/deployment/usmt/usmt-recognized-environment-variables#variables-that-are-recognized-only-in-the-user-context">%APPDATA%</a>\cabal\bin\ormolu.exe'
&nbsp;
<b>&gt; <a href="https://hackage.haskell.org/package/ormolu-0.6.0.1#usage">ormolu</a> --version</b>
ormolu 0.6.0.1 UNKNOWN UNKNOWN
using ghc-lib-parser 9.2.2.20220307
</pre>

## <span id="footnotes">Footnotes</span> [**&#x25B4;**](#top)

<span id="footnote_01">[1]</span> **1<sup>st</sup> installation of `cabal.exe`** [↩](#anchor_01)

<dl><dd>
Windows users can download the Zip archive <a href="https://downloads.haskell.org/~cabal/cabal-install-latest/" rel="external"><code>cabal-install-XXXX-x86_64-unknown-mingw32.zip</code></a> and put <a href="https://man.archlinux.org/man/cabal.1"><code>cabal.exe</code></a> somewhere on <a href="https://en.wikipedia.org/wiki/PATH_(variable)" rel="external"><code>%PATH%</code></a>
(in our case <code>C:\opt\ghc-8.10.7\bin\</code>).
</dd>
<dd>
<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1" rel="external">where</a> cabal</b>
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
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/dir">dir</a> /b <a href="https://cabal.readthedocs.io/en/latest/installing-packages.html#environment-variables">%CABAL_DIR%</a>\store\ghc-8.10.7\package.db\h*</b>
hackage-secur_-0.6.2.0-4bcfc91bab3c9672887242ecd330bcd74353fb3a.conf
haddock-libra_-1.10.0-75d854e07f5a0b27babf51cc17072e68eb6e245e.conf
hashable-1.3.5.0-65bc77b538ac7d072f1fcd030b6ee6a57e0b26c4.conf
hashtables-1.2.4.2-f4d494972092c75e404f02da79d84e1f242dfbf7.conf
haskell-langu_-1.6.1.0-f31acb97be2fbd5fed0b7e021cff339e3c175d6c.conf
haskell-src-e_-1.23.1-4440c55b672a41bb11a00f76ff75f45612ae1cd4.conf
heapsize-0.3.0.1-27572f5939b1d90b9cb6eddd909b80e8cc943471.conf
hie-bios-0.8.1-d6478efe291f7474286ad3633a2b3625ddbd4cff.conf
hie-compat-0.2.1.1-3d80479a30af4ccfe5285e11884b258270cf5da5.conf
hiedb-0.4.1.0-f92bc53bbba3b57bf3a8e19b79d25ef9e66b449a.conf
hlint-3.2.8-07a017a8a2bca5a63749ab6686cca487f10a8038.conf
[...]
hscolour-1.24.4-0061dbe6d18e3cd673fce03ea0660dc5aaea655c.conf
hslogger-1.3.1.0-ee928afffd7ec0713310f830500c93fbd2f8c290.conf
hspec-2.9.4-630a6b50002ab7a0f91aaaeb68c05dff9fad57b5.conf
hspec-core-2.9.4-5d423483180beed29c5cb7bd5432d1296f6e3127.conf
hspec-discover-2.9.4-c1877178bbde3b2ac7c7a39dcf35b3e17924ad6c.conf
hspec-expecta_-0.8.2-298fc3d376661a3415ebc3ea5fb2e5c975cc9f9b.conf
HsYAML-0.2.1.0-07ad9aac91ccaff8e81e57b0c2b3f7c41d13eb79.conf
HsYAML-aeson-0.2.0.1-566689edb1c87f23b9e83ca7a4f7206983c56ccd.conf
HTTP-4000.3._-a14a8e673826c92e06e8f90d8b92ed1ad00f6951.conf
HUnit-1.6.2.0-7584a845e4464486d878b49a7f17ebf7806dc12b.conf
hyphenation-0.8.2-bd515ef41c0e7481a693cfb43a634ffee182525d.conf
</pre>
</dd></dl>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/July 2024* [**&#9650;**](#top)
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
[haskell_lsp]: https://hackage.haskell.org/package/haskell-language-server
[haskell_lsp_downloads]: https://hackage.haskell.org/package/haskell-language-server-1.10.0.0
[haskell_lsp_changelog]: https://hackage.haskell.org/package/haskell-language-server-2.1.0.0/changelog
[haskell_relnotes]: https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/9.0.1-notes.html
[hlint_downloads]: https://hackage.haskell.org/package/hlint
[hlint_readme]: https://hackage.haskell.org/package/hlint-3.5#readme
[hlint_changelog]: https://hackage.haskell.org/package/hlint/changelog
[hlint_downloads]: https://hackage.haskell.org/package/hlint
[hpack_changelog]: https://hackage.haskell.org/package/hpack/changelog
[hpack_downloads]: https://hackage.haskell.org/package/hpack
[hpack_readme]: https://github.com/sol/hpack#readme
[hspec_changelog]: https://hackage.haskell.org/package/hspec-2.11.0/changelog
[hspec_downloads]: https://hackage.haskell.org/package/hspec
[htf_changelog]: https://hackage.haskell.org/package/HTF-0.15.0.0/changelog
[htf_downloads]: https://hackage.haskell.org/package/HTF
[hunit_changelog]: https://hackage.haskell.org/package/HUnit-1.6.2.0/changelog
[hunit_downloads]: https://hackage.haskell.org/package/HUnit
[kotlin_examples]: https://github.com/michelou/kotlin-examples
[llvm_examples]: https://github.com/michelou/llvm-examples
[nodejs_examples]: https://github.com/michelou/nodejs-examples
[ormolu_changelog]: https://hackage.haskell.org/package/ormolu-0.7.0.0/changelog
[ormolu_downloads]: https://hackage.haskell.org/package/ormolu
[stack_changelog]: https://docs.haskellstack.org/en/stable/ChangeLog/
[stack_downloads]: https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows
[trufflesqueak_examples]: https://github.com/michelou/trufflesqueak-examples
[unix_opt]: http://tldp.org/LDP/Linux-Filesystem-Hierarchy/html/opt.html
[windows_batch_file]: https://en.wikibooks.org/wiki/Windows_Batch_Scripting
[windows_limitation]: https://support.microsoft.com/en-gb/help/830473/command-prompt-cmd-exe-command-line-string-limitation
[windows_subst]: https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/subst
[zip_archive]: https://www.howtogeek.com/178146/htg-explains-everything-you-need-to-know-about-zipped-files/
