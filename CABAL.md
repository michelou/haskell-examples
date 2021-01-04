# <span id="top">Cabal on Microsoft Windows</span> <span style="size:25%;"><a href="README.md">↩</a></span>

<table style="font-family:Helvetica,Arial;font-size:14px;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:120px;"><a href="https://www.haskell.org/" rel="external"><img style="border:0;" src="https://wiki.haskell.org/wikiupload/6/62/Double_lambda.png" width="120" alt="Haskell logo"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">This document gathers usage information on running <a href="https://www.haskell.org/cabal/" rel="external">Cabal</a>, a system for building and packaging <a href="https://www.haskell.org/" rel="external">Haskell</a> libraries and programs, on the Windows platform.
  </td>
  </tr>
</table>


## <span id="cabal"><code>cabal</code> installation</span>

Windows users can download the Zip archive [cabal-install-XXXX-x86_64-unknown-mingw32.zip][cabal_downloads] and put the **`cabal.exe`** executable somewhere on the `%PATH%`
(in our case `C:\opt\ghc-8.10.3\bin\`).

<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> cabal</b>
C:\opt\ghc-8.10.3\bin\cabal.exe
&nbsp;
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.2/intro.html#a-tool-for-working-with-packages">cabal</a> update</b>
Config file path source is default config file.
Config file %APPDATA%\cabal\config not found.
Writing default configuration to %APPDATA%\cabal\config
Downloading the latest package list from hackage.haskell.org

<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/echo">echo</a> %CABAL_DIR%</b>
C:\Users\%USERNAME%\AppData\Roaming\cabal
</pre>

> **:mag_right:** The `CABAL_DIR` variable defines where the `cabal` command will install the software packages.
> <pre style="font-size:80%;">
> <b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/dir">dir</a> /b %CABAL_DIR%\store\</b>
> ghc-8.10.1
> ghc-8.10.2
> ghc-8.10.3
> ghc-8.8.2
> ghc-8.8.3
> </pre>
> For version GHC 8.10.3, installed packages whose name starts with letter `h` are:
> <pre style="font-size:80%;">
> &gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/dir">dir</a> /b %CABAL_DIR%\store\ghc-8.10.3\package.db\h*
> hashable-1.3.0.0-f6dc8c628df97d10d4acd7100fec3f26d48ab331.conf
> hlint-3.2.6-9335bc0881d32d9e25dbb12047d8c00cc85086dc.conf
> hourglass-0.2.12-9b267f7072f985394bb6dfb0a27c5f0dd4c1d574.conf
> hpack-0.34.3-0233213a3d58261652391c07ba2a68fdc69342be.conf
> hscolour-1.24.4-75721f4a144a2c90144263c8e6c83cb6e16b5302.conf
> http-client-0.7.3-15b668474f824dc8f7048747e8b08018acb4cf99.conf
> http-client-t_-0.3.5.3-f22320bbc816d549f709f1c8128ae02140151b16.conf
> http-types-0.12.3-a6e0038cee2c3b733b35d402955629f8c7c75cde.conf
> HUnit-1.6.1.0-5907212489ecbff9f96c7b78c1c2c0dea3131d9b.conf
> </pre>

## <span id="hlint"><code>hlint</code> installation</span>

[HLint][hlint_readme] is a tool for suggesting possible improvements to [Haskell] code. We install [`hlint`][hlint_downloads] in two steps on Windows.

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.2/intro.html#a-tool-for-working-with-packages">cabal</a> install hlint</b>
Resolving dependencies...
Build profile: -w ghc-8.10.3 -O1
In order, the following will be built (use -v for more details):
 - base-compat-0.11.2 (lib) (requires download & build)
 - base-orphans-0.8.3 (lib) (requires download & build)
[...]
Starting     hlint-3.2.6 (lib)
Building     hlint-3.2.6 (lib)
Installing   hlint-3.2.6 (lib)
Completed    hlint-3.2.6 (lib)
Starting     hlint-3.2.6 (exe:hlint)
Building     hlint-3.2.6 (exe:hlint)
Installing   hlint-3.2.6 (exe:hlint)
Completed    hlint-3.2.6 (exe:hlint)
Symlinking 'hlint.exe'
cabal: Symlinking feature not available on Windows
</pre>

Since the last installation step fails on Windows, we search for the path to the `hlint` installation directory inside the [Cabal][cabal_downloads] local store and copies its contents to a new directory (e.g. `c:\opt\ghc-8.10.3\hlint\`):

<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> /r  %APPDATA%\cabal hlint.exe</b>
%APPDATA%\cabal\store\ghc-8.10.3\hlint-3.2.6-4f7e185c559e1d19fe6ca7ad88283e3d6cb0ddf8\bin\hlint.exe
&nbsp;
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/xcopy">xcopy</a> /e /i /q %APPDATA%\cabal\store\ghc-8.10.3\hlint-3.2.6-4f7e185c559e1d19fe6ca7ad88283e3d6cb0ddf8 c:\opt\ghc-8.10.3\hlint</b>
12 file(s) copied
&nbsp;
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> /r %HLINT_HOME% hlint</b>
C:\opt\ghc-8.10.3\hlint\bin\hlint.exe
&nbsp;
<b>&gt; C:\opt\ghc-8.10.3\hlint\bin\<a href="https://hackage.haskell.org/package/hlint">hlint</a> --version</b>
HLint v3.2.6, (C) Neil Mitchell 2006-2020
</pre>

> **:mag_right:** Command **`cabal list hlint`** shows the latest available version of package `hlint` (*do not* forget **`cabal update`**):
> <pre style="font-size:80%;">
> <b>&gt; <a href="https://cabal.readthedocs.io/en/3.2/intro.html#a-tool-for-working-with-packages">cabal</a> update</b>
> Downloading the latest package list from hackage.haskell.org
> To revert to previous state run:
>    cabal v2-update 'hackage.haskell.org,2020-06-16T03:59:14Z
> &nbsp;
> <b>&gt; <a href="https://cabal.readthedocs.io/en/3.2/intro.html#a-tool-for-working-with-packages">cabal</a> list hlint</b>
> * hlint
>     Synopsis: Source code suggestions
>     Default available version: 3.2.6
>     Installed versions: [ Not installed ]
>     Homepage: https://github.com/ndmitchell/hlint#readme
>     License:  BSD3
> [..]
> </pre>

## <span id="hpack"><code>hpack</code> installation</span>

[Hpack][hpack_readme] is a format for Haskell packages. Similarly to `hlint` we install [`hpack`][hpack_downloads] in two steps on Windows.

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.2/intro.html#a-tool-for-working-with-packages">cabal</a> install hpack</b>
Resolving dependencies...
Build profile: -w ghc-8.10.3 -O1
In order, the following will be built (use -v for more details):
 - cabal-doctest-1.0.8 (lib) (requires build)
[...]
Starting     hpack-0.34.3 (lib)
Building     hpack-0.34.3 (lib)
Installing   hpack-0.34.3 (lib)
Completed    hpack-0.34.3 (lib)
Starting     hpack-0.34.3 (exe:hpack)
Building     hpack-0.34.3 (exe:hpack)
Installing   hpack-0.34.3 (exe:hpack)
Completed    hpack-0.34.3 (exe:hpack)
Symlinking 'hpack.exe'
cabal: Symlinking feature not available on Windows
</pre>

Since the last installation step fails on Windows, we search for the path to the `hpack` installation directory inside the [Cabal][cabal_downloads] local store and copies its contents to a new directory (e.g. `c:\opt\ghc-8.10.2\hpack\`):

<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> /r  %APPDATA%\cabal hpack.exe</b>
%APPDATA%\cabal\store\ghc-8.10.3\hpack-0.34.3-66f92fa5c7eb33250bcdd5caa70c3c8dcd631e97\bin\hpack.exe
&nbsp;
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/xcopy">xcopy</a> /e /i /q %APPDATA%\cabal\store\ghc-8.10.3\hpack-0.34.3-66f92fa5c7eb33250bcdd5caa70c3c8dcd631e97 c:\opt\ghc-8.10.3\hpack</b>
3 file(s) copied
&nbsp;
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> /r %HPACK_HOME% hpack</b>
C:\opt\ghc-8.10.3\hpack\bin\hpack.exe
&nbsp;
<b>&gt; C:\opt\ghc-8.10.3\hpack\bin\<a href="https://hackage.haskell.org/package/hpack">hpack</a> --version</b>
hpack version 0.34.3
</pre>

> **:mag_right:** Command **`cabal list hpack`** shows the latest available version of package `hpack` (*do not* forget **`cabal update`**) :
> <pre style="font-size:80%;">
> <b>&gt; <a href="https://cabal.readthedocs.io/en/3.2/intro.html#a-tool-for-working-with-packages">cabal</a> update</b>
> Downloading the latest package list from hackage.haskell.org
> To revert to previous state run:
>    cabal v2-update 'hackage.haskell.org,2020-06-16T03:59:14Z
> &nbsp;
> <b>&gt; <a href="https://cabal.readthedocs.io/en/3.2/intro.html#a-tool-for-working-with-packages">cabal</a> list hpack</b>
> * hpack
>     Synopsis: A modern format for Haskell packages
>     Default available version: 0.34.3
>     Installed versions: [ Not installed ]
>     Homepage: https://github.com/sol/hpack#readme
>     License:  MIT
> [..]
> </pre>


## <span id="hunit"><code>HUnit</code> installation</span>

<pre style="font-size:80%;">
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.2/intro.html#a-tool-for-working-with-packages">cabal</a> install --lib HUnit</b>
Resolving dependencies...
Build profile: -w ghc-8.10.2 -O1
In order, the following will be built (use -v for more details):
 - call-stack-0.2.0 (lib) (requires download & build)
 - HUnit-1.6.0.0 (lib) (requires download & build)
Downloading  call-stack-0.2.0
Downloaded   call-stack-0.2.0
Downloading  HUnit-1.6.0.0
Starting     call-stack-0.2.0 (lib)
Downloaded   HUnit-1.6.0.0
Building     call-stack-0.2.0 (lib)
Installing   call-stack-0.2.0 (lib)
Completed    call-stack-0.2.0 (lib)
Starting     HUnit-1.6.1.0 (lib)
Building     HUnit-1.6.1.0 (lib)
Installing   HUnit-1.6.1.0 (lib)
Completed    HUnit-1.6.1.0 (lib)
</pre>


## <span id="footnotes">Footnotes</span>

<b name="footnote_01">[1]</b> ***Downloads*** [↩](#anchor_01)

<p style="margin:0 0 1em 20px;">
In our case we downloaded the following installation files (<a href="#proj_deps">see section 1</a>):
</p>
<pre style="margin:0 0 1em 20px; font-size:80%;">
<a href="https://www.haskell.org/cabal/download.html">cabal-install-3.2.0.0-x86_64-unknown-mingw32.zip</a>  <i>(  5 MB)</i>
<a href="https://downloads.haskell.org/~ghc/8.10.2/">ghc-8.10.3-x86_64-unknown-mingw32.tar.xz</a>          <i>(377 MB)</i>
<a href="https://docs.haskellstack.org/en/stable/install_and_upgrade/#manual-download">stack-2.5.1-windows-x86_64.zip</a>                    <i>( 15 MB)</i>
</pre>

<b name="footnote_02">[2]</b> ***<code>stack.yaml</code> versus a <code>.cabal</code> file*** [↩](#anchor_02)

<p style="margin:0 0 1em 20px;">
Differences between a <code>stack.yaml</code> file and a <code>.cabal</code> file &#8213; as described in the <a href="https://docs.haskellstack.org/en/stable/stack_yaml_vs_cabal_package_file/">Online Stack documentation</a> &#8213; can be resumed as follows: 
<ul>
<li><a href="https://www.haskell.org/cabal/">Cabal</a> is a build system, which is used by <a href="https://docs.haskellstack.org/en/stable/README/">Stack</a>. Cabal defines the concept of a <i>package</i> (eg. name, version, 0 or more executables, etc.).</li>
<li><a href="https://docs.haskellstack.org/en/stable/README/">Stack</a> is a build tool that works <i>on top</i> of the <a href="https://www.haskell.org/cabal/">Cabal</a> build system, and defines a new concept called a <i>project</i> (eg. GHC options, etc.)</li>
</ul>
</p>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/January 2021* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[article_abela]: http://www.cse.chalmers.se/~abela/master/layout-parsing.html
[cabal_changelog]: https://hackage.haskell.org/package/Cabal/changelog
[cabal_downloads]: https://www.haskell.org/cabal/download.html
[dotty_examples]: https://github.com/michelou/dotty-examples
[ghc_parser]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/parser
[github_markdown]: https://github.github.com/gfm/
[graalvm_examples]: https://github.com/michelou/graalvm-examples
[haskell]: https://www.haskell.org
[haskell_downloads]: https://downloads.haskell.org/ghc/latest/
[haskell_relnotes]: https://downloads.haskell.org/~ghc/8.10.2/docs/html/users_guide/8.10.2-notes.html
[hlint_changelog]: https://hackage.haskell.org/package/hlint-3.2.3/changelog
[hlint_downloads]: https://hackage.haskell.org/package/hlint
[hlint_readme]: https://hackage.haskell.org/package/hlint-3.2#readme
[hpack_changelog]: https://hackage.haskell.org/package/hpack-0.34.2/changelog
[hpack_downloads]: https://hackage.haskell.org/package/hpack
[hpack_readme]: https://github.com/sol/hpack#readme
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
