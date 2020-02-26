# <span id="top">Haskell examples</span> <span style="size:30%;"><a href="../README.md">⬆</a></span>

<table style="font-family:Helvetica,Arial;font-size:14px;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:120px;"><a href="https://www.haskell.org/"><img src="https://wiki.haskell.org/wikiupload/6/62/Double_lambda.png" width="120"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">Directory <strong><code>examples\</code></strong> contains <a href="https://www.haskell.org/" alt="Haskell">Haskell</a> code examples coming from various websites - mostly from the <a href="https://www.haskell.org/">Haskell project</a>.
  </td>
  </tr>
</table>

We can build/run each example in directory [**`examples\`**](.) using [**`cabal`**][cabal], [**`mvn`**][apache_maven_cli] or [**`stack`**][stack_userguide] as an alternative to the **`build`** batch command.

In the following we explain in more detail the build tools available in the [**`Factorial\`**](Factorial/) example (and also in other examples from directory [**`examples\`**](./)):

<pre style="font-size:80%;">
<b>&gt; cd</b>
H:\examples\Factorial
</pre>

## Factorial

The directory structure of project `Factorial` looks as follows:
<pre style="font-size:80%;">
<b>&gt; tree /a /f . | findstr /v "^[A-Z]"</b>
|   .gitignore
|   .hlint.yaml
|   build.bat
|   Factorial.cabal
|   stack.yaml
|   Setup.hs
|
\---app
        Main.hs
</pre>

Command `cabal run all` builds and execute the Haskell application:
<pre style="font-size:80%;">
<b>&gt; where cabal</b>
C:\opt\ghc-8.8.2\bin\cabal.exe
&nbsp;
<b>&gt; cabal run all</b>
Resolving dependencies...
Build profile: -w ghc-8.8.2 -O1
In order, the following will be built (use -v for more details):
 - Factorial-0.1.0.0 (exe:Factorial) (first run)
Configuring executable 'Factorial' for Factorial-0.1.0.0..
Preprocessing executable 'Factorial' for Factorial-0.1.0.0..
Building executable 'Factorial' for Factorial-0.1.0.0..
[1 of 1] Compiling Main             ( app\Main.hs, H:\examples\Factorial\dist-newstyle\build\x86_64-windows\ghc-8.8.2\Factorial-0.1.0.0\x\Factorial\build\Factorial\Factorial-tmp\Main.o )
Linking H:\examples\Factorial\dist-newstyle\build\x86_64-windows\ghc-8.8.2\Factorial-0.1.0.0\x\Factorial\build\Factorial\Factorial.exe ...
factorialRec(5) =120
factorialRec2(5)=120
factorialFold(5)=120
factorialProd(5)=120
</pre>

Command `stack run` builds and executes the Haskell application:
<pre style="font-size:80%;">
$ where stack
C:\opt\ghc-8.8.2\stack\stack.exe
&nbsp;
<b>&gt; stack run</b>
Stack has not been tested with GHC versions above 8.6, and using 8.8.2, this may fail
Stack has not been tested with Cabal versions above 2.4, but version 3.0.1.0 was found, this may fail
Building all executables for `Factorial' once. After a successful build of all of them, only specified executables will be rebuilt.
Factorial> configure (exe)
Configuring Factorial-0.1.0.0...
Factorial> build (exe)
Preprocessing executable 'Factorial' for Factorial-0.1.0.0..
Building executable 'Factorial' for Factorial-0.1.0.0..
[1 of 1] Compiling Main
Linking .stack-work\dist\29cc6475\build\Factorial\Factorial.exe ...
Factorial> copy/register
Installing executable Factorial in H:\examples\Factorial\.stack-work\install\95d34d36\bin
factorialRec(5) =120
factorialRec2(5)=120
factorialFold(5)=120
factorialProd(5)=120
</pre>

Command `build clean run` builds and executes the Haskell application:
<pre style="font-size:80%;">
<b>&gt; where build</b>
H:\examples\Factorial\build.bat
&nbsp;
<b>&gt; build clean run</b>
factorialRec(5) =120
factorialRec2(5)=120
factorialFold(5)=120
factorialProd(5)=120
</pre>

Command `build -debug clean run` also displays the internally executed commands:
<pre style="font-size:80%;">
<b>&gt; build -debug clean run</b>
[build] _CLEAN=1 _COMPILE=1 _DOC=0 _RUN=1 _VERBOSE=0
[build] rmdir /s /q "H:\examples\Factorial\target"
[build] ghc.exe -Wall -Werror -o "H:\examples\Factorial\target\Main.exe" -hidir "H:\examples\Factorial\target\gen" -odir "H:\examples\Factorial\target\gen"  "H:\examples\Factorial\app\Main.hs"
[1 of 1] Compiling Main             ( H:\examples\Factorial\app\Main.hs, H:\examples\Factorial\target\gen\Main.o )
Linking H:\examples\Factorial\target\Main.exe ...
[build] H:\examples\Factorial\target\Main.exe
factorialRec(5) =120
factorialRec2(5)=120
factorialFold(5)=120
factorialProd(5)=120
[build] _EXITCODE=0
</pre>

## <span id="footnotes">Footnotes</span>
<!--
<a name="footnote_01">[1]</a> ***hlint installation*** [↩](#anchor_01)

<p style="margin:0 0 1em 20px;">
We use <a href="https://www.haskell.org/cabal/"><code>cabal</code></a> to install package <a href="https://hackage.haskell.org/package/hlint"><code>hlint</code></a>; see  document <a href="CABAL.md"><code>CABAL.md</code></a> for more information about its usage.
</p>
-->

***

*[mics](https://lampwww.epfl.ch/~michelou/)/February 2020* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[apache_maven_cli]: https://maven.apache.org/ref/3.6.3/maven-embedder/cli.html
[article_abela]: http://www.cse.chalmers.se/~abela/master/layout-parsing.html
[cabal]: https://www.haskell.org/cabal/
[cabal_changelog]: https://hackage.haskell.org/package/Cabal/changelog
[cabal_downloads]: https://www.haskell.org/cabal/download.html
[dotty_examples]: https://github.com/michelou/dotty-examples
[ghc_parser]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/parser
[git_cli]: https://git-scm.com/docs/git
[git_releases]: https://git-scm.com/download/win
[git_relnotes]: https://raw.githubusercontent.com/git/git/master/Documentation/RelNotes/2.25.0.txt
[github_markdown]: https://github.github.com/gfm/
[graalsqueak_examples]: https://github.com/michelou/graalsqueak-examples
[haskell]: https://www.haskell.org
[haskell_downloads]: https://downloads.haskell.org/~ghc/8.8.2/
[haskell_relnotes]: https://downloads.haskell.org/~ghc/8.8.2/docs/html/users_guide/8.8.2-notes.html
[hlint_changelog]: https://hackage.haskell.org/package/hlint-2.2.11/changelog
[hlint_downloads]: https://hackage.haskell.org/package/hlint
[kotlin_examples]: https://github.com/michelou/kotlin-examples
[llvm_examples]: https://github.com/michelou/llvm-examples
[man1_awk]: https://www.linux.org/docs/man1/awk.html
[man1_diff]: https://www.linux.org/docs/man1/diff.html
[man1_file]: https://www.linux.org/docs/man1/file.html
[man1_grep]: https://www.linux.org/docs/man1/grep.html
[man1_more]: https://www.linux.org/docs/man1/more.html
[man1_mv]: https://www.linux.org/docs/man1/mv.html
[man1_rmdir]: https://www.linux.org/docs/man1/rmdir.html
[man1_sed]: https://www.linux.org/docs/man1/sed.html
[man1_wc]: https://www.linux.org/docs/man1/wc.html
[stack_userguide]: https://docs.haskellstack.org/en/stable/GUIDE/
[unix_opt]: https://tldp.org/LDP/Linux-Filesystem-Hierarchy/html/opt.html
[windows_batch_file]: https://en.wikibooks.org/wiki/Windows_Batch_Scripting
[windows_limitation]: https://support.microsoft.com/en-gb/help/830473/command-prompt-cmd-exe-command-line-string-limitation
[windows_subst]: https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/subst
[zip_archive]: https://www.howtogeek.com/178146/htg-explains-everything-you-need-to-know-about-zipped-files/
