# <span id="top">Playing with Haskell on Windows</span>

<table style="font-family:Helvetica,Arial;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:100px;"><a href="https://www.haskell.org/" rel="external"><img style="border:0;" src="./docs/images/Double_lambda.png" width="100" alt="Haskell project"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">This repository gathers <a href="https://www.haskell.org/" rel="external">Haskell</a> examples coming from various websites and books.<br/>
  It also includes several build scripts (<a href="https://en.wikibooks.org/wiki/Windows_Batch_Scripting" rel="external">batch files</a>, <a href="https://maven.apache.org/guides/introduction/introduction-to-the-pom.html">Maven scripts</a>) for experimenting with <a href="https://www.haskell.org/" rel="external">Haskell</a> on a Windows machine.
  </td>
  </tr>
</table>

[Ada][ada_examples], [Akka][akka_examples], [C++][cpp_examples], [Dart][dart_examples], [Deno][deno_examples], [Docker][docker_examples], [Flix][flix_examples], [Golang][golang_examples], [GraalVM][graalvm_examples], [Kafka][kafka_examples], [Kotlin][kotlin_examples], [LLVM][llvm_examples], [Modula-2][m2_examples], [Node.js][nodejs_examples], [Rust][rust_examples], [Scala 3][scala3_examples], [Spark][spark_examples], [Spring][spring_examples], [TruffleSqueak][trufflesqueak_examples] and [WiX Toolset][wix_examples] are other topics we are continuously monitoring.

## <span id="proj_deps">Project dependencies</span>

This project relies on the following external software for the **Microsoft Windows** platform:

- [Cabal 3.8][cabal_downloads] <sup id="anchor_01">[1](#footnote_01)</sup> ([*changelog*][cabal_changelog])
- [Git 2.44][git_downloads] ([*release notes*][git_relnotes])
- [Haskell 8.10 LTS][haskell_lts_downloads] ([*release notes*][haskell_lts_relnotes])

> **&#9755;** ***Haskell packages***<br/>
> We present the installed Haskell packages in document [`PACKAGES.md`](./PACKAGES.md).

Optionally one may also install the following software:

- [Apache Maven 3.9][apache_maven] ([requires Java 8+][apache_maven_history])  ([*release notes*][apache_maven_relnotes])
- [Haskell 9.8][haskell_latest_downloads] ([*release notes*][haskell_latest_relnotes])
- [Stack 2.13][stack_downloads] ([*changelog*][stack_changelog])
- [Temurin OpenJDK 11][temurin_openjdk11] ([*release notes*][temurin_openjdk11_relnotes], for Maven)

> **&#9755;** ***Installation policy***<br/>
> When possible we install software from a [Zip archive][zip_archive] rather than via a Windows installer. In our case we defined **`C:\opt\`** as the installation directory for optional software tools (*in reference to* the [`/opt/`][unix_opt] directory on Unix).

For instance our development environment looks as follows (*February 2024*) <sup id="anchor_02">[2](#footnote_02)</sup>:

<pre style="font-size:80%;">
C:\opt\apache-maven\             <i>( 10 MB)</i>
C:\opt\ghc-8.10.7\               <i>(2.5 GB)</i>
C:\opt\ghc-9.6.4\                <i>(2.6 GB)</i>
C:\opt\ghc-9.8.1\                <i>(2.6 GB)</i>
C:\opt\Git\                      <i>(358 MB)</i>
C:\opt\jdk-temurin-11.0.22_7\    <i>(303 MB)</i>
C:\opt\stack-2.13.1\             <i>( 74 MB)</i>
</pre>

<!--
> **:mag_right:** GHC features two backends: the default native code generator (option `-fasm`) and the LLVM (version 7) code generator (option `-fllvm`). The C code generator is deprecated since GHC 7.0.
-->

> **:mag_right:** [Git for Windows][git_downloads] provides a BASH emulation used to run [**`git`**][git_cli] from the command line (as well as over 250 Unix commands like [**`awk`**][man1_awk], [**`diff`**][man1_diff], [**`file`**][man1_file], [**`grep`**][man1_grep], [**`more`**][man1_more], [**`mv`**][man1_mv], [**`rmdir`**][man1_rmdir], [**`sed`**][man1_sed] and [**`wc`**][man1_wc]).

## <span id="structure">Directory structure</span> [**&#x25B4;**](#top)

This project is organized as follows:

<pre style="font-size:80%;">
docs\
examples\{<a href="examples/README.md">README.md</a>, <a href="examples/HelloWorld/">HelloWorld</a>, ..}
parconc-examples\{<a href="parconc-examples/README.md">README.md</a>, ..}
<a href="PACKAGES.md">PACKAGES.md</a>
README.md
<a href="RESOURCES.md">RESOURCES.md</a>
<a href="REPL.md">REPL.md</a>
<a href="setenv.bat">setenv.bat</a>
</pre>

where

- directory [**`docs\`**](docs/) contains [Haskell] related papers/articles.
- directory [**`examples\`**](examples/) contains [Haskell] examples grabbed from various websites (see file [**`examples\README.md`**](examples/README.md)).
- directory [**`parconc-examples`**](parconc-examples/) contains [Haskell] examples from Simon Marlow's [book][book_parconc] (see file [**`parconc-examples\README.md`**](parconc-examples/README.md)).
- file [**`PACKAGES.md`**](PACKAGES.md) presents the [Haskell] packages our projects depend on.
- file **`README.md`** is the [Markdown][github_markdown] document for this page.
- file [**`REPL.md`**](REPL.md) presents [GHCi] usage examples.
- file [**`RESOURCES.md`**](RESOURCES.md) gathers [Haskell] related informations.
- file [**`setenv.bat`**](setenv.bat) is the batch command for setting up our environment.

<!--
> **:mag_right:** We use [VS Code][microsoft_vscode] with the extension [Markdown Preview Github Styling](https://marketplace.visualstudio.com/items?itemName=bierner.markdown-preview-github-styles) to edit our Markdown files (see article ["Mastering Markdown"](https://guides.github.com/features/mastering-markdown/) from [GitHub Guides][github_guides].
-->

We also define a virtual drive &ndash; e.g. drive **`H:`** &ndash; in our working environment in order to reduce/hide the real path of our project directory (see article ["Windows command prompt limitation"][windows_limitation] from Microsoft Support).
> **:mag_right:** We use the Windows external command [**`subst`**][windows_subst] to create virtual drives; for instance:
>
> <pre style="font-size:80%;">
> <b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/subst">subst</a> H: <a href="https://learn.microsoft.com/en-us/windows/deployment/usmt/usmt-recognized-environment-variables#variables-that-are-recognized-only-in-the-user-context">%USERPROFILE%</a>\workspace\haskell-examples</b>
> </pre>

In the next section we give a brief description of the [batch files][windows_batch_file] present in this project.

## <span id="commands">Batch commands</span>

We distinguish different sets of batch commands:

1. [**`setenv.bat`**](setenv.bat) - This batch command makes external tools such as [**`cabal.exe`**][cabal_userguide], [**`git.exe`**][git_cli], [**`sh.exe`**][sh_cli] and [**`stack.exe`**][stack_userguide] directly available from the command prompt (see section [**Project dependencies**](#proj_deps)).

   <pre style="font-size:80%;">
   <b>&gt; <a href="./setenv.bat">setenv</a> help</b>
   Usage: setenv { &lt;option&gt; | &lt;subcommand&gt; }
   &nbsp;
     Options:
       -debug      print commands executed by this script
       -verbose    print progess messages
   &nbsp;
     Subcommands:
       help        print this help message
   </pre>

2. [**`examples\*\build.bat`**](examples/Factorial/build.bat) - Each example can be built/run using the [**`build`**](examples/Factorial/build.bat) command.<br/>

    <pre style="font-size:80%;">
    <b>&gt; <a href="examples/Factorial/build.bat">build</a></b>
    Usage: build { &lt;option&gt; | &lt;subcommand&gt; }
    &nbsp;
    Options:
      -debug      print commands executed by this script
      -timer      print total execution time
      -verbose    print progress messages
    &nbsp;
    Subcommands:
      clean       delete generated files
      compile     generate program executable
      doc         generate HTML documentation
      help        print this help message
      run         execute the generated program
    </pre>

## <span id="usage">Usage examples</span> [**&#x25B4;**](#top)

### `setenv.bat`

We execute command [**`setenv`**](setenv.bat) once to setup our development environment; it makes external tools such as [**`cabal.exe`**][cabal_userguide], [**`git.exe`**][git_cli], [**`sh.exe`**][sh_cli] and [**`stack.exe`**][stack_userguide] directly available from the command prompt:

<pre style="font-size:80%;">
<b>&gt; <a href="setenv.bat">setenv</a></b>
Tool versions:
   cabal 3.8.1.0, ghc version 8.10.7, stack 2.13.1, haddock 2.24.2
   hlint v3.5, hpack 0.35.1, htfpp 0.14.0.6, ormolu 0.7.0.0
   java 11.0.22, mvn 3.9.6, git 2.44.0.windows.1, diff 3.10

<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> cabal git hlint hpack sh stack</b>
C:\opt\ghc-8.10.7\bin\cabal.exe
C:\opt\Git\bin\git.exe
C:\opt\Git\mingw64\bin\git.exe
<a href="https://docs.microsoft.com/en-us/windows/deployment/usmt/usmt-recognized-environment-variables#bkmk-2">%APPDATA%</a>\Cabal\bin\hlint.exe
<a href="https://docs.microsoft.com/en-us/windows/deployment/usmt/usmt-recognized-environment-variables#bkmk-2">%APPDATA%</a>\Cabal\bin\hpack.exe
C:\opt\Git\bin\sh.exe
C:\opt\Git\usr\bin\sh.exe
C:\opt\stack-2.13.1\stack.exe
</pre>

Command [**`setenv -verbose`**](setenv.bat) also displays the tool paths and defined variables:

<pre style="font-size:80%;">
<b>&gt; <a href="setenv.bat">setenv</a> -verbose</b>
Tool versions:
   cabal 3.8.1.0, ghc version 8.10.7, stack 2.13.1, haddock 2.24.2
   hlint v3.5, hpack 0.35.1, htfpp 0.14.0.6, ormolu 0.7.0.0
   java 11.0.20, mvn 3.9.6, git 2.44.0.windows.1, diff 3.10
Tool paths:
   <a href="https://docs.microsoft.com/en-us/windows/deployment/usmt/usmt-recognized-environment-variables#bkmk-2">%APPDATA%</a>\cabal\bin\cabal.exe
   C:\opt\ghc-8.10.7\bin\ghc.exe
   C:\opt\stack\stack.exe
   C:\opt\ghc-8.10.7\bin\haddock.exe
   %APPDATA%\Cabal\bin\hlint.exe
   %APPDATA%\Cabal\bin\hpack.exe
   %APPDATA%\Cabal\bin\htfpp.exe
   %APPDATA%\Cabal\bin\ormolu.exe
   C:\opt\jdk-temurin-11.0.22_7\bin\java.exe
   C:\opt\apache-maven\bin\mvn.cmd
   C:\opt\Git\bin\git.exe
   C:\opt\Git\mingw64\bin\git.exe
   C:\opt\Git\usr\bin\diff.exe
Environment variables:
   "CABAL_DIR=%APPDATA%\cabal"
   "GHC_HOME=C:\opt\ghc-8.10.7"
   "JAVA_HOME=C:\opt\jdk-temurin-11.0.22_7"
   "MAVEN_HOME=C:\opt\apache-maven"
   "STACK_HOME=C:\opt\stack"
</pre>

## <span id="footnotes">Footnotes</span> [**&#x25B4;**](#top)

<span id="footnote_01">[1]</span> ***Cabal compatibility*** [↩](#anchor_01)

<dl><dd>
<table>
<tr><th>GHC version</th><th>Cabal version</th></tr>
<tr><td>9.8</td><td>?</td></tr>
<tr><td>9.6</td><td>?</td></tr>
<tr><td>9.4</td><td>3.8 or later</td></tr>
<tr><td>8.x, 9.x</td><td>3.4</td></tr>
<tr><td>8.x</td><td>3.2</td</tr>
</table>
</dd></dl>

<span id="footnote_02">[2]</span> ***Downloads*** [↩](#anchor_02)

<dl><dd>
In our case we downloaded the following installation files (<a href="#proj_deps">see section 1</a>):
</dd>
<dd>
<pre style="font-size:80%;">
<a href="https://maven.apache.org/download.cgi">apache-maven-3.9.6-bin.zip</a>                         <i>( 10 MB)</i>
<a href="https://www.haskell.org/cabal/download.html">cabal-install-3.8.1.0-x86_64-unknown-mingw32.zip</a>   <i>(  5 MB)</i>
<a href="https://downloads.haskell.org/ghc/8.10.7/">ghc-8.10.7-x86_64-unknown-mingw32.tar.xz </a>          <i>(414 MB)</i>
<a href="https://downloads.haskell.org/ghc/9.6.1/">ghc-9.6.1-x86_64-unknown-mingw32.tar.xz </a>           <i>(471 MB)</i>
<a href="https://adoptium.net/releases.html?variant=openjdk11&jvmVariant=hotspot">OpenJDK11U-jdk_x64_windows_hotspot_11.0.22_7.zip</a>   <i>( 99 MB)</i>
<a href="https://git-scm.com/download/win">PortableGit-2.44.0-64-bit.7z.exe</a>                   <i>( 41 MB)</i>
<a href="https://github.com/commercialhaskell/stack/releases">stack-2.13.1-windows-x86_64.zip</a>                    <i>( 15 MB)</i>
</pre>
</dd>
<dd>
<table>
<tr><th>Version</th><th>LLVM</th><th>Archive</th><th>Installation</th><th>Remarks</th></tr>
<tr><td><a href="https://downloads.haskell.org/ghc/9.8.1/docs/users_guide/9.8.1-notes.html">9.6.3</a></td><td></td><td></td><td></td><td><a href="https://downloads.haskell.org/ghc/9.8.1/docs/users_guide/win32-dlls.html">Running GHC on Win32 systems</a></td></tr>
<tr><td><a href="https://downloads.haskell.org/ghc/9.6.3/docs/users_guide/9.6.3-notes.html">9.6.3</a></td><td>9-12</td><td>479 MB</td><td>2.78 GB</td><td><a href="https://downloads.haskell.org/ghc/9.6.3/docs/users_guide/win32-dlls.html">Running GHC on Win32 systems</a></td></tr>
<tr><td><a href="https://downloads.haskell.org/ghc/9.4.4/docs/users_guide/9.4.4-notes.html">9.4.4</a></td><td>9-12</td><td>479 MB</td><td>2.78 GB</td><td><a href="https://downloads.haskell.org/ghc/9.4.4/docs/users_guide/win32-dlls.html">Running GHC on Win32 systems</a></td></tr>
<tr><td><a href="https://downloads.haskell.org/ghc/9.2.2/docs/html/users_guide/9.2.2-notes.html">9.2.2</a> <sup><b>a)</b></sup></td><td>10,11</td><td>331 MB</td><td>1.85 GB</td><td><a href="https://downloads.haskell.org/ghc/9.2.2/docs/html/users_guide/9.2.2-notes.html#included-libraries">Included libraries</a></td></tr>
<tr><td><a href="https://downloads.haskell.org/~ghc/9.0.2/docs/html/users_guide/9.0.1-notes.html">9.0.1</a></td><td>9</td><td>236 MB</td><td>2.64 GB</td><td><a href="https://downloads.haskell.org/~ghc/9.0.2/docs/html/users_guide/9.0.1-notes.html#included-libraries">Included libraries</a></td></tr>
<tr><td><a href="https://downloads.haskell.org/~ghc/8.10.7/docs/html/users_guide/8.10.7-notes.html">8.10.7</a></td><td>9-12</td><td>414 MB</td><td>2.80 GB</td><td><a href="https://downloads.haskell.org/~ghc/8.10.7/docs/html/users_guide/8.10.7-notes.html#included-libraries">Included libraries</a></td></tr>
</table>
<span style="font-size:80%;"><sup><b>a)</b></sup> We observe a big size difference between version 9.2.2 and the others versions listed above.The difference is located in the <code>lib\x8_64-windows-ghc-9.x.x\</code> directory, e.g. <code>array-0.5.4.0\</code> with 1.7 MB versus 4.3 MB due to the absence/presence of the profiling object files (<code>.p_o</code> extension).</span>
</dd></dl>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/February 2024* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[ada_examples]: https://github.com/michelou/ada-examples
[akka_examples]: https://github.com/michelou/akka-examples
[apache_maven]: https://maven.apache.org/download.cgi
[apache_maven_cli]: https://maven.apache.org/ref/current/maven-embedder/cli.html
[apache_maven_history]: https://maven.apache.org/docs/history.html
[apache_maven_relnotes]: https://maven.apache.org/docs/3.9.6/release-notes.html
[book_parconc]: https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/
[cabal_changelog]: https://github.com/haskell/cabal/blob/master/release-notes/Cabal-3.8.1.0.md
[cabal_downloads]: https://downloads.haskell.org/~cabal/
[cabal_userguide]: https://www.haskell.org/cabal/users-guide/
[cpp_examples]: https://github.com/michelou/cpp-examples
[dart_examples]: https://github.com/michelou/dart-examples
[deno_examples]: https://github.com/michelou/deno-examples
[docker_examples]: https://github.com/michelou/docker-examples
[flix_examples]: https://github.com/michelou/flix-examples
[ghc_userguide]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/using.html
[ghci]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html
[git_cli]: https://git-scm.com/docs/git
[git_downloads]: https://git-scm.com/download/win
[git_relnotes]: https://raw.githubusercontent.com/git/git/master/Documentation/RelNotes/2.44.0.txt
[github_markdown]: https://github.github.com/gfm/
[golang_examples]: https://github.com/michelou/golang-examples
[graalvm_examples]: https://github.com/michelou/graalvm-examples
[haddock_userguide]: https://www.haskell.org/haddock/doc/html/index.html
[haskell]: https://www.haskell.org
[haskell_lts_downloads]: https://downloads.haskell.org/ghc/8.10.7/
[haskell_lts_relnotes]: https://downloads.haskell.org/ghc/8.10.7/docs/html/users_guide/8.10.7-notes.html
[haskell_latest_downloads]: https://downloads.haskell.org/ghc/latest/
<!--
### https://www.haskell.org/ghc/blog.html
9.4.4 => https://www.haskell.org/ghc/blog/20221224-ghc-9.4.4-released.html
9.6.1 => https://www.haskell.org/ghc/blog/20230310-ghc-9.6.1-released.html
9.6.3 => https://www.haskell.org/ghc/blog/20230925-ghc-9.6.3-released.html
9.6.4 => https://www.haskell.org/ghc/blog/20240109-ghc-9.6.4-released.html
9.8.1 => https://www.haskell.org/ghc/blog/20231009-ghc-9.8.1-released.html
-->
[haskell_latest_relnotes]: https://www.haskell.org/ghc/blog/20230310-ghc-9.6.1-released.html
[kafka_examples]: https://github.com/michelou/kafka-examples
[kotlin_examples]: https://github.com/michelou/kotlin-examples
[llvm_examples]: https://github.com/michelou/llvm-examples
[m2_examples]: https://github.com/michelou/m2-examples
[man1_awk]: https://www.linux.org/docs/man1/awk.html
[man1_diff]: https://www.linux.org/docs/man1/diff.html
[man1_file]: https://www.linux.org/docs/man1/file.html
[man1_grep]: https://www.linux.org/docs/man1/grep.html
[man1_more]: https://www.linux.org/docs/man1/more.html
[man1_mv]: https://www.linux.org/docs/man1/mv.html
[man1_rmdir]: https://www.linux.org/docs/man1/rmdir.html
[man1_sed]: https://www.linux.org/docs/man1/sed.html
[man1_wc]: https://www.linux.org/docs/man1/wc.html
[nodejs_examples]: https://github.com/michelou/nodejs-examples
<!--
11.0.9  -> https://mail.openjdk.java.net/pipermail/jdk-updates-dev/2020-October/004007.html
11.0.11 -> https://mail.openjdk.java.net/pipermail/jdk-updates-dev/2021-April/005860.html
11.0.12 -> https://mail.openjdk.java.net/pipermail/jdk-updates-dev/2021-July/006954.html
11.0.13 -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2021-October/009368.html
11.0.14 -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2022-January/011643.html
11.0.15 -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2022-April/014104.html
11.0.16 -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2022-July/016017.html
11.0.17 -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2022-October/018119.html
11.0.18 -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2023-January/020111.html
11.0.19 -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2023-April/021900.html
11.0.20 -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2023-July/024064.html
11.0.21 -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2023-October/026351.html
11.0.22 -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2024-January/029215.html
-->
[temurin_openjdk11]: https://adoptium.net/?variant=openjdk11&jvmVariant=hotspot
[temurin_openjdk11_relnotes]: https://mail.openjdk.java.net/pipermail/jdk-updates-dev/2022-April/014104.html
<!--
17.0.7  -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2023-April/021899.html
17.0.8  -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2023-July/024063.html
17.0.9  -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2023-October/026352.html
17.0.10 -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2024-January/029089.html
-->
[temurin_openjdk17]: https://adoptium.net/releases.html?variant=openjdk17&jvmVariant=hotspot
[temurin_openjdk17_bugfixes]: https://www.oracle.com/java/technologies/javase/17-0-2-bugfixes.html
[temurin_openjdk17_relnotes]: https://mail.openjdk.org/pipermail/jdk-updates-dev/2023-October/026352.html
<!--
21_35   -> https://adoptium.net/fr/temurin/release-notes/?version=jdk-21+35
21.0.1  -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2023-October/026351.html
21.0.2  -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2024-January/029090.html
-->
[temurin_openjdk21]: https://adoptium.net/releases.html?variant=openjdk21&jvmVariant=hotspot
[temurin_openjdk21_bugfixes]: https://www.oracle.com/java/technologies/javase/17-0-2-bugfixes.html
[temurin_openjdk21_relnotes]: https://mail.openjdk.org/pipermail/jdk-updates-dev/2023-October/026351.html
[rust_examples]: https://github.com/michelou/rust-examples
[scala3_examples]: https://github.com/michelou/dotty-examples
[sh_cli]: https://man7.org/linux/man-pages/man1/sh.1p.html
[spring_examples]: https://github.com/michelou/spring-examples
[spark_examples]: https://github.com/michelou/spark-examples
[stack_changelog]: https://docs.haskellstack.org/en/stable/ChangeLog/
[stack_downloads]: https://github.com/commercialhaskell/stack/releases
[stack_userguide]: https://docs.haskellstack.org/en/stable/GUIDE/
[trufflesqueak_examples]: https://github.com/michelou/trufflesqueak-examples
[unix_opt]: https://tldp.org/LDP/Linux-Filesystem-Hierarchy/html/opt.html
[windows_batch_file]: https://en.wikibooks.org/wiki/Windows_Batch_Scripting
[windows_limitation]: https://support.microsoft.com/en-gb/help/830473/command-prompt-cmd-exe-command-line-string-limitation
[windows_subst]: https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/subst
[wix_examples]: https://github.com/michelou/wix-examples
[zip_archive]: https://www.howtogeek.com/178146/htg-explains-everything-you-need-to-know-about-zipped-files/
