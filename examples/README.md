# <span id="top">Haskell examples</span> <span style="size:30%;"><a href="../README.md">⬆</a></span>

<table style="font-family:Helvetica,Arial;font-size:14px;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:100px;"><a href="https://www.haskell.org/"><img style="border:0;" src="https://wiki.haskell.org/wikiupload/6/62/Double_lambda.png" width="100" alt="Haskell logo"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">Directory <strong><code>examples\</code></strong> contains <a href="https://www.haskell.org/" alt="Haskell">Haskell</a> code examples coming from various websites - mostly from the <a href="https://www.haskell.org/" rel="external">Haskell project</a>.
  </td>
  </tr>
</table>

In the following we present the two examples [**`Factorial`**](#factorial) and [**`QuickSort`**](#quicksort).

We can build/run code examples in directory [`examples`](./) in several ways. For instance we have the following configuration files for project [**`Factorial`**](#factorial):

| Build tool                    | Configuration file                           | Parent file                |
|-------------------------------|----------------------------------------------|----------------------------|
| [**`build.bat`**](Factorial/build.bat) | [**`Factorial.cabal`**](Factorial/Factorial.cabal) <sup id="anchor_01">[1](#footnote_01)</sup> | n.a.                       |
| [**`cabal.exe`**][cabal_cli]      | [**`Factorial.cabal`**](Factorial/Factorial.cabal) | n.a.             |
| [**`mvn.cmd`**][apache_maven_cli] | [**`pom.xml`**](Factorial/pom.xml)       | [**`pom.xml`**](./pom.xml) |
| [**`stack.exe`**][stack_cli]      | [**`stack.yaml`**](Factorial/stack.yaml) | n.a.                       |


## <span id="factorial">Factorial</span>

The directory structure of project `Factorial` looks as follows:
<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/cd">cd</a></b>
H:\examples\<a href="Factorial/">Factorial</a>
&nbsp;
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/tree">tree</a> /a /f . | <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/findstr">findstr</a> /v "^[A-Z]"</b>
|   .gitignore
|   <a href="Factorial/.hlint.yaml">.hlint.yaml</a>
|   <a href="Factorial/build.bat">build.bat</a>
|   <a href="Factorial/Factorial.cabal">Factorial.cabal</a>
|   <a href="Factorial/stack.yaml">stack.yaml</a>
|   Setup.hs
|
\---app
        <a href="./Factorial/app/Main.hs">Main.hs</a>
</pre>

### <span id="factorial_cabal">***Cabal***</span>

Command [`cabal run all`][cabal_cli] builds and execute the [Haskell] application (configuration file [`Factorial.cabal`](./Factorial/Factorial.cabal)):
<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> cabal</b>
C:\opt\ghc-8.10.7\bin\cabal.exe
&nbsp;
<b>&gt; <a href="https://cabal.readthedocs.io/en/3.2/intro.html#a-tool-for-working-with-packages">cabal</a> run all</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - Factorial-0.1.0.0 (exe:Factorial) (first run)
Configuring executable 'Factorial' for Factorial-0.1.0.0..
Preprocessing executable 'Factorial' for Factorial-0.1.0.0..
Building executable 'Factorial' for Factorial-0.1.0.0..
[1 of 1] Compiling Main             ( app\Main.hs, H:\examples\Factorial\dist-newstyle\build\x86_64-windows\ghc-8.10.7\Factorial-0.1.0.0\x\Factorial\build\Factorial\Factorial-tmp\Main.o )
Linking H:\examples\Factorial\dist-newstyle\build\x86_64-windows\ghc-8.10.7\Factorial-0.1.0.0\x\Factorial\build\Factorial\Factorial.exe ...
factorialRec(5) =120
factorialRec2(5)=120
factorialFold(5)=120
factorialProd(5)=120
</pre>

### <span id="factorial_stack">***Stack***</span>

Command [`stack run`][stack_cli] builds and executes the [Haskell] application (configuration file [`stack.yaml`](./Factorial/stack.yaml)):
<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1" rel="external">where</a> stack</b>
C:\opt\stack-2.7.5\stack.exe
&nbsp;
<b>&gt; <a href="https://docs.haskellstack.org/en/stable/build_command/">stack</a> --silent run</b>
factorialRec(5) =120
factorialRec2(5)=120
factorialFold(5)=120
factorialProd(5)=120
</pre>

> **:mag_right:** We can build a profile-version of the project and execute the profile-instrumented [Haskell] application as follows:
> <pre style="font-size:80%;">
> <b>&gt; <a href="https://docs.haskellstack.org/en/stable/build_command/">stack</a> build --profile</b>
> &nbsp;
> <b>&gt; <a href="https://docs.haskellstack.org/en/stable/build_command/">stack</a> exec target\dist\29cc6475\build\Factorial\Factorial.exe -- +RTS -p</b>
> factorialRec(5) =120
> factorialRec2(5)=120
> factorialFold(5)=120
> factorialProd(5)=120
> </pre>
> Profiling results are stored in file `Factorial.prof`:
> <pre style="font-size:80%;">
> <b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/more">more</a> Factorial.prof</b>
>         Tue Mar 03 22:21 2020 Time and Allocation Profiling Report  (Final)
> &nbsp;
>            Factorial.exe +RTS -p -RTS
> &nbsp;
>         total time  =        0.00 secs   (0 ticks @ 1000 us, 1 processor)
>         total alloc =      56,488 bytes  (excludes profiling overheads)
> &nbsp;
> COST CENTRE MODULE           SRC                         %time %alloc
> &nbsp;
> main        Main             app\Main.hs:(33,1)-(39,13)    0.0   36.0
> CAF         GHC.IO.Handle.FD &lt;entire-module&gt;               0.0   61.5
> &nbsp;
>                                                                                       individual      inherited
> COST CENTRE      MODULE                   SRC                        no.     entries  %time %alloc   %time %alloc
> &nbsp;
> MAIN             MAIN                     &lt;built-in&gt;                 125          0    0.0    0.9     0.0  100.0
>  CAF             GHC.IO.Handle.Text       &lt;entire-module&gt;            176          0    0.0    0.1     0.0    0.1
> [..]
> </pre>

### <span id="factorial_maven">***Maven***</span>

Command [`mvn -q compile exec:exec`][mvn_cli] builds and executes the [Haskell] application (configuration file [`pom.xml`](./Factorial/pom.xml))

<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> mvn.cmd</b>
C:\opt\apache-maven-3.8.5\bin\mvn.cmd
&nbsp;
<b>&gt; <a href="https://maven.apache.org/ref/3.8.5/maven-embedder/cli.html">mvn</a> -q clean compile exec:exec</b>
[1 of 1] Compiling Main             ( app\Main.hs, target\gen\Main.o )
Linking target/Main.exe ...
factorialRec(5) =120
factorialRec2(5)=120
factorialFold(5)=120
factorialProd(5)=120
</pre>

### <span id="factorial_batch">***Batch***</span>

Command [`build clean run`](Factorial/build.bat) builds and executes the [Haskell] application:
<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> build</b>
H:\examples\Factorial\build.bat
&nbsp;
<b>&gt; <a href="Factorial/build.bat">build</a> clean run</b>
factorialRec(5) =120
factorialRec2(5)=120
factorialFold(5)=120
factorialProd(5)=120
</pre>

> **:mag_right:** Commands `build` and `mvn` accept one or more subcommands while `cabal` and `stack` accept only one. For instance the following command lines produce the same result:
> <pre style="font-size:80%;">
> <b>&gt; <a href="Factorial/build.bat">build</a> clean run</b>
> <b>&gt; <a href="https://maven.apache.org/ref/3.8.1/maven-embedder/cli.html">mvn</a> clean compile exec:exec</b>
> <b>&gt; <a href="https://cabal.readthedocs.io/en/3.2/intro.html#a-tool-for-working-with-packages">cabal</a> clean && cabal run all</b>
> <b>&gt; <a href="https://docs.haskellstack.org/en/stable/build_command/">stack</a> clean && stack run</b>
> </pre>

With option `-debug` command [`build`](Factorial/build.bat) also displays several useful informations, e.g.
- Execution environment : display *actual value* of properties, options, variables.
- Conditional processing : compile *only if* target is older than sources.
- Execution transparency : the executed console command with options and arguments can be *copied/run separately*.
<pre style="font-size:80%;">
<b>&gt; <a href="Factorial/build.bat">build</a> -debug run</b>
[build] Properties : _PACKAGE_NAME=Factorial
[build] Options    : _TIMER=0 _VERBOSE=0
[build] Subcommands: _CLEAN=1 _COMPILE=1 _DOC=0 _LINT=0 _RUN=1 _TEST=0
[build] Variables  : "CABAL_DIR=%APPDATA%\cabal"
[build] Variables  : "GHC_HOME=C:\opt\ghc-8.10.7"
[build] 00000000000000 Target : "H:\examples\Factorial\target\Factorial.exe"
[build] 20210208190257 Sources: "H:\examples\Factorial\app\*.hs"
[build] _ACTION_REQUIRED=1
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

## <span id="quicksort">QuickSort</span>

The directory structure of project `QuickSort` looks as follows:
<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/cd">cd</a></b>
H:\examples\QuickSort
&nbsp;
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/tree">tree</a> /a /f . | <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/findstr">findstr</a> /v "^[A-Z]"</b>
|   .gitignore
|   <a href="./QuickSort/build.bat">build.bat</a>
|   <a href="./QuickSort/QuickSort.cabal">QuickSort.cabal</a>
|   Setup.hs
|   <a href="./QuickSort/stack.yaml">stack.yaml</a>
|
\---app
        <a href="./QuickSort/app/Main.hs">Main.hs</a>
</pre>

### <span id="quicksort_cabal">***Cabal***</span>

Command [`cabal run all`][cabal_cli] builds and executes the [Haskell] application (configuration file [`QuickSort.cabal`](./QuickSort/QuickSort.cabal)):

<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> cabal</b>
C:\opt\ghc-8.10.7\bin\cabal.exe
&nbsp;
<b>&gt; <a href="https://man.archlinux.org/man/cabal.1">cabal</a> clean &amp;&amp; <a href="https://man.archlinux.org/man/cabal.1">cabal</a> run all</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - QuickSort-0.1.0.0 (exe:QuickSort) (first run)
Configuring executable 'QuickSort' for QuickSort-0.1.0.0..
Preprocessing executable 'QuickSort' for QuickSort-0.1.0.0..
Building executable 'QuickSort' for QuickSort-0.1.0.0..
[1 of 1] Compiling Main             ( app\Main.hs, H:\examples\QuickSort\dist-newstyle\build\x86_64-windows\ghc-8.10.7\QuickSort-0.1.0.0\x\QuickSort\build\QuickSort\QuickSort-tmp\Main.o )
Linking H:\examples\QuickSort\dist-newstyle\build\x86_64-windows\ghc-8.10.7\QuickSort-0.1.0.0\x\QuickSort\build\QuickSort\QuickSort.exe ...
input list       : [8,4,0,3,1,23,11,18]
sorted(filter)   : [0,1,3,4,8,11,18,23]
sorted(list comp): [0,1,3,4,8,11,18,23]
</pre>

### <span id="quicksort_stack">***Stack***</span>

Command [`stack run`][stack_cli] builds and executes the [Haskell] application (configuration file [`stack.yaml`](./QuickSort/stack.yaml)):

<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> stack</b>
C:\opt\stack-2.7.5\stack.exe
&nbsp;
<b>&gt; <a href="https://docs.haskellstack.org/en/stable/build_command/">stack</a> clean &amp;&amp; <a href="https://docs.haskellstack.org/en/stable/build_command/">stack</a> --silent run</b>
input list       : [8,4,0,3,1,23,11,18]
sorted(filter)   : [0,1,3,4,8,11,18,23]
sorted(list comp): [0,1,3,4,8,11,18,23]
</pre>

### <span id="quicksort_maven">***Maven***</span>

Command `mvn -q compile exec:exec` builds and executes the [Haskell] application (configuration file [`pom.xml`](./QuickSort/pom.xml))

<pre style="font-size:80%;">
<b>&gt; <a href="https://maven.apache.org/ref/current/maven-embedder/cli.html" rel="external">mvn</a> -q clean compile exec:exec</b>
[1 of 1] Compiling Main             ( app\Main.hs, target\gen\Main.o )
Linking target/Main.exe ...
input list       : [8,4,0,3,1,23,11,18]
sorted(filter)   : [0,1,3,4,8,11,18,23]
sorted(list comp): [0,1,3,4,8,11,18,23]
</pre>

### <span id="quicksort_batch">***Batch***</span>

Command [`build clean run`](QuickSort/build.bat) builds and executes the [Haskell] application:
<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1" rel="external">where</a> build</b>
H:\examples\QuickSort\build.bat
&nbsp;
<b>&gt; <a href="QuickSort/build.bat">build</a> clean run</b>
input list       : [8,4,0,3,1,23,11,18]
sorted(filter)   : [0,1,3,4,8,11,18,23]
sorted(list comp): [0,1,3,4,8,11,18,23]
</pre>

With option `-debug` command [`build`](QuickSort/build.bat) also displays several useful informations, e.g.
- Execution environment : display *actual value* of properties, options, variables.
- Conditional processing : compile *only if* target is older than sources.
- Execution transparency : the executed console command with options and arguments can be *copied/run separately*.
<pre style="font-size:80%;">
<b>&gt; <a href="QuickSort/build.bat">build</a> -debug run</b>
[build] Properties : _PACKAGE_NAME=Factorial
[build] Options    : _TIMER=0 _VERBOSE=0
[build] Subcommands: _CLEAN=0 _COMPILE=1 _DOC=0 _LINT=0 _RUN=1 _TEST=0
[build] Variables  : "CABAL_DIR=%APPDATA%\cabal"
[build] Variables  : "GHC_HOME=C:\opt\ghc-8.10.7"
[build] 00000000000000 Target : "H:\examples\Factorial\target\Factorial.exe"
[build] 20210208190257 Sources: "H:\examples\Factorial\app\*.hs"
[build] _ACTION_REQUIRED=1
[build] "C:\opt\ghc-8.10.7\bin\ghc.exe" -Wall -Wmissing-import-lists -Wincomplete-uni-patterns -Werror -hidir "H:\examples\Factorial\target\gen" -odir "H:\examples\Factorial\target\gen" -o "H:\examples\Factorial\target\Factorial.exe"  "H:\examples\Factorial\app\Main.hs"

Loaded package environment from %APPDATA%\ghc\x86_64-mingw32-8.10.7\environments\default
[1 of 1] Compiling Main             ( H:\examples\Factorial\app\Main.hs, H:\examples\Factorial\target\gen\Main.o )
Linking H:\examples\Factorial\target\Factorial.exe ...
[build] "H:\examples\Factorial\target\Factorial.exe"
factorialRec(5) =120
factorialRec2(5)=120
factorialFold(5)=120
factorialProd(5)=120
[build] _EXITCODE=0
</pre>

## <span id="footnotes">Footnotes</span>

<span id="footnote_01">[1]</span> ***build.bat*** [↩](#anchor_01)

<dl><dd>
In project <a href="./Factorial/"><code>Factorial</code></a> the batch file <a href="./Factorial/build.bat"><code>build.bat</code></a> reads several properties directly from <a href="Factorial/Factorial.cabal"><code>Factorial.cabal</code></a> if the Cabal project file is present; for instance: <code>name</code>, <code>synopsis</code>, <code>version</code> and <code>ghc_options<code>.
</dd></dl>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/May 2022* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[apache_maven_cli]: https://maven.apache.org/ref/3.8.1/maven-embedder/cli.html
[cabal]: https://www.haskell.org/cabal/
[cabal_changelog]: https://hackage.haskell.org/package/Cabal/changelog
[cabal_cli]: https://cabal.readthedocs.io/en/3.4/
[cabal_downloads]: https://www.haskell.org/cabal/download.html
[dotty_examples]: https://github.com/michelou/dotty-examples
[ghc_parser]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/parser
[graalsqueak_examples]: https://github.com/michelou/graalsqueak-examples
[haskell]: https://www.haskell.org
[kotlin_examples]: https://github.com/michelou/kotlin-examples
[llvm_examples]: https://github.com/michelou/llvm-examples
[mvn_cli]: https://maven.apache.org/ref/3.8.1/maven-embedder/cli.html
[stack_cli]: https://docs.haskellstack.org/en/stable/build_command/
[stack_userguide]: https://docs.haskellstack.org/en/stable/GUIDE/
[windows_batch_file]: https://en.wikibooks.org/wiki/Windows_Batch_Scripting
