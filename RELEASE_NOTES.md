### 0.0.41
* Adaptify.MSBuild is now a developmentDependency

### 0.0.40
* quick workaround for broken lazy adaptor

### 0.0.39
* netstandard compatibility

### 0.0.38
* OSX v2

### 0.0.37
* OSX attempt

### 0.0.36
* fixes crash for types including types defined in nested classes

### 0.0.35
* fixed lens generation for types with members - only uses record fields now.

### 0.0.34
* fixed option bug when containing IndexList/etc.

### 0.0.33
* different kind of daemon start again

### 0.0.32
* multitargeting adaptify tool

### 0.0.31
* linux daemon fixes

### 0.0.30
* fixed HashMap<_,ModelType> handling for union types
* cancellable MSBuild task

### 0.0.29
* output written to intermediate folder
* caching improvements

### 0.0.28
* preprocessor defines handled correctly
* special logic for DesignTime builds
* Adaptify.runAsync no longer blocking on FSharpChecker

### 0.0.27
* attempt at linux compat 

### 0.0.26
* better server/client implementation

### 0.0.25
* server shutdown improvements

### 0.0.24
* automatic server shutdown when memory large

### 0.0.23
* fixed VS integration problems (called before references built)
* verbose server logs everything now

### 0.0.22
* fixed locking 
* cooperative server shutdown

### 0.0.21
* normalized (full) paths in all project infos

### 0.0.20
* fixed embarrassing relative path error

### 0.0.19
* fixed folder creation

### 0.0.18
* fixed IPC lock

### 0.0.17
* fixed cache files
* added --killserver flag

### 0.0.16
* fixed server exit

### 0.0.15
* IPC improved

### 0.0.14
* fixed tool

### 0.0.13
* server mode working

### 0.0.12
* experimental server mode

### 0.0.11
* fixed shadowing problems with system type names (Object)
* added option (GenerateLenses) to generate aether-compatible lens extensions

### 0.0.10
* shared code between tool and MSBuild
* improved logging
* dotnet tool "adaptify"

### 0.0.9
* fixed compilation for netframework projects
* added clean/rebuild target

### 0.0.8
* fixed caching

### 0.0.7
* building with FSharp.Core 4.6.2 for backwards compat

### 0.0.6
* update FSharp.Data.Adaptive to 0.0.16
* fixed Unpersist members (no longer on Union-Case-Types)

### 0.0.5
* updated FSharp.Data.Adaptive
* removed ChangeableModelMap/List
* improved warning handling (also reported when cached)

### 0.0.4
* updated FSharp.Data.Adaptive

### 0.0.3
* updated FSharp.Data.Adaptive
* improved CList/CMap diffing

### 0.0.2
* generator rewrite

### 0.0.1
* initial version
